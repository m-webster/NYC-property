library(tidyverse)
library(lubridate)
library(caret)
library(fs)

# read raw data from csv file
raw <- read_csv(path(getwd(),'data/nyc-rolling-sales.csv'))

# sales will be used to clean and manipulate data
sales <- raw

# number of rows in raw data
nrow(sales)

# raw data has all caps column names, with spaces in between words - hard to use
# select relevant cols and turn col names to lowecase, with no spaces
sales <- sales %>% 
  select(borough = `BOROUGH`,
         neighbourhood = `NEIGHBORHOOD`,
         address = ADDRESS,
         apartment_number = `APARTMENT NUMBER`,
         block = BLOCK,
         lot = LOT,
         zip_code = `ZIP CODE`,
         units_res = `RESIDENTIAL UNITS`,
         units_comm = `COMMERCIAL UNITS`,
         units_total = `TOTAL UNITS`,
         land_area = `LAND SQUARE FEET`,
         gross_area = `GROSS SQUARE FEET`,
         year_built = `YEAR BUILT`,
         tax_class = `TAX CLASS AT TIME OF SALE`,
         bldg_category = `BUILDING CLASS CATEGORY`,
         building_class = `BUILDING CLASS AT TIME OF SALE`,
         sale_price = `SALE PRICE`,
         sale_date = `SALE DATE`
  ) 

# convert the following columns into numerics
numerics = c('land_area','gross_area','sale_price')
for(x in numerics){
  sales[[x]] <- as.numeric(sales[[x]])
  sales[[x]][is.na(sales[[x]])] <-0
}

# sale prices need to not be NA or zero
sales <- sales %>% 
  filter(`sale_price` > 0) 
nrow(sales)

#tax_classes <- read_csv(path(getwd(),'data/tax_classes.csv'))

# got building classes from the NYC website. 
# These are quite useful as they are more descriptive than existing column
# classified them manually into the following categories
# Condos - seem to be individual units
# Family Homes - houses with single or multiple families
# Rentals  - seem to be largely whole buildings of rental apartments
# Commercial - industrial/warehouse/retail
# Offices - office buildings
# Other - parks, parking, and unclassified

building_classes <- read_csv(path(getwd(),'data/building_classes.csv'))

# left join building classes to sales database
sales <- sales %>% 
  left_join(building_classes, by='building_class') 

# let's look at a summary by building class
# notice that there are a large proportion of entries which have zero land_area or gross_area 
# particularly for condos
# most other property classes have land/gross areas set
# these apply building by building not individual units
bldg_class_summ <- sales %>% 
  group_by(building_class,building_class) %>% 
  summarize(description=min(building_class_descr),
            dataset=min(dataset),
            n=n(),
            land_area= mean(land_area), 
            land_area_zero = mean(land_area==0),
            gross_area=mean(gross_area),
            gross_area_zero=mean(gross_area==0),
            avg_sale_price=mean(sale_price))

# get rid of duplicate rows - where all the following are the same: address,block,lot,land_area,gross_area,sale_date,sale_price
duplicate <- sales %>% select(address,block,lot,land_area,gross_area,sale_date,sale_price) %>% duplicated()
sum(duplicate)
sales <- sales %>% mutate(duplicate = ifelse(duplicate,1,0))
sales <- sales %>% filter(duplicate==0) 
nrow(sales)

# get rid of rows which are the same, but one doesn't have area details
duplicate <- sales %>% select(address,block,lot,sale_date,sale_price) %>% duplicated()

sales <- sales %>% mutate(duplicate = ifelse(duplicate & land_area==0 & gross_area==0,1,0))
# how many duplicate rows
sum(sales$duplicate)
# filter out duplicates
sales <- sales %>% filter(duplicate==0) 
nrow(sales)

# condos don't need area details other types do...
# If a rental doesn't have area details, move to Condos
sales$dataset[sales$dataset == 'Rentals' & sales$land_area==0 & sales$gross_area==0] <- "Condos"

# Unless a condo, need area details - delete rows without any area information
sales <- sales %>% 
  filter(dataset == 'Condos' | land_area!=0 & gross_area!=0)
nrow(sales)

# if no gsf, then the block is vacant
zero_gsf = sales$dataset %in% c('Commercial','Family Homes','Office','Rentals') & sales$gross_area==0
sum(zero_gsf)

# analyse address
# addresses <- sales %>% group_by(address) %>% summarize(address_count = n()) %>% arrange(desc(n)) 
# sales <- sales %>% left_join(addresses, by=address) 
# 
# sales <- sales %>% 
#   mutate(appt_no = str_extract(address,", (.*)")) %>%
#   mutate(appt_no = substr(appt_no,3,length(appt_no))) %>% 
#   mutate(apartment_number = ifelse(is.na(appt_no),apartment_number,appt_no))
# 
# apt_nos <- sales %>% group_by(apartment_number) %>% summarize(n=n(),len = str_length(min(apartment_number)))

# get rid of any properties sold for under $50,000 - such transactions seem not to be at a commercial rate, but "peppercorn" consideration
sales <- sales %>% 
  filter(`sale_price` > 50000) 
#nrow(sales)

# get rid of any properties sold for over $100m - these tend to be outliers
sales <- sales %>% 
  filter(`sale_price` < 1e8) 
#nrow(sales)

# let's look at some summary data by category
dataset_summary <- sales %>% group_by(dataset) %>% summarize(count=n(),avg=mean(sale_price),max=max(sale_price),min=min(sale_price),sd=sd(sale_price))

# group by sale month & week
sales <- sales %>% 
  mutate(sale_month = round_date(sale_date,'month'),
         sale_week = round_date(sale_date,'week'))

# perc_error - function to calculate mean square % error
# seems to be an appropriate metric for fitting models
perc_error <- function(true_ratings, predicted_ratings){
  adj_ratings <- ifelse(true_ratings==0,1,true_ratings)
  sqrt(mean(((true_ratings-predicted_ratings)/adj_ratings)^2))
}

# RMSE - function to calculate mean square error
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings-predicted_ratings)^2))
}

# this function fits the data using various models - eg "lm", "knn" etc
fit_data <- function(category,model){
  # sales_mod - limit data to the specific category
  sales_mod <- sales %>% filter(dataset==category)
  
  # turn the following columns into binary ones
  factor_cols <- c('neighbourhood','bldg_category')
  
  # start with the following predictors
  X <- sales_mod %>% select(land_area,gross_area,year_built,unit)
  
  for(x in factor_cols){
    # get the names of the possible values of the category
    category_names <- sort(unique(sales_mod[[x]]))
    # create a column for each possible value
    for(y in category_names){
      # 1 if the value matches, 0 otherwise
      X[[(paste(x,y,sep="_"))]] = (sales_mod[[x]] == y)
    }
  }
  
  
  # add the sales price as the Y to be predicted
  X$Y <- sales_mod$sale_price
  
  # check that all columns have been added
  names(X)
  
  # partition into Test and Train Data
  set.seed(11)
  test_index <- createDataPartition(y = X$Y, times = 1, p = 0.2, list = FALSE)
  train_set <- as(slice(X,-test_index),"matrix")
  test_set <- as(slice(X,test_index),"matrix")
  
  nrow(train_set)
  rowlength <- ncol(test_set)    

  # fit the training data to the required model
  fit <- train(Y ~ ., method = model, data = train_set)
  
  # calculate the Y_hat values for the test set
  Y_hat <- predict(fit,test_set)
  # return percentage error of predicted values compared to the actual values of the test set 
  perc_error(Y_hat,test_set[,rowlength])
}


# this function generalises the method used in the movielens project
fit_data_iterative_means <- function(category,verbose=FALSE){
  # sales_mod - select only the data from the requried category
  sales_mod <- sales %>% filter(dataset == category)
  
  # create bins for area predictors
  # condos don't have area data
  if(category != "Condos"){
    # area predictors are gross_area and land_area
    area_types <- c('gross_area','land_area')
    for (x in area_types){
      
      if(verbose){
        # plot the distribution of areas 
        p <- sales_mod %>%
          ggplot(aes(gross_area,sale_price)) +
          geom_point() +
          scale_x_log10()+
          scale_y_log10()
        print(p)
      }

      # use log scale for areas
      # max/min of the range in log terms - add 1 to avoid taking a log of zero
      area_max <- ceiling(log10(max(1+sales_mod[[x]])))
      area_min <- floor(log10(min(1+sales_mod[[x]])))
      # create 50 bins - calculate stepsize
      area_stepsize <- (area_max - area_min)/50
      # create category_gross_area or category_land_area
      sales_mod[[paste(x,"category",sep="_")]] = ceiling((log10(sales_mod[[x]]) - area_min)/area_stepsize)
    }

  } else {
    # handle condos which don't have area information
    sales_mod <- sales_mod %>% mutate(gross_area_category = 1,land_area_category = 1)
  }
  
  # use logs of sales prices
  # save the sale_price down as sale_price_orig
  sales_mod$sale_price_orig <- sales_mod$sale_price
  # set sale_price to be log10 of original sale price
  sales_mod$sale_price <- log10(sales_mod$sale_price)
  
  # create test and train data
  set.seed(11)
  test_index <- createDataPartition(y = sales_mod$sale_price, times = 1, p = 0.2, list = FALSE)
  train_set <- sales_mod[-test_index,]
  test_set <- sales_mod[test_index,]
  
  # generate means by category
  category_means <- function(x){
    #for debugging:
    #browser()
    #print(x)
    # check the x argument is a valid category name
    if(!is.character(x) | str_length(x) == 0){
      return(data.frame(x="", avg=1e8))
    } 
    # calculate the mean of the residual by each category value
    temp <- residuals %>% 
      filter(!is.na(residuals[[x]])) %>% 
      group_by(residuals[[x]]) %>% 
      summarize(avg = mean(residual))
    # make more useful column names 
    colnames(temp) <- c(x,'avg')
    # return temp
    temp
  }
  
  # estimate residual based on the mean by category value
  category_fit <- function(x,means,data){
    # subsitute means calculated by the category_means function
    pred <- data %>% left_join(means, by=x) %>% 
      mutate(pred = avg) %>%
      .$pred
    # don't want N/A values - set to zero
    pred[is.na(pred)] <- 0
    pred
  }
  
  # find RMSE of data fitted to category
  category_rmse <- function(x){
    category_means <- category_means(x)
    RMSE(residuals$residual,category_fit(x,category_means,residuals))
  }
  
  # make predictions based on the model
  apply_model <- function(category_order,means,data,verbose=FALSE){
    # pred is a vector with our predictions (Y_hat)
    pred <- rep(mu,nrow(data))
    # what's the RMSE if we use the overall mean - report on this if verbose==TRUE
    rmse <- RMSE(data$sale_price,pred)
    if(verbose) print(paste("Overall Mean: RMSE = ",rmse))
    # go through each category in order of importance
    for(i in seq(1:length(category_order))){
      # add the category_fit - ie mean by category value
      pred <- pred + category_fit(category_order[i],means[[i]],data)
      # what's the percentage error - report on this if verbose==TRUE
      rmse <- RMSE(data$sale_price,pred)
      if(verbose) print(paste(category_order[i],": RMSE = ",rmse))
    }
    # return predicted values
    pred
  }
  
  # get a report on the biggest misses by abs(percentage error)
  max_delta <- function(){
    max_delta <- data %>% mutate(delta = sale_price - pred) %>% arrange(desc(abs(delta))) %>% slice(1:20)
    print(max_delta)
  }
  
  # mu is the overall average sale price
  mu <- mean(train_set$sale_price)
  
  # residuals holds the categories we are looking at and the residual so far
  residuals <- train_set %>%
    mutate(residual = sale_price - mu) %>%
    select(neighbourhood,units_res,units_comm, units_total,year_built,building_class,bldg_category,sale_month, gross_area_category, land_area_category,residual) 
  
  # categories we will look at in making predictions
  categories <- names(residuals)
  # chop off the residual column
  categories <- categories[1:(length(categories)-1)]
  # means holds the average by category value in dataframe format - hence a list
  means <- vector("list",length(categories))
  # category order is the order of importance for each category
  category_order <- rep("",length(categories))
  i <- 0
  last_min <- 1e8
  while(i < length(category_order)){
    i <- i + 1
    # calculate percentage errors
    rmses <- unlist(lapply(categories,category_rmse))
    #browser()
    #print(rmses)
    #print(categories[which.min(rmses)])
    # print(is.infinite(min(rmses)))
    
    if(is.infinite(min(rmses))){
    #if(last_min < min(rmses)){
        
      means <- means[1:i-1]
      category_order <- category_order[1:i-1]
      i <- length(category_order) + 1
    } else {
      last_min <- min(rmses)
      # which category results in the smallest % error
      x <- categories[which.min(rmses)]
      # x is the next most important category - save this into category_order
      category_order[i] <- x
      # calculate the means by category value, save it into our means list
      means[[i]] <- category_means(x)
      # subtract off the category means from residuals
      pred <- category_fit(x,category_means(x),residuals)
      residuals$residual <- residuals$residual - pred
      # remove x from the remaining categories to be prioritised
      categories <- categories[which(categories != x)]
    }

  }
  
  if(verbose) print(category_order) 
  
  # this is applying the model to the train data
  # pred <- apply_model(category_order,means,train_set)
  # pred <- 10^pred
  # delta <- train_set %>% mutate(delta = sale_price_orig - pred) %>% arrange(desc(abs(delta)))
  # perc_error(pred,train_set$sale_price_orig)
  
  # apply the model to the test data
  pred <- apply_model(category_order,means,test_set)
  # revers taking log10 for predicted values
  pred <- 10^pred
  # calculate largest errors
  delta <- test_set %>% mutate(delta = sale_price_orig - pred) %>% arrange(desc(abs(delta)))
  # return percentage error
  perc_error(pred,test_set$sale_price_orig)
}

# data visualisation - by category
analyse_dataset <- function(category){
  sales_mod <- sales %>% filter(dataset==category)
  # how many rows left
  nrow(sales_mod)
  
  
  # distribution of prices
  sales_mod %>% 
    ggplot(aes(sale_price)) + 
    geom_histogram(color='black') +
    scale_x_log10() 
  
  # which cols to turn into factors
  factor_cols <- c('building_class','bldg_category','neighbourhood')
  
  # turn columns into factors, ranked by median price
  
  for(x in factor_cols){
    mytable <- sales_mod %>% 
      group_by(sales_mod[[x]]) %>% 
      summarize(avg = median(sale_price), count = n()) %>%
      arrange(avg)
    sales_mod[[x]] <- factor(sales_mod[[x]],mytable[[1]])
  }
  
  #str(sales_mod)
  
  #analyse factor cols using boxplot
  for(x in factor_cols){
    p <- sales_mod %>% ggplot(aes(sales_mod[[x]],sale_price)) +
      geom_boxplot() +
      scale_y_log10() +
      xlab(x) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    print(p)
   #plot.new()
  }
  
  # look at prices by month
  sales_mod %>%
    group_by(sale_month) %>%
    summarize(sale_price = median(sale_price)) %>%
    ggplot(aes(sale_month, sale_price)) +
    geom_line()
  
  # sales_mod %>%
  #   ggplot(aes(sale_month, sale_price)) +
  #   geom_point() + 
  #   geom_smooth()+
  #   scale_y_log10()
  
}

# iterate through each dataset
datasets = c("Family Homes","Condos","Rentals","Commercial","Office")
for(ds in datasets){
  # fit using LM
  lm <- fit_data(ds,"lm")
  print(paste(ds," lm fit :", lm))
  # fit using iterative means
  iterative_means <- fit_data_iterative_means(ds)
  print(paste(ds," iterative means fit :", iterative_means))
}