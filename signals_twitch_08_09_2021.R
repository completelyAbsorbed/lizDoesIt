###### work yahoo data

library(lubridate)

### definitions
data_directory <- "C:/Users/liz/iCloudDrive/data/numerai"
filename_yahoo_finance <- "yahoo_writeout.csv"



### load data 
# 
# took 2-3 minutes locally
price_volume <- read.csv(file = paste(data_directory,
                                      "/",
                                      filename_yahoo_finance,
                                      sep = ""))

### examine data
head(price_volume)

# X symbol       date  open  high   low close   volume adjusted
# 1 1   MLCO 2006-12-19 22.00 23.55 21.10 21.55 52517300 17.17864
# 2 2   MLCO 2006-12-20 21.99 22.00 19.81 20.23 13510700 16.12640
# 3 3   MLCO 2006-12-21 19.88 20.44 18.88 20.06 14750400 15.99089
# 4 4   MLCO 2006-12-22 20.15 20.18 19.35 19.51  3800500 15.55246
# 5 5   MLCO 2006-12-26 19.70 20.07 19.40 19.88  1749000 15.84740
# 6 6   MLCO 2006-12-27 20.00 21.00 20.00 20.70  4178400 16.50107

# X column is tracking row number, could be discarded 

# ~7.4 million rows

### date

# investigate NA dates

index_date_NA <- which(is.na(price_volume$date))
length(index_date_NA) # 4
price_volume[index_date_NA,] # look at the date NA rows

# remove date NA rows

price_volume <- price_volume[(-1 * index_date_NA),]


class(price_volume$date) # character... want to convert to date
date_formatted <- as.Date(price_volume$date)
head(date_formatted) # looks good
tail(date_formatted) # looks good
dates_unique <- unique(date_formatted)
date_max <- max(dates_unique,
                na.rm = TRUE)
date_min <- min(dates_unique,
                na.rm = TRUE)
date_max
date_min
date_max - date_min

# add date_formatted column to data.frame()b

price_volume$date_formatted <- date_formatted

### volume

index_volume_NA <- which(is.na(price_volume$volume))
length(index_volume_NA) # 3
price_volume[index_volume_NA,] # look at the volume NA rows

# remove volume NA rows

price_volume <- price_volume[(-1 * index_volume_NA),]


### pricing

## function definition

NA_remove_by_column <- function(dataset,
                                name_column,
                                print_flag = TRUE){ # needs error catching/handling
  index_NA <- which(is.na(dataset[, name_column]))
  
  if(print_flag){
    # print to console : how many NA rows were found for this column
    message <- paste("found ",
                     length(index_NA),
                     " NA rows for column name : '",
                     name_column,
                     "'",
                     sep = "")
    print(message)
  }
  
  # do nothing if length index_NA is 0
  if(length(index_NA) == 0){
    # do nothing
  }else{
    dataset <-  dataset[-1 * (index_NA),]
  }
  
  return(dataset) # return dataset with NA columns removed for that column name
}

## open
price_volume <- NA_remove_by_column(dataset = price_volume,
                                    name_column = "open")
## high
price_volume <- NA_remove_by_column(dataset = price_volume,
                                    name_column = "high")
## low
price_volume <- NA_remove_by_column(dataset = price_volume,
                                    name_column = "low")
## close
price_volume <- NA_remove_by_column(dataset = price_volume,
                                    name_column = "close")

## do a summary call on price_volume

summary(price_volume)

# X              symbol              date                open         
# Min.   :      1   Length:7387455     Length:7387455     Min.   :     0.0  
# 1st Qu.:1846870   Class :character   Class :character   1st Qu.:    14.0  
# Median :3693734   Mode  :character   Mode  :character   Median :    26.8  
# Mean   :3693736                                         Mean   :   133.1  
# 3rd Qu.:5540602                                         3rd Qu.:    49.0  
# Max.   :7387466                                         Max.   :478650.0  
# high               low               close              volume         
# Min.   :     0.0   Min.   :     0.0   Min.   :     0.0   Min.   :0.000e+00  
# 1st Qu.:    14.3   1st Qu.:    13.7   1st Qu.:    14.0   1st Qu.:1.487e+05  
# Median :    27.2   Median :    26.4   Median :    26.8   Median :4.863e+05  
# Mean   :   135.3   Mean   :   130.5   Mean   :   132.9   Mean   :2.326e+06  
# 3rd Qu.:    49.7   3rd Qu.:    48.3   3rd Qu.:    49.0   3rd Qu.:1.598e+06  
# Max.   :483525.0   Max.   :471375.0   Max.   :475500.0   Max.   :3.565e+09  
# adjusted        date_formatted      
# Min.   :     0.0   Min.   :2003-01-31  
# 1st Qu.:    12.1   1st Qu.:2009-01-16  
# Median :    23.1   Median :2014-02-05  
# Mean   :   125.4   Mean   :2013-06-28  
# 3rd Qu.:    43.8   3rd Qu.:2018-03-09  
# Max.   :475500.0   Max.   :2021-08-06  

### symbol
price_volume <- NA_remove_by_column(dataset = price_volume,
                                    name_column = "symbol")

length(unique(price_volume$symbol)) # ... unique tickers overall

### split out initial train/test 

# train : first three years, skipping 2003
train_year_condition_left <- year(price_volume$date_formatted) >= 2004
train_year_condition_right <- year(price_volume$date_formatted) <= 2006

index_train <- which(train_year_condition_left & train_year_condition_right)

train <- price_volume[index_train,]

# test : second three years, skipping 2003 and 2007
test_year_condition_left <- year(price_volume$date_formatted) >= 2008
test_year_condition_right <- year(price_volume$date_formatted) <= 2010

index_test <- which(test_year_condition_left & test_year_condition_right)

test <- price_volume[index_test,]

dim(train)
dim(test)

## construct targets (20d)

# function definition

derive_start_end_for_target <- function(dataset,
                                        days_return_target = 20,
                                        date_col_name = "date_formatted"){
  # computes for most recent day for each symbol
  
  symbols_US <- unique(dataset$symbol)
  length_symbols <- length(symbols_US)
  date_start_index <- vector()
  date_end_index <- vector()
  
  for(symbol in symbols_US){
    if(is.na(symbol)){
      # do nothing!
      print("NA, bro...")
    }else{
      # print(symbol)
      # subset from symbol
      symbol_data <- dataset[which(dataset$symbol == symbol),]
      # extract most recent date ... will be last date
      index_last <- -999
      index_first <- -999
      date_length <- length(symbol_data[,date_col_name]) # relies on knowledge of data structure
      index_last <- date_length
      date_last <- symbol_data[index_last,date_col_name]
      span_flag <- FALSE
      index_first <- index_last - 1
      if(index_first < 1){
        index_first <- 1
      }
      
      while(!span_flag){
        
        span <- symbol_data[index_last,date_col_name] - symbol_data[index_first,date_col_name]
        
        if(span == days_return_target){ # if span == days_return_target, stop, goal is  met  
          span_flag <- TRUE
        }else if(span < days_return_target){ # while span < days_return_target, decrement index_first
          index_first <- index_first - 1
          
          if(index_first < 1){
            span_flag <- TRUE
            index_first <- 1
          }
        }else if(span > days_return_target){ # if span > days_return_target, stop, goal is met 
          span_flag <- TRUE
        }else{ # this should never happen!
          print("else tree error when finding span!!!! ~")
          span_flag <- TRUE
        }
      }
      
      # build index vectors
      date_start_index <- c(date_start_index,
                            index_first)
      date_end_index <- c(date_end_index,
                          index_last)
    }
  }
  
  df_return <- data.frame(symbols_US,
                          date_start_index,
                          date_end_index)
  names(df_return) <- c("symbols",
                        "date_start_index",
                        "date_end_index")
  return(df_return)
}

calculate_return <- function(date_start,
                             date_end,
                             value_start,
                             value_end,
                             basis_length = 20){
  
  length_between_dates <- as.numeric(date_end - date_start)
  return_raw <- value_end / value_start - 1
  power_factor <- (basis_length / length_between_dates)
  # adjust return to basis_length
  return_adjusted <- ((1 + return_raw) ^ power_factor) - 1
  return(return_adjusted)
}

construct_target <- function(dataset,
                             date_column_name = "date_formatted",
                             constructor_column_name,
                             days_return = 20,
                             target_column_name){
  # derive unique dates
  dates_unique <- unique(dataset[, date_column_name])
  
  # sort unique dates
  dates_unique_sorted <- sort(dates_unique)
  
  # instantiate empty vectors and data.frame() for building
  return_sub <- vector()
  date_start_sub <- Date()
  date_end_sub <- Date()
  symbol_sub <- vector()
  
  df_target <- data.frame(symbol_sub,
                          date_start_sub,
                          date_end_sub,
                          return_sub)
  
  names(df_target) <- c("symbol",
                        "date_start",
                        "date_end",
                        target_column_name)
  
  # loop over sorted unique dates(backwards)
  for(date_sub in rev(dates_unique_sorted)){
    # subsetting data <= index date
    date_sub_index <- which(dataset[, date_column_name] <= date_sub)
    dataset_sub <- dataset[date_sub_index,]
    # # derive start end
    df_start_end_sub <- derive_start_end_for_target(dataset = dataset_sub,
                                                    days_return_target = days_return,
                                                    date_col_name = date_column_name)
    # # calculate return
    height_df_sub <- dim(df_start_end_sub)[1]
    return_sub <- vector()
    date_start_sub <- Date()
    date_end_sub <- Date()
    symbol_sub <- vector()
    
    for(index_row in seq(height_df_sub)){
      row_sub <- df_start_end_sub[index_row,]
      date_starting <- dataset_sub[row_sub$date_start_index, date_column_name]
      date_ending <- dataset_sub[row_sub$date_end_index, date_column_name]
      value_starting <- dataset_sub[row_sub$date_start_index, constructor_column_name]
      value_ending <- dataset_sub[row_sub$date_end_index, constructor_column_name]
      
      return_row <- calculate_return(date_start = date_starting,
                                     date_end = date_ending,
                                     value_start = value_starting,
                                     value_end = value_ending,
                                     basis_length = days_return)
      
      return_sub <- c(return_sub,
                      return_row)
      
      date_start_sub <- c(date_start_sub,
                          date_starting)
      
      date_end_sub <- c(date_end_sub,
                          date_ending)
      
      symbol_sub <- c(symbol_sub,
                      row_sub$symbols)
    }
    
    # # build df_target (to be returned later)
    
    df_target_sub <- data.frame(symbol_sub,
                                date_start_sub,
                                date_end_sub,
                                return_sub)
    
    names(df_target_sub) <- c("symbol",
                              "date_start",
                              "date_end",
                              target_column_name)
    
    df_target <- rbind(df_target,
                       df_target_sub)
  }
  
  return(df_target)
}

# derive targets, work around NA as appropriate after

# train$target <- rep(NA)
# test$target <- rep(NA)
  
train_20 <- construct_target(dataset = train,
                             date_column_name = "date_formatted",
                             constructor_column_name = "adjusted",
                             days_return = 20,
                             target_column_name = "target_20d")

test_20 <- construct_target(dataset = test,
                             date_column_name = "date_formatted",
                             constructor_column_name = "adjusted",
                             days_return = 20,
                             target_column_name = "target_20d")

train_volume_20 <- construct_target(dataset = train,
                             date_column_name = "date_formatted",
                             constructor_column_name = "volume",
                             days_return = 20,
                             target_column_name = "volume_20d")

test_volume_20 <- construct_target(dataset = test,
                            date_column_name = "date_formatted",
                            constructor_column_name = "volume",
                            days_return = 20,
                            target_column_name = "volume_20d")

write.csv(x = train_20,
          file = "train_20.csv")

write.csv(x = test_20,
          file = "test_20.csv")

write.csv(x = train_volume_20,
          file = "train_volume_20.csv")

write.csv(x = test_volume_20,
          file = "test_volume_20.csv")


### construct indicators 