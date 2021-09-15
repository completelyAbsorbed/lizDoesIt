###### work yahoo data

library(lubridate)

### definitions

# data_directory <- "C:/Users/liz/iCloudDrive/data/numerai"
data_directory <- "C:/Users/liz/OneDrive/Documents"

filename_yahoo_finance <- "yahoo_writeout.csv"
train_20_filename <- "train_20.csv"
test_20_filename <- "test_20.csv"
train_volume_20_filename <- "train_volume_20.csv"
test_volume_20_filename <- "test_volume_20.csv"


### load data 

setwd(data_directory)

train_20 <- read.csv(file = train_20_filename)
test_20 <- read.csv(file = test_20_filename)
train_volume_20 <- read.csv(file = train_volume_20_filename)
test_volume_20 <- read.csv(file = test_volume_20_filename)

### set up for training

# take a peek
head(train_20)
head(test_20) # repeated value in target_20d
head(train_volume_20)
head(test_volume_20) # repeated value in volume_20d

tail(train_20) # 0s in same-day target_20day
tail(test_20) # 0s in same-day target_20day
tail(train_volume_20) # 0s in same-day volume_20day
tail(test_volume_20) # 0s in same-day volume_20day

summary(train_20$target_20d)
summary(test_20$target_20d)
summary(train_volume_20$volume_20d)
summary(test_volume_20$volume_20d) # looks like raw volume instead of returns

dim(train_20) # looks good
dim(test_20) # looks good
dim(train_volume_20) # looks good
dim(test_volume_20) # looks good

length(unique(train_20$target_20d))
length(unique(test_20$target_20d))
length(unique(train_volume_20$volume_20d))
length(unique(test_volume_20$volume_20d)) 

# construct day_spread column, then cut to 20-day only

get_day_spread <- function(from_date,
                           to_date){
  day_spread <- as.numeric(as.Date(to_date) - as.Date(from_date))
  
  return(day_spread)
}

train_20$days_spread <- get_day_spread(from_date = train_20$date_start,
                                       to_date = train_20$date_end)

# examine summary

summary(train_20$days_spread) # almost all 20, max = 22
sum(train_20$days_spread == 19) # 1231 cases
sum(train_20$days_spread == 18) # 1205
sum(train_20$days_spread == 17) # 57

# cut down to 18 <= days_spread
dim_previous <- dim(train_20)
index_long_enough <- which(train_20$days_spread >= 18)
train_20 <- train_20[index_long_enough,]
dim_new <- dim(train_20)

dim_previous
dim_new # lost ~14k out of ~903k

### make days_spread for the rest 
test_20$days_spread <- get_day_spread(from_date = test_20$date_start,
                                       to_date = test_20$date_end)
# cut down to 18 <= days_spread
index_long_enough <- which(test_20$days_spread >= 18)
test_20 <- test_20[index_long_enough,]
# 
train_volume_20$days_spread <- get_day_spread(from_date = train_volume_20$date_start,
                                       to_date = train_volume_20$date_end)
# cut down to 18 <= days_spread
index_long_enough <- which(train_volume_20$days_spread >= 18)
train_volume_20 <- train_volume_20[index_long_enough,]
# 
test_volume_20$days_spread <- get_day_spread(from_date = test_volume_20$date_start,
                                       to_date = test_volume_20$date_end)
# cut down to 18 <= days_spread
index_long_enough <- which(test_volume_20$days_spread >= 18)
test_volume_20 <- test_volume_20[index_long_enough,]
# 


summary(test_20$days_spread) # 
summary(train_volume_20$days_spread) # 
summary(test_volume_20$days_spread) # all 3 look fine

dim(test_20)
dim(train_volume_20)
dim(test_volume_20)

################################################################################
# train a model! going with gradient boosted machines ("gbm")
# https://cran.r-project.org/web/packages/gbm/gbm.pdf

library(caret)
library(gbm)



# a reminder that test and train right now are 3 years each, early in our data

# inputs : month_from, 
#          day_from, 
#          month_to, 
#          day_to, 
#          pricing_return, 
#          volume_return
# 
# target : (future pricing) target_20d

# transformation schematic : 
#                             derive month and day for _from, _to
#                             glue in pricing_return
#                             glue in volume_return
#                             glue in target (20d pricing return)
#                             dispose of NA target rows
#                             censure pricing_return
#                             censure volume_return (maybe)
#                             censure target (same rule as pricing_return)
#                             normalize each column, including target (0,1)
#                             save our transformation operations (for test trans)
#                             drop irrelevant columns
#                             (apply transformation to test)

# remove rows with NA volume
index_NA_rows <- which(is.na(train_volume_20$volume_20d))
train_20 <- train_20[-1 * index_NA_rows,]
train_volume_20 <- train_volume_20[-1 * index_NA_rows,]
b
index_NA_rows <- which(is.na(test_volume_20$volume_20d))
test_20 <- test_20[-1 * index_NA_rows,]
test_volume_20 <- test_volume_20[-1 * index_NA_rows,]

# censure reconnaissance 
summary(train_20$target_20d)
summary(train_volume_20$volume_20d)

# what portion of various returns are >5? >2? >1?
sum(train_20$target_20d > 5) / length(train_20$target_20d)
sum(train_20$target_20d > 2) / length(train_20$target_20d)
sum(train_20$target_20d > 1) / length(train_20$target_20d)

sum(train_volume_20$volume_20d > 5) / length(train_volume_20$volume_20d)
sum(train_volume_20$volume_20d > 2) / length(train_volume_20$volume_20d)
sum(train_volume_20$volume_20d > 1) / length(train_volume_20$volume_20d)
sum(train_volume_20$volume_20d > 15) / length(train_volume_20$volume_20d)
sum(train_volume_20$volume_20d > 50) / length(train_volume_20$volume_20d)
sum(train_volume_20$volume_20d > 100) / length(train_volume_20$volume_20d)
sum(train_volume_20$volume_20d > 1000) / length(train_volume_20$volume_20d)

# setting censure points at :
#                               pricing_return > 1
#                               volume_return > 50


summary(test_20$target_20d)
summary(test_volume_20$volume_20d)

################################################################################

candidate_data <- train_20
candidate_data$volume_20d <- train_volume_20$volume_20d

# derive month and day for _from, _to
month_to <- month(candidate_data$date_end)
day_to <- day(candidate_data$date_end)
month_from <- month(candidate_data$date_start)
day_from <- day(candidate_data$date_start)

candidate_data <- cbind(candidate_data,
                        month_to,
                        day_to,
                        month_from,
                        day_from)
# rename pricing_return # rename volume_return
names(candidate_data)[c(5,7)] <- c("pricing_return",
                                   "volume_return")
# glue in target (20d pricing return) by matching date_end to date_start with symbol
candidate_data$target <- rep(NA)
symbols_candidate <- unique(candidate_data$symbol)

for(el_symbol in symbols_candidate){
  indices_symbol <- which(candidate_data$symbol == el_symbol)
  candidate_symbol_data <- candidate_data[indices_symbol,]
  end_date_candidate <- unique(candidate_symbol_data$date_end)
 
  for(el_end in end_date_candidate){ 
    ### glue in the "target" taking mean() in case any tricky multiplicity
    which_date <- which(candidate_data$date_end == el_end)
    index_target <- intersect(indices_symbol, which_date)
    value_target <- mean(candidate_data[index_target,]$pricing_return)
    
    index_start_match_end_without_symbol <- 
      which(candidate_data$date_start == el_end)
    
    index_start_match_end <- intersect(indices_symbol,
                                       index_start_match_end_without_symbol)
    # check if length of index > 0 before assigning target, or get error
    if(length(index_start_match_end) == 0){
      # do nothing!
    }else{
      candidate_data[index_start_match_end,]$target <- value_target
    }
  }
}

write.csv(x = candidate_data,
          file = "candidate_data_09_06_2021.csv")

### dispose of NA target rows

sum(is.na(candidate_data$target)) 
index_na <- which(is.na(candidate_data$target))
candidate_data <- candidate_data[-1 * index_na,]
summary(candidate_data$target) 


write.csv(x = candidate_data,
          file = "candidate_data_09_07_2021.csv")

### censure pricing_return ### censure volume_return ### censure target
# setting censure points at :
#                               pricing_return > 1
#                               volume_return > 50

censor_point_pricing_return <- 1
censor_point_volume_return <- 50

index_to_censor_pricing_return <- 
  which(candidate_data$pricing_return >= censor_point_pricing_return)
index_to_censor_target <- 
  which(candidate_data$target >= censor_point_pricing_return)
index_to_censor_volume_return <- 
  which(candidate_data$volume_return >= censor_point_volume_return)

candidate_data[index_to_censor_pricing_return,]$pricing_return <- 
  censor_point_pricing_return
candidate_data[index_to_censor_target,]$target <- 
  censor_point_pricing_return
candidate_data[index_to_censor_volume_return,]$volume_return <- 
  censor_point_volume_return


write.csv(x = candidate_data,
          file = "candidate_data_censored_09_07_2021.csv")

### drop irrelevant columns 1, 2, 3, 4, 6
index_columns_to_drop <- c(1, 2, 3, 4, 6)
cleaned_data <- candidate_data[, -1 * index_columns_to_drop]

### normalize each column, including target (0,1) ### save our transformation 
normalize <- function(input_vector,
                      min_override = NA,
                      max_override = NA){
  if(!(is.na(min_override))){
    min_input <- min_override
  }else{
    min_input <- min(input_vector)
  }
  
  if(!(is.na(max_override))){
    max_factor <- max_override - min_input
  }else{
    max_factor <- max(input_vector) - min_input
  }
  
  output_vector <- (input_vector - min_input) / max_factor
  
  return(output_vector)
}

# set up transformation save, do transformation
column_name <- names(cleaned_data)
normalize_minimum <- vector()
normalize_maximum <- vector()

for(column in column_name){
  normalizing_vector <- cleaned_data[, column]
  column_minimum <- min(normalizing_vector)
  column_maximum <- max(normalizing_vector)
  
  normalizing_vector <- normalize(input_vector = normalizing_vector,
                                  min_override = column_minimum,
                                  max_override = column_maximum)
  
  normalize_minimum <- c(normalize_minimum,
                         column_minimum)
  normalize_maximum <- c(normalize_maximum,
                         column_maximum)
  
  cleaned_data[, column] <- normalizing_vector
}

df_transform <- data.frame(column_name,
                           normalize_minimum,
                           normalize_maximum)

write.csv(x = cleaned_data,
          file = "cleaned_data_09_07_2021.csv")


################################################################################

### train a model!

interaction_depth <- 375

set.seed(10)
model_cv_10 <- gbm(target~.,
                   data = cleaned_data,
                   n.trees = 35,
                   shrinkage = 0.01,
                   interaction.depth = interaction_depth,
                   train.fraction = 1,
                   cv.folds = 10,
                   bag.fraction = 0.1,
                   keep.data = FALSE, 
                   verbose = T)
summary(model_cv_10$train.error)


set.seed(10)
model_cv_100 <- gbm(target~.,
                    data = cleaned_data,
                    n.trees = 35,
                    shrinkage = 0.01,
                    interaction.depth = interaction_depth,
                    train.fraction = 1,
                    cv.folds = 100,
                    bag.fraction = 0.1,
                    keep.data = FALSE, 
                    verbose = T)

summary(model_cv_10$train.error)
summary(model_cv_100$train.error) # nearly identical

################################################################################
### process the test data, evaluate model_cv_10 and model_cv_100 on it
################################################################################

# candidate_test_data <- test_20
# candidate_test_data$volume_20d <- test_volume_20$volume_20d
# 
# # derive month and day for _from, _to
# month_to <- month(candidate_test_data$date_end)
# day_to <- day(candidate_test_data$date_end)
# month_from <- month(candidate_test_data$date_start)
# day_from <- day(candidate_test_data$date_start)
# 
# candidate_test_data <- cbind(candidate_test_data,
#                              month_to,
#                              day_to,
#                              month_from,
#                              day_from)
# # rename pricing_return # rename volume_return
# names(candidate_test_data)[c(5,7)] <- c("pricing_return",
#                                         "volume_return")
# # glue in target (20d pricing return) by matching date_end to date_start with symbol
# candidate_test_data$target <- rep(NA)
# symbols_candidate <- unique(candidate_test_data$symbol)
# 
# for(el_symbol in symbols_candidate){
#   indices_symbol <- which(candidate_test_data$symbol == el_symbol)
#   candidate_symbol_data <- candidate_test_data[indices_symbol,]
#   end_date_candidate <- unique(candidate_symbol_data$date_end)
#   
#   for(el_end in end_date_candidate){ 
#     ### glue in the "target" taking mean() in case any tricky multiplicity
#     which_date <- which(candidate_test_data$date_end == el_end)
#     index_target <- intersect(indices_symbol, which_date)
#     value_target <- mean(candidate_test_data[index_target,]$pricing_return)
#     
#     index_start_match_end_without_symbol <- 
#       which(candidate_test_data$date_start == el_end)
#     
#     index_start_match_end <- intersect(indices_symbol,
#                                        index_start_match_end_without_symbol)
#     # check if length of index > 0 before assigning target, or get error
#     if(length(index_start_match_end) == 0){
#       # do nothing!
#     }else{
#       candidate_test_data[index_start_match_end,]$target <- value_target
#     }
#   }
# }
# 
# write.csv(x = candidate_test_data,
#           file = "candidate_test_data_09_06_2021.csv")
# 
# ### dispose of NA target rows
# 
# sum(is.na(candidate_test_data$target)) # 
# index_na <- which(is.na(candidate_test_data$target))
# candidate_test_data <- candidate_test_data[-1 * index_na,]
# summary(candidate_test_data$target) 
# 
# ### censure pricing_return ### censure volume_return ### censure target
# # setting censure points at :
# #                               pricing_return > 1
# #                               volume_return > 50
# 
# censor_point_pricing_return <- 1
# censor_point_volume_return <- 50
# 
# index_to_censor_pricing_return <- 
#   which(candidate_test_data$pricing_return >= censor_point_pricing_return)
# index_to_censor_target <- 
#   which(candidate_test_data$target >= censor_point_pricing_return)
# index_to_censor_volume_return <- 
#   which(candidate_test_data$volume_return >= censor_point_volume_return)
# 
# candidate_test_data[index_to_censor_pricing_return,]$pricing_return <- 
#   censor_point_pricing_return
# candidate_test_data[index_to_censor_target,]$target <- 
#   censor_point_pricing_return
# candidate_test_data[index_to_censor_volume_return,]$volume_return <- 
#   censor_point_volume_return
# 
# 
# write.csv(x = candidate_test_data,
#           file = "candidate_test_data_censored_09_09_2021.csv")
# 
# ### drop irrelevant columns 1, 2, 3, 4, 6
# 
# index_columns_to_drop <- c(1, 2, 3, 4, 6)
# cleaned_test_data <- candidate_test_data[, -1 * index_columns_to_drop]
# 
# ### normalize each column, including target (0,1)
# 
# 
# # using saved transformation, do transformation
# # 
# # df_transform
# 
# column_name <- names(cleaned_test_data)
# 
# for(column in column_name){
#   normalizing_vector <- cleaned_test_data[, column]
#   
#   which_row_column <- which(df_transform$column_name == column)
#   
#   column_minimum <- df_transform[which_row_column,]$normalize_minimum
#   column_maximum <- df_transform[which_row_column,]$normalize_maximum
#   
#   normalizing_vector <- normalize(input_vector = normalizing_vector,
#                                   min_override = column_minimum,
#                                   max_override = column_maximum)
#   
#   cleaned_test_data[, column] <- normalizing_vector
# }
# 
# summary(cleaned_test_data)

################################################################################
### predict

chunk2 <- function(x,n) { # https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks-in-r
  split(x, 
        cut(seq_along(x), 
            n, 
            labels = FALSE)) 
}

height <- dim(cleaned_test_data)[1]
chunk_size <- 100
chunks <- chunk2(1:height,
                 chunk_size)
pr <- vector()
predictions_cv_10 <- vector()

index <- 0

for(chunk in chunks){
  index <- index + 1
  print(index / chunk_size)
  
  block_time <- system.time({
    pr <- predict.gbm(model_cv_10, 
                      cleaned_test_data[as.vector(chunk),])
  })
  
  predictions_cv_10 <- c(predictions_cv_10,
                         pr)
  
  print(block_time)
}

###


height <- dim(cleaned_test_data)[1]
chunk_size <- 100
chunks <- chunk2(1:height,
                 chunk_size)
pr <- vector()
predictions_cv_100 <- vector()

index <- 0

for(chunk in chunks){
  index <- index + 1
  print(index / chunk_size)
  
  block_time <- system.time({
    pr <- predict.gbm(model_cv_100, 
                      cleaned_test_data[as.vector(chunk),])
  })
  
  predictions_cv_100 <- c(predictions_cv_100,
                          pr)
  
  print(block_time)
}

###

cor(predictions_cv_10,
    predictions_cv_100)

cor(cleaned_test_data$target,
    predictions_cv_100)

cor(predictions_cv_10,
    cleaned_test_data$target)

# cor by interaction depth ; 100; 10
# interaction.depth = 7 ; ; -0.09655698; -0.1015409
# interaction.depth = 6 ; ;
# interaction.depth = 5 ; ;
# interaction.depth = 4 ; ;
# interaction.depth = 3 ; ;
# interaction.depth = 2 ; ;
# interaction.depth = 1 ; ;

# predictions_cv_10_i_7 <- predictions_cv_10
# predictions_cv_100_i_7 <- predictions_cv_100
# model_cv_10_i_7 <- model_cv_10
# model_cv_100_i_7 <- model_cv_100

# predictions_cv_10_i_6 <- predictions_cv_10
# predictions_cv_100_i_6 <- predictions_cv_100
# model_cv_10_i_6 <- model_cv_10
# model_cv_100_i_6 <- model_cv_100

# predictions_cv_10_i_5 <- predictions_cv_10
# predictions_cv_100_i_5 <- predictions_cv_100
# model_cv_10_i_5 <- model_cv_10
# model_cv_100_i_5 <- model_cv_100

# predictions_cv_10_i_4 <- predictions_cv_10
# predictions_cv_100_i_4 <- predictions_cv_100
# model_cv_10_i_4 <- model_cv_10
# model_cv_100_i_4 <- model_cv_100


# predictions_cv_10_i_3 <- predictions_cv_10
# predictions_cv_100_i_3 <- predictions_cv_100
# model_cv_10_i_3 <- model_cv_10
# model_cv_100_i_3 <- model_cv_100
 
# predictions_cv_10_i_2 <- predictions_cv_10
# predictions_cv_100_i_2 <- predictions_cv_100
# model_cv_10_i_2 <- model_cv_10
# model_cv_100_i_2 <- model_cv_100
 
# predictions_cv_10_i_1 <- predictions_cv_10
# predictions_cv_100_i_1 <- predictions_cv_100
# model_cv_10_i_1 <- model_cv_10
# model_cv_100_i_1 <- model_cv_100

# predictions_cv_10_i_15 <- predictions_cv_10
# predictions_cv_100_i_15 <- predictions_cv_100
# model_cv_10_i_15 <- model_cv_10
# model_cv_100_i_15 <- model_cv_100

# predictions_cv_10_i_75 <- predictions_cv_10
# predictions_cv_100_i_75 <- predictions_cv_100
# model_cv_10_i_75 <- model_cv_10
# model_cv_100_i_75 <- model_cv_100

predictions_cv_10_i_375 <- predictions_cv_10
predictions_cv_100_i_375 <- predictions_cv_100
model_cv_10_i_375 <- model_cv_10
model_cv_100_i_375 <- model_cv_100

predictions_df <- data.frame(predictions_cv_10_i_1,
                             predictions_cv_100_i_1,
                             predictions_cv_10_i_2,
                             predictions_cv_100_i_2,
                             predictions_cv_10_i_3,
                             predictions_cv_100_i_3,
                             predictions_cv_10_i_4,
                             predictions_cv_100_i_4,
                             predictions_cv_10_i_5,
                             predictions_cv_100_i_5,
                             predictions_cv_10_i_6,
                             predictions_cv_100_i_6,
                             predictions_cv_10_i_7,
                             predictions_cv_100_i_7,
                             predictions_cv_10_i_15,
                             predictions_cv_100_i_15,
                             predictions_cv_10_i_75,
                             predictions_cv_100_i_75,
                             predictions_cv_10_i_375,
                             predictions_cv_100_i_375)

cor(predictions_df)

cor(predictions_df,
    cleaned_test_data$target)

cor(predictions_df[, 11:20])

cor(cleaned_test_data$target,
    predictions_df[,1]+predictions_df[,15])
 # 5 or something is broke

# save models

save(model_cv_10_i_1,
     model_cv_100_i_1, # = -1 * model_B
     model_cv_10_i_2,
     model_cv_100_i_2,
     model_cv_10_i_3,
     model_cv_100_i_3,
     model_cv_10_i_4,
     model_cv_100_i_4,
     model_cv_10_i_5,
     model_cv_100_i_5,
     model_cv_10_i_6,
     model_cv_100_i_6,
     model_cv_10_i_7, # = -1 * model_A
     model_cv_100_i_7,
     model_cv_10_i_15,
     model_cv_100_i_15,
     model_cv_10_i_75,
     model_cv_100_i_75,
     model_cv_10_i_375,
     model_cv_100_i_375,
     file = "signals.models")

# model_C = (1/2) * (model_A + model_B)

model_A_negative <- model_cv_10_i_7
model_B_negative <- model_cv_100_i_1

save(model_A_negative,
     model_B_negative,
     file = "negative_signals_selected.models")

################################################################################
# 
# make live predictions!
# 
################################################################################
## 1 ## scrape yahoo, updating our historical database
## 2 ## subset data (using ticker universe provided by numerai) 
## 3 ## process the data into : 
##   ##     pricing_return, volume_return, month_to, day_to, month_from, day_from
##   ##     (no target)
## 4 ## censor the data; pricing_return > 1, volume_return > 50
## 5 ## normalize the data
## 6 ## load model negatives
## 7 ## make predictions * -1
## 8 ## make averaged predictions ("model_C")
## 9 ## submit predictions
################################################################################


## 1 ## scrape yahoo, updating our historical database

## 2 ## subset data (using ticker universe provided by numerai) 

## 3 ## process the data into : 
##   ##     pricing_return, volume_return, month_to, day_to, month_from, day_from
##   ##     (no target)

## 4 ## censor the data; pricing_return > 1, volume_return > 50

## 5 ## normalize the data

## 6 ## load model negatives

## 7 ## make predictions * -1

## 8 ## make averaged predictions ("model_C")

## 9 ## submit predictions




