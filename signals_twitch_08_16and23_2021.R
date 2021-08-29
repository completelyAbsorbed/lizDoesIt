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
sum(train_20$days_spread == 17) # 17

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

# test out timing pattern

elapsed_time <- system.time(
  for(index in seq(250000)){
  aaa <- index + 1
})

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
end_date_candidate <- unique(candidate_data$date_end)
for(el_end in end_date_candidate){
  for(el_symbol in symbols_candidate){
    which_start_match_end <- candidate_data$date_start == el_end
    which_symbol <- candidate_data$symbol == el_symbol
    index_combined <- which(which_start_match_end & which_symbol)
    if(length(index_combined) > 0){
      # non-empty, proceed
      if(length(index_combined) > 1){
        print("something went wrong in target construction! 8384yu1983")
      }else{
        which_el_end <- candidate_data$date_end == el_end
        match_el_end <- which(which_symbol & which_el_end)
        candidate_data$target[index_combined] <- candidate_data$pricing_return[match_el_end] 
      }
    }
  }
}
moc# dispose of NA target rows

# censure pricing_return

# censure volume_return (maybe)

# censure target (same rule as pricing_return)

# normalize each column, including target (0,1)

# save our transformation operations (for test trans)

# drop irrelevant columns
