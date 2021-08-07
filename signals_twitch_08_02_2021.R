# C:/Users/liz/OneDrive/Desktop/latest_signals_dataset_08_02_2021


file_dir <- "C:/Users/liz/OneDrive/Desktop/latest_signals_dataset_08_02_2021"

historical_targets <- read.csv(file = paste(file_dir,
                                            "/historical_targets.csv",
                                            sep = ""))

example_signals_upload <- read.csv(file = paste(file_dir,
                                                "/example_signal_upload.csv",
                                                sep = ""))

################################################################################
################################################################################
################################################################################
### Exploratory Data Analysis ### 
################################################################################
################################################################################
################################################################################

################################################################################
# Initial data analysis # 
################################################################################

dim(historical_targets) # 4386292       5
head(historical_targets)
# bloomberg_ticker friday_date data_type target target_20d
# 1        000270 KS    20030131     train   0.50       0.50
# 2        000810 KS    20030131     train   0.50       0.50
# 3        000830 KS    20030131     train   0.50       0.50
# 4        002790 KS    20030131     train   0.25       0.25
# 5        003450 KS    20030131     train   0.25       0.50
# 6        003490 KS    20030131     train   0.25       0.75
unique(historical_targets$data_type) # "train" "validation"
unique(historical_targets$target) # 0.00 0.25 0.50 0.75 1.00
unique(historical_targets$target_20d) # NA 0.00 0.25 0.50 0.75 1.00
  # hypothesis : NA's in target_20d from rows younger than 20 days
length(unique(historical_targets$bloomberg_ticker)) # 13262 unique tickers
length(unique(historical_targets$friday_date)) # 964 unique friday dates
historical_validation_index <- which(historical_targets$data_type == "validation")
length(unique(historical_targets[historical_validation_index,]$friday_date)) 
  # 446 unique validation friday dates in historical_targets



dim(example_signals_upload) # 1566409    4
head(example_signals_upload)
# numerai_ticker friday_date  data_type  signal
# 1      000060 KS    20130104 validation 0.50180
# 2      000080 KS    20130104 validation 0.49909
# 3      000100 KS    20130104 validation 0.50093
# 4      000120 KS    20130104 validation 0.50324
# 5      000210 KS    20130104 validation 0.50128
# 6      000270 KS    20130104 validation 0.50420

unique(example_signals_upload$data_type) # "validation" "live"
length(unique(example_signals_upload$numerai_ticker)) # 5258 unique tickers
length(unique(example_signals_upload$friday_date)) # 441 unique friday dates
example_validation_index <- which(example_signals_upload$data_type == "validation")
length(unique(example_signals_upload[example_validation_index,]$friday_date)) 
  # 440 unique validation friday dates in example_signals_upload
summary(example_signals_upload$signal)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 0.3119  0.5003  0.5019  0.5019  0.5038  0.6383 

################################################################################
# First Distributional Exploration # (on the training data only)
################################################################################

# looking at whole dataset distributional properties

training_index <- which(historical_targets$data_type == "train")
train <- historical_targets[training_index,]
dim(train) # 2148958    5

# test if train overlaps validation
max(train$friday_date) # 20121228
min(example_signals_upload$friday_date) # 20130104 # good! they do not overlap

# summaries of $target $target_20d
summary(train$target)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0     0.5     0.5     0.5     0.5     1.0 
summary(train$target_20d)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0     0.5     0.5     0.5     0.5     1.0 

# variance and standard deviation
var(train$target)           # 0.04997241
var(train$target) ^ 0.5     # 0.2235451
var(train$target_20d)       # 0.04983307
var(train$target_20d) ^ 0.5 # 0.2232332

cor(train$target,
    train$target_20d) # 0.3745201

cov(train$target,
    train$target_20d) # 0.01868956

################################################################################
# Second Distributional Exploration # (on a subset of the training data only)
################################################################################
# where to go from here?
#   set a smaller train
#   examine distributional qualities friday-by-friday

fridays <- unique(train$friday_date)
length(fridays) # 518
fridays_subset <- fridays[1:75]

subset_index <- which(train$friday_date %in% fridays_subset)
train_subset <- train[subset_index,]

head(train_subset) # sanity check
tail(train_subset)
head(fridays_subset)
tail(fridays_subset) # looks good

### look at mean, median, variance for each friday

# $target
target_means <- vector()
target_medians <- vector()
target_variances <- vector()

for(index_date in fridays_subset){
  friday_indices <- which(train_subset$friday_date == index_date)
  data_friday <- train_subset[friday_indices,]
  
  target_means <- c(target_means,
                    mean(data_friday$target))
  target_medians <- c(target_medians,
                      median(data_friday$target))
  target_variances <- c(target_variances,
                        var(data_friday$target))
}

plot(target_means)
plot(target_medians)
plot(target_variances)


# $target_20d
target_20d_means <- vector()
target_20d_medians <- vector()
target_20d_variances <- vector()

for(index_date in fridays_subset){
  friday_indices <- which(train_subset$friday_date == index_date)
  data_friday <- train_subset[friday_indices,]
  
  target_20d_means <- c(target_20d_means,
                    mean(data_friday$target_20d))
  target_20d_medians <- c(target_20d_medians,
                      median(data_friday$target_20d))
  target_20d_variances <- c(target_20d_variances,
                        var(data_friday$target_20d))
}

plot(target_20d_means)
plot(target_20d_medians)
plot(target_20d_variances)

plot(sort(target_20d_means))
# plot(target_20d_medians)
plot(sort(target_20d_variances))

# friday-by-friday correlation between target to target_20d

correlations <- vector()

for(index_date in fridays_subset){
  friday_indices <- which(train_subset$friday_date == index_date)
  data_friday <- train_subset[friday_indices,]
  
  correlations <- c(correlations,
                    cor(data_friday$target,
                        data_friday$target_20d))
}

plot(correlations)


################################################################################
# Rudimentary Model # momentum-based! 
################################################################################
# 
# hypothesis : target_20d is predictive of target_20d in 5 weeks
# 
# experiment structure : 
#   train : train_subset 
#   validation : {after a 10-week gap} validation_subset (next 75 fridays)

fridays <- unique(train$friday_date)
length(fridays) # 518
fridays_validation_subset <- fridays[85 + 1:75]

subset_validation_index <- which(train$friday_date %in% fridays_validation_subset)
validation_subset <- train[subset_validation_index,]

### construct target_20d_5week

# train
train_5week <- train$target_20d[5 + 1:75]
# validation 
validation_5week <- train$target_20d[90 + 1:75]

# train_subset$target_20d_5week <- train_5week
# 
# 
# Error in `$<-.data.frame`(`*tmp*`, target_20d_5week, value = c(0.75, 0.5,  : 
#                                                                  replacement has 75 rows, data has 229849
# validation_subset$target_20d_5week <- validation_5week

# above doesn't resolve cleanly due to ticker in/out

# solution : collect the target 5week ticker-by-ticker,
#             note any NA's in the 5week target column
#             remove any NA's in the 5week target column
# go friday-by-friday, ticker-by-ticker

train_subset$target_20d_5week <- rep(-888)
validation_subset$target_20d_5week <- rep(-888)

train_date_index <- 1:75
validation_date_index <- 85 + 1:75

# train data 
for(friday_index in train_date_index){
  target_index <- friday_index + 5
  
  index_this_friday <- which(train$friday_date == fridays[friday_index])
  index_in_5_fridays <- which(train$friday_date == fridays[target_index])
  tickers_friday <- unique(train[index_this_friday,]$bloomberg_ticker)
  tickers_in_5_fridays <- unique(train[index_in_5_fridays,]$bloomberg_ticker)
  
  surviving_tickers <- intersect(tickers_friday,
                                 tickers_in_5_fridays)
  
  index_surviving <- which(tickers_friday %in% surviving_tickers)
  index_survived <- which(surviving_tickers %in% tickers_friday)
  
  train_subset[index_this_friday,]$target_20d_5week[index_surviving] <- train[index_in_5_fridays,]$target_20d[index_survived]
}

summary(train_subset$target_20d_5week)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -888.00    0.25    0.50  -34.19    0.50    1.00 

# fix out the -888
train_subset_fixed <- train_subset[which(train_subset$target_20d_5week > -888),]
summary(train_subset_fixed$target_20d_5week)

# validation data 
for(friday_index in validation_date_index){
  target_index <- friday_index + 5
  
  index_this_friday <- which(train$friday_date == fridays[friday_index])
  index_in_5_fridays <- which(train$friday_date == fridays[target_index])
  tickers_friday <- unique(train[index_this_friday,]$bloomberg_ticker)
  tickers_in_5_fridays <- unique(train[index_in_5_fridays,]$bloomberg_ticker)
  
  surviving_tickers <- intersect(tickers_friday,
                                 tickers_in_5_fridays)
  
  index_surviving <- which(tickers_friday %in% surviving_tickers)
  index_survived <- which(surviving_tickers %in% tickers_friday)
  
  validation_subset[index_this_friday - 85,]$target_20d_5week[index_surviving] <- train[index_in_5_fridays,]$target_20d[index_survived]
}

summary(validation_subset$target_20d_5week)

# fix out the -888
validation_subset_fixed <- validation_subset[which(validation_subset$target_20d_5week > -888),]
summary(validation_subset_fixed$target_20d_5week)

### do we miss a significant amount of tickers each week, this way?

# train_subset
count_train_missing <- vector()

for(friday_index in train_date_index){
  friday <- fridays[friday_index]
  index_this_friday <- which(train_subset$friday_date == friday)
  index_missing <-  which(train_subset[index_this_friday,]$target_20d_5week == -888)
  number_missing <- length(train_subset[index_missing,]$target_20d_5week)
  
  count_train_missing <- c(count_train_missing,
                           number_missing)
}

plot(count_train_missing)

# validation
# count_validation_missing <- vector()
# 
# for(friday_index in validation_date_index){
#   friday <- fridays[friday_index]
#   index_this_friday <- which(validation_subset$friday_date == friday)
#   index_missing <- which(validation_subset[index_this_friday,]$target_20d_5week == -888)
#   number_missing <- length(validation_subset[index_missing,]$target_20d_5week)
#   
#   count_validation_missing <- c(count_validation_missing,
#                            number_missing)
# }
# 
# plot(count_validation_missing)
# 

# something wrong with validation... look at graph. 