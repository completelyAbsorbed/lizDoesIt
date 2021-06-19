# > dim(signals_data)
# [1] 6937525       9
# > dim(symbol_data)
# [1] 3630    9
# > 
#   > 
#   > 
#   > 
#   > 
#   > 
#   > 
#   > names(signals_data)
# [1] "X"        "symbol"   "date"     "open"     "high"     "low"      "close"    "volume"   "adjusted"
# > names(symbol_data)
# [1] "X"        "symbol"   "date"     "open"     "high"     "low"      "close"    "volume"   "adjusted"
# > unique(symbol_data$symbol)
# [1] "MLCO"
# > 
#   > 
#   > 
#   > 

# symbol_data is just symbol MLCO


head(signals_data)

length(unique(signals_data$symbol)) # 2121

length(unique(signals_data$date)) # 4610, ~ 12.63 years

4610/365

# date math investigation

class(signals_data$date)

# date tests

signals_data$date[2] - signals_data$date[1] # "Time difference of 1 days"

signals_data$date[2] - signals_data$date[1] == 1 # TRUE

signals_data$date[2] - signals_data$date[1] == 2 # FALSE

# confirmed that date column functions intuitively

# develop a scheme for finding most recent 4 day span for a symbol

symbols_US <- unique(signals_data$symbol)
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
    symbol_data <- signals_data[which(signals_data$symbol == symbol),]
    # extract most recent date ... will be last date
    index_last <- -999
    index_first <- -999
    date_length <- length(symbol_data$date) # relies on knowledge of data structure
    index_last <- date_length
    date_last <- symbol_data$date[index_last]
    span_flag <- FALSE
    index_first <- index_last - 1
    
    while(!span_flag){
      span <- symbol_data$date[index_last] - symbol_data$date[index_first]
      
      if(span == 4){ # if span == 4, stop, goal is  met  
        span_flag <- TRUE
      }else if(span < 4){ # while span < 4, decrement index_first
        index_first <- index_first - 1
      }else if(span >= 5){ # if span >= 5, decrement index_last, reset index_first
        index_last <- index_last - 1
        index_first <- index_last - 1
      }else{ # this should never happen!
        print("else tree error when finding perfect span!!!! ~")
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

# if there are any NA's, there will be length misalignment, need to adjust
# ... ideally this should have been accounted for earlier

NA_symbol_index <- which(is.na(symbols_US))

if(length(NA_symbol_index) > 0){
 symbols_US <- symbols_US[which(!((1:length_symbols) %in% NA_symbol_index))] 
 length_symbols <- length(symbols_US)
}



symbols_date_reference <- data.frame(symbols_US,
                                     date_start_index,
                                     date_end_index)

head(symbols_date_reference)

# symbols_US date_start_index date_end_index
# 1       MLCO             3626           3630
# 2        HCM             1302           1306
# 3       FUTU              553            557
# 4         SE              898            902
# 5       SIMO             3997           4001
# 6       HIMX             3808           3812

# create naive "predictions" for returns, and logreturns

predictions <- vector()
log_predictions <- vector()

for(symbol in symbols_US){
  # subset symbol
  symbol_data <- signals_data[which(signals_data$symbol == symbol),]
  
  # build predictions vectors
  symbol_index <- which(symbols_US == symbol)
  last_index <- symbols_date_reference$date_end_index[symbol_index]
  first_index <- symbols_date_reference$date_start_index[symbol_index]
  last_value <- symbol_data$adjusted[last_index]
  first_value <- symbol_data$adjusted[first_index]
  predictions <- c(predictions,
                   last_value / first_value)
  log_predictions <- c(log_predictions,
                       log(last_value) - log(first_value))
}

head(predictions) 
#[1] 1.0023122 1.0295327 1.0188168 1.1256169 0.9791411 0.9673203

head(log_predictions)
#[1]  0.002309528  0.029105056  0.018641926  0.118331258 -0.021079470 -0.0332...

summary(predictions) 
# looks good

summary(log_predictions) 
# looks good

plot(predictions) 
# looks good

plot(log_predictions) 
# looks good

plot(sort(predictions)) 
# looks good

plot(sort(log_predictions)) 
# looks good

# which is -.626 on log?

outlier_index <- which(log_predictions < -0.6)

symbols_US[outlier_index] # BDTX ~ May 13 ???

# create submission file(s), names in header : "ticker", "signal"
# ... note signal must be between 0-1, exclusive

bloomberg_ticker <- symbols_US
signal <- predictions

# adjust signal to (0,1) excluding 0, 1
signal <- signal - min(signal)
signal <- signal / max(signal)
signal <- 0.99999 * signal
signal <- signal + 0.0000001

summary(signal)
                       
submission <- data.frame(ticker,
                         signal)

write.table(x = submission,
            file = "live_adjusted_predictions.csv",
            sep = ",",
            row.names = FALSE)

write.table(x = logsubmission,
            file = "ghost_gradient_adjusted_logpredictions.csv",
            sep = ",",
            row.names = FALSE)

################################################################################

predictions <- vector()
log_predictions <- vector()

for(symbol in symbols_US){
  # subset symbol
  symbol_data <- signals_data[which(signals_data$symbol == symbol),]
  
  # build predictions vectors
  symbol_index <- which(symbols_US == symbol)
  last_index <- symbols_date_reference$date_end_index[symbol_index]
  first_index <- symbols_date_reference$date_start_index[symbol_index]
  last_value <- symbol_data$open[last_index]
  first_value <- symbol_data$open[first_index]
  predictions <- c(predictions,
                   last_value / first_value)
  log_predictions <- c(log_predictions,
                       log(last_value) - log(first_value))
}

head(predictions) 
#[1] 1.0023122 1.0295327 1.0188168 1.1256169 0.9791411 0.9673203

head(log_predictions)
#[1]  0.002309528  0.029105056  0.018641926  0.118331258 -0.021079470 -0.0332...

summary(predictions) 
# looks good

summary(log_predictions) 
# looks good

plot(predictions) 
# looks good

plot(log_predictions) 
# looks good

plot(sort(predictions)) 
# looks good

plot(sort(log_predictions)) 
# looks good

# which is -.626 on log?

outlier_index <- which(log_predictions < -0.6)

symbols_US[outlier_index] # BDTX ~ May 13 ???

# create submission file(s), names in header : "ticker", "signal"
# ... note signal must be between 0-1, exclusive

bloomberg_ticker <- symbols_US
signal <- predictions

# adjust signal to (0,1) excluding 0, 1
signal <- signal - min(signal)
signal <- signal / max(signal)
signal <- 0.99999 * signal
signal <- signal + 0.0000001

summary(signal)

submission <- data.frame(ticker,
                         signal)

write.table(x = submission,
            file = "free_adjusted_predictions.csv",
            sep = ",",
            row.names = FALSE)

write.table(x = logsubmission,
            file = "four_color_deck_suits_adjusted_logpredictions.csv",
            sep = ",",
            row.names = FALSE)

################################################################################

predictions <- vector()
log_predictions <- vector()

for(symbol in symbols_US){
  # subset symbol
  symbol_data <- signals_data[which(signals_data$symbol == symbol),]
  
  # build predictions vectors
  symbol_index <- which(symbols_US == symbol)
  last_index <- symbols_date_reference$date_end_index[symbol_index]
  first_index <- symbols_date_reference$date_start_index[symbol_index]
  last_value <- symbol_data$adjusted[last_index]
  first_value <- symbol_data$adjusted[first_index]
  predictions <- c(predictions,
                   last_value / first_value)
  log_predictions <- c(log_predictions,
                       log(last_value) - log(first_value))
}

head(predictions) 
#[1] 1.0023122 1.0295327 1.0188168 1.1256169 0.9791411 0.9673203

head(log_predictions)
#[1]  0.002309528  0.029105056  0.018641926  0.118331258 -0.021079470 -0.0332...

summary(predictions) 
# looks good

summary(log_predictions) 
# looks good

plot(predictions) 
# looks good

plot(log_predictions) 
# looks good

plot(sort(predictions)) 
# looks good

plot(sort(log_predictions)) 
# looks good

# which is -.626 on log?

outlier_index <- which(log_predictions < -0.6)

symbols_US[outlier_index] # BDTX ~ May 13 ???

# create submission file(s), names in header : "ticker", "signal"
# ... note signal must be between 0-1, exclusive

bloomberg_ticker <- symbols_US
signal <- predictions

# adjust signal to (0,1) excluding 0, 1
signal <- signal - min(signal)
signal <- signal / max(signal)
signal <- 0.99999 * signal
signal <- signal + 0.0000001

summary(signal)

submission <- data.frame(ticker,
                         signal)

write.table(x = submission,
            file = "live_adjusted_predictions.csv",
            sep = ",",
            row.names = FALSE)

write.table(x = logsubmission,
            file = "ghost_gradient_adjusted_logpredictions.csv",
            sep = ",",
            row.names = FALSE)

################################################################################

predictions <- vector()
log_predictions <- vector()

for(symbol in symbols_US){
  # subset symbol
  symbol_data <- signals_data[which(signals_data$symbol == symbol),]
  
  # build predictions vectors
  symbol_index <- which(symbols_US == symbol)
  last_index <- symbols_date_reference$date_end_index[symbol_index]
  first_index <- symbols_date_reference$date_start_index[symbol_index]
  last_value <- symbol_data$adjusted[last_index]
  first_value <- symbol_data$adjusted[first_index]
  predictions <- c(predictions,
                   last_value / first_value)
  log_predictions <- c(log_predictions,
                       log(last_value) - log(first_value))
}

head(predictions) 
#[1] 1.0023122 1.0295327 1.0188168 1.1256169 0.9791411 0.9673203

head(log_predictions)
#[1]  0.002309528  0.029105056  0.018641926  0.118331258 -0.021079470 -0.0332...

summary(predictions) 
# looks good

summary(log_predictions) 
# looks good

plot(predictions) 
# looks good

plot(log_predictions) 
# looks good

plot(sort(predictions)) 
# looks good

plot(sort(log_predictions)) 
# looks good

# which is -.626 on log?

outlier_index <- which(log_predictions < -0.6)

symbols_US[outlier_index] # BDTX ~ May 13 ???

# create submission file(s), names in header : "ticker", "signal"
# ... note signal must be between 0-1, exclusive

bloomberg_ticker <- symbols_US
signal <- predictions

# adjust signal to (0,1) excluding 0, 1
signal <- signal - min(signal)
signal <- signal / max(signal)
signal <- 0.99999 * signal
signal <- signal + 0.0000001

summary(signal)

submission <- data.frame(ticker,
                         signal)

write.table(x = submission,
            file = "live_adjusted_predictions.csv",
            sep = ",",
            row.names = FALSE)

write.table(x = logsubmission,
            file = "ghost_gradient_adjusted_logpredictions.csv",
            sep = ",",
            row.names = FALSE)

################################################################################

predictions <- vector()
log_predictions <- vector()

for(symbol in symbols_US){
  # subset symbol
  symbol_data <- signals_data[which(signals_data$symbol == symbol),]
  
  # build predictions vectors
  symbol_index <- which(symbols_US == symbol)
  last_index <- symbols_date_reference$date_end_index[symbol_index]
  first_index <- symbols_date_reference$date_start_index[symbol_index]
  last_value <- symbol_data$adjusted[last_index]
  first_value <- symbol_data$adjusted[first_index]
  predictions <- c(predictions,
                   last_value / first_value)
  log_predictions <- c(log_predictions,
                       log(last_value) - log(first_value))
}

head(predictions) 
#[1] 1.0023122 1.0295327 1.0188168 1.1256169 0.9791411 0.9673203

head(log_predictions)
#[1]  0.002309528  0.029105056  0.018641926  0.118331258 -0.021079470 -0.0332...

summary(predictions) 
# looks good

summary(log_predictions) 
# looks good

plot(predictions) 
# looks good

plot(log_predictions) 
# looks good

plot(sort(predictions)) 
# looks good

plot(sort(log_predictions)) 
# looks good

# which is -.626 on log?

outlier_index <- which(log_predictions < -0.6)

symbols_US[outlier_index] # BDTX ~ May 13 ???

# create submission file(s), names in header : "ticker", "signal"
# ... note signal must be between 0-1, exclusive

bloomberg_ticker <- symbols_US
signal <- predictions

# adjust signal to (0,1) excluding 0, 1
signal <- signal - min(signal)
signal <- signal / max(signal)
signal <- 0.99999 * signal
signal <- signal + 0.0000001

summary(signal)

submission <- data.frame(ticker,
                         signal)

write.table(x = submission,
            file = "live_adjusted_predictions.csv",
            sep = ",",
            row.names = FALSE)

write.table(x = logsubmission,
            file = "ghost_gradient_adjusted_logpredictions.csv",
            sep = ",",
            row.names = FALSE)

################################################################################

predictions <- vector()
log_predictions <- vector()

for(symbol in symbols_US){
  # subset symbol
  symbol_data <- signals_data[which(signals_data$symbol == symbol),]
  
  # build predictions vectors
  symbol_index <- which(symbols_US == symbol)
  last_index <- symbols_date_reference$date_end_index[symbol_index]
  first_index <- symbols_date_reference$date_start_index[symbol_index]
  last_value <- symbol_data$adjusted[last_index]
  first_value <- symbol_data$adjusted[first_index]
  predictions <- c(predictions,
                   last_value / first_value)
  log_predictions <- c(log_predictions,
                       log(last_value) - log(first_value))
}

head(predictions) 
#[1] 1.0023122 1.0295327 1.0188168 1.1256169 0.9791411 0.9673203

head(log_predictions)
#[1]  0.002309528  0.029105056  0.018641926  0.118331258 -0.021079470 -0.0332...

summary(predictions) 
# looks good

summary(log_predictions) 
# looks good

plot(predictions) 
# looks good

plot(log_predictions) 
# looks good

plot(sort(predictions)) 
# looks good

plot(sort(log_predictions)) 
# looks good

# which is -.626 on log?

outlier_index <- which(log_predictions < -0.6)

symbols_US[outlier_index] # BDTX ~ May 13 ???

# create submission file(s), names in header : "ticker", "signal"
# ... note signal must be between 0-1, exclusive

bloomberg_ticker <- symbols_US
signal <- predictions

# adjust signal to (0,1) excluding 0, 1
signal <- signal - min(signal)
signal <- signal / max(signal)
signal <- 0.99999 * signal
signal <- signal + 0.0000001

summary(signal)

submission <- data.frame(ticker,
                         signal)

write.table(x = submission,
            file = "live_adjusted_predictions.csv",
            sep = ",",
            row.names = FALSE)

write.table(x = logsubmission,
            file = "ghost_gradient_adjusted_logpredictions.csv",
            sep = ",",
            row.names = FALSE)

################################################################################
