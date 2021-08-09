library(tidyverse)
library(tidyquant)

data_directory <- "..." # fill in your own here 

filename_historical_targets <- "historical_targets.csv"
filename_example_signal_upload <- "example_signal_upload.csv"
filename_live_universe <- "live_universe.csv"

setwd(data_directory)

# historical_targets <- read.table(filename_historical_targets) # broken
historical_targets <- read.csv(filename_historical_targets)

head(historical_targets)
tail(historical_targets)

example_signal_upload <- read.csv(filename_example_signal_upload)

head(example_signal_upload)
tail(example_signal_upload)

live_universe <- read.csv(filename_live_universe)

head(live_universe)


live_US_raw <- live_universe[which(endsWith(x = live_universe[,1],
                                            suffix = " US")),]
live_US <- substr(live_US_raw,
                  1,
                  nchar(live_US_raw) - 3)

length_live_US <- length(live_US)

################################################################################

frist <- tq_get(live_US[1], 
                get = "stock.prices", # yahoo finance
                from = " 2003-01-31") # as per train data, adjust as needed
dim(frist)
head(frist)

dim(rbind(frist,
          frist))

time <- system.time(sconde <- tq_get(live_US[2], 
                                     get = "stock.prices", # yahoo finance
                                     from = " 2003-01-31") # as per train data, adjust as needed
)

elapsed <- time[3]

joint <- rbind(frist,
               sconde)

head(joint)
tail(joint)

# loop over tickers in live_US, stop often enough to play nice with rate limit

rate_limit_hour <- 2000
hours <- length(live_US) / rate_limit_hour
minutes <- hours * 60
seconds <- minutes * 60
rounded_seconds <- signif(seconds,
                          digits = 2) # round to not hit rate limit

time_per_call <- rounded_seconds / length(live_US)

blob <- vector() # an empty object to start, ok that it starts as a vector

for(ticker in live_US){
  time <- system.time({blip <- tq_get(ticker, # blip is this tickers data 
                                     get = "stock.prices", # yahoo finance
                                     from = " 2003-01-31") # as per train data, adjust?
                      blob <- rbind(blob,
                                    blip)}
  ) 
  
  elapsed <- time[3] # third entry is elapsed
  
  time_wait <- max(time_per_call - elapsed,
                   0) # calculate how much longer to wait
  
  Sys.sleep(time_wait) # sleep for a bit before next loop cycle
}



signals_data <- blob

write.csv(blob,
          file = "yahoo_writeout.csv")
