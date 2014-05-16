# TODO - NEED TO SEE HOW WELL PREDICTIONS MAKE SENSE BY MONTH
# TODO - WHAT IS THE PREDICTION FOR THE WEEK BEFORE EASTER? DOES IT MAKE SENSE?

options(stringsAsFactors = FALSE)
setwd("~/walmart")
require(Hmisc) # Hmisc is first so it it's summarize function does not mask plyr's
require(plyr)
require(testthat)
require(lubridate)
require(stringr)

trend_sales <- function(v_sales, v_store, v_dt, store_num, trend_fctr) {
  # Apply a trend factor to the historical sales, moving them to the
  # beginning of the test period
  
  # v_sales = vector of all sales in the test set
  # v_store = vector of all stores in the test set
  # v_dt = vector of all dates in the test set
  # store_num = the store for which sales are to be trended
  # trend_fctr = the historical (not prospective) trend factor
  # Value = the revised sales vector with trend applied to the
  # store_num entries
  
  ind <- which(v_store == store_num)
  wks_between <- as.integer(difftime(v_dt[ind], min(v_dt[ind]), units="weeks"))
  fctr <- trend_fctr^(1/52 *(51 - wks_between)) 
  v_sales[ind] <- round(v_sales[ind] * fctr, 2)
  return(v_sales)
}

blend_weeks <- function(next_yr_dt, coef1 = NULL, coef2 = NULL) {
  # Given a date from the test set, week ending on the corresponding date
  # in the training set will usually straddle two training weeks. This function
  # calculates an appropirate weighted average of the train weeks for
  # predicting the test week.
  # PARAMETERS:
  # next_year_dt: An end of week date (must be a friday) from the test set
  # RETURN VALUE:
  # A data frame with the test set id and predicted sales for next_yr_dt
  
  # Dataframes test and train are global and are referenced within the
  # blend_weeks function
  
  stopifnot(wday(next_yr_dt) == 6)
  dt <- next_yr_dt  - years(1)
  days_to_friday <- (13 - wday(dt)) %% 7
  next_friday <- dt + days(days_to_friday)
  prev_friday <- next_friday - days(7)
  stopifnot(wday(next_friday) == 6)
  stopifnot(wday(prev_friday) == 6)
  
  df1 <- subset(train, dt == next_friday)
  df2 <- subset(train, dt == prev_friday)
  df_valid <- subset(test, dt == next_yr_dt)[, c("Store", "Dept")]
  
  df_both <- merge(df1[, 1:4], df2[, 1:4], by = c("Store", "Dept"), 
        all = TRUE)
  df_both <- merge(df_valid, df_both, by = c("Store", "Dept"), all.x = T)
  df_both[, c("sales.x", "sales.y")] <- 
    impute(df_both[, c("sales.x", "sales.y")], 0)
  
  if(is.null(coef1)) coef1 <- 1 - days_to_friday/7
  if(is.null(coef2)) coef2 <- days_to_friday/7
  
  
  blended_sales <- round(with(df_both, coef1 * sales.x + 
                                  coef2 * sales.y), 0)
  
  Id <- with(df_both, paste(Store, Dept, next_yr_dt, sep = "_"))
  df_ans <- data.frame(Id = Id, sales = blended_sales)
  return(df_ans)
}

# Training data covers 2010-02-05 to 2012-11-01
train <- readRDS("train.rds")
# Test data covers 2012-11-02 to 2013-07-26
test <- readRDS("test.rds")
expect_equal(with(train, length(unique(paste(Store, Dept, Date)))), nrow(train))
expect_equal(with(test, length(unique(paste(Store, Dept, Date)))), nrow(test))

train <- mutate(train, dt = ymd(Date), yr = year(dt), wk = week(dt))
train <- rename(train, replace = c("Weekly_Sales" = "sales"))
test <- mutate(test, dt = ymd(Date), yr = year(dt), wk = week(dt),   
               prior_yr = yr - 1)

# Week adjustments
# Thanksgiving 2011 is in week 48, thanksgiving 2012 in week 47
ind1 <- test$wk == 47
ind2 <- test$wk == 48
# The week 2012-04-06 (14) contains a 14% spike in sales due to Easter.
# Easter 2013 is on March 31 (week 13).
ind4 <- test$wk == 14
ind5 <- test$wk == 13

test$wk[ind1] <- 48   # Works well!
test$wk[ind2] <- 49   # Works well!
test$wk[ind4] <- 15  # Model week after Easter by week after Easter; Improvement of 28.3
# test$wk[ind5] <- 14  Why doesn't this work? Even 50/50 blend doesn't work
#test$wk[ind5] <- 15 # Negligible improvement - how to get a good estimate for Easter?
                    # Easter is on 3/31, this may slow down shopping.
#Easter - try blending 2013-03-29

# Construct the test set predictions
ans <- merge(test, train, by.x = c("Store", "Dept", "prior_yr", "wk"),
             by.y = c("Store", "Dept", "yr", "wk"), all.x = TRUE)
ans$sales[is.na(ans$sales)] <- 0

ans <- ans[, c("Store", "Dept", "Date.x", "sales")]
ans$Id <- with(ans, paste(Store, Dept, Date.x, sep = "_"))

#Week blending
drop_dates <- c("2012-11-02", "2012-11-09", "2012-11-16", "2012-12-07", 
                "2012-12-14", "2012-12-21", "2012-12-28", "2013-02-08", "2013-03-29")
ans <- subset(ans, !(Date.x %in% drop_dates))
sub <- ans[, c("Id", "sales")]
blended_weeks <- rbind.fill(lapply(ymd(drop_dates), blend_weeks))
sub <- rbind(sub, blended_weeks)

# Reconstruct date, store, and department from the submission
# (awkward - could be cleaned up)
dt <- ymd(str_extract(sub$Id, ".{10}$" ))
store <- str_extract(sub$Id, "[0-9]+")
dept <- substr(str_extract(sub$Id, "_[0-9]+"), 2, 3)

# Apply the historical trend
sub$sales <- trend_sales(sub$sales, store, dt, 36, 0.81)
sub$sales <- trend_sales(sub$sales, store, dt, 14, 0.85)
sub$sales <- trend_sales(sub$sales, store, dt, 21, 0.90)
sub$sales <- trend_sales(sub$sales, store, dt, 44, 1.12)
sub$sales <- trend_sales(sub$sales, store, dt, 38, 1.10)
sub$sales <- trend_sales(sub$sales, store, dt, 19, 0.95)
sub$sales <- trend_sales(sub$sales, store, dt, 27, 0.94)
sub$sales <- trend_sales(sub$sales, store, dt, 3, 1.07)

# Store 39 adjustment did not work
# ind <- which(store == 39)
# wks_between <- as.integer(difftime(dt[ind], min(dt[ind]), units="weeks"))
# fctr <- (1.07)^(1/52 *(51 - wks_between)) 
# sub$sales[ind] <- round(sub$sales[ind] * fctr, 2)

# Estimate the impact of markdowns (this should really be based on change in
# markdown, not the entire markdown)
features <- read.csv("features.csv")
sub$dt <- dt
sub$store <- store
sub$dept <- dept
features$dt <- ymd(features$Date)
features$store <- features$Store
sub <- merge(sub, features, by = c("dt", "store"))

sub$MarkDown1 <- impute(sub$MarkDown1, 0)
sub$sales <- with(sub, sales + ifelse(MarkDown1 > 8810 & dept == "87" , 2428, 0))
sub$sales <- with(sub, sales + ifelse(MarkDown1 > 4970 & MarkDown1 <= 8810 
                                      & dept == "87" , 2238, 0))
sub$sales <- with(sub, sales + ifelse(MarkDown1 > 1560 & MarkDown1 <= 4970 
                                      & dept == "87" , 1630, 0))

# This one didn't work
# sub$sales <- with(sub, sales + ifelse(MarkDown1 > 4970 & dept == "2" , 770, 0))


###############temperature#####################
features <- read.csv("features.csv")
features <- mutate(features, dt = ymd(Date), wk = week(dt), yr = year(dt),
                   prev_yr = yr - 1)
df_dat1 <- merge(features, features[, -15], by.x = c("Store", "yr", "wk"), 
                 by.y = c("Store", "prev_yr", "wk"))
df_dat1 <- subset(df_dat1, dt.y >= ymd("2011-11-02") & dt.y <= ymd("2013-07-26"))
df_dat1 <- df_dat1[, -c(4, 17)]
df_dat1 <- mutate(df_dat1, temp_diff = Temperature.y - Temperature.x, 
                  fuel_pct = Fuel_Price.y/Fuel_Price.x -1,
                  cpi_pct = CPI.y/CPI.x -1,
                  MarkDown1 = MarkDown1.y, 
                  MarkDown2 = MarkDown2.y,
                  MarkDown3 = MarkDown3.y,
                  MarkDown4 = MarkDown4.y,
                  MarkDown5 = MarkDown5.y,
                  unemployment_diff = Unemployment.y - Unemployment.x)
df_temp <- df_dat1[, c("Store", "dt.y", "temp_diff")]
sub <- merge(sub, df_temp, by.x = c("store", "dt"), by.y = c("Store", "dt.y"))
sub$sales <- with(sub, sales * ifelse(temp_diff > 9.29 & dept == "16" , 1.139, 1))
sub$sales <- with(sub, sales * ifelse(temp_diff <= 9.29 & temp_diff > 5.42 & 
                                        dept == "16" , 1.061, 1))
sub$sales <- with(sub, sales * ifelse(temp_diff <= 5.42 & temp_diff > 1.44 & 
                                        dept == "16" , 0.980, 1))
sub$sales <- with(sub, sales * ifelse(temp_diff <= 1.44 & temp_diff > -2.84 & 
                                        dept == "16" ,0.986 , 1))
sub$sales <- with(sub, sales * ifelse(temp_diff <= -2.84 & 
                                        dept == "16" ,0.956 , 1))


# sub$sales <- with(sub, sales * ifelse(temp_diff > 9.29 & dept == "7" , 1.061, 1))
# sub$sales <- with(sub, sales * ifelse(temp_diff <= 9.29 & temp_diff > 5.42 & 
#                                         dept == "7" , 1.044, 1))
# sub$sales <- with(sub, sales * ifelse(temp_diff <= 5.42 & temp_diff > 1.44 & 
#                                         dept == "7" , 0.992, 1))
# sub$sales <- with(sub, sales * ifelse(temp_diff <= 1.44 & temp_diff > -2.84 & 
#                                         dept == "7" ,0.964 , 1))
# sub$sales <- with(sub, sales * ifelse(temp_diff <= -2.84 & 
#                                         dept == "7" ,0.919 , 1))




# Write the submission
sub <- sub[, c("Id", "sales")]
names(sub) <- c("Id", "Weekly_Sales")
sub <- arrange(sub, Id)
expect_equal(nrow(sub), 115064)
write.csv(sub, "submission.csv", row.names = FALSE)
#############################################################

stop("Finished.")

# Weighted average of sales in test period?

sum(with(test, ifelse(IsHoliday, 5, 1))) * 21924.30518
# 3,305,659,038

test$Id <- with(test, paste(Store, Dept, Date, sep = "_"))
check <- merge(test, sub, by = "Id")
sum(with(check, ifelse(IsHoliday, 5, 1) * Weekly_Sales))
# 2,553,575,561

sum(with(check, ifelse(IsHoliday, 5, 1) * Weekly_Sales))/sum(with(check, ifelse(IsHoliday, 5, 1)))

# Predicted weighted sales too low? 2.57/3.31 = 0.776. Seems impossible.
# Are there new stores, or new departments? Looks like 45 stores and 81
# departments.