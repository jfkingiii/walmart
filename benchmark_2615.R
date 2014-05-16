options(stringsAsFactors = FALSE)
setwd("~/walmart")
require(plyr)
require(testthat)
require(lubridate)
require(Hmisc)

blend_weeks <- function(next_yr_dt) {
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
  
  blended_sales <- round(with(df_both, (1 - days_to_friday/7) * sales.x + 
                                  days_to_friday/7 * sales.y), 0)
  
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
test <- mutate(test, dt = ymd(Date), yr = year(dt), wk = week(dt),   
               prior_yr = yr - 1)
train <- rename(train, replace = c("Weekly_Sales" = "sales"))

# Week adjustments
# Thanksgiving 2011 is in week 48, thanksgiving 2012 in week 47
ind1 <- test$wk == 47
ind2 <- test$wk == 48
ind3 <- test$wk == 49
test$wk[ind1] <- 48   # Works well!
test$wk[ind2] <- 49   # Works well!
test$wk[ind3] <- 50   # Only slight improvement - maybe blending is needed
# The week 2012-04-06 (14) contains an unexplained 14% spike in sales, 
# not expected to be repeated in 2013-04-05 (14)
ind4 <- test$wk == 14
test$wk[ind4] <- 15  # Improvement of 28.3

ans <- merge(test, train, by.x = c("Store", "Dept", "prior_yr", "wk"),
             by.y = c("Store", "Dept", "yr", "wk"), all.x = TRUE)
ans$sales[is.na(ans$sales)] <- 0

ans <- ans[, c("Store", "Dept", "Date.x", "sales")]
ans$Id <- with(ans, paste(Store, Dept, Date.x, sep = "_"))

#Get rid of the weeks that will be replaced with a blend
ans <- subset(ans, Date.x != "2012-12-28")
ans <- subset(ans, Date.x != "2012-12-21")
ans <- subset(ans, Date.x != "2012-12-14")
ans <- subset(ans, Date.x != "2012-12-07")
ans <- subset(ans, Date.x != "2013-02-08") # Superbowl adjustment adds 41 points
sub <- ans[, c("Id", "sales")]

# Add back records from blended weeks
sub <- rbind(sub, 
             blend_weeks(ymd("2012-12-28")), 
             blend_weeks(ymd("2012-12-21")), 
             blend_weeks(ymd("2012-12-14")),
             blend_weeks(ymd("2012-12-07")),
             blend_weeks(ymd("2013-02-08")))

# Write the submission
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
sum(with(check, ifelse(IsHoliday, 5, 1) * Weekly_Sales))/sum(with(check, ifelse(IsHoliday, 5, 1)))
# 2,569,867,494
# Predicted weighted sales too low? 2.57/3.31 = 0.776. Seems impossible.
# Are there new stores, or new departments? Looks like 45 stores and 81
# departments.

