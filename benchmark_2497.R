# TODO - NEED TO SEE HOW WELL PREDICTIONS BALANCE BY MONTH
# TODO - WHAT IS THE PREDICTION FOR THE WEEK BEFORE EASTER? DOES IT MAKE SENSE?

options(stringsAsFactors = FALSE)
setwd("~/walmart")
require(plyr)
require(testthat)
require(lubridate)
require(Hmisc)
require(stringr)

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
test <- mutate(test, dt = ymd(Date), yr = year(dt), wk = week(dt),   
               prior_yr = yr - 1)
train <- rename(train, replace = c("Weekly_Sales" = "sales"))

# Week adjustments
# Thanksgiving 2011 is in week 48, thanksgiving 2012 in week 47
ind1 <- test$wk == 47
ind2 <- test$wk == 48
test$wk[ind1] <- 48   # Works well!
test$wk[ind2] <- 49   # Works well!

# The week 2012-04-06 (14) contains a 14% spike in sales due to Easter.
# Easter 2013 is on March 31 (week 13).
ind4 <- test$wk == 14
ind5 <- test$wk == 13

test$wk[ind4] <- 15  # Model week after Easter by week after Easter; Improvement of 28.3
# test$wk[ind5] <- 14  Why doesn't this work? Even 50/50 blend doesn't work
test$wk[ind5] <- 15 # Negligible improvement - how to get a good estimate for Easter?
                    # Easter is on 3/31, this may slow down shopping.


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

#new blend test
# What about 2012-Nov-02 2012-Nov-09 2012-Nov-16
ans <- subset(ans, Date.x != "2012-11-02") # Small improvement - 7.9
ans <- subset(ans, Date.x != "2012-11-09") # Small improvement - 9.4
ans <- subset(ans, Date.x != "2012-11-16") # 
#ans <- subset(ans, Date.x != "2012-11-30") # 


sub <- ans[, c("Id", "sales")]

# Add back records from blended weeks
sub <- rbind(sub, 
             blend_weeks(ymd("2012-12-28")),
             blend_weeks(ymd("2012-12-21")), 
             blend_weeks(ymd("2012-12-14")),
             blend_weeks(ymd("2012-12-07")),
             blend_weeks(ymd("2013-02-08")),
             blend_weeks(ymd("2012-11-02")),
             blend_weeks(ymd("2012-11-09")),
             blend_weeks(ymd("2012-11-16")))

dt <- ymd(str_extract(sub$Id, ".{10}$" ))
store <- str_extract(sub$Id, "[0-9]+")

# Store 36 adjustment
ind <- which(store == 36)
wks_between <- as.integer(difftime(dt[ind], min(dt[ind]), units="weeks"))
fctr <- (0.81)^(1/52 *(51 - wks_between)) 
sub$sales[ind] <- round(sub$sales[ind] * fctr, 2)

# Store 14 adjustment
ind <- which(store == 14)
wks_between <- as.integer(difftime(dt[ind], min(dt[ind]), units="weeks"))
fctr <- (0.85)^(1/52 *(51 - wks_between)) 
sub$sales[ind] <- round(sub$sales[ind] * fctr, 2)

# Store 20 adjustment did not work

# Store 21 adjustment
ind <- which(store == 21)
wks_between <- as.integer(difftime(dt[ind], min(dt[ind]), units="weeks"))
fctr <- (0.90)^(1/52 *(51 - wks_between)) 
sub$sales[ind] <- round(sub$sales[ind] * fctr, 2)

# Store 44 adjustment
ind <- which(store == 44)
wks_between <- as.integer(difftime(dt[ind], min(dt[ind]), units="weeks"))
fctr <- (1.12)^(1/52 *(51 - wks_between)) 
sub$sales[ind] <- round(sub$sales[ind] * fctr, 2)

# Store 38 adjustment
ind <- which(store == 38)
wks_between <- as.integer(difftime(dt[ind], min(dt[ind]), units="weeks"))
fctr <- (1.10)^(1/52 *(51 - wks_between)) 
sub$sales[ind] <- round(sub$sales[ind] * fctr, 2)

# Store 39 adjustment did not work
# ind <- which(store == 39)
# wks_between <- as.integer(difftime(dt[ind], min(dt[ind]), units="weeks"))
# fctr <- (1.07)^(1/52 *(51 - wks_between)) 
# sub$sales[ind] <- round(sub$sales[ind] * fctr, 2)

# Store 19 adjustment
ind <- which(store == 19)
wks_between <- as.integer(difftime(dt[ind], min(dt[ind]), units="weeks"))
fctr <- (0.95)^(1/52 *(51 - wks_between)) 
sub$sales[ind] <- round(sub$sales[ind] * fctr, 2)


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
# 2,569,867,494

sum(with(check, ifelse(IsHoliday, 5, 1) * Weekly_Sales))/sum(with(check, ifelse(IsHoliday, 5, 1)))

# Predicted weighted sales too low? 2.57/3.31 = 0.776. Seems impossible.
# Are there new stores, or new departments? Looks like 45 stores and 81
# departments.

quantilize <- function(v, n = 20) {
  qntls <- unique(quantile(v, seq(0, 1, 1/n)))
  if(length(qntls) > 1) {
    ans <- cut(v, breaks = qntls, include.lowest = T)
  } else {
    ans <- as.factor(rep(1, length(v)))
  }
  return(ans)
}

