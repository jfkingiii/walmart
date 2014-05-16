options(stringsAsFactors = FALSE)
setwd("~/walmart")
require(plyr)
require(testthat)
require(lubridate)
require(Hmisc)

# This is the historical training data, which covers to 2010-02-05 to 2012-11-01
train <- readRDS("train.rds")
# Test data covers 2012-11-02 to 2013-07-26
test <- readRDS("test.rds")
features <- read.csv("features.csv")

expect_equal(with(train, length(unique(paste(Store, Dept, Date)))), nrow(train))
expect_equal(with(test, length(unique(paste(Store, Dept, Date)))), nrow(test))

train <- mutate(train, dt = ymd(Date), yr = year(dt), wk = week(dt))
test <- mutate(test, dt = ymd(Date), yr = year(dt), wk = week(dt), 
               prior_yr = yr - 1)

ans <- merge(test, train, by.x = c("Store", "Dept", "prior_yr", "wk"),
             by.y = c("Store", "Dept", "yr", "wk"), all.x = TRUE)
ans$Weekly_Sales[is.na(ans$Weekly_Sales)] <- 0
keep_cols <- c("Store", "Dept", "Date.x", "Weekly_Sales")
ans <- ans[, keep_cols]
ans$Id <- with(ans, paste(Store, Dept, Date.x, sep = "_"))

sub <- ans[, c("Id", "Weekly_Sales")]

# Make a blending adjustment for the week ending 2012-Dec-28
# Multiply by a factor of 0.74

ind <- (substr(sub$Id, 6, 15) == "2012-12-28")
sub$Weekly_Sales[ind] <- sub$Weekly_Sales[ind] * 0.74

write.csv(sub, "submission.csv", row.names = FALSE)

# All zeros benchmark 21924.30518
# A week gets a weight of 5 if it is a holiday week, 1 oterwise

# train_wt <- with(train, sum(ifelse(IsHoliday, 5, 1)))
# test_wt <- with(test, sum(ifelse(IsHoliday, 5, 1))) 
# test_avg_sales <-  sum(ans$Weekly_Sales * ifelse(test$IsHoliday, 5, 1))/test_wt
# adj_fctr = 21924.30518/test_avg_sales
# ans$Weekly_Sales <- ans$Weekly_Sales * adj_fctr
# Add holiday indicator to test set
# Super Bowl: 8-Feb-13
# Labor Day: 
# Thanksgiving: 23-Nov-12
# Christmas: 28-Dec-12
# "2012-11-02 UTC" "2013-07-26 UTC"
# week(ymd("2013-02-08")) #6
# week(ymd("2012-11-23")) #47
# week(ymd("2012-12-28")) #52
# 
# test$IsHoliday <- FALSE
# test$IsHoliday[test$yr == 2013 & test$wk == 6] <- TRUE
# test$IsHoliday[test$yr == 2012 & test$wk == 47] <- TRUE
# test$IsHoliday[test$yr == 2012 & test$wk == 52] <- TRUE


Super Bowl: 12-Feb-10, 11-Feb-11, 10-Feb-12, 8-Feb-13
Labor Day: 10-Sep-10, 9-Sep-11, 7-Sep-12, 6-Sep-13
Thanksgiving: 26-Nov-10, 25-Nov-11, 23-Nov-12, 29-Nov-13
Christmas: 31-Dec-10, 30-Dec-11, 28-Dec-12, 27-Dec-13

week(ymd("12-Feb-10"))
week(ymd("11-Feb-11"))
week(ymd("10-Feb-12"))
week(ymd("08-Feb-13"))

week(ymd("26-Nov-10"))
week(ymd("25-Nov-11"))
week(ymd("23-Nov-12"))
week(ymd("29-Nov-13"))

week(ymd("10-Sep-10"))
week(ymd("09-Sep-11"))
week(ymd("07-Sep-12"))
week(ymd("06-Sep-13"))

week(ymd("31-Dec-10"))
week(ymd("30-Dec-11"))
week(ymd("28-Dec-12"))
week(ymd("27-Dec-13"))


