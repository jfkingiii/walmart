options(stringsAsFactors = FALSE)
setwd("~/walmart")
require(plyr)
require(testthat)
require(lubridate)

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

# Thanksgiving 2011 is in week 48, thanksgiving 2012 in week 47

ind1 <- test$wk == 47
ind2 <- test$wk == 48
test$wk[ind1] <- 48   #Works well!
test$wk[ind2] <- 49   #Works well!



ans <- merge(test, train, by.x = c("Store", "Dept", "prior_yr", "wk"),
             by.y = c("Store", "Dept", "yr", "wk"), all.x = TRUE)
ans$Weekly_Sales[is.na(ans$Weekly_Sales)] <- 0


keep_cols <- c("Store", "Dept", "Date.x", "Weekly_Sales")
ans <- ans[, keep_cols]
ans$Id <- with(ans, paste(Store, Dept, Date.x, sep = "_"))




sub <- ans[, c("Id", "Weekly_Sales")]

# Make a blending adjustment for the week ending 2012-Dec-28
# Multiply by a factor of 0.74
# This week blending can be pushed to store and department level

ind <- (substr(sub$Id, 6, 15) == "2012-12-28")
sub$Weekly_Sales[ind] <- sub$Weekly_Sales[ind] * 0.74
write.csv(sub, "submission.csv", row.names = FALSE)

