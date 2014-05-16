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
# Labor day 2011 is in week 37, labor day 2012 in week 36
ind1 <- test$wk == 47
ind2 <- test$wk == 48
ind3 <- test$wk == 49
test$wk[ind1] <- 48   #Works well!
test$wk[ind2] <- 49   #Works well!
test$wk[ind3] <- 50   #Only slight improvement - maybe blending is needed

# I want to calculate Christmas week 2013 (2012-Dec-28) as
# 2/7 * (2011-Dec-23) + 5/7 * (2011-Dec-30)
# i.e. week 52 = 2/7 * (week 52) + 5/7 * (week 53)
# This is a good adjustment - +53 positions on the leader board!
wk_2011_52 <- subset(train, yr == 2011 & wk == 52)
wk_2011_53 <- subset(train, yr == 2011 & wk == 53)
wk_join <- merge(wk_2011_52[, 1:4], wk_2011_53[, 1:4], by = c("Store", "Dept"), 
                 all = TRUE)
test_2012_52 <- subset(test, Date == "2012-12-28")[, 1:2]
wk_join <- merge(test_2012_52, wk_join, by = c("Store", "Dept"), all.x = T)
wk_join$Weekly_Sales.x[is.na(wk_join$Weekly_Sales.x)] <- 0
wk_join$Weekly_Sales.y[is.na(wk_join$Weekly_Sales.y)] <- 0

xmas_sales_2012 <- round(with(wk_join, 2/7 * Weekly_Sales.x + 4/7 * Weekly_Sales.y), 0)
df_xmas_2012 <- data.frame(Id = with(wk_join, paste(Store, Dept, "2012-12-28", sep = "_")), 
                           Weekly_Sales = xmas_sales_2012)

ans <- merge(test, train, by.x = c("Store", "Dept", "prior_yr", "wk"),
             by.y = c("Store", "Dept", "yr", "wk"), all.x = TRUE)
ans$Weekly_Sales[is.na(ans$Weekly_Sales)] <- 0

keep_cols <- c("Store", "Dept", "Date.x", "Weekly_Sales")
ans <- ans[, keep_cols]

#Get rid of Christmas records
ans <- subset(ans, Date.x != "2012-12-28")

ans$Id <- with(ans, paste(Store, Dept, Date.x, sep = "_"))

sub <- ans[, c("Id", "Weekly_Sales")]

sub <- rbind(sub, df_xmas_2012)
sub <- arrange(sub, Id)

write.csv(sub, "submission.csv", row.names = FALSE)
#115064