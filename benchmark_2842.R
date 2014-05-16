options(stringsAsFactors = FALSE)
setwd("~/walmart")
require(plyr)
require(testthat)
require(lubridate)
require(Hmisc)

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


###################CHRISTMAS 2013 CALCULATION###################
# I want to calculate Christmas week 2013 (2012-Dec-28) as
# 2/7 * (2011-Dec-23) + 5/7 * (2011-Dec-30)
# i.e. week 52 = 2/7 * (week 52) + 5/7 * (week 53)
# This is a good adjustment - +53 positions on the leader board!

wk1 <- subset(train, yr == 2011 & wk == 52)
wk2 <- subset(train, yr == 2011 & wk == 53)
wk_join <- merge(wk1[, 1:4], wk2[, 1:4], by = c("Store", "Dept"), 
                 all = TRUE)
test_wk3 <- subset(test, Date == "2012-12-28")[, c("Store", "Dept")]
wk_join <- merge(test_wk3, wk_join, by = c("Store", "Dept"), all.x = T)
wk_join[, c("sales.x", "sales.y")] <- 
  impute(wk_join[, c("sales.x", "sales.y")], 0)

xmas_sales_2012 <- round(with(wk_join, 2/7 * sales.x + 
                                4/7 * sales.y), 0)
xmas_id_2012 <- with(wk_join, paste(Store, Dept, "2012-12-28", sep = "_"))
df_xmas_2012 <- data.frame(Id = xmas_id_2012, sales = xmas_sales_2012)

###################END OF CHRISTMAS 2013 CALCULATION###################

ans <- merge(test, train, by.x = c("Store", "Dept", "prior_yr", "wk"),
             by.y = c("Store", "Dept", "yr", "wk"), all.x = TRUE)
ans$sales[is.na(ans$sales)] <- 0

keep_cols <- c("Store", "Dept", "Date.x", "sales")
ans <- ans[, keep_cols]

#Get rid of Christmas records
ans <- subset(ans, Date.x != "2012-12-28")
ans$Id <- with(ans, paste(Store, Dept, Date.x, sep = "_"))
sub <- ans[, c("Id", "sales")]

#Add back recalculated Christmas records
sub <- rbind(sub, df_xmas_2012)
names(sub) <- c("Id", "Weekly_Sales")
sub <- arrange(sub, Id)
expect_equal(nrow(sub), 115064)
write.csv(sub, "submission.csv", row.names = FALSE)
