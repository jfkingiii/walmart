# Kaggle Walmart recruiting competition 2014-02-20 to 2014-05-05.
# James F. King
# Reads training and test sets in rds format (train.rds and test.rds)
# and writes a submission file (submission.csv).
# The working directory ~/walmart is assumed to contain the input files.


# WHAT IF I JUST BLENDED ALL THE WEEKS (EXCEPT THANKSGIVING)? WHAT WOULD THAT DO?
# IS THERE A BETTER WAY TO DO THE CHRISTMAS CALCULATION THAN JUST USING THE USUAL
# BLENDING LOGIC?

# SHOULD I REVERSE THE ORDER OF TRENDING AND BLENDING?

WORKING_DIRECTORY = "~/walmart"

options(stringsAsFactors = FALSE)
setwd(WORKING_DIRECTORY)

require(Hmisc)  # Hmisc is first so its summarize function does not mask plyr's
require(plyr)
require(testthat)
require(lubridate)
require(stringr)

trend_sales <- function(v_sales, v_id, v_dt, id_num, trend_fctr) {
  
  # Apply a trend factor to the historical sales, moving them to the
  # beginning of the test period.
  #
  # Args:
  #   v_sales: Vector of all sales in the test set.
  #   v_id: Vector of all store or department ids in the test set.
  #   v_dt: Vector of all dates in the test set.
  #   id_num: The store or department for which sales are to be trended.
  #   trend_fctr: The historical (not prospective) trend factor.
  # 
  # Returns:
  #   The revised sales vector with trend applied to the
  #   components corresponding to id_num.
  
  ind <- which(v_id == id_num)
  wks_between <- as.integer(difftime(v_dt[ind], min(v_dt[ind]), units="weeks"))
  fctr <- trend_fctr^(1/52 * (51 - wks_between)) 
  v_sales[ind] <- round(v_sales[ind] * fctr, 2)
  return(v_sales)
}

blend_weeks <- function(next_yr_dt, coef1 = NULL, coef2 = NULL) {
  
  # Given a date from the test set, the week ending on the corresponding date
  # in the training set will usually straddle two training weeks. This function
  # calculates an appropriate weighted average of the train weeks for
  # predicting the test week.
  #
  # Args:
  #   next_year_dt: An end of week date (must be a Friday) from the test set
  #   coef1, coef2: Specify the weights rather than calculating them. Not used.
  #
  # Returns:
  #   A data frame with the test set id and predicted sales for next_yr_dt.
  #
  # Note:
  # Dataframes test and train are used globally and are referenced within the
  # blend_weeks function, although not passed as arguments.
  
  stopifnot(wday(next_yr_dt) == 6)  # End of week must be a Friday.
  dt <- next_yr_dt  - years(1)
  stopifnot(wday(dt) != 6)  # TODO: does it work if dt is a Friday?
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

# Read and validate the data --------------------------------------------------
train <- readRDS("train.rds")  # Training data covers 2010-02-05 to 2012-11-01
test <- readRDS("test.rds")    # Test data covers 2012-11-02 to 2013-07-26
expect_equal(nrow(train), 421570)
expect_equal(nrow(test), 115064)
expect_equal(with(train, length(unique(paste(Store, Dept, Date)))), nrow(train))
expect_equal(with(test, length(unique(paste(Store, Dept, Date)))), nrow(test))

# Create derived variables ----------------------------------------------------
train <- mutate(train, dt = ymd(Date), yr = year(dt), wk = week(dt))
train <- rename(train, replace = c("Weekly_Sales" = "sales"))
test <- mutate(test, dt = ymd(Date), yr = year(dt), wk = week(dt),   
               prior_yr = yr - 1)

# Map weeks of test period to corresponding weeks in train period -------------
# Week Mapping Adjustments:
# Thanksgiving 2012 is in week 47, Thanksgiving 2012 in week 48,
# thus 47 is replaced with 48.
# 
# The week 2012-04-06 (14) contains a 14% spike in sales due to Easter.
# Easter 2013 is on March 31 (week 13).
# Model week after Easter by week after Easter; Improvement of 28.3
# Mapping Easter week to Easter week doesnt't work. Even 50/50 blend doesn't work.
# Easter is on 3/31, this may slow down shopping.
# For Easter, wound up blending the week 2013-03-29.

test$wk <- mapvalues(test$wk, from = c(47, 48, 14), to = c(48, 49, 15))
 
# Make initial predictions ----------------------------------------------------
# Construct the initial test set predictions (just a merge with train, lagging
# the test set by one year).
ans <- merge(test, train, by.x = c("Store", "Dept", "prior_yr", "wk"),
             by.y = c("Store", "Dept", "yr", "wk"), all.x = TRUE)
ans$sales[is.na(ans$sales)] <- 0
ans <- ans[, c("Store", "Dept", "Date.x", "sales")]
ans$Id <- with(ans, paste(Store, Dept, Date.x, sep = "_"))

# Week blending adjustments ---------------------------------------------------
# Remove records in the test set that will be replaced by records derived
# from blending.
# NB: Blending July 4th gives no improvement.
DROP_DATES <- c("2012-11-02", "2012-11-09", "2012-11-16", "2012-12-07", 
                "2012-12-14", "2012-12-21", "2012-12-28", "2013-02-08", 
                "2013-03-01", "2013-03-29", "2013-04-26", "2013-05-03", 
                "2013-05-31", "2013-07-12")  
# 2.16 improvement by blending Memorial day
# 0.218 improvement by blending week after July 4
# 2.45 improvement by blending 2013-04-26 (week after tax day)
# 2.32 improvement by blending 2013-05-03 (the following week)
# 1.02 improvement by blending 2013-03-01 (should also try prior week)


ans <- subset(ans, !(Date.x %in% DROP_DATES))
sub <- ans[, c("Id", "sales")]

# Calculate the blended weeks and add them back to sub using plyr::rbind.fill.
blended_weeks <- rbind.fill(lapply(ymd(DROP_DATES), blend_weeks))
sub <- rbind(sub, blended_weeks)

# Reconstruct date, store, and department from the submission
# (awkward - could be cleaned up)
dt <- ymd(str_extract(sub$Id, ".{10}$" ))
store <- str_extract(sub$Id, "[0-9]+")
dept <- substr(str_extract(sub$Id, "_[0-9]+"), 2, 3)

# Make the store trend adjustments. -------------------------------------------
# Store 39 adjustment - improvement < 1; leave it in for now
store_trend_data <- list(c(36, 0.81), c(14, 0.85), c(21, 0.90), c(44, 1.12),
                   c(38, 1.10), c(19, 0.95), c(27, 0.94), c(3, 1.07), c(39, 1.035))
for(v in store_trend_data) {
  sub$sales <- trend_sales(sub$sales, store, dt, v[1], v[2]) 
}

# Make the department trend adjustments. --------------------------------------
sub$sales <- trend_sales(sub$sales, dept, dt, 6, 0.79) # 12.1 improvement
sub$sales <- trend_sales(sub$sales, dept, dt, 5, 0.91) # 14.2 improvement
sub$sales <- trend_sales(sub$sales, dept, dt, 21, 0.94) # 2.7 improvement
sub$sales <- trend_sales(sub$sales, dept, dt, 55, 0.83) # 4.4 improvement
sub$sales <- trend_sales(sub$sales, dept, dt, 72, 0.96) # 15.2 improvement
sub$sales <- trend_sales(sub$sales, dept, dt, 92, 1.04) # 6.6 improvement
sub$sales <- trend_sales(sub$sales, dept, dt, 41, 0.94) # 0.19 improvement
sub$sales <- trend_sales(sub$sales, dept, dt, 48, 1.96) # 0.42 improvement
sub$sales <- trend_sales(sub$sales, dept, dt, 85, 0.90) # 0.93 improvement
sub$sales <- trend_sales(sub$sales, dept, dt, 59, 0.70) # 1.13 improvement
sub$sales <- trend_sales(sub$sales, dept, dt, 54, 0.54) # 0.06 improvement
sub$sales <- trend_sales(sub$sales, dept, dt, 58, 1.13) # 0.46  improvement

# Stuff that did not work:
# dept ==  1, trend = 0.96
# dept == 26, trend = 0.96
# dept == 30, trend = 0.92
# dept == 31, trend = 0.90
# dept == 17, trend = 0.97
# dept == 28, trend = 0.89
# dept == 49, trend = 0.94
# dept == 98, trend = 0.95
######################################################################

######################################################################
# THE REMAINING ADJUSTMENTS ARE MESSY, DON'T GENERALIZE WELL, AND GIVE
# VERY LITTLE LEADERBOARD IMPROVEMENT

# Estimate the impact of markdowns (this should really be based on change in
# markdown, not the entire markdown). 4/16 - can't get change in MarkDown1 to
# give me anything.
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

# sub$sales <- with(sub, sales + ifelse(MarkDown1 > 9990 & dept == "46" , 1164, 0))

# This one didn't work
# sub$sales <- with(sub, sales + ifelse(MarkDown1 > 4970 & dept == "2" , 335, 0))


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
                  MD1_diff = MarkDown1.y - MarkDown1.x,
                  MarkDown1 = MarkDown1.y, 
                  MarkDown2 = MarkDown2.y,
                  MarkDown3 = MarkDown3.y,
                  MarkDown4 = MarkDown4.y,
                  MarkDown5 = MarkDown5.y,
                  unemployment_diff = Unemployment.y - Unemployment.x)
df_temp <- df_dat1[, c("Store", "dt.y", "temp_diff", "MD1_diff")]
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

# Some sanity check calculations
sum(with(test, ifelse(IsHoliday, 5, 1)))
# 150776

test$Id <- with(test, paste(Store, Dept, Date, sep = "_"))
check <- merge(test, sub, by = "Id")
sum(with(check, ifelse(IsHoliday, 5, 1) * Weekly_Sales))
# 2,553,575,561

sum(with(check, ifelse(IsHoliday, 5, 1) * Weekly_Sales))/sum(with(check, ifelse(IsHoliday, 5, 1)))
