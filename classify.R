require(RPostgreSQL)
require(DBI)
library(caret)
library(mgcv)
library(doParallel)
library(ROSE)
library(corrplot)
library(lubridate)

pw <- {"password"}

drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, 
                 dbname = "trojan",
                 host = "localhost", 
                 port = 5432,
                 user = "postgres", 
                 password = pw)

testdata = dbGetQuery(con, "SELECT * FROM testdata")
traindata = dbGetQuery(con, "SELECT * FROM traindata")

head(testdata)
head(traindata)

# feature engineer data time
df = traindata
df$Timestamp <- as.POSIXct(df$Timestamp)
df$day_of_week <- as.factor(wday(df$Timestamp, label = TRUE))
df$hour_of_day <- as.factor(hour(df$Timestamp))

# Chi-Sq. test for association between times
chisq.test(df$class, df$day_of_week)
chisq.test(df$class, df$hour_of_day)

# For IP Address data, take everyting left of the dash
# Split at the first hyphen
split_data <- strsplit(data, "-", fixed = TRUE)

# Extract the first part
data = df$`Flow ID`
first_parts <- sapply(split_data, function(x) x[1])


