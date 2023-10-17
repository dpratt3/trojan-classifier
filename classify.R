require(RPostgreSQL)
require(DBI)
library(caret)
library(mgcv)
library(doParallel)
library(ROSE)
library(corrplot)

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


