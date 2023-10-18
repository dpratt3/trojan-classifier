require(RPostgreSQL)
require(DBI)
library(caret)
library(mgcv)
library(doParallel)
library(ROSE)
library(corrplot)
library(lubridate)
library(mice)
library(VIM)
library(doParallel)


cl <- makeCluster(15)  # Use 4 cores
registerDoParallel(cl)

pw <- {"password"}

drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, 
                 dbname = "trojan",
                 host = "localhost", 
                 port = 5432,
                 user = "postgres", 
                 password = pw)

traindata = dbGetQuery(con, "SELECT * FROM traindata") 
testdata = dbGetQuery(con, "SELECT * FROM testdata") 

# Evaluate columns 1-84 for missingness in traindata
for (i in 1:84) {
    print(sum(is.na(traindata[, i])))
}

# Evaluate columns 1-84 for missingness in testdata
for (i in 1:84) { 
    print(sum(is.na(testdata[, i])))
}

# impute missing data
imputed_data <- foreach(i = 1:3) %dopar% {
  VIM::kNN(traindata, k = 3)
}

# stop cluser and garbage collect
stopCluster(cl)
gc()

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

# include ONLY the numeric variables that differ between the classes (bonferroni)
pvalues = vector()
indices = vector()
for(i in 12:84){
    trojan_data <- traindata[traindata$class == "Trojan", i]
    benign_data <- traindata[traindata$class == "Benign", i]

    # Perform the Wilcoxon rank-sum test
    pvalues[i] = wilcox.test(trojan_data, benign_data, na.action = na.omit)$p.value
    indices[i] = i
    print(i)
}

correlations = cor(traindata[, indices[12: 84] ] )
# Assuming you want to identify pairs with a correlation of at least 0.7
high_corr_pairs <- which(abs(correlations) >= 0.95 & correlations != 1, arr.ind = TRUE)

# Remove data with constant columns
# constant_columns <- apply(traindata[, 12:84], 2, var) == 0
# traindata <- traindata[, -as.numeric(which(constant_columns))]
# pca_result <- prcomp(traindata[, 12:84], center = TRUE, scale = TRUE)