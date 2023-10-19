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

set.seed(123) # reproducibility

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

# Create a pre-processing object to impute with medians
preproc <- preProcess(traindata, method = "medianImpute")

# Apply the pre-processing to your data
# aggr(traindata, col = c("navy", "red"), numbers = TRUE, sortVars = TRUE)
preproc <- preProcess(traindata, method = "medianImpute")
traindata <- predict(preproc, newdata = traindata)

# feature engineer data time into days and hours
traindata$Timestamp <- as.POSIXct(traindata$Timestamp)
traindata$day_of_week <- as.factor(wday(traindata$Timestamp, label = TRUE))
traindata$hour_of_day <- as.factor(hour(traindata$Timestamp))
traindata <- traindata[, !names(traindata) %in% "Timestamp"]
traindata <- traindata[, c("day_of_week", "hour_of_day", names(traindata)[!names(traindata) %in% c("day_of_week", "hour_of_day")])]

# Chi-Sq. test for association between times
chisq.test(traindata$class, traindata$day_of_week)
chisq.test(traindata$class, traindata$hour_of_day)

# For IP Address data, take everyting left of the dash
# Split at the first hyphen and Extract the first part
data = traindata$`Flow ID`
split_data <- strsplit(data, "-", fixed = TRUE)
traindata$`Flow ID` <- as.factor(sapply(split_data, function(x) x[1]))
traindata[, 1:9] = apply(traindata[, 1:9], 2, as.factor)

cat_pvalues = vector()
for(i in 1:9){
    cat_pvalues[i] = (chisq.test(traindata$class, traindata[, i]))$p.value
}

# Omit nonsignificant columns
traindata <- traindata[, -which(cat_pvalues > 0.01)]

# remove near-zero variance
zero_var_cols <- nearZeroVar(traindata)
traindata = traindata[, -zero_var_cols]

# include ONLY the numeric variables that differ between the classes (bonferroni)
pvalues = vector()
indices = vector()

for(i in 9:(dim(traindata)[[2]] - 1)){
    trojan_data <- traindata[traindata$class == "Trojan", i]
    benign_data <- traindata[traindata$class == "Benign", i]

    # Perform the Wilcoxon rank-sum test
    pvalues[i] = wilcox.test(trojan_data, benign_data, na.action = na.omit)$p.value
    indices[i] = i
    print(i)
}

traindata = traindata[, -which(pvalues > 0.01 / length(pvalues))] # bonferroni correction]

correlations = cor(traindata[, indices[9: (dim(traindata)[[2]] - 1) ] ] )
# Assuming you want to identify pairs with a correlation of at least 0.7
high_corr_pairs <- which(abs(correlations) >= 0.90 & correlations != 1, arr.ind = TRUE)

print(high_corr_pairs) # just for fun

numeric_pca = prcomp(traindata[, 9:(dim(traindata)[[2]] - 1)], center = TRUE, scale = TRUE)
cumulative_variance <- cumsum(numeric_pca$sdev^2) / sum(numeric_pca$sdev^2)
plot(cumulative_variance, type = "b", xlab = "Number of Principal Components", ylab = "Cumulative Variance Explained")

# look at the cumulative variance contained in the above
traindata_pca = cbind.data.frame(traindata[, 1:8], numeric_pca$x[ ,1:22], traindata$class)
colnames(traindata_pca)[ncol(traindata_pca)] <- "class"
traindata_pca$class <- as.factor(ifelse(traindata_pca$class == "Trojan", 1, 0))

#down sample
set.seed(123)
sample_size <- round(nrow(traindata_pca) * 0.20)
rows = sample(nrow(traindata_pca), sample_size)
down_sampled_train <- traindata_pca[rows, ]
testing_data <- traindata_pca[-rows, ]

dbWriteTable(con, name = "down_sampled_train", value = down_sampled_train, overwrite = TRUE)
dbWriteTable(con, name = "down_sampled_test", value = testing_data, overwrite = TRUE)