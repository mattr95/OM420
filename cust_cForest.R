install.packages('party')
library(party)

# Working directory will likely be different on your computer
setwd("~/Google Drive/OM420 Final Project")

cust <- read.csv("Day2_directmarketing.csv", sep=";")

# No incomplete cases
nrow(cust[!complete.cases(cust),])

# Convert pdays values of -1 to NULL
cust$pdays[cust$pdays == -1] <- NA

# Delete observation where previous = 275
cust <- cust[cust$previous != 275,]


# Random Forest (Conditional inference trees) Classification ========================

# Split data set into training and test data
train_size = round((2/5) * nrow(cust), 0)
id <- sample(1:nrow(cust), size=train_size, replace=FALSE)
cust_train = cust[id,]
cust_test = cust[-id,]


fit <- cforest(y~.,
               data = cust_train,
               controls = cforest_unbiased(ntree=100))


# Training set error
predictCust_train <- table(cust_train$y, predict(fit, cust_train, OOB=TRUE, type = "response"))
train_error <- 1 - (sum(diag(predictCust_train))/sum(predictCust_train))

# Test set error
predictCust_test <- table(cust_test$y, predict(fit, cust_test, OOB=TRUE, type = "response"))
test_error <- 1 - (sum(diag(predictCust_test))/sum(predictCust_test))
