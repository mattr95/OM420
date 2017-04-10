install.packages("randomForest")
library(randomForest)


# Working directory will likely be different on your computer
setwd("~/Google Drive/OM420 Final Project")

cust <- read.csv("Day2_directmarketing.csv", sep=";")

# No incomplete cases
nrow(cust[!complete.cases(cust),])

# Convert pdays values of -1 to NULL
cust$pdays[cust$pdays == -1] <- 10000

# Delete observation where previous = 275
cust <- cust[cust$previous != 275,]


# Random Forest Classification ===============================================

# Split data set into training and test data
train_size = round((2/3) * nrow(cust), 0)
id <- sample(1:nrow(cust), size=train_size, replace=FALSE)
cust_train = cust[id,]
cust_test = cust[-id,]


fit <- randomForest(y~.,
                    data = cust_train,
                    nodesize = 30,
                    ntree=100000)


# Training set error
predictCust_train <- table(cust_train$y, predict(fit, cust_train, type="class"))
train_error <- 1 - (sum(diag(predictCust_train))/sum(predictCust_train))

# Test set error
predictCust_test <- table(cust_test$y, predict(fit, cust_test, type="class"))
test_error <- 1 - (sum(diag(predictCust_test))/sum(predictCust_test))
