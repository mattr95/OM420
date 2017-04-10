install.packages("rpart.plot")
install.packages("rattle")
library(rpart)
library(rattle)
library(rpart.plot)

# Working directory will likely be different on your computer
setwd("~/Google Drive/OM420 Final Project")

cust <- read.csv("Day2_directmarketing.csv", sep=";")

# No incomplete cases
nrow(cust[!complete.cases(cust),])

# Convert pdays values of -1 to NULL
cust$pdays[cust$pdays == -1] <- NA


# Make a tree =============================================================

# Split data set into training and test data
train_size = round((2/3) * nrow(cust), 0)
id <- sample(1:nrow(cust), size=train_size, replace=FALSE)
cust_train = cust[id,]
cust_test = cust[-id,]

fit <- rpart(y~.,
             data=cust_train, 
             method="class", 
             parms=list(split="information"),
             cp=0, minbucket=100, minsplit=200)

fancyRpartPlot(fit)
printcp(fit)
plotcp(fit)

opt <- fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
finalTree <- prune(fit, cp=0.01)
fancyRpartPlot(finalTree)

# Training set error
predictCust_train <- table(cust_train$y, predict(finalTree, cust_train, type="class"))
train_error <- 1 - (sum(diag(predictCust_train))/sum(predictCust_train))

# Test set error
predictCust_test <- table(cust_test$y, predict(finalTree, cust_test, type="class"))
test_error <- 1 - (sum(diag(predictCust_test))/sum(predictCust_test))
