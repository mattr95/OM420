install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)

install.packages("arulesCBA")
library(arulesCBA)
library(caret)

cust <- read.csv("Day2_directmarketing.csv", sep=";")
# Convert pdays values of -1 to NULL
cust$pdays[cust$pdays == -1] <- NA

cust$duration <- sapply(cust[12], function(x) discretize(x, categories=10))
#cust$age <- sapply(cust[1], function(x) discretize(x, categories=5))
#cust$balance <- sapply(cust[6], function(x) discretize(x, categories=5))
#cust$day <- sapply(cust[10], function(x) discretize(x, categories=31))
#cust$campaign <- sapply(cust[13], function(x) discretize(x, categories=5))
#cust$pdays <- sapply(cust[14], function(x) discretize(x, categories=5))
cust$previous <- sapply(cust[15], function(x) discretize(x, categories=5))

cust$duration <- as.factor(cust$duration)
#cust$age <- as.factor(cust$age)
#cust$balance <- as.factor(cust$balance)
#cust$day <- as.factor(cust$day)
#cust$campaign <- as.factor(cust$campaign)
#cust$pdays <- as.factor(cust$pdays)
cust$previous <- as.factor(cust$previous)

cust <- cust[,c(2,3,4,5,7,8,11,12,16,17)]



# Split data set into training and test data
train_size = round((2/3) * nrow(cust), 0)
id <- sample(1:nrow(cust), size=train_size, replace=FALSE)
cust_train = cust[id,]
cust_test = cust[-id,]

classifier <- CBA(y~., cust_train, supp = 0.05, conf = 0.4)
print(classifier)


#Using the Classifier on training data
classes <- predict(classifier, cust_train)
#classes
#table(classes)
predResults_train <- table(cust_train$y, classes)
predResults_train
train.error <- 1-sum(diag(predResults_train))/sum(predResults_train)
train.error

#Using the Classifier on test data
classes2 <- predict(classifier, cust_test)
#classes2
#table(classes2)
predResults_test <- table(cust_test$y, classes2)
predResults_test
test.error <- 1-sum(diag(predResults_test))/sum(predResults_test)
test.error





# THIS DOESNT WORK =========================================================================

#Discretize the continuous variables


#Add back the "Species" Column to the discretized dataset
cust.disc$age <- age.disc
cust.disc$balance <- balance.disc
cust.disc$day <- day.disc
cust.disc$duration <- duration.disc
cust.disc$campaign <- campaign.disc
cust.disc$pdays <- pdays.disc
cust.disc$previous <- previous.disc
cust.disc$job <- cust$job
cust.disc$marital <- cust$marital
cust.disc$education <- cust$education
cust.disc$default <- cust$default
cust.disc$housing <- cust$housing
cust.disc$loan <- cust$loan
cust.disc$contact <- cust$contact
cust.disc$month <- cust$month
cust.disc$poutcome <- cust$poutcome
cust.disc$y <- cust$y

rules <- apriori(cust.disc, parameter = list(minlen = 2, supp = 0.4, conf = 0.6))









