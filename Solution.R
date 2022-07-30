library(tidyverse)
library(rpart)
library(rpart.plot)

#load in training data
train <- read_csv("train.csv", col_types = "nffcfnnncncf")
glimpse(train)

#And testing data
test <- read_csv("test.csv", col_types = "nfcfnnncncf")
glimpse(test)

#summary of training data
summary(train)

#Note: embarked variable has two blank values 
#A short Google search shows these should be "S"
train$Embarked <- train$Embarked %>%
  replace(train$Embarked=="", "S") %>%
  droplevels()

#Create model
titanic_model <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + 
                         Embarked, data = train, method = "class")

#Plot model
rpart.plot(titanic_model)

#Predict using test data
titanic_pred <- predict(titanic_model, test, type="class")

#Format data frame for submission
Submission <- data.frame(test$PassengerId, titanic_pred)
names(Submission) <- c("PassengerId","Survived")

#write csv for submission
write_csv(Submission, file = "Submission.csv")