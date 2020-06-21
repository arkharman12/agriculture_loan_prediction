############ GLM Logistic Regression ############

library('ggplot2')
library('ggthemes')
library('scales')
library('dplyr')
library('mice')
library('randomForest')
library('readr')

# Set the working directory
setwd("/Users/singhh/Downloads/CSCI48900/agriculture-loan-prediction")

agri_train <- read.csv("train_comp1_2020.csv", stringsAsFactors = F)
agri_test <- read.csv("test_comp1_2020.csv", stringsAsFactors = F)

agriculture<-bind_rows(agri_train,agri_test)

# Data check
str(agri_train)
table(agri_train$PastDue)
3876/95641

summary(agri_train)
head(agri_train)
colnames(agri_train)


head(agri_train$CreditScore)
agri_train$CreditScore <- agri_train$CreditScore %>% replace(.=="NULL", 0) # replace with 0
head(agri_train$CreditScore)

head(agri_train$TYCornUnits)
agri_train$TYCornUnits <- agri_train$TYCornUnits %>% replace(.=="NULL", 0) # replace with 0
head(agri_train$TYCornUnits)
agri_train$TYCornUnits <- as.numeric(as.factor(agri_train$TYCornUnits))
head(agri_train$TYCornUnits)


set.seed(144)
vars = setdiff(names(agri_train), "PastDue")
imputed = complete(mice(agri_train[vars]))
agri_train[vars] = imputed
imputed <- complete(mice(agri_train,m=5,maxit=50,meth='pmm',seed=144))
imputed <- subset(imputed, select = -c(id))

library(caTools)
sample = sample.split(agri_train, SplitRatio = 0.7)
agri_train = subset(agri_train,sample == TRUE)
agri_test = subset(agri_train,sample == FALSE)

model1 = glm(PastDue ~ CreditScore + TYCornUnits, data = imputed, family = "binomial")
summary(model1)


agri_test$predicted_risk = predict(model1, newdata = agri_test, type="response")

table(agri_test$PastDue, agri_test$predicted_risk > 0.5)
accuracy = 14115/(14115+563)
accuracy

CompSubmission <- data.frame(id = agri_test$id, PastDue = round(agri_test$predicted_risk, 0))
write.csv(CompSubmission, file = 'CompSubmission.csv', row.names = FALSE)
