######## GLMSub on Kaggle using GLM ##########

library(tidyverse)
library(ggplot2)
library(rsample)
library(dplyr)

setwd("/Users/singhh/Downloads/CSCI48900/agriculture-loan-prediction")

X <- read.csv("train_comp1_2020.csv")
Xt <- read.csv("test_comp1_2020.csv")

set.seed(144)
# head(X$CreditScore)
# X$CreditScore <- X$CreditScore %>% replace(.=="NULL", 1) # replace with 1
# head(X$CreditScore)

head(X$TYCornUnits)
X$TYCornUnits <- X$TYCornUnits %>% replace(.=="NULL", 1) # replace with 1
head(X$TYCornUnits)
X$TYCornUnits <- as.numeric(as.factor(X$TYCornUnits))
head(X$TYCornUnits)

head(X$TYBeanUnits)
X$TYBeanUnits <- X$TYBeanUnits %>% replace(.=="NULL", 1) # replace with 1
head(X$TYBeanUnits)
X$TYBeanUnits <- as.numeric(as.factor(X$TYBeanUnits))
head(X$TYBeanUnits)

head(X$TYWheatUnits)
X$TYWheatUnits <- X$TYWheatUnits %>% replace(.=="NULL", 1) # replace with 1
head(X$TYWheatUnits)
X$TYWheatUnits <- as.numeric(as.factor(X$TYWheatUnits))
head(X$TYWheatUnits)

head(X$LYCornUnits)
X$LYCornUnits <- X$LYCornUnits %>% replace(.=="NULL", 1) # replace with 1
head(X$LYCornUnits)
X$LYCornUnits <- as.numeric(as.factor(X$LYCornUnits))
head(X$LYCornUnits)

head(X$LYBeanUnits)
X$LYBeanUnits <- X$LYBeanUnits %>% replace(.=="NULL", 1) # replace with 1
head(X$LYBeanUnits)
X$LYBeanUnits <- as.numeric(as.factor(X$LYBeanUnits))
head(X$LYBeanUnits)

head(X$LYWheatUnits)
X$LYWheatUnits <- X$LYWheatUnits %>% replace(.=="NULL", 1) # replace with 1
head(X$LYWheatUnits)
X$LYWheatUnits <- as.numeric(as.factor(X$LYWheatUnits))
head(X$LYWheatUnits)

# head(X$AT28)
# X$AT28 <- X$AT28 %>% replace(.=="NULL", 1) # replace with 1
# head(X$AT28)
# X$AT28 <- as.numeric(as.factor(X$AT28))
# head(X$AT28)

head(X$AT33)
X$AT33 <- X$AT33 %>% replace(.=="NULL", 1) # replace with 1
head(X$AT33)
X$AT33 <- as.numeric(as.factor(X$AT33))
head(X$AT33)

head(X$AT36)
X$AT36 <- X$AT36 %>% replace(.=="NULL", 1) # replace with 1
head(X$AT36)
X$AT36 <- as.numeric(as.factor(X$AT36))
head(X$AT36)

# head(X$BC33)
# X$BC33 <- X$BC33 %>% replace(.=="NULL", 1) # replace with 1
# head(X$BC33)
# X$BC33 <- as.numeric(as.factor(X$BC33))
# head(X$BC33)

head(X$BC98)
X$BC98 <- X$BC98 %>% replace(.=="NULL", 1) # replace with 1
head(X$BC98)
X$BC98 <- as.numeric(as.factor(X$BC98))
head(X$BC98)

head(X$COLL)
X$COLL <- X$COLL %>% replace(.=="NULL", 1) # replace with 1
head(X$COLL)
X$COLL <- as.numeric(as.factor(X$COLL))
head(X$COLL)

head(X$G068)
X$G068 <- X$G068 %>% replace(.=="NULL", 1) # replace with 1
head(X$G068)
X$G068 <- as.numeric(as.factor(X$G068))
head(X$G068)

head(X$G091)
X$G091 <- X$G091 %>% replace(.=="NULL", 1) # replace with 1
head(X$G091)
X$G091 <- as.numeric(as.factor(X$G091))
head(X$G091)

head(X$G093)
X$G093 <- X$G093 %>% replace(.=="NULL", 1) # replace with 1
head(X$G093)
X$G093 <- as.numeric(as.factor(X$G093))
head(X$G093)

head(X$G096)
X$G096 <- X$G096 %>% replace(.=="NULL", 1) # replace with 1
head(X$G096)
X$G096 <- as.numeric(as.factor(X$G096))
head(X$G096)

head(X$MT28)
X$MT28 <- X$MT28 %>% replace(.=="NULL", 1) # replace with 1
head(X$MT28)
X$MT28 <- as.numeric(as.factor(X$MT28))
head(X$MT28)

head(X$MT36)
X$MT36 <- X$MT36 %>% replace(.=="NULL", 1) # replace with 1
head(X$MT36)
X$MT36 <- as.numeric(as.factor(X$MT36))
head(X$MT36)

head(X$RE34)
X$RE34 <- X$RE34 %>% replace(.=="NULL", 1) # replace with 1
head(X$RE34)
X$RE34 <- as.numeric(as.factor(X$RE34))
head(X$RE34)

head(X$S063)
X$S063 <- X$S063 %>% replace(.=="NULL", 0) # replace with 1
head(X$S063)
X$S063 <- as.numeric(as.factor(X$S063))
head(X$S063)

# Conversions for Xt dataset

# head(Xt$CreditScore)
# Xt$CreditScore <- Xt$CreditScore %>% replace(.=="NULL", 1) # replace with 0
# head(Xt$CreditScore)

head(Xt$TYCornUnits)
Xt$TYCornUnits <- Xt$TYCornUnits %>% replace(.=="NULL", 1) # replace with 1
head(Xt$TYCornUnits)
Xt$TYCornUnits <- as.numeric(as.factor(Xt$TYCornUnits))
head(Xt$TYCornUnits)

head(Xt$TYBeanUnits)
Xt$TYBeanUnits <- Xt$TYBeanUnits %>% replace(.=="NULL", 1) # replace with 1
head(Xt$TYBeanUnits)
Xt$TYBeanUnits <- as.numeric(as.factor(Xt$TYBeanUnits))
head(Xt$TYBeanUnits)

head(Xt$TYWheatUnits)
Xt$TYWheatUnits <- Xt$TYWheatUnits %>% replace(.=="NULL", 1) # replace with 1
head(Xt$TYWheatUnits)
Xt$TYWheatUnits <- as.numeric(as.factor(Xt$TYWheatUnits))
head(Xt$TYWheatUnits)

head(Xt$LYCornUnits)
Xt$LYCornUnits <- Xt$LYCornUnits %>% replace(.=="NULL", 1) # replace with 1
head(Xt$LYCornUnits)
Xt$LYCornUnits <- as.numeric(as.factor(Xt$LYCornUnits))
head(Xt$LYCornUnits)

head(Xt$LYBeanUnits)
Xt$LYBeanUnits <- Xt$LYBeanUnits %>% replace(.=="NULL", 1) # replace with 1
head(Xt$LYBeanUnits)
Xt$LYBeanUnits <- as.numeric(as.factor(Xt$LYBeanUnits))
head(Xt$LYBeanUnits)

head(Xt$LYWheatUnits)
Xt$LYWheatUnits <- Xt$LYWheatUnits %>% replace(.=="NULL", 1) # replace with 1
head(Xt$LYWheatUnits)
Xt$LYWheatUnits <- as.numeric(as.factor(Xt$LYWheatUnits))
head(Xt$LYWheatUnits)

# head(Xt$AT28)
# Xt$AT28 <- Xt$AT28 %>% replace(.=="NULL", 1) # replace with 1
# head(Xt$AT28)
# Xt$AT28 <- as.numeric(as.factor(Xt$AT28))
# head(Xt$AT28)

head(Xt$AT33)
Xt$AT33 <- Xt$AT33 %>% replace(.=="NULL", 1) # replace with 1
head(Xt$AT33)
Xt$AT33 <- as.numeric(as.factor(Xt$AT33))
head(Xt$AT33)

head(Xt$AT36)
Xt$AT36 <- Xt$AT36 %>% replace(.=="NULL", 1) # replace with 1
head(Xt$AT36)
Xt$AT36 <- as.numeric(as.factor(Xt$AT36))
head(Xt$AT36)

# head(Xt$BC33)
# Xt$BC33 <- Xt$BC33 %>% replace(.=="NULL", 1) # replace with 1
# head(Xt$BC33)
# Xt$BC33 <- as.numeric(as.factor(Xt$BC33))
# head(Xt$BC33)

head(Xt$BC98)
Xt$BC98 <- Xt$BC98 %>% replace(.=="NULL", 1) # replace with 1
head(Xt$BC98)
Xt$BC98 <- as.numeric(as.factor(Xt$BC98))
head(Xt$BC98)

head(Xt$COLL)
Xt$COLL <- Xt$COLL %>% replace(.=="NULL", 1) # replace with 1
head(Xt$COLL)
Xt$COLL <- as.numeric(as.factor(Xt$COLL))
head(Xt$COLL)

head(Xt$G068)
Xt$G068 <- Xt$G068 %>% replace(.=="NULL", 1) # replace with 1
head(Xt$G068)
Xt$G068 <- as.numeric(as.factor(Xt$G068))
head(Xt$G068)

head(Xt$G091)
Xt$G091 <- Xt$G091 %>% replace(.=="NULL", 1) # replace with 1
head(Xt$G091)
Xt$G091 <- as.numeric(as.factor(Xt$G091))
head(Xt$G091)

head(Xt$G093)
Xt$G093 <- Xt$G093 %>% replace(.=="NULL", 1) # replace with 1
head(Xt$G093)
Xt$G093 <- as.numeric(as.factor(Xt$G093))
head(Xt$G093)

head(Xt$G096)
Xt$G096 <- Xt$G096 %>% replace(.=="NULL", 1) # replace with 1
head(Xt$G096)
Xt$G096 <- as.numeric(as.factor(Xt$G096))
head(Xt$G096)

head(Xt$MT28)
Xt$MT28 <- Xt$MT28 %>% replace(.=="NULL", 1) # replace with 1
head(Xt$MT28)
Xt$MT28 <- as.numeric(as.factor(Xt$MT28))
head(Xt$MT28)

head(Xt$MT36)
Xt$MT36 <- Xt$MT36 %>% replace(.=="NULL", 1) # replace with 1
head(Xt$MT36)
Xt$MT36 <- as.numeric(as.factor(Xt$MT36))
head(Xt$MT36)

head(Xt$RE34)
Xt$RE34 <- Xt$RE34 %>% replace(.=="NULL", 1) # replace with 1
head(Xt$RE34)
Xt$RE34 <- as.numeric(as.factor(Xt$RE34))
head(Xt$RE34)

head(Xt$S063)
Xt$S063 <- Xt$S063 %>% replace(.=="NULL", 0) # replace with 1
head(Xt$S063)
Xt$S063 <- as.numeric(as.factor(Xt$S063))
head(Xt$S063)

# Impute the data
library(mice)
imputed <- mice::complete(mice(X,m=5,maxit=50,meth='pmm',seed=500))
imputed <- subset(imputed, select = -c(id))
head(imputed$S063)

# Fit the model
model.fit = glm(PastDue ~ CreditScore + TYCornUnits + TYBeanUnits + TYWheatUnits +
                  LYCornUnits + LYBeanUnits + LYWheatUnits + AT33 + AT36 +
                  BC98 + COLL + G068 + G091 + G093 + G096 + MT28 + MT36 + 
                  RE34 + S063,
                  family = binomial, data=imputed)
summary(model.fit)

# Refit the model with most important variables
model.fit = glm(PastDue ~ CreditScore + TYCornUnits + TYBeanUnits + TYWheatUnits +
                  LYCornUnits + LYBeanUnits + AT36 + G093,
                  family = binomial(), data=imputed)
summary(model.fit)

# Make the prediction on the Xt data
pred <- predict(model.fit, newdata = Xt, type = 'response')
# Save the solution to a dataframe with two columns
GLMSub <- data.frame(id = Xt$id, PastDue = round(pred, 1))
# Write the output to file
write.csv(GLMSub, file = 'GLMSub.csv', row.names = FALSE)
