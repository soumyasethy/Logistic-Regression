# Cleaning
rm(list = ls())

# Importing Dataset 
library(readr)
data <- read_csv("~/Desktop/Data Science/DataSet/Pima.te.csv")
View(data)
nrow(data)

# Checking and Removing NA
is.na(data)
data <- na.omit(data)

# Trying to convert Yes-> 1 , No -> 0
data$type <- sapply(data$type,switch,"Yes"=1,"No"=0)

# Split Data in Training and Testing Set.
library(caTools)
require(caTools)
set.seed(100) 
sample = sample.split(data, SplitRatio = .80)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)


# Running Models.
fit <- glm(typ~.-type,train, family = "binomial")
#?glm: Generalized Linear Models,  "~": Vs, ".": all the variables, "-": ignore variable.

# finding summary
summary(fit)


# Optimizing Models.( Removing not signifed Variable based Residual Deviance & AIC Value )
var<- step(fit)
# Based on best AIC Value which we got from STEP Function
# Step Function will find the variable for your model.
fitFinal <- glm(typ ~ npreg + glu + bmi + ped,train, family = "binomial")
summary(fitFinal)

#Manually finding...
fit2 <- glm(typ~.-type -bp,train, family = "binomial")
summary(fit2)
fit3 <- glm(typ~.-type -age,train, family = "binomial")
summary(fit3)
fit4 <- glm(typ~.-type -skin,train, family = "binomial")
summary(fit4)
fit5 <- glm(typ~.-type -bp -age -skin,train, family = "binomial")
summary(fit5)
plot(fit5)


# Predicting with Test Data
res <- predict(fitFinal,test,type = "response") #------->>>>>>>> Test Data
#test
#res
# Confusion Matrix with random Threshold on Test Dataset.
table(ActualValue=test$typ,PredictValues = res>0.5)#Testing Random 0.5
table(ActualValue=test$typ,PredictValues = res>0.3)#Testing Random 0.3


# Roc Curve: To calculate accurate Threshold Value for Confusion Matrics with Train Data Set.
res <- predict(fitFinal,train,type = "response")
# table(ActualValue=train$typ,PredictValues = res>0.3)
# Installing ROCR Package
# install.packages("ROCR")
library(ROCR)
ROCRPred = prediction(res,train$type)
ROCRPref = performance(ROCRPred,"tpr","fpr")
plot(ROCRPref,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))
# We see from graph, threshold Value = 0.3(Best)


# Now Testing with TestData on basis of calculated Threshold Value i.e "0.3"
res <- predict(fit,test,type = "response") 
# Confusion Matrix
table(ActualValue=test$typ,PredictValues = res>0.3)
accurarcy <- 32+11/31+6+11+10#tp+fp/tp+tpr+fp+fpr
accurarcy#59.35484

  