#Cleaning
rm(list = ls())

#Reading CSV File  
library(readr)  
cars <- read_csv("~/Desktop/Data Science/cars.csv") 
#View imported Data  
View(cars)  
#data(cars)  
#Top 6 Data Preview  
head(cars)  
#Last 6 Data Preview  
tail(cars)  

#Checking Number of Rows  
nrow(cars)  
#Checking Number of Columns  
ncol(cars)  
#Checking Column Names  
names(cars)  

#Checking Summary(Mean, Mode, Median)  
summary(cars)  

#Check missing values  
is.na(cars)  
is.na(cars$speed)  

#Omitting NA Values  
is.na(cars$dist)  
is.null(cars$speed)

#Check linear trend by ploting graph  

plot(cars$speed,col=10)  
plot(cars$dist,col="red")  
plot(cars$dist,cars$speed,col=5)  

#Check outliers by ploting Box Plot Graph  
boxplot(cars$speed)  
boxplot(cars$dist)  
boxplot(cars,col=3)   


#Applying Linear Regression Model  
fit<-lm(speed~dist,cars)  
fit


#Checking Summary after applying Linear Regression to the Model  
summary(fit)  
abline(fit)  

#Ploting Multiple graph  
par(mfrow=c(2,2))  
plot((fit)) 


#Y <- 8.28391+0.16557*50;
#Y <- alpha + Beta(X-Value);
#Y
