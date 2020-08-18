#IsyE6416 Homework 4 Question 3
install.packages("MASS")
install.packages("parcor")
install.packages("hdi")
install.packages("EAinference")
library(MASS)
library(parcor)
library(hdi)
library(EAinference)
#load the dataset and categorize and clean the dataset
house = read.csv("RealEstate.csv")
house_shortsale = house[which(house$Status == "Short Sale"),]
house_shortsale = house_shortsale[,-c(1,2,7,8)]
house_foreclosure = house[which(house$Status == "Foreclosure"),]
house_foreclosure = house_foreclosure[,-c(1,2,7,8)]
house_regular = house[which(house$Status == "Regular"),]
house_regular = house_regular[,-c(1,2,7,8)]
#prepare the data
matrix_shortsale = as.matrix(house_shortsale)
target1 = matrix_shortsale[,1]
observation1 = matrix_shortsale[,c(2,3,4)]
matrix_foreclosure = as.matrix(house_foreclosure)
target2 = matrix_foreclosure[,1]
observation2 = matrix_foreclosure[,c(2,3,4)]
matrix_regular = as.matrix(house_regular)
target3 = matrix_regular[,1]
observation3 = matrix_regular[,c(2,3,4)]
#fit a ridge regression with cross validation method(parcor)
ridge.cv(observation1,target1,k = 10,lambda = seq(0,50,by = 0.1),plot.it = T)
ridge.cv(observation2,target2,k = 10,lambda = seq(0,50,by = 0.1),plot.it = T)
ridge.cv(observation3,target3,k = 10,lambda = seq(0,50,by = 0.1),plot.it = T)
#fit a lasso regression with cross validation
lasso_fit1 = cv.glmnet(observation1,target1,alpha = 1)
plot(lasso_fit1)
opt_lambda1 = lasso_fit1$lambda.min
opt_lambda1
coef(lasso_fit1)
lasso_fit2 = cv.glmnet(observation2,target2,alpha = 1)
plot(lasso_fit2)
opt_lambda2 = lasso_fit2$lambda.min
opt_lambda2
coef(lasso_fit2)
lasso_fit3 = cv.glmnet(observation3,target3,alpha = 1)
plot(lasso_fit3)
opt_lambda3 = lasso_fit3$lambda.min
opt_lambda3
coef(lasso_fit3)
