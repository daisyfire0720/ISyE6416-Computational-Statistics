#ISyE 6416 Homework 2 Question 2
#import the data
install.packages("dplyr")
install.packages("tidyr")
library(dplyr)
library(tidyr)
setwd("C:/Users/Daisy/Desktop/data")
x_value = read.delim("logit-x.dat")
y_value = read.delim("logit-y.dat")
names(x_value) = c("x_value")
View(x_value)
x_value = as.matrix(x_value)
ncol(x_value)
x_valid = as.matrix(sapply(x_value, as.numeric)) 
print(x_valid)
#x_value = as.character(x_value)
#x_value = suppressWarnings(as.numeric(as.character(x_value)))

x_value %>% separate(x_value, c(x1,x2))
x_value = separate(x_value)
#load data
x1 = as.matrix(x_value)
print(x1)
nrow(x1)
ncol(x1)
class(x1)

x = apply(x1,2,as.numeric)
print(x)
y = as.matrix(y_value)
print(y)
m = length(y)
X = cbind(rep(1,nrow(x)),x)
theta = rep(0,3)
#define the sigmoid function and cost function
sigmoid = function(x){
  1/(1+exp(-x))
}

compCost = function(theta,X,y){
  J = (-1/m)*sum(y*log(sigmoid(X%*%theta))+(1-y)*log(1-sigmoid(X%*%theta)))
  
  return(J)
}
#iteration process
newton = function(X,y,theta,num_iter){
  
  library(numDeriv)
  library(MASS)
  J_hist = vector()
  #loop for iteration
  for (i in 1:num_iter){
    grad = (1/m)*(t(X)%*%(sigmoid(X%*%theta) - y))
    H = hessian(compCost,theta,method = "complex", X = X, y = y)
    
    theta = theta - ginv(H)%*%grad
    J_hist[i] = compCost(theta,X,y)
  }
  result = list(theta,J_hist)
  return(result)
  
}