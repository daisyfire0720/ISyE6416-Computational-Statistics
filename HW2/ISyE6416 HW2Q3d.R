# ISyE6416 HW2 Question3(d)
#introduce the dataset
setwd("C:/Users/Daisy/Downloads/GT Coursework/Spring Semester 2019/6416 Computational Statistics/Homework/HW2/data")
rx = read.delim("rx.dat")
ry = read.delim("ry.dat")
names(rx) = c('x_value')
names(ry) = c('y_value')
rx = as.matrix(rx)
y = as.matrix(ry)
# set the iteration number and step-size
iter_num = 300
alpha = 0.0002
cost = NULL
# add an intercept term
x = cbind(matrix(rx),1)
# initialize the coefficients
theta = matrix(c(0,0),nrow = 2)
# set the weight function
W = diag(c(exp(-rx^2/(20))))
print(W)
# gradient descent algorithm
for (i in 1:iter_num){
  delta = 2 * t(x) %*% W %*% (x %*% theta - y)
  theta = theta - alpha * delta
  f = t(x %*% theta - y) %*% W %*% (x %*% theta - y)
  cost = rbind(cost,f)
  print(theta)
  print(f)
}
# plot the iteration versus likelihood 
plot(c(1:iter_num),cost,col = 'red', pch="o", lty=1,  xlab="iterations" ,  ylab="cost" )
# plot the output result
plot(x,y)
lines(x,x %*% theta,col = 'blue')
