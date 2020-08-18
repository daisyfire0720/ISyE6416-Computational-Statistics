# ISyE6416 HW2 Question3(c)
# introduce the data
rx = read.delim("rx.dat")
ry = read.delim("ry.dat")
names(rx) = c('x_value')
names(ry) = c('y_value')
x = as.matrix(rx)
y = as.matrix(ry)
# set cost function
cost = function(X,y,theta){
  sum((X %*% theta - y)^2) / (2*length(y))
}
# set learning rate and iteration number
alpha = 0.01
num_iter = 1000
# keep history
cost_history = double(num_iter)
theta_history = list(num_iter)
#initialize the coefficients
theta = matrix(c(0,0),nrow = 2)
# add a column for intercept term
X = cbind(1,matrix(x))
# gradient descient algorithm
for (i in 1:num_iter){
  error = (X %*% theta - y)
  delta = t(X) %*% error / length(y)
  theta = theta - alpha*delta
  cost_history[i] = cost(X,y,theta)
  theta_history[[i]] = theta
}
# output the result
print(theta)
t = theta
test = X %*% t
data = matrix(c(x,y,test),ncol = 3)
data = as.data.frame(data)
names(data) = c('x','y','test')
View(data)
# plot the result
library(ggplot2)
plot1 = ggplot(data,aes(x = x, y = y, test = test))+
  geom_point(mapping = NULL, data = NULL)+
  stat_smooth(method = 'lm', formula = test~x, se = F, size = 2, col = 'steelblue')+
  labs(title = 'Linear Regression by Gradient Descent')+
  theme(title = element_text(size = 12, face = 'bold'))
plot1

