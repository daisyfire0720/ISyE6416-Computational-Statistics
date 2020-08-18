#ISyE6416 Homework 6 Question 2
#direct monte carlo method
theta1_total = as.data.frame(NULL)
for (i in 1:500){
  y1 = rnorm(100, mean = 1, sd = 1)
  theta1_spec = sum(y1 >= 10)/100
  theta1_total = c(theta1_total,theta1_spec)
}
mean1 = mean(theta1_total)
std1 = sd(theta1_total)

#importance sampling for Gaussian distribution
y2 = rnorm(100, mean = 10, sd = 1)
ratio = exp(((y2 - 1)^2 - (y2 - 10)^2)/2)
theta2 = sum((y2 >= 10)*ratio)/100
mean2 = mean(theta2)
std2 = sd(theta2)

