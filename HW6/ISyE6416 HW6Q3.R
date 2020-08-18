#ISyE6416 Homework 6 Question 3
#introduce the dataset
student = read.csv("HW06Q3.csv")
#calculate the correlation coefficient
cor(student$LSAT,student$GPA)

#nonparametric bootstrapping
cor_value = NULL
for (i in 1:1000){
  LSAT_value = NULL
  GPA_value = NULL
  for (j in 1:15){
    ram = sample(1:15,1,replace = F)
    LSAT_spec = student$LSAT[ram]
    GPA_spec = student$GPA[ram]
    LSAT_value = c(LSAT_value,LSAT_spec)
    GPA_value = c(GPA_value,GPA_spec)
  }
  cor_value = c(cor_value,cor(LSAT_value,GPA_value))
}
#calculate standard deviation
std = sqrt((1/(1000-1))*sum((cor_value - mean(cor_value))^2))
std
#determine the confidence interval
alpha = 0.05
CI_upper  = 2*mean(cor_value) - quantile(cor_value, 1 - alpha/2)
CI_lower  = 2*mean(cor_value) - quantile(cor_value, alpha/2)
CI = c(CI_lower,CI_upper)
CI
#plot the density curve
plot(density(cor_value),xlab = "Correlation Density Plot Using Nonparametric Bootstrapping")

#parametric bootstrapping
cor_value_new = NULL
mu = c(mean(student$LSAT),mean(student$GPA))
sigma = cov(student)
library(MASS)
for (i in 1:1000){
  LSAT_value_new = NULL
  GPA_value_new = NULL
  for (j in 1:15){
    ram_new = mvrnorm(1,mu,sigma)
    LSAT_spec_new = ram_new[1]
    GPA_spec_new = ram_new[2]
    LSAT_value_new = c(LSAT_value_new,LSAT_spec_new)
    GPA_value_new = c(GPA_value_new,GPA_spec_new)
  }
  cor_value_new = c(cor_value_new,cor(LSAT_value_new,GPA_value_new))
}
#calculate standard deviation
std_new = sqrt((1/(1000-1))*sum((cor_value_new - mean(cor_value_new))^2))
std_new
#determine the confidence interval
alpha = 0.05
CI_upper_new  = 2*mean(cor_value_new) - quantile(cor_value_new, 1 - alpha/2)
CI_lower_new  = 2*mean(cor_value_new) - quantile(cor_value_new, alpha/2)
CI_new = c(CI_lower_new,CI_upper_new)
CI_new
#plot the density curve
plot(density(cor_value_new),xlab = "Correlation Density Plot Using Parametric Bootstrapping")