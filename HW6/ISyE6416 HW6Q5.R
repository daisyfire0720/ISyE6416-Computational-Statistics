#ISyE6416 Homework6 Question 5

#Gibbs sampling for change point detection
gibbsloop = function(nsim,y,a,b,kposs,phi,k){
  #matrix to store simulated values for each cycle
  out = matrix(NA,nrow = nsim, ncol = 3)
  #number of observations
  n = length(y)
  
  for (i in 1:nsim){
    #generate value from full conditional of phi based on current parameter values
    lambda = rgamma(1, a + sum(y[1:k]), b + k)
    phi = rgamma(1, a + sum(y[min((k+1),n):n]), b + n - k)
    #generate value of k and determine probability for k
    pmf = kprobloop(kposs,y,lambda,phi)
    k = sample(x = kposs, size = 1, prob = pmf)
    
    out[i,] = c(lambda, phi, k)
  }
  out
}

#calculate the logsumexp function
logsumexp = function(x){
  log(sum(exp(x - max(x)))) + max(x)
}

#determine pmf for full condition of k
kprobloop = function(kposs, y, lambda, phi){
  #create vector to store argument of exponential function of unnormalied pmf
  x = numeric(length(kposs))
  for (i in kposs){
    x[i] = i*(phi - lambda) + sum(y[1:i]) * log(lambda/phi)
  }
  #return full conditional pmf of k
  return(exp(x - logsumexp(x)))
}

#set initial values
nsim = 5200
set.seed(1)
n = 100
lambda = 2;
phi = 1;

#generate time series data
counts = ts(c(rpois(n/2,lambda = lambda),rpois(n/2,lambda = phi)))
chain = gibbsloop(nsim = nsim, y = counts, a = 2, b = 2, kposs =1:100, phi = 1,k = 50)

#convert result into dataframe, discard the first 200 iteration results
chain_new = as.data.frame(chain)
chain_new = chain_new[201:5200,]
names(chain_new) = c("lambda1","lambda2","changepoint")

#plot the histogram of posterior distribution 
hist(chain_new$lambda1, xlab = "Value of lambda1",main = "Histogram of lambda1")
hist(chain_new$lambda2, xlab = "Value of lambda2",main = "Histogram of lambda2")
hist(chain_new$changepoint, xlab = "Change Point",main = "Histogram of change point")
