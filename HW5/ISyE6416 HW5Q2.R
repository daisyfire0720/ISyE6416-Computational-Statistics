#ISyE6416 HW5 Question2
install.packages("HMM")
library(HMM)
states = c("Fair","Loaded")
symbol = 1:6
transprobs = matrix(c(0.95,0.05,0.1,0.9),c(length(states),length(states)),byrow = T)
emissionprobs = matrix(c(rep(1/6,6),c(rep(1/10,5),1/2)),c(length(states),length(symbols)),byrow = T)
obs = c(4,4,5,4,3,6,3,1,6,5,6,6,2,6,5,6,6,6)
hmm = initHMM(States = states,Symbols = symbol,transProbs = transprobs,emissionProbs = emissionprobs)
forprob = forward(hmm,obs)
backprob = backward(hmm,obs)
seq = viterbi(hmm,obs)
View(seq)
