library(tidyverse)

set.seed(0)
true_func <- function(x){
  dnorm(x, mean = 0, sd =1)
}

plot(x = seq(-10, 10, 0.01), y = true_func(seq(-10, 10, 0.01)), type = "l", xlab = "x",
     ylab = "g(x)", main = "A graph of g(x)", col="red")

RWMH <- function(niter, initial_val, sigma_val){
  samples <- vector(mode="numeric",niter)
  acceptance <- c()
  samples[1] <- initial_val
  for(i in 1 : (niter-1)){
    prop <- rnorm(1, mean = samples[i], sd = sigma_val)
    U <- runif(1)
    if(U < true_func(prop)/true_func(samples[i])){
      samples[i+1] <- prop
      next
    }
    samples[i+1] <- samples[i]
    ifelse(samples[i+1]==samples[i],acceptance[i]<-0,acceptance[i]<-1)
  }
  result_list <- list(samples, acceptance)
  return(result_list)
}



result <- RWMH(5000, 0, 0.5)
result[[2]]
ts.plot(sample1, main = "Sigma = 0.5")
acf(sample1, main = "Sigma = 0.5")
hist = hist(sample1,breaks=50,main="Sigma = 0.5", prob = TRUE, xlab = " ")
lines(seq(min(sample1), max(sample1), 0.01), 
      true_func(seq(min(sample1), max(sample1), 0.01)), 
      col = 'red', lty = 1, lwd = 2)

sample1 = RWMH(5000, 0, 1)
ts.plot(sample1, main = "Sigma = 1")
acf(sample1, main = "Sigma = 1")
hist = hist(sample1,breaks=50,main="Sigma = 1", prob = TRUE, xlab = " ")
lines(seq(min(sample1), max(sample1), 0.01), 
      true_func(seq(min(sample1), max(sample1), 0.01)), 
      col = 'red', lty = 1, lwd = 2)

sample1 = RWMH(5000, 0, 2)
ts.plot(sample1, main = "Sigma = 2")
acf(sample1, main = "Sigma = 2")
hist = hist(sample1,breaks=50,main="Sigma = 2", prob = TRUE, xlab = " ")
lines(seq(min(sample1), max(sample1), 0.01), 
      true_func(seq(min(sample1), max(sample1), 0.01)), 
      col = 'red', lty = 1, lwd = 2)

sigma <- 2.38
sample1 = RWMH(5000, 0, sigma)
ts.plot(sample1, main = paste("Sigma =",sigma))
acf(sample1, main = paste("Sigma =",sigma))
hist = hist(sample1,breaks=50,main = paste("Sigma =",sigma), prob = TRUE, xlab = " ")
lines(seq(min(sample1), max(sample1), 0.01), 
      true_func(seq(min(sample1), max(sample1), 0.01)), 
      col = 'red', lty = 1, lwd = 2)

sigma <- 3
sample1 = RWMH(5000, 0, sigma)
ts.plot(sample1, main = paste("Sigma =",sigma))
acf(sample1, main = paste("Sigma =",sigma))
hist = hist(sample1,breaks=50,main = paste("Sigma =",sigma), prob = TRUE, xlab = " ")
lines(seq(min(sample1), max(sample1), 0.01), 
      true_func(seq(min(sample1), max(sample1), 0.01)), 
      col = 'red', lty = 1, lwd = 2)

sigma <- 5
sample1 = RWMH(5000, 0, sigma)
ts.plot(sample1, main = paste("Sigma =",sigma))
acf(sample1, main = paste("Sigma =",sigma))
hist = hist(sample1,breaks=50,main = paste("Sigma =",sigma), prob = TRUE, xlab = " ")
lines(seq(min(sample1), max(sample1), 0.01), 
      true_func(seq(min(sample1), max(sample1), 0.01)), 
      col = 'red', lty = 1, lwd = 2)

#######################################
set.seed(0)
N=5000
samples <-c()
samples[1] <-0
accepted_proposals <- 0
sigma_val <- 5

target.ft=function(x){
  dnorm(x, mean = 0, sd =1)
}

for(i in 1:N){
  prop <- rnorm(1, mean = samples[i], sd = sigma_val)
  U <- runif(1)
  acceptance_ratio <- target.ft(prop)/target.ft(samples[i])                                                                                                              
  if(U < acceptance_ratio){
    samples[i+1] <- prop
    accepted_proposals <- accepted_proposals + 1
    next
  }
  samples[i+1] <- samples[i]
}

# acceptance rate
accepted_proposals/N * 100

# time-series plot & autocorrelation plot
ts.plot(samples, main = paste("Sigma =",sigma_val))
acf(samples, main = paste("Sigma =",sigma_val))


hist = hist(samples,breaks=50,main = paste("Sigma =",sigma_val), prob = TRUE, xlab = " ")
lines(seq(min(samples), max(samples), 0.01), 
      target.ft(seq(min(samples), max(samples), 0.01)), 
      col = 'red', lty = 1, lwd = 2)

##########################################################
####################### Gibs sampling #################
install.packages("Rlab")
library(Rlab)
y <- c(162,267,271,185,111,61,27,8,3,1)

update_z = function(y, pi, mu1, mu2) {
  z <- c()
  for (i in 1:length(y)) {
    p <- pi*dpois(y[i], mu1) / (pi*dpois(y[i],mu1) + (1-pi)*dpois(y[i],mu2))
    z[i] <- rbern(1,prob=p)
  }
  return(z)
}

update_pi = function(z, n, alpha, beta) {
  return(rbeta(1, sum(z)+alpha, n-sum(z)+beta))
}


update_mu1 = function(y, n, alpha, beta) {
  return(rgamma(1,sum(y)+alpha, n+beta))
}

update_mu2 = function(y, n, alpha, beta) {
  return(rgamma(1,sum(y)+alpha, n+beta))
}

n_iter <- 10000
pi_now <- 0.2
mu1_now <- 10 
mu2_now <- 10

pi_out <- numeric(n_iter)
mu1_out <- numeric(n_iter)
mu2_out <- numeric(n_iter)

for (i in 1:n_iter){
  z_now = update_z(y,pi_now, mu1_now, mu2_now)
  pi_now = update_pi(z_now, 9, 1, 1)
  mu1_now = update_mu1(y, 9, 1,10)
  mu2_now = update_mu2(y, 9, 1,10)
  
  pi_out[i] <- pi_now
  mu1_out[i] <- mu1_now
  mu2_out[i] <- mu2_now
  
}
pi_out
mu1_out
mu2_out

par(mfrow=c(2,1),mar=c(3,4,1,2))
ts.plot(mu1_out, main = " ")
acf(mu1_out)

ts.plot(mu2_out)
acf(mu2_out)
hist(mu2_out)

ts.plot(pi_out)
acf(pi_out)

