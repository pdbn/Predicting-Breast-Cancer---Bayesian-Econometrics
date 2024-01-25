#setwd

#Load Excel file
data <- read.csv("BreastCancerData.csv", sep = ",")
library(Rlab)
library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

#Logistic model y_i = 1/(1 +exp(-theta * x_i))
#theta: vector of coefficients 
#x: explanatory variables
#y: dependent variable
#k: number of parameters
k <- ncol(data)-1
x <- as.matrix(data[,2:(k+1)])
y <- data[,1]


#Prior function
prior_logistic <- function(theta) {
  p_theta <- c() 
  #prior  thetas normally distributed
  p_theta[1:(k+1)] <- dnorm(theta[1:(k+1)], 0, 1)
  prior <- prod(p_theta)
  print(prior)
  scaled_prior<- prior/sum(prior)
  print(scaled_prior)
  return(prior)
}

#Likelihood function
lik_logistic <- function(theta, k, x, y){
  intercept <- theta[1]
  coeff <- theta[2:(k+1)] #9
  coeff <- matrix(coeff, nrow = length(coeff), ncol = 1)
  z <- intercept + (x %*% coeff)
  p <- 1/(1+exp(-z))  
  lik <- dbern(y, prob = p) 
  lik1 <- prod(lik)
  return(lik1)
} 

#Posterior function
post_logistic <-  function(theta, k, x, y){
  #Likelihood
  likelihood <- lik_logistic(theta, k, x, y)
  #Priors
  prior <- prior_logistic(theta)
  posterior <- prior * likelihood
  #MAYBE SCALE POSTERIOR?
  return(posterior)
}

#prior_logistic(theta = c(0.01,0.02,0.33,0.4,0.05, 6, 7, 8,9, 1))

#Unit test 1: Posterior function return vector size 1
test1 <- function(){
  # set some input values
  aux = post_logistic(theta = c(1:10), k, x, y)
  if(!is.vector(aux))
    stop('post_logistic failed unit testing')
  if(length(aux) != 1)
    stop('post_logistic failed unit testing')
  # return a message and return value 1 if no test has failed
  cat("post_logistic passed unit testing", fill = TRUE)
  return(1)
}
# Run Unit test 1
test1()

# Check the shape of the posterior with respect to one of the parameters
K <- 300# number of debt_income_ratio (theta1) parameters we will check
theta1_range <- seq(-4, 4, length = K)

thetas <- matrix(0, nrow = K, ncol = (k+1)) #matrix 300x9 all 0s
thetas[,2] <- theta1_range #range theta1 in column 2 (debt_income_ratio)

post <- rep(NA, 300)
lik <- rep(NA,300)
prior <- rep(NA,300)

for(i in 1:K){
  post[i] <-  post_logistic(theta = thetas[i,],k, x,y)
  lik[i] <- lik_logistic(theta = thetas[i,],k, x,y)
  prior[i] <- prior_logistic(theta = thetas[i,])
}

plot(theta1_range, prior, type = 'l',col = "blue",xlab = 'Parameter Value', ylab = 'Prior', main = 'Prior vs. Parameter Values')
plot(theta1_range, lik , type = 'l',col = "green",xlab = 'Parameter Value', ylab = 'Likelihood', main = 'Likelihood vs. Parameter Values')
plot(theta1_range, post, type = 'l',col = "red",xlab = 'Parameter Value', ylab = 'Posterior', main = 'Posterior vs. Parameter Values')
#lines(theta1_range, lik, col='green')
l#ines(theta1_range, post, col = "red")
#legend("topright", legend=c("Posterior", "Likelihood"), col=c("blue", "green"), lty=1)

model <- stan_model("RStan_tutorial2.stan")

datalist <- list(n = 499, K = 5, y, x)
fit = sampling(model, data = datalist, iter = 200, chains = 4)
print(fit)
plot(fit)

