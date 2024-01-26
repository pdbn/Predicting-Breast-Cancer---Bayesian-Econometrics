#setwd 

#Load Excel file
data <- read.csv("BreastCancerData.csv", sep = ",")
summary(data)
library(Rlab)
library("rstan")
library(corrplot)
library(ggplot2)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

#Logistic model y_i = 1/(1 +exp(-theta * x_i))
#theta: vector of coefficients 
#x: explanatory variables
#y: dependent variable
#k: number of parameters
k <- ncol(data)-1
x <- scale(as.matrix(data[,2:(k+1)]))
y <- data[,1]

#Plot data
boxplot(x, col = "lightblue", main = "Boxplot of 10 Variables") #outlier
cor_matrix <- cor(x)
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", tl.cex = 0.7, tl.col = "black", addCoef.col = "black") #correlation between variables
#histogram
column_names <- colnames(x)
par(mfrow = c(3, 4))
for (i in 1:k) {
  hist(x[, i], main = paste("Histogram of", column_names[i]), col = "lightblue", xlab = "")
}

#Prior function
prior_logistic <- function(theta) {
  p_theta <- c() 
  #prior  thetas normally distributed
  p_theta[1:(k+1)] <- dnorm(theta[1:(k+1)], 0, 1)
  #normalization
  sum_p_theta<-sum(p_theta)
  p_theta<-p_theta/sum_p_theta
  #joint probability assuming independence
  prior <- prod(p_theta)
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
  #normalization
  sum_posterior<- sum(posterior)
  posterior<-posterior/sum_posterior
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

library(rstan)

# Load your Stan model
model <- stan_model("Project.stan")

# Define k
k <- ncol(data) - 1

# Create the data list
data_list <- list(
  n = 116,
  k = k,
  x = as.matrix(data[, 2:(k + 1)]),
  y = data[, 1]
)
# Run the sampling
fit <- sampling(model, data = data_list, iter = 5000, warmup = 1000, chains = 4)

pairs(fit)  # Pairs plot
traceplot(fit)  # Trace plot
