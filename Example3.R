install.packages('rstudioapi')
library("rstan")
library(rstudioapi)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


# Reading the data
schools = list(J=8,y=c(28,8,-3,7,-1,1,18,12),sigma=c(15,10,16,11,9,11,10,18))

# Running stan code
model = stan_model("Example3.stan")

fit = sampling(model,data=schools,iter=200,chains=4)

print(fit)

plot(fit)

pairs(fit, pars = c("mu", "tau", "lp__"))
la <- extract(fit, permuted = TRUE) # return a list of arrays 
mu <- la$mu
a  <- extract(fit, permuted = FALSE) 
a2 <- as.array(fit)
m  <- as.matrix(fit)
d  <- as.data.frame(fit)
traceplot(fit)
