library(rstan)
library(ggplot2)
library(gtools)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_model = "
data {
int<lower=1> K;  // num categories
vector<lower=0>[K] alpha;  // prior
int<lower=1> N;  // total num counts across all categories
int<lower=1,upper=N> z[K];  // count in each category
}
parameters {
simplex[K] theta;
}
transformed parameters {

}
model {
// Prior Distribution for theta simplex
theta ~ dirichlet(alpha);
// Observed Counts
z ~ multinomial(theta);
}
"

# generate data
alpha = c(2, 3, 10, 3, 1)
theta = rdirichlet(n = 1, alpha = alpha)
N = 1000
z = rmultinom(n = 1, size = N, prob = theta)
data = list(alpha = alpha, K = length(alpha), N = N, z = z[,1])

# fit model
fit = stan(model_code=stan_model, data=data, iter=6000,
           warmup=1000, chains=3)

# print summary stats and plot of fit
print(fit)
plot(fit)

# extract combined chains
trace = extract(fit, permuted=TRUE)
names(trace)
mean(trace$theta[,1])
mean(trace$theta[,2])
mean(trace$theta[,3])
mean(trace$theta[,4])
mean(trace$theta[,5])

# print true theta values
print(theta)

qplot(trace$theta[,1], geom="density", alpha=I(.5), 
      main="marginal posterior of theta1", xlab="theta1", 
      ylab="Density")
qplot(trace$theta[,2], geom="density", alpha=I(.5), 
      main="marginal posterior of theta2", xlab="theta2", 
      ylab="Density")
qplot(trace$theta[,3], geom="density", alpha=I(.5), 
      main="marginal posterior of theta3", xlab="theta3", 
      ylab="Density")
qplot(trace$theta[,4], geom="density", alpha=I(.5), 
      main="marginal posterior of theta4", xlab="theta4", 
      ylab="Density")
qplot(trace$theta[,5], geom="density", alpha=I(.5), 
      main="marginal posterior of theta5", xlab="theta5", 
      ylab="Density")


