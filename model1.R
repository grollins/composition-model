library(rstan)
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
stan_plot(fit)

# print true theta values
print(theta)

