library(rstan)
library(gtools)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_model = "
data {
  int<lower=1> K;  // num elements
  int<lower=1> N;  // num counts per dose
  int<lower=1> M;  // num dose-response points
  vector<lower=0.0,upper=1.0>[K] theta0;  // initial composition
  int<lower=0,upper=N> z[M,K];  // element counts at each dose-response point
}
parameters {
  vector<lower=0.0,upper=1.0>[K] k;
}
transformed parameters {
  vector<lower=0.0,upper=1.0>[K] theta[M];
  for (i in 1:M) {
    theta[i] <- theta0;
    for (j in 1:i) {
      theta[i] <- theta[i] .* k;  // element-wise multiply
    }
    theta[i] <- theta[i] / sum(theta[i]);
  }
}
model {
  k ~ beta(7, 1);
  for (i in 1:M) {
    z[i] ~ multinomial(theta[i]);
  }
}
"

# generate initial composition
K = 10  # num elements
M = 5  # num dose-response points
theta0 = rdirichlet(n = 1, alpha = array(1, c(K)))[1,]  # initial composition

# generate compositions at subsequent response points
k = rbeta(n = K, shape1 = 5, shape2 = 1)  # decay rates
theta = array(0.0, dim = c(M,K))  # composition at each dose-response point
for(i in 1:M) {
  theta[i,] = theta0 * k^i
  theta[i,] = theta[i,] / sum(theta[i,])
}

# generate observed category counts at each response point
N = 1000  # num counts per dose
z = array(0, dim = c(M,K))
for(i in 1:M) {
  z[i,] = rmultinom(n = 1, size = N, prob = theta[i,])[,1]
}

# fit model
data = list(K = K, N = N, M = M, theta0 = theta0, z = z)
fit = stan(model_code=stan_model, data=data, iter=6000,
           warmup=1000, chains=1)

# print summary stats and plots of fit
print(fit)
stan_plot(fit, pars = c("k"))
# stan_trace(fit, pars = c("k"))
# stan_scat(fit, pars = c("k[1]", "k[2]"))

# print true k values
print(k)
