library(rstan)
library(gtools)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_model = "
data {
  int<lower=1> K;  // num categories
  int<lower=1> N;  // num draws per group
  vector<lower=0.0,upper=1.0>[K] theta1;
  int<lower=0,upper=N> z2[K];  // count in each category of group 2
  int<lower=0,upper=N> z3[K];  // count in each category of group 3
  int<lower=0,upper=N> z4[K];  // count in each category of group 4
}
parameters {
  vector<lower=0.0,upper=1.0>[K] k;
}
transformed parameters {
  vector<lower=0.0,upper=1.0>[K] theta2;
  vector<lower=0.0,upper=1.0>[K] theta3;
  vector<lower=0.0,upper=1.0>[K] theta4;
  theta2[1] <- theta1[1] * k[1];
  theta2[2] <- theta1[2] * k[2];
  theta2[3] <- theta1[3] * k[3];
  theta3[1] <- theta1[1] * k[1] * k[1];
  theta3[2] <- theta1[2] * k[2] * k[2];
  theta3[3] <- theta1[3] * k[3] * k[3];
  theta4[1] <- theta1[1] * k[1] * k[1] * k[1];
  theta4[2] <- theta1[2] * k[2] * k[2] * k[2];
  theta4[3] <- theta1[3] * k[3] * k[3] * k[3];
  theta2 <- theta2 / sum(theta2);
  theta3 <- theta3 / sum(theta3);
  theta4 <- theta4 / sum(theta4);
}
model {
  k ~ beta(7, 1);
  z2 ~ multinomial(theta2);
  z3 ~ multinomial(theta3);
  z4 ~ multinomial(theta4);
}
"

# generate data
N1 = 10000
N1a = 3000
N1b = 1000
N1c = 6000
theta1 = c(N1a, N1b, N1c) / N1
z1 = rmultinom(n = 1, size = 100, prob = theta1)

k = rbeta(n = 3, shape1 = 5, shape2 = 1)
theta2 = theta1 * k
theta3 = theta1 * k * k
theta4 = theta1 * k * k * k
theta2 = theta2 / sum(theta2)
theta3 = theta3 / sum(theta3)
theta4 = theta4 / sum(theta4)
z2 = rmultinom(n = 1, size = 100, prob = theta2)
z3 = rmultinom(n = 1, size = 100, prob = theta3)
z4 = rmultinom(n = 1, size = 100, prob = theta4)

data = list(K = 3, N = 100, theta1 = theta1,
            z2 = z2[,1], z3 = z3[,1], z4 = z4[,1])

# fit model
fit = stan(model_code=stan_model, data=data, iter=6000,
           warmup=1000, chains=3)

# print summary stats and plots of fit
print(fit)
stan_plot(fit, pars = c("k"))
# stan_trace(fit, pars = c("k"))
# stan_scat(fit, pars = c("k[1]", "k[2]"))

# print true k values
print(k)

