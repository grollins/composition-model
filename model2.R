library(rstan)
library(gtools)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_model = "
data {
  int<lower=1> K;  // num categories
  vector<lower=0>[K] alpha;  // prior
  int<lower=1> N1;  // total num counts across all categories in group 1
  int<lower=1> N2;  // total num counts across all categories in group 2
  int<lower=1> N3;  // total num counts across all categories in group 3
  int<lower=1,upper=N1> z1[K];  // count in each category of group 1
  int<lower=1,upper=N2> z2[K];  // count in each category of group 2
  int<lower=1,upper=N3> z3[K];  // count in each category of group 3
}
parameters {
  simplex[K] theta1;
  simplex[K] theta2;
  simplex[K] theta3;
}
transformed parameters {

}
model {
  theta1 ~ dirichlet(alpha);
  theta2 ~ dirichlet(alpha);
  theta3 ~ dirichlet(alpha);

  z1 ~ multinomial(theta1);
  z2 ~ multinomial(theta2);
  z3 ~ multinomial(theta3);
}
"

# generate data
alpha = c(2, 3, 10, 3, 1)
theta = rdirichlet(n = 3, alpha = alpha)
N1 = 1000
N2 = 100
N3 = 10000
z1 = rmultinom(n = 1, size = N1, prob = theta[1,])
z2 = rmultinom(n = 1, size = N2, prob = theta[2,])
z3 = rmultinom(n = 1, size = N3, prob = theta[3,])
data = list(alpha = alpha, K = length(alpha), N1 = N1, N2 = N2, N3 = N3,
            z1 = z1[,1], z2 = z2[,1], z3 = z3[,1])

# fit model
fit = stan(model_code=stan_model, data=data, iter=6000,
           warmup=1000, chains=3)

# print summary stats and plots of fit
print(fit)
stan_plot(fit, pars = c("theta1"))
stan_plot(fit, pars = c("theta2"))
stan_plot(fit, pars = c("theta3"))
# stan_trace(fit, pars = c("theta1"))
# stan_scat(fit, pars = c("theta1[1]", "theta1[2]"))

# print true theta values
print(theta)

