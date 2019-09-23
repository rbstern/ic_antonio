library(invgamma)
library(tidyverse)

data = read_csv("./data/carlin1995.csv")

# Trechos a completar
prop_M = function(prop)
{
  sum(log(dnorm(data$y, prop$alpha + data$x*prop$beta, sqrt(prop$sigma)))) +
    -
    sum(log(dnorm(data$y, prop$gamma + data$z*prop$delta, sqrt(prop$tau)))) -
    
}

# Trecho a completar
prop_alpha_beta_sigma = function(prop_atual)
{
 
}

d_alpha_beta_sigma_prior
d_gamma_delta_tau_prior

Prior = function(){rnorm(1,3000,185)}
PriorVar = function(){rinvgamma(1,3,scale = 180000)}
PriorAlpha = function(){rnorm(1,3000,52^2)}
PriorBeta = function(){rnorm(1,185,12^2)}
PriorGamma = function(){rnorm(1,3000,43^2)}
PriorDelta = function(){rnorm(1,185,9^2)}

# Trecho a completar
prop_gamma_delta_tau = function(prop_atual)
{
 
}

gibbs = function(B = 5000)
{
  data_simul = tibble(M = rep(NA, B),
                      alpha = rep(NA, B),
                      beta = rep(NA, B),
                      sigma = rep(NA, B),
                      gamma = rep(NA, B),
                      delta = rep(NA, B),
                      tau = rep(NA, B))
  data_simul[1,] = rep(0, 7)
  for(ii in 2:B)
  {
    prop = data_simul[ii-1,]
    prop = prop_M(prop)
    prop = prop_alpha_beta_sigma(prop)
    prop = prop_gamma_delta_tau(prop)
    data_simul[ii, ] = prop
  }
  data_simul
}
