library(tidyverse)

metropolis = function(B = 10^5)
{
  theta = rep(NA, B)
  theta[1] = rcauchy(1) #rnorm(1, 0, 1)
  g = function(theta) dcauchy(theta) * dnorm(2, theta, 1)
  for(ii in 2:B)
  {
    theta_prop = theta[ii-1] + rcauchy(1) #rnorm(1, 0, 1)
    L = g(theta_prop)/g(theta[ii-1])
    theta[ii] = theta_prop
    if(runif(1, 0, 1) > L) theta[ii] = theta[ii-1]
  }
  theta
}

theta = metropolis()

mean(theta)
mean(theta^2) - mean(theta)^2

plot(as.ts(theta))
plot(density(theta))

#Provavelmente x|theta é N(theta,1) , não N(0,1)