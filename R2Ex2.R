metropolis = function(B = 10^5)
{
  theta = rep(NA, B)
  theta[1] = rgamma(1, 1, 1) #abs(rnorm(1, 0, 1)) ?
  g = function(theta) dgamma(theta,1,1) * dexp(0.5, theta)
  for(ii in 2:B)
  {
    # Pensar se existe uma proposta melhor do que somar rnorm
    theta_prop = theta[ii-1] + rnorm(1, 0, 1)
    L = ifelse(theta_prop > 0, 
               g(theta_prop)/g(theta[ii-1]), 0)
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

#Variancia e media muito altas.