library(tidyverse)
library(rstan)

N = 100
x = 1:N
y = 3 + 8*x + rnorm(N)

fit = stan(file = "regression.stan",
           data = c("N", "x", "y"),
           iter = 1000, chains = 4)
fit
plot(fit)

fit %>% 
  rstan::extract("alpha") %>% 
  unlist() %>% 
  density() %>% 
  plot()

fit %>% 
  rstan::extract("beta") %>% 
  unlist() %>% 
  density() %>% 
  plot()
