library(invgamma)
library(tidyverse)
library(PIGShift)


data = read_csv("./data/carlin1995.csv")

Pi_0 = 0.5
Pi_1 = 0.5 


Prior1 = function(theta){dnorm(theta,3000,sqrt(10^6))}
Prior2 = function(theta){dnorm(theta,185,sqrt(10^4))}


PriorVar = function(theta){dinvgamma(theta,3,scale = 180000)}
PriorAlpha = function(alpha){dnorm(alpha,3000,52)}
PriorBeta = function(beta){dnorm(beta,185,12)}
PriorGamma = function(gamma){dnorm(gamma,3000,43)}
PriorDelta = function(delta){dnorm(delta,185,9)}


d_alpha_beta_sigma_prior = function(prop, M)
{
  if(M == 0){
    ABS_prior = Prior1(prop$alpha) * Prior2(prop$beta) * PriorVar(prop$sigma)
  }
  else{
    ABS_prior = PriorAlpha(prop$alpha) * PriorBeta(prop$beta) * PriorVar(prop$sigma)
  }
  ABS_prior
}

d_gamma_delta_tau_prior = function(prop, M)
{
  if(M == 1){
    GDT_prior = Prior1(prop$gamma) * Prior2(prop$delta) * PriorVar(prop$tau)
  }
  else{
    GDT_prior =  PriorGamma(prop$gamma) * PriorDelta(prop$delta) * PriorVar(prop$tau)
  }
  GDT_prior
}


Sum_Log_0 =function(prop){(sum(log(dnorm(data$y, prop$alpha + data$x*prop$beta, sqrt(prop$sigma)))) + 
                             (log(d_alpha_beta_sigma_prior(prop,0)) + log(d_gamma_delta_tau_prior(prop,0) )) + log(Pi_0))}


Sum_Log_1 =function(prop){sum(log(dnorm(data$y, prop$gamma + data$z*prop$delta, sqrt(prop$tau)))) + 
  log(d_alpha_beta_sigma_prior(prop,1)) + log(d_gamma_delta_tau_prior(prop,1) ) + log(Pi_1)}



# Trechos a completar
prop_M = function(prop)
{
  if(Sum_Log_0(prop) > Sum_Log_1(prop)){
    log_prop = Sum_Log_0(prop) - Sum_Log_0(prop) - log1p(exp(Sum_Log_1(prop) - Sum_Log_0(prop) ))
    
  }
  else{
  log_prop =  Sum_Log_0(prop) - Sum_Log_1(prop) - log1p(exp(Sum_Log_0(prop) - Sum_Log_1(prop)))
    
  }
       
       
  if(exp(log_prop) < runif(1)){
    prop$M = 0
  }
  else{ 
    prop$M = 1
    }
       
    
    
    
    
prop    
}




dnorm(data$y, prop$alpha + data$x*prop$beta, sqrt(prop$sigma))

# Trecho a completar
prop_alpha_beta_sigma = function(prop)
{
  if(prop$M == 0) {
    
    
    Sigma2Alpha = (1/10^6 + length(data$y)/prop$sigma)^-1
    
    MeanAlpha = Sigma2Alpha*(3000/10^6 + sum(data$y-data$x*prop$beta)/prop$sigma)
    
    prop$alpha = rnorm(1, MeanAlpha, sqrt(Sigma2Alpha))
    
    
    
    Sigma2Beta = (1/10^4 + length(data$y)/prop$sigma)^-1
      
    MeanBeta = Sigma2Beta*(185/10^4 + sum(data$y-prop$alpha)/prop$sigma)
    
      
    prop$beta= rnorm(1, MeanBeta, sqrt(Sigma2Beta))
    
    AlphaSigma = 3 +length(data$y)/2
    BetaSigma = 180000 + (sum(data$y - prop$alpha + data$x*prop$beta)^2)/2
    
    prop$sigma= rinvgamma(1,AlphaSigma,BetaSigma)
    
    
  }
  else {
    prop$alpha=rnorm(1,3000,52)
    prop$beta =rnorm(1,185,12)
    prop$sigma =rinvgamma(1,3,scale = 180000)
      
  }
  
  prop
}

# Trecho a completar
prop_gamma_delta_tau = function(prop)
{
  if(prop$M == 1) {
    
    Sigma2Gamma = (1/10^6 + length(data$y)/prop$tau)^-1
    
    MeanGamma = Sigma2Gamma*(3000/10^6 + sum(data$y-data$z*prop$delta)/prop$tau)
    
    prop$gamma = rnorm(1, MeanGamma, sqrt(Sigma2Gamma))
    
    
    
    Sigma2Delta =(1/10^4 + length(data$y)/prop$tau)^-1
      
    MeanDelta = Sigma2Delta*(185/10^4 + sum(data$y-prop$gamma)/prop$tau)
      
      
    prop$delta= rnorm(1, MeanDelta, sqrt(Sigma2Delta))
    
    AlphaTau = 3 +length(data$y)/2
    BetaTau = 180000 + (sum(data$y - prop$tau + data$z*prop$tau)^2)/2
    
    prop$sigma= rinvgamma(1,AlphaTau,BetaTau)
  
    
      
  }
  else {
    prop$gamma=rnorm(1,3000,43)
    prop$delta =rnorm(1,185,9)
    prop$tau =rinvgamma(1,3,scale = 180000)
    
    
  }
  
  prop
  
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
  data_simul[1,] = c(0,3000,185,300^2,3000,185,300^2)
  for(ii in 2:B)
  {
    prop = data_simul[ii-1,]
    prop = prop_M(prop)
    prop = prop_alpha_beta_sigma(prop)
    prop = prop_gamma_delta_tau(prop)
    data_simul[ii, ] = prop
    print(prop)
  }
  data_simul
}
