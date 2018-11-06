

n = 50
p = 0.1
X = matrix(1, n, n)
X[1:(n/2),] = -1 # X e a imagem verdadeira
image(1:n, 1:n, X)
Y = as.matrix(2*(rbinom(n^2, 1, p) - 0.5))
dim(Y) = c(n, n) # Y sao os pixels corrompido
image(1:n, 1:n, Y)
Z = X*Y # Z e a imagem observada
image(1:n, 1:n, Z)


rprop = function(Z)
{
  i = ceiling(runif(1, min=0, max=50))
  j = ceiling(runif(1, min=0, max=50))
  if (Z[i,j] < 0) Z[i,j] = 1
  else Z[i,j] = -1
  Z
}

dif_log_mu = function(Z,Z*,i,j)
  dif = a*(ifelse(Z[i,j]!=X[i,j],1,0) - ifelse((Z[i,j]!=X[i,j],1,0)) + 
             2*(1-a)*((ifelse(Z[i,j]!=Z[i+1,j],1,0) - ifelse((Z*[i,j]!=Z[i+1,j],1,0))) +
                        (ifelse(Z[i,j]!=Z[i-1,j],1,0) - ifelse((Z*[i,j]!=Z[i-1,j],1,0))) +
                           (ifelse(Z[i,j]!=Z[i,j+1],1,0) - ifelse((Z*[i,j]!=Z[i,j+1],1,0))) +
                              (ifelse(Z[i,j]!=Z[i,j-1],1,0) - ifelse((Z*[i,j]!=Z[i,j-1],1,0))))


metropolis_hastings = function(rprop, log_dprop, dif_log_mu, theta_1 = Z, B = 10^5)
{
  theta = list(matrix(1, n, n))
  theta[1] = theta_1
  for(ii in 2:B)
  {
    theta[ii] = rprop(theta[ii-1])
    log_L = log_mu(theta[ii]) + log_dprop(theta[ii-1], theta[ii]) -
      log_mu(theta[ii-1]) -log_dprop(theta[ii], theta[ii-1])
    if(log(runif(1)) > log_L) theta[ii] = theta[ii-1]
  }
  theta
}