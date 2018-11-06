n = 50
p = 0.1
X = matrix(1, n, n)
X[1:(n/2),] = -1 # X e a imagem verdadeira
image(1:n, 1:n, X)
Y = as.matrix(2*(rbinom(n^2, 1, 1-p) - 0.5))
dim(Y) = c(n, n) # Y sao os pixels corrompido
Y[1,] = 1
Y[,1] = 1
Y[n,] = 1
Y[,n] = 1
image(1:n, 1:n, Y)
Z = X*Y # Z e a imagem observada
image(1:n, 1:n, Z)

# Alterar um unico pixel de Z como proposta.
rprop = function(Z)
{
  i = ceiling(runif(1, min = 1, max = 49))
  j = ceiling(runif(1, min = 1, max = 49))
  Z[i, j] = -Z[i, j]
  list(i = i, j = j, Z = Z)
}

# Ajeitar isso de forma a so depender de i, j, Z e
# o numero de 1's em Z ao redor de (i,j)
dif_log_mu = function(Z,Z*,i,j)
  dif = a*(ifelse(Z[i,j]!=X[i,j],1,0) - ifelse((Z[i,j]!=X[i,j],1,0)) + 
             2*(1-a)*((ifelse(Z[i,j]!=Z[i+1,j],1,0) - ifelse((Z*[i,j]!=Z[i+1,j],1,0))) +
                        (ifelse(Z[i,j]!=Z[i-1,j],1,0) - ifelse((Z*[i,j]!=Z[i-1,j],1,0))) +
                           (ifelse(Z[i,j]!=Z[i,j+1],1,0) - ifelse((Z*[i,j]!=Z[i,j+1],1,0))) +
                              (ifelse(Z[i,j]!=Z[i,j-1],1,0) - ifelse((Z*[i,j]!=Z[i,j-1],1,0))))

metropolis_hastings = function(rprop, log_dprop, dif_log_mu, theta_1 = Z, B = 10^5)
{
  theta = as.list(rep(NA, B))
  theta[[1]] = matrix(1, n, n)
  for(ii in 2:B)
  {
    prop = rprop(theta[[ii-1]])
    i_prop = prop$i
    j_prop = prop$j
    Z_prop = prop$Z
    theta[[ii]] = Z_prop
    # modificar este pedaco para depender de i_prop, j_prop e Z_prop.
    log_L = log_mu(theta[ii]) + log_dprop(theta[ii-1], theta[ii]) -
      log_mu(theta[ii-1]) -log_dprop(theta[ii], theta[ii-1])
    if(log(runif(1)) > log_L) theta[[ii]] = theta[[ii-1]]
  }
  theta
}

image(theta[[B]])
