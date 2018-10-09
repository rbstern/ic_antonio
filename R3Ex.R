gibbs = function(B = 10^5)
{
  data = matrix(NA, nrow = B, ncol = 2)
  colnames(data) = c("X", "lambda")
  data[1,] = c(0, 0) # Valor inicial é arbitrário
  for(ii in 2:B)
  {
    if(runif(1) < 0.5) 
      data[ii,] = c(data[ii-1,1],
                    rgamma(1, 3 + data[ii-1, 1], 6))
    else 
      data[ii,] = c(rpois(1, data[ii-1, 2]),
                    data[ii-1,2])
  }
  data
}

#Adequando a questão 8.91, a = 3, b = 5
data = gibbs()
plot(as.ts(data[,1]))
barplot(table(data[,1])/sum(table(data[,1])))
plot(density(data[,2]))

#plot(sort(rgamma(10^5,3,5)),sort(data[,2])) #lambda ~ gamma(a,b)
qqplot(rgamma(10^5,3,5), data[,2])

mean(data[,1])  #E[X] = a/b = 0.6
mean(data[,2]) #E[Lambda] = a/b = 0.6
var(data[,1]) # Var[X] = a*(b+1)/b² = 0.72
var(data[,2]) #Var[Lambda] = a/b² = 0.12
