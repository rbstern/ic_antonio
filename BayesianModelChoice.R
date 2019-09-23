x= mean(29.2,24.7,32.3,31.3,31.5,24.5,19.9,27.3,27.1,24,33.8,21.5,32.2,22.5,27.5,25.6,34.5,26.2,26.7,21.1,24.1,30.7,32.7,32.6,22.1,25.3,30.8,38.9,22.1,29.2,30.1,31.4,26.7,22.1,30.3,32,23.2,30.3,29.9,20.8,33.2,28.2)
z = mean(25.4,22.2,32.2,31,30.9,23.9,19.2,27.2,26.3,23.9,33.2,21,29,22,23.8,25.3,34.2,25.7,26.4,20,23.9,30.7,32.6,32.5,20.8,23.1,29.8,38.1,21.3,28.5,29.2,31.4,25.9,21.4,29.8,30.6,22.6,30.3,23.8,18.4,29.4,28.2)


gibbs = function(B = 5000)
{
  data = matrix(NA, nrow = B, ncol = 7)
  colnames(data) = c("Y", "alpha","beta","sigma","gamma","delta","tau")
  data[1,] = c(0, 0, 0, 0, 0, 0, 0)
  for(ii in 2:B)
  {
    if(runif(1) > 0.5) 
      data[ii,] = c((data[ii-1,2] + data[ii-1,3]*x + rnorm(1,0,data[ii-1,4]) ) ,Prior(),Prior(),PriorVar(),PriorGamma(),PriorDelta(),PriorVar())
    else 
      data[ii,] = c((data[ii-1,5] + data[ii-1,6]*z + rnorm(1,0,data[ii-1,7]) ),PriorAlpha(),PriorBeta(),PriorVar(),Prior(),Prior(),PriorVar())
  }
  data
}


library(invgamma)


Prior = function(){rnorm(1,3000,185)}
PriorVar = function(){rinvgamma(1,3,scale = 180000)}

PriorAlpha = function(){rnorm(1,3000,52^2)}
PriorBeta = function(){rnorm(1,185,12^2)}
PriorGamma = function(){rnorm(1,3000,43^2)}
PriorDelta = function(){rnorm(1,185,9^2)}


gibb = gibbs()
