beta1 <- function(theta){
  if (theta <= -0.05) {
    y = 0
  }else if(-0.05 < theta & theta <= 0.95){
      y = 0.05 + theta
  }else{
      y = 1
  }
return(y)
}

beta2 <- function(theta){
  c = 2-sqrt(0.1)
  if (theta <= c/2 -1){
    y = 0
  }else if (c/2 - 1 < theta & theta <= (c-1)/2){
    y = ((2*theta + 2 -c)^2)/2
  }else if((c-1)/2 < theta & theta <= c/2){
    y = 1 - ((c-2*theta)^2)/2
  }else{
    y = 1
  }
return(y)
}
x.phi1 <- as.numeric(seq(-1,3,0.01))
y.phi1 <- as.numeric(lapply(x, beta1))
plot(x.phi1,y.phi1,type = 'l',lwd = 3,
     main = bquote(beta[1](theta)~'vs'~beta[2](theta)),
     xlab = '',ylab ='power')

par(new = TRUE)
x.phi2 <- as.numeric(seq(-1,3,0.01))
y.phi2 <- as.numeric(lapply(x, beta2))
plot(x.phi2,y.phi2,type = 'l',lwd = 3,col = 'red',
     xlab ='',ylab = '')

legend('topright',
       legend = c('phi1','phi2'),
       col = c('black','red'),
       lwd = 3)
