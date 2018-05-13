rm( list = ls( all = TRUE ))

library(splines)
library(expectreg)
library(quantreg)
library(stabledist)

n 		= 100
x 		= runif(n, min=0, max=3)

# specify noise type for regression
#e 		= rnorm(n, 0, 0.1)
e 		= rt(n, 5)

# choose type of regression 
# B-spline-based regression
y       = 1.5*sin(x) + 4 + cos(3*x) + e
# linear regression
#y       = 4 + 3*x + e

# for location model choose different distributions for 'y' and x as a vector of ones
#x 		= as.numeric(rep(1,n))
#y 		= rnorm(n, 0, 1)
#y 		= rt(n,5)
#y 		= rstable(n, alpha = 1.5, beta = -0.5, gamma = 1, delta = 0, pm = 0)
#y 		= rstable(n, alpha = 1.7, beta = -0.5, gamma = 1, delta = 0, pm = 0)

# quantile level
tau     = 0.9
# degree for spline basis 
df 		= 3
# threshold value for Huber location and quantile loss
k       = 0.1

# compute initial weights 
# B-spline-based regression
r0        = lm( y ~ bs(x, df = df) )
# linear regression
#r0 		  = lm( y ~ x)

# weights for Huber location
#w0        = as.numeric(abs(resid(r0)) >= k) + (k/abs(resid(r0)))*as.numeric(abs(resid(r0)) < k)
# weights for quantile
#w0        = ((tau*as.numeric(resid(r0)>=0)+(1-tau)*as.numeric(resid(r0)<0)))/abs(resid(r0))
# weights for expectile
#w0        = (tau*as.numeric(resid(r0)>=0)+(1-tau)*as.numeric(resid(r0)<0))
# weights for Huber quantile (trimming)
w0 = ((1 - tau)*k/resid(r0)^2)*as.numeric(resid(r0) < -k)*0.5 + (1-tau)*as.numeric(resid(r0) > -k & resid(r0) < 0)*0.5 + tau*as.numeric(resid(r0) > 0 & resid(r0) < k)*0.5 + (tau*k/resid(r0)^2)*as.numeric(resid(r0) > k)*0.5

crit      = 0.0001
iter      = 1
maxiter   = 10
w1        = rep(0,length(x))
w2        = rep(0,length(x))

while  ((iter<maxiter) & (mean(abs(w1-w0)) > crit))
{ 
   print(iter)
   iter = iter+1 
 	
   # choose regression type	
   r1        = lm( y ~ bs(x, df = df), w = as.numeric(w0) )
   #r1        = lm( y ~ x, w = as.numeric(w0) )

   # choose weights
   # weights for Huber location
   # w1        = as.numeric(abs(resid(r1)) >= k) + (k/abs(resid(r1)))*as.numeric(abs(resid(r1)) < k)
   # weights for quantile
   # w1        = ((tau*as.numeric(resid(r1)>=0)+(1-tau)*as.numeric(resid(r1)<0)))/abs(resid(r1))
   # weights for expectile
   # w1       = (tau*as.numeric(resid(r1)>=0)+(1-tau)*as.numeric(resid(r1)<0))
   # weights for Huber quantile   
   w1 = ((1 - tau)*k/resid(r1)^2)*as.numeric(resid(r1) < -k)*0.5 + (1-tau)*as.numeric(resid(r1) > -k & resid(r1) < 0)*0.5 + tau*as.numeric(resid(r1) > 0 & resid(r1) < k)*0.5 + (tau*k/resid(r1)^2)*as.numeric(resid(r1) > k)*0.5
 
   # choose regression type	
   r2        = lm( y ~ bs(x, df = df), w = as.numeric(w1) )
   #r2        = lm( y ~ x, w = as.numeric(w1) )

   # choose weights
   # weights for Huber location   
   #w2        = as.numeric(abs(resid(r2)) >= k) + (k/abs(resid(r2)))*as.numeric(abs(resid(r2)) < k)
   # weights for quantile
   # w2        = ((tau*as.numeric(resid(r2)>=0)+(1-tau)*as.numeric(resid(r2)<0)))/abs(resid(r2))
   # weights for expectile
   # w2        = (tau*as.numeric(resid(r2)>=0)+(1-tau)*as.numeric(resid(r2)<0)) 
   # weights for Huber quantile  
   w2 = ((1 - tau)*k/resid(r2)^2)*as.numeric(resid(r2) < -k)*0.5 + (1-tau)*as.numeric(resid(r2) > -k & resid(r2) < 0)*0.5 + tau*as.numeric(resid(r2) > 0 & resid(r2) < k)*0.5 + (tau*k/resid(r2)^2)*as.numeric(resid(r2) > k)*0.5
  
   w0 = w2
}

# # plots for B-spline-regression
plot(x,y,col="lightsteelblue3",pch=20,cex=1.2)
lines(sort(x),r0$fitted[order(x)], col="black",lwd=2, lty = 2)
lines(sort(x),r2$fitted[order(x)], col="blue",lwd=2, lty = 2)  # Huber location
# lines(sort(x),r2$fitted[order(x)], col="black",lwd=2)  # quantile
# lines(sort(x),r2$fitted[order(x)], col="red",lwd=2)   # expectile
# lines(sort(x),r2$fitted[order(x)], col="blue",lwd=2)   # Huber quantile

# double-check whether the curve fitted with LAWS algorithm is correct 
es = expectreg.ls(y ~ rb(x, "pspline"),  estimate = "laws",  expectiles = tau, ci = TRUE)
lines(sort(x),es$fitted[order(x)], col="aquamarine4",lwd=2)

# plots for linear regression
# plot(x,y,col="lightsteelblue3",pch=20,cex=1.2)
# abline(r2,col="blue",lwd=2, lty = 2)  # Huber location
# abline(r2,col="black",lwd=2)  # quantiles
# #abline(r2,col="red",lwd=2)   # expectiles
# #abline(r2,col="blau",lwd=2)   # Huber quantile

# # plot mean and quantile regression curves to double-check the results
# abline(r0,col="black",lwd=2, lty = 2)
# abline(rq(y ~ x, tau = tau), col="aquamarine4",lwd=2)




