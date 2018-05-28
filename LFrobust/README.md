[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **LFrobust** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : LFrobust

Published in : 'Metis - LAWS'

Description : 'Plots different asymmetric loss functions'

Keywords : 
- LQRrobust
- LQRcheck
- loss function
- asymmetric
- quantile regression
- expectile

See also : 
- LQRcheck
- LFrobust
- LQRlidar
- LQRquantilecurve

Author : 
- Natalia Sirotko-Sibirskaya

Submitted : Sun, May 13 2018 by Petra Burdejova

Input : 
- x : grid
- tau : quantile/expectile level
- k : parameter

Example : 'An example is generated for predefined xgrid [0,3] and tau = 0.9'

```

### R Code
```r


rm( list = ls( all = TRUE ))

"rho.l" <- function(u) 
 {	f = u^2  
  list(f = f) }
  
"rho.a" <- function(u) 
 {	f = abs(u)  
  list(f = f) }  
        
"rho.h" <- function(u,k) 
 {	f = ifelse (abs(u) < k, u^2/2, k*(abs(u) - k/2))  
  list(f = f) }  

"rho.q" <- function(u, q) 
 {	f = ifelse (u < 0, q*abs(u), (1-q)*abs(u) )  
  list(f = f) }
        
"rho.e" <- function(u, q) 
{f = ifelse (u < 0, q*u^2, (1-q)*u^2 )   
  list(f = f) }
  
# "rho.hq" <- function(u, q, k) 
# {f = ifelse(u < (-k)*q, q*abs(u) - (k*q^2)/2, ifelse( u >= (-k)*q |u <= k*(1-q), u^2/(2*k), (1-q)*abs(u) - (k*(1-q)^2)/2) )
  # list(f = f) }
  
"rho.hq" <- function(u, q, k) 
{f = ifelse(u < (-k)*q, q*abs(u) - (k*q^2)/2, ifelse( u >= (-k)*q |u <= k*(1-q), u^2/(2*k), (1-q)*abs(u) - (k*(1-q)^2)/2) )
  list(f = f) }
  
n 	= 	100
tau   = 0.1
k   =   1

x	=	-3 + 6*(0:n)/(n)
#x   = rnorm(100,0,10)
#x   = rt(100,3)

y.l = rho.l(x)
y.a = rho.a(x)
y.h = rho.h(x, k = k)

y.q = rho.q(x, q = tau)
y.e = rho.e(x, q = tau)
y.hq = rho.hq(x, q = tau, k=k)

# par(mfrow=c(1,2))
# plot(sort(x), y.a$f[order(x)],type="l",col="black", lwd=3, cex.axis=1, xaxs="i", yaxs="i", xlab="x", ylab="Loss Functions for Location Estimation") 
# lines(sort(x), y.l$f[order(x)], type="l",col="red",lwd=3)
# lines(sort(x), y.h$f[order(x)], type="l",col="blue",lwd=3)

plot(sort(x), y.q$f[order(x)],type="l",col="black", lwd=3, cex.axis=1, xaxs="i", yaxs="i", xlab="x", ylab="Loss Functions for Tail Estimation") 
lines(sort(x), y.e$f[order(x)], type="l",col="red",lwd=3)
lines(sort(x), y.hq$f[order(x)], type="l",col="blue",lwd=3)   

# q = 0.5
# y.q = rho.q(x, q = q)
# y.e = rho.e(x, q = q)
# y.hq = rho.hq(x, q = (1-q), k=k)
# lines(x, y.q$f, type="l",col="black",lwd=3, lty = 2)
# lines(x, y.e$f, type="l",col="red",lwd=3,lty = 2)
# lines(x, y.hq$f, type="l",col="blue",lwd=3, lty = 2) 
















```

automatically created on 2018-05-28