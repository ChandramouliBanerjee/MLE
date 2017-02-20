#Codes for example and exercise of Chapter 1 of Millar.
library("ggplot2")
library("Bhat")

#Binomial Example
nloglhood=function(p){
  return(-(log(choose(100,10))+10*log(p)+90*log(1-p)))
}

#Visualizing the log-likelihood
x = seq(0.01,0.99,0.01)
y = nloglhood(x)
data = data.frame(x,y)
p1 = ggplot(data, aes(y =y, x = x)) + geom_line(color = "dodgerblue3") + theme_bw() + labs(list(x =  "p", y = "-L(p)"))

#Minimize the negative log-likelihood
binom.fit=optim(0.5,nloglhood,lower=0.0001,upper=0.9999, hessian=TRUE)

#The MLE
phat=binom.fit$par 

#Variance is inverse hessian
phat.var=1/binom.fit$hessian 

#Calculate 95% Wald CI
WCI = phat+c(-1,1)*qnorm(0.975)*sqrt(phat.var)

#Calculate LR based CI
#Set up list for input into plkchi function. The first argument to the profile likelihood function plkhci is a list with
#elements giving the parameters of nloglhood, the MLE, and lower and
#upper bounds of the parameter space.
control.list=list(label="p",est=phat,low=0,upp=1)
plkhci(control.list,nloglhood,"p")

#Poisson Exercise
nlogl.poisson = function(lambda)
{
  return(-(-lambda+3*log(lambda)-log(6)))
}
x = seq(0.1,15,0.01)
y = nlogl.poisson(x)
data = data.frame(x,y)
p2 = ggplot(data, aes(y =y, x = x)) + geom_line(color = "dodgerblue3") + theme_bw() + labs(list(x =  expression(lambda), y = "-ve log-likelihood"))

#Minimize the negative log-likelihood
binom.fit=optim(1.0 ,nlogl.poisson,lower=0.0001,upper=15, hessian=TRUE)

#The MLE
lambdahat=binom.fit$par 

#Variance is inverse hessian
lambdahat.var=1/binom.fit$hessian 

#Calculate 95% Wald CI
WCI.poisson = lambdahat+c(-1,1)*qnorm(0.975)*sqrt(phat.var)

#Calculate LR based CI
#Set up list for input into plkchi function. The first argument to the profile likelihood function plkhci is a list with
#elements giving the parameters of nloglhood, the MLE, and lower and
#upper bounds of the parameter space.
control.list=list(label="lambda",est=lambdahat,low=0,upp=25)
plkhci(control.list,nlogl.poisson,"lambda")