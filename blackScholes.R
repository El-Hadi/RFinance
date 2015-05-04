blackScholes<-function(S0,k,sigma,r,t){

d1<-(log(S0/k)+(r+sigma^2/2)*t)/(sigma*sqrt(t))
d2=d1-sigma*sqrt(t)
c0<-S0*pnorm(d1)-k*exp(-r*t)*pnorm(d2)

}
