#fonction de calcul de la valeur  du call selon la formule de black and scholes
blackScholesCall<-function(S0,k,sigma,r,t){

d1<-(log(S0/k)+(r+sigma^2/2)*t)/(sigma*sqrt(t))
d2<-d1-sigma*sqrt(t)
c0<-S0*pnorm(d1)-k*exp(-r*t)*pnorm(d2)

}

#fonction de calcul de la valeur du  put selon la formule de black and scholes
blackSholesPut<-function(S0,k,sigma,r,t){
d1<-(log(S0/k)+(r+sigma^2/2)*t)/(sigma*sqrt(t))
d2<-d1-sigma*sqrt(t)
p0<--S0*pnorm(-d1)+k*exp(-r*t)*pnorm(-d2)


}
