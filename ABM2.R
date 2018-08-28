# Agent based model
# Simulating the economics of firms within an industry
# Jonathan Clark
# 02/07/2018
# CSC3003S DEMO

##################################
## This model simulates a change in monetary policy with a hike in interest rates from 7.5% to 10%
##################################

#SET TIME SPAN AND NUMBER OF FIRMS
#Number of simulation periods
Time <- 1000
#Number of firms
Ni <- 100
#Number of multiple simulations
MC <- 2

#PARAMETER SETTING
#Investment accelerator
gamma <- 2
#Capital productivity
phi <- 0.1
#Interest rate
#r <- 0.1
#Random price constant
Pbar <- 0.01
#Pbar <- 0

#Depreciation rate
delta = 0.05

#rbar=0.075 #Interest rate


#Aggregate production
YY <- matrix(data=0,ncol=MC,nrow=Time)
AA <- matrix(data=0,ncol=MC,nrow=Time)
BB <- matrix(data=0,ncol=MC,nrow=Time)
LEV <- matrix(data=0,ncol=MC,nrow=Time)


for (mc in 1:MC) {
  
  set.seed(15)
  #Sensitivty Analysis
  #Pbar <- Pbar + 0.005
  
  #ALLOCATING VARIABLES AND INITIAL CONDITIONS
  #Firms’ net worth
  A <- matrix(data=1,ncol=1,nrow=Ni)
  #Firms’ capital
  K <- matrix(data=1,ncol=1,nrow=Ni)
  #Firms’ debt
  B <- matrix(data=0,ncol=1,nrow=Ni)
  #Firms’ investment
  I <- matrix(data=0,ncol=1,nrow=Ni)
  #Stochastic price
  P <- matrix(data=0,ncol=1,nrow=Ni)
  #Firms’ production
  Y <- matrix(data=0,ncol=1,nrow=Ni)
  #Firms’ profit
  Z <- matrix(2*runif(Ni)+Pbar,ncol=1,nrow=Ni)
  
  
  #SEQUENCE OF EVENTS
  for (t in 2:Time) {
    
    if (mc>1 & t>500) {
      rbar = 0.1  #Interest rate
    } else {
      rbar = 0.075 #Interest rate
    }
    
    
    I <- gamma * Z #Investment choice
    I[I<0] = 0 # assume that there is no investment in case of negative profits
    K <- K + I - K*delta #Capital accumulation
    Y <- phi * K #Production
    B <- K - A #Debt
    B[B<0] <- 0 #Self-financed firms
    P <- 2*runif(Ni)+ Pbar #Stochastic price
    r <- rbar + rbar*(B/A)^rbar #Interest rate, this is firm specific dependant on both a fized policy rate rbar, and a firm specific risk premium
    Z <- P * Y - r * K #Profit
    A <- A + Z #Net worth
    Z[A<0] <- 0 #Entry condition
    K[A<0] <- 1 #Entry condition
    A[A<0] <- 1 #Entry condition
    YY[t,mc] <- sum(Y) #Aggregate production
    AA[t,mc] <- sum(A) #Net worth
    BB[t,mc] <- sum(B) #Debt
    LEV[t,mc] <- BB[t]/AA[t]
  }
}

par(mfrow=c(1,1))
#PLOTTING AGGR. PRODUCTION WITH AND WITHOUT THE POLICY CHANGE
plot(2:Time, YY[2:Time,1],type="l",
     ylim=range(0,max(YY)),ylab="YY (Aggreagte Output)",xlab="Time")
lines(2:Time, YY[2:Time,2],type="l",
      ylim=range(0,max(YY)),lty=2,lwd=2)

