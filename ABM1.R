# Agent based model
# Simulating the economics of firms within an industry
# Jonathan Clark
# 02/07/2018

#set.seed(15)

##################################
## This model conducts sensitivity analysis on proce points of the firms
##################################

#SET TIME SPAN AND NUMBER OF FIRMS
#Number of simulation periods
Time <- 1000
#Number of firms
Ni <- 100
#Number of multiple simulations
MC <- 3



#PARAMETER SETTING
#Investment accelerator
gamma <- 2
#Capital productivity
phi <- 0.1
#Interest rate
#r <- 0.1
#Random price constant
#Pbar <- 0.01
Pbar <- 0

#Depreciation rate
delta = 0.05

rbar=0.075 #Interest rate


#Aggregate production
YY <- matrix(data=0,ncol=MC,nrow=Time)
AA <- matrix(data=0,ncol=MC,nrow=Time)
BB <- matrix(data=0,ncol=MC,nrow=Time)
LEV <- matrix(data=0,ncol=MC,nrow=Time)


for (mc in 1:MC) {
  
  set.seed(15)
  #Sensitivty Analysis
  Pbar <- Pbar + 0.005

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

#PLOTTING AGGREGATE PRODUCTION
#plot(2:Time, YY[2:Time,1],type="l",ylab="YY",xlab="t")

#PLOTTING AGGREGATE NET WORTH
#plot(2:Time, AA[2:Time,1],type="l",ylab="AA",xlab="t")

#PLOTTING AGGREGATE DEBT
#plot(2:Time, BB[2:Time,1],type="l",ylab="BB",xlab="t")

#plot(2:Time, log(YY[2:Time,1]),type="l", ylim=range(log(YY[2:Time])),col=1,ylab="log(YY)",xlab="t")
 

# layout(matrix(c(1, 3, 2, 4), 2, 2))
# layout.show(4)
# #PLOTTING AGGREGATE PRODUCTION
# plot(201:Time, log(YY[201:Time,1]),type="l", ylim=range(log(YY[201:Time])),col=1,ylab="log(YY)",xlab="t")
# #PLOTTING AGGREGATE DEBT
# plot(201:Time, LEV[201:Time,1],type="l", ylim=range(LEV[201:Time]),col=1,ylab="leverage",xlab="t")
# #PLOTTING AVERAGE LEVERAGE
# plot(201:Time, log(AA[201:Time,1]),type="l", ylim=range(log(AA[201:Time])),col=1,ylab="log(AA)",xlab="t")
# #PLOTTING AVERAGE INTEREST RATE
# plot(201:Time, log(BB[201:Time,1]),type="l", ylim=range(log(BB[201:Time])),col=1,ylab="log(BB)",xlab="t")



plot(2:Time, YY[2:Time,1],type="l",ylim=range(0,max(YY)),ylab="YY",xlab="t", col ="red")
lines(2:Time, YY[2:Time,2],type="l",
      ylim=range(0,max(YY)),lty=2, lwd=2, col ="green")
lines(2:Time, YY[2:Time,3],type="l",
      ylim=range(0,max(YY)),lty=3, lwd=2, col= "blue")


# #COMPUTE MC MEAN, STANDARD DEVIATION AND CONFIDENCE INTERVALS
# meanYY = matrix(data=0,nrow=Time,ncol=1)
# stdYY = matrix(data=0,nrow=Time,ncol=1)
# YYup = matrix(data=0,nrow=Time,ncol=1)
# YYdown = matrix(data=0,nrow=Time,ncol=1)
# for (t in 1:Time) {
#   meanYY[t] = median(YY[t,]) # i replaced mean with median here for the below plot...
#   stdYY[t] = sd(YY[t,])
# }
# YYup = meanYY + 2*stdYY
# YYdown = meanYY - 2*stdYY


plot(2:Time, meanYY[2:Time],type="l",ylim=range(0,max(meanYY)),
     ylab="median(YY)",xlab="t")

#lines(2:Time, YYup[2:Time],type="l", ylim=range(0,max(YYup)),lty=2, lwd=2)
#lines(2:Time, YYdown[2:Time],type="l", ylim=range(0,max(YYdown)),lty=2, lwd=2)


