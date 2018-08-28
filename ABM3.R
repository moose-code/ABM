# Agent based model
# Simulating a credit network of hetrogenous agents. Here we have Banks, and firms
# Jonathan Clark
# 12/07/2018


MC=1 #Number of Monte Carlo Simulations to be executed

#MONTE CARLO INVARIANT PARAMETERS SETTING
#Number of simulation rounds
Time=1000
#Number of firms
Nf = 500
#Number of banks
Nb = 50
#Interest rate parameter
gamma = 0.02
#Number of potential partners on credit market
chi = 5
#Partner choice parameter: intensity of choice
lambda = 4
#Leverage adjustment speed parameter
adj = 0.1
#Production function parameter
phi = 3
#Production function parameter
beta = 0.7
#Firms’ price parameter: mean of Normal Distribution
alpha = 0.1
#Firms’ price parameter:variance of Normal Distribution
varpf = 0.4
#Central bank’s interest rate
rCB = 0.02
#Banks’ costs
cB = 0.01


#(AGGREGATE) REPORTS INITIALIZATION
#Switching rate Report
changeFB = matrix (data = 0, ncol=MC, nrow=Time)
#Aggregate output Report
YF = matrix(data = 0, ncol = MC, nrow = Time)
#Aggregate Banks’ NW Report
AB = matrix(data = 0, ncol = MC, nrow = Time)
#Aggregate Firms’ NW Report
AF = matrix(data = 0, ncol = MC, nrow = Time)
#Aggregate Debt Report
BF = matrix(data = 0, ncol = MC, nrow = Time)
#(Simple) Average interest rate Report
RBF = matrix(data = 0, ncol = MC, nrow = Time)
#Aggregate bad debt Report
BAD = matrix(data = 0, ncol = MC, nrow = Time)
#Total firms’ defaults Report
FALLF = matrix(data = 0, ncol = MC, nrow = Time)
#Total banks’ defaults Report
FALLB = matrix(data = 0, ncol = MC, nrow = Time)
#Aggregate Leverage (BF/AF) Report
LEV = matrix(data = 0, ncol = MC, nrow = Time)
#Aggregate firms’ profits Report
PRF = matrix(data = 0, ncol = MC, nrow = Time)
#Aggregate banks’ profit Report
PRB = matrix(data = 0, ncol = MC, nrow = Time)
#Aggregate growth rate Report
GR = matrix(data = 0, ncol = MC, nrow = Time)
#(Simple) average price
PF = matrix(data = 0, ncol = MC, nrow = Time)
#Firms’ probability of default report
PBF= matrix(data = 0, ncol = MC, nrow = 1)
#Banks’ probability of default report
PBB= matrix(data = 0, ncol = MC, nrow = 1)



for (mc in 1:MC) {
  #HERE SIMULATION SETUP
  print(paste("Monte Carlo N", mc))
  set.seed (mc)
  
  
  #INITIAL CONDITIONS OF THE SIMULATION RUN
  #Banks specific component of the interest rate
  Rb = matrix(data = 0, ncol = Nb, nrow = 1)
  #Banks’ Net Wealth
  Ab = matrix(data = 0, ncol = Nb, nrow = 1)
  #Firms-Banks credit matching
  link_fb = matrix(data =0, ncol = Nf, nrow = 1)
  #Firms’ interest rate on loans
  Rbf = matrix(data = 0, ncol = Nf, nrow = 1)
  #Firms’ leverage
  lev = matrix(data = 0, ncol = Nf, nrow = 1)
  #Firms’ price
  pf = matrix(data = 0, ncol = Nf, nrow = 1)
  #Firms’ debt
  Bf = matrix(data = 0, ncol = Nf, nrow = 1)
  #Firms’ Net Wealth
  Af = matrix(data = 0, ncol = Nf, nrow = 1)
  #Firms’ defaults (1=defaulted,, 0=surviving)
  fallf = matrix(data = 0, ncol = Nf, nrow = 1)
  #Banks’ defaults (1=defaulted,, 0=surviving)
  fallb = matrix(data = 0, ncol = Nb, nrow = 1)
  #Loss-given-default ratio
  LGDf = matrix(data = 0, ncol = Nf, nrow = 1)
  #Deposits
  D=matrix(data = 0, ncol = Nb, nrow = 1)
  #Banks’ non performing loans
  Badb=matrix(data = 0, ncol = Nb, nrow = 1)
  #Banks’ profits
  Prb=matrix(data = 0, ncol = Nb, nrow = 1)
  #Banks’ credit link degree
  creditDegree=matrix(data = 0, ncol = Nb, nrow = Time)
  
  
  
  #INITIALIZE THE VALUES OF VARIABLES REQUIRED TO START THE SIMULATION
  Af[1:Nf] = 10
  Ab[1:Nb] = 10
  pf[1:Nf] = rnorm(Nf)*varpf+alpha
  lev[1:Nf] = 1
  link_fb[1:Nf] = ceiling(runif(Nf)*Nb)
  
  for (t in 1:Time) {
    #HERE BODY OF THE SIMULATION
    
    # 0)REPLACEMENT OF DEFAULTED FIRMS & BANKS
    #FIRMS
    for (i in 1:Nf) {
      if (fallf[i] == 1) {
        #NEW FIRMS INITIALIZATION
        Af[i] = 2*runif(1)
        lev[i] = 1
        pf[i] =  rnorm(1) * varpf + alpha
        link_fb[i]=ceiling(runif(1)*Nb)
        Rbf[i] = rCB + Rb[link_fb[i]] + gamma*(lev[i]) / ((1+Af[i]/max(Af)))
      }
    } #BANKS
    for (j in 1:Nb) {
      if (fallb[j] == 1) {
        #NEW BANK INITIALIZATION
        Ab[j] = 2 * runif(1)
      } 
    }
    
    
    # 1)UPDATE BANK SPECIFIC INTEREST COMPONENT
    Rb = gamma * Ab^(-gamma)
    
    
    # 2)MATCHING ON CREDIT MARKET
    for (i in 1:Nf) {
      #SELECT POTENTIAL PARTNERS
      newfb=ceiling(runif(chi)*Nb)
      #SELECT BEST INTEREST AMONG POTENTIAL PARTNERS
      inew = min(Rb[newfb])
      #PICK UP THE INTEREST OF THE OLD PARTNER
      iold = Rb[link_fb[i]]
      #COMPARE OLD AND NEW INTERESTS
      if (runif(1) < (1-exp(lambda*(inew-iold)/(inew)))) {
        #THEN SWITCH TO A NEW PARTNER
        changeFB[t] = changeFB[t] + 1
        research = which(Rb[newfb]==min(Rb[newfb]))
        #CHECK IF MULTIPLE BEST INTERESTS
        if (length(research)>1){
          research=research[ceiling(runif(1)*length(research))]
        }
        #UPDATE THE LINK
        link_fb[i] = newfb[research[1]]
      }
      else {
        #STICK TO THE OLD PARTNER
        link_fb[i]=link_fb[i]
      }
    }
    #COMPUTE SWITCHING RATE
    changeFB[t]=changeFB[t]/Nf
    
    
    # 3) FIRMS UPDATE LEVERAGE TARGET
    #RANDOM SAMPLE FROM U(0,1) FOR EACH FIRM
    u = runif(Nf)
    #IF PRICE>INTEREST INCREASE LEVERAGE
    lev[pf>Rbf] = lev[pf>Rbf] * (1 + adj*u[pf>Rbf])
    #OTHERWISE...
    lev[pf<=Rbf] = lev[pf<=Rbf] * (1 - adj*u[pf<=Rbf])
    
    
    # 4)DETERMINE DEMAND FOR LOANS
    Bf = lev * Af
    # 5)COMPUTE TOTAL FINANCIAL CAPITAL
    Kf = Af + Bf
    # 6)COMPUTE (FINANCIALLY CONSTRAINED) OUTPUT
    Yf = phi * Kf^beta
    # 7)UPDATE THE PRICE
    pf = rnorm(Nf) * varpf + alpha
    # 8)COMPUTE THE INTEREST RATE CHARGED TO FIRMS
    Rbf = rCB + Rb[link_fb] + gamma*(lev) / ((1+Af/max(Af)))
    
    # 9)COMPUTE FIRMS’ PROFITS
    Prf = pf * Yf - Rbf * Bf #Firms profits
    # 10)UPDATE FIRMS’ NET WORTH AND CHECK WHETER THEY ARE DEFAULTED
    Af=Af+Prf
    # AND DEFAULT CHECK
    fallf[Af>0]=0
    fallf[Af<=0]=1 #Firm defaults
    
    # 11)COMPUTE THE LOSS GIVEN DEFAULT RATIO
    LGDf[1:Nf]= -(Af) / (Bf)
    LGDf[LGDf>1] = 1
    LGDf[LGDf<0] = 0
  
    
    ### Now looking at banks...
    
    # 12)COMPUTE "DEPOSITS"
    for (j in 1:Nb) {
      D[j] = sum(Bf[link_fb==j])-Ab[j]
      if (D[j]<0) {
        D[j]=0 }
      # 13) COMPUTE THE BAD DEBT
      Badb[j] = sum(LGDf[fallf==1&link_fb==j] * Bf[fallf==1&link_fb==j])
      # 14) COMPUTE BANKS’ PROFITS
      Prb[j] = Bf[link_fb==j&fallf==0] %*% Rbf[link_fb==j&fallf==0]
      - rCB * D[j] - cB * Ab[j]-Badb[j]
    }
    
    # 15)UPDATE BANKS’ NET WORTH AND CHECK WHETHER THEY ARE DEFAULTED
    Ab=Ab+Prb
    fallb[Ab>0] = 0
    fallb[Ab<=0] = 1 #Bank defaults
    
    
    
    
    #REPORTS
    YF[t,mc]=sum(Yf) #Total output
    AB[t,mc]=sum(Ab) #Total banks’ wealth
    AF[t,mc]=sum(Af) #Total firms’ wealth
    BF[t,mc]=sum(Bf) #Total debt
    RBF[t,mc]=sum(Rbf*Bf)/sum(Bf) #(Weighted) average interest rate
    BAD[t,mc]=sum(Badb) #Total bad debt
    PRF[t,mc]=sum(Prf) #Firms’ profits
    PRB[t,mc]=sum(Prb) #Banks’ profits
    FALLF[t,mc]=sum(fallf==1) #Number of failures (firms)
    FALLB[t,mc]=sum(fallb==1) #Number of failures (banks)
    LEV[t,mc]=BF[t,mc]/AF[t,mc] #The economy leverage
    #The growth rate
    if (t==1){
      GR[t,mc]=0 }
    else{
      GR[t,mc]=YF[t,mc]/YF[t-1,mc]-1
    }
    PF[t,mc]= mean(pf) #Average price
    
    
    
  }
  
  #PLOTS SIMULATION REPORTS
  folder="Results/"
  par(mfrow=c(3,3))
  #Output
  plot(YF[,mc],type="l",main="Output (YF)",
       ylab="",xlab="",lwd=1, lty=1)
  #Firms’ profit
  plot(PRF[,mc],type="l",main="Firms’ profits (PRF)",
       ylab="",xlab="",lwd=1, lty=1)
  #Banks’ profit
  plot(PRB[,mc],type="l",main="Banks’ profits (PRF)",
       ylab="",xlab="",lwd=1, lty=1)
  #Firms’ net worth
  plot(AF[,mc],type="l",main="Firms’ Net Worth (AF)",
       ylab="",xlab="",lwd=1, lty=1)
  #Banks’ net worth
  plot(AB[,mc],type="l",main="Banks’ Net Worth (AB)",
       ylab="",xlab="",lwd=1, lty=1)
  #Loans
  plot(BF[,mc],type="l",main="Total Loans (BF)",
       ylab="",xlab="",lwd=1, lty=1)
  #Average leverage
  plot(LEV[,mc],type="l",main="Firms’ Average Leverage",
       ylab="",xlab="",lwd=1)
  #Average interest rate
  plot(RBF[,mc],type="l",main="Average Loan Interest Rate (RBF)",
       ylab="",xlab="",lwd=1, lty=1)
  #Average price
  plot(PF[,mc],type="l",main="Average Price (PF)",
       ylab="",xlab="",lwd=1, lty=1)
  dev.copy2eps(file=paste(folder,"/SimOverview",mc,".ps",sep=""),
               width = 10, height= 7)
  #dev.off()

  
  #Firms’ and banks’ defaults
  #par(mfrow=c(2,1))
  #barplot(FALLF[,mc], main="Firms’ Defaults", names.arg=c(1:Time),
          #xlab="")
  #barplot(FALLB[,mc], main="Banks’ Defaults", names.arg=c(1:Time),
          #xlab="")
  #dev.copy2eps(file=paste(folder,"/Defaults",mc,".ps",sep=""), width = 15, height= 9.5)
#dev.off()

  
}




