###############################
#  EXP MODEL: Simulated Data  #
###############################

### Data ###
set.seed(1)
nn=100
x <- rexp(nn, 1 )
#x=rlnorm(nn,1,4)
#y=rweibull(nn, .5,2)
y=rlnorm(nn,4,1)
surv.time=ifelse(x<=y,x,y)
status=ifelse(x<=y,1,0)
surv.time
status
T=surv.time
surv.dat=data.frame(T, status)
surv.dat
surv.dat1<- surv.dat[surv.dat[,2]==1,] 
T1=surv.dat1[,1]
T1
### MLE ###
#install.packages("parmsurvfit")
library(parmsurvfit)
fit=fit_data(data = surv.dat, dist = "exp",time = "T",censor = "status" )
fit
#attributes(fit)
#mle
mle=fit$estimate[[1]] #rate
mle
#Model: EXPONENTIAL
# MLE
r=length(T1)
mle=r/sum(T)
mle

###################	
#    Parameters   # 
###################
c=10 # parameters:  c=a
epsilon=0.01
lambda <- c/epsilon
lambda  
# Model
curve(pexp(x, mle) ,xlab = NULL, xlim=c(0,200),ylim=c(0,1),lty = c(1),
ylab="Prob", lwd=4, main=("Exponential Model"))
legend(130,0.3,c("Model", "Prior", "Posterior"), col = c(1,4,2),text.col = "black", 
lty = c(1, 2,3),lwd =c(2,2,2))
mtext(paste("n=",nn, ", k=",c))

set.seed(NULL)

##############
#  Distance  #
##############
rep=500 # distance size
d.prior=numeric(rep)
d.post=numeric(rep)

########################	
#    Prior Distance    # 
########################
for (L in 1:rep){

# Generating log-beta process via Algorithm 2
# Step (b)
M <- rpois(1,lambda)
M
# Step (c)
t <- rexp(M,mle)  
J.cont.prior=matrix(0,M,1)
# Step (d)
beta<-function(t){c*exp(-mle*t)}# beta(t)
for  (j in 1:M) 
         {	
          # J: jump sizes
          a=epsilon     
          b=beta(t[j])
          J.cont.prior[j] <- (-1)*log(1-rbeta(1,a,b))
          }
# Step (e)
theta.Z=t # location of log-beta
J.Z=J.cont.prior # weights of log-beta 

# Reordinging the locations and weights: to plot
orderIndex=order(theta.Z)
theta.Z=theta.Z[orderIndex] 
J.Z= J.Z[orderIndex]
x=cbind(theta.Z,J.Z)
x.sort=x[order(theta.Z),]
theta.Z=c(x.sort[,1])
J.Z= c(x.sort[,2]) # weights

Y=theta.Z #sorted locations
J=J.Z #sorted jumps
#remove zero weights
nonzero.prior=cbind(Y,J)
nonzero.prior <- nonzero.prior[nonzero.prior[,2]>0,] 
Y=c(nonzero.prior[,1])
J=c(nonzero.prior[,2])
#############
# Distance  #
#############
n.new=length(Y) # number of terms in the summation
m = ceiling(sqrt(n.new) + 0.5)   # Positive integer smaller than r/2

# Case 1: theta(i-l) = theta(1) if 1<= i <= m
# Case 2: m+1<= i <= n.new-m 
# Case 3: n.new-m+1 <= i <= n.new  theta(i+m) = theta(n.new)
summation <- numeric(n.new) # stores the entire sum

# Case 1
for (i in 1:m){
numerator=pexp(Y[i+m],mle)-pexp(Y[1],mle) # pexp: F0
denominator=exp(-1*J[1])-exp(-1*sum(J[1:(i+m)]))
summation[i] = log( numerator/denominator)
}

# Case 2
for (i in (1+m):(n.new-m)) {
numerator=pexp(Y[i+m],mle)-pexp(Y[i-m],mle)
denominator=exp(-1*sum(J[1:(i-m)]))-exp(-1*sum(J[1:(i+m)]))
summation[i] = log( numerator/denominator)
}

# Case 3 
for (i in (n.new-m+1):n.new){
numerator=pexp(Y[n.new],mle)-pexp(Y[i-m],mle)
denominator=exp(-1*sum(J[1:(i-m)]))-exp(-1*sum(J[1:n.new]))
summation[i] = log( numerator/denominator)
}

summation=summation[is.finite(summation)]
# omit NA values
d.prior[L]=mean(summation)
}
############################	
#    Postrior  Distance    # 
############################

for (L in 1:rep){

# Step 1.
# Generating log-beta process via Algorithm 3
# Step (a)
########################
# Continuous: censored #
########################
M <- rpois(1,lambda)
M
t <- rexp(M,mle)
J.cont.post=matrix(0,M,1)
beta.post<-function(t){c*exp(-mle*t)+sum(T >= t)}# beta(t)

for  (j in 1:M) 
         {	
          # J: jump sizes
          a=epsilon     
          b=beta.post(t[j])
          J.cont.post[j] <- (-1)*log(1-rbeta(1,a,b))
          }

####################################
# Continuous: fixed (non-censored) #
####################################
# Step (b)
#Nt=N(t)-N(t-)=the number of observations at time t
T1=sort(T1)
tab=table(T1)
Nt=as.matrix(tab)
T2=unique(T1)
T2=sort(T2)
J.fixed=numeric(length(T2))
for  (s in 1: length(T2))
{
v=beta.post(T2[s])-Nt[s]
y1=rbeta(1,Nt[s],v)
J.fixed[s]=-log(1-y1)
}

# Step(c)
#location
theta.Z=c(t,T2)
#weight
J.Z= c(J.cont.post,J.fixed)
x=cbind(theta.Z,J.Z)
x.sort=x[order(theta.Z),]
theta.Z=c(x.sort[,1])
J.Z= c(x.sort[,2]) # weights

Y=theta.Z #sorted locations
J=J.Z #sorted jumps

#############
# Distance  #
#############
n.new=length(Y) # number of terms in the summation
m = ceiling(sqrt(n.new) + 0.5)   # Positive integer smaller than r/2

# Case 1: theta(i-l) = theta(1) if 1<= i <= m
# Case 2: m+1<= i <= n.new-m 
# Case 3: n.new-m+1 <= i <= n.new  theta(i+m) = theta(n.new)

summation <- numeric(n.new) # stores the entire sum

# Case 1
for (i in 1:m){
numerator=pexp(Y[i+m],mle)-pexp(Y[1],mle)
denominator=exp(-1*J[1])-exp(-1*sum(J[1:(i+m)]))
summation[i] = log( numerator/denominator)
}

# Case 2
for (i in (1+m):(n.new-m)) {
numerator=pexp(Y[i+m],mle)-pexp(Y[i-m],mle)
denominator=exp(-1*sum(J[1:(i-m)]))-exp(-1*sum(J[1:(i+m)]))
summation[i] = log( numerator/denominator)
}

# Case 3 
for (i in (n.new-m+1):n.new){
numerator=pexp(Y[n.new],mle)-pexp(Y[i-m],mle)
denominator=exp(-1*sum(J[1:(i-m)]))-exp(-1*sum(J[1:n.new]))
summation[i] = log( numerator/denominator)
}

summation=summation[is.finite(summation)]
# omit NA values

d.post[L]=mean(summation)
}
d.post

d.prior=d.prior[d.prior>=0]
d.post=d.post[d.post>=0]

###Relative Belief Ratio###
NN=20
#prior quantiles i=0,...,NN
psi.prior=matrix(0,NN+1,1)
psi.prior[1]=0
for (i in 1:NN) psi.prior[i+1]=quantile(d.prior,i/NN)
psi.star=psi.prior[2]# 1/N quantile
Fn.prior=ecdf(d.prior) # Fn for prior distance
Fn.post=ecdf(d.post) #Fn for posterior diistance
RBR=Fn.post(psi.star)/Fn.prior(psi.star)

###Strength###
RBR.hat=matrix(0,NN,1)
for (i in 1:NN) RBR.hat[i]=NN*(Fn.post(psi.prior[i+1])-Fn.post(psi.prior[i]))
sum0=matrix(0,NN,1)
for (i in 1:NN) {if (RBR.hat[i]<=RBR) sum0[i]=Fn.post(psi.prior[i+1])-Fn.post(psi.prior[i])}
strength=sum(sum0)
cbind(RBR,strength)

###################################
# Non-Bayesian Test: LogRank Test #
###################################
LogRank1 <- function(z1,d1,Hz1)
# This function compute the one sample logrank test.
# z1 are the observed (possibly censored) times, 
# d1 are the censoring indicators, 1--uncensored, 0--censored;
# Hz1 should be a vector of null cumulative hazard at the observed times
# i.e. H_0(z1). 

{
   if( (length(z1)!=length(d1)) | (length(d1)!=length(Hz1)) ) 
       stop("input must have same length")
   if(any((d1!=0)&(d1!=1)) ) stop("d1 must be 0/1's for censor/not-censor") 
   if(any(Hz1 < 0)) stop("Hz1 must be nonnegative")

   EE <- sum(Hz1) 
   diff <- sum(d1) - EE  

   temp2 <- diff/sqrt(EE)
   pval <- 1-pchisq((temp2)^2, df=1)

   list(Logrank=temp2, ApproxPvalue2side=pval)
} 
z1=T
d1=status
Hz1=z1*mle
pvalue=LogRank1(z1,d1,Hz1)$ApproxPvalue2side

###Results####
cbind(RBR,strength,pvalue)
cbind(mean(d.prior),mean(d.post))

c
nn
mle
status
