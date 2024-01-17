#################
#  EXAMPLE 3    #
#################

#################
#     DATA      #
#################
#Full Data: contains censored & non-cencored (fixed)
T=c(4.5,19.13,14.24,7.87,5.49,2.02,9.22,3.82,26.31,
4.65,2.62,0.90,21.73,0.87,0.51,3.36,43.01,0.81,3.36,
1.46,24.80,10.86,17.14,15.96,7.28,4.33,22.69,2.46,3.48,
4.23,6.54,8.65,5.41,2.23,4.34,32.15,4.87,5.71,7.59,3.02,
4.51,1.05,9.47,79.05,2.02,4.26,11.25,10.34,10.66,12.03,2.64,
14.74,1.19,8.66,14.83,5.62,18.10,25.74,17.36,1.35,9.02,6.94,7.26,
4.70,3.70,3.64,3.57,11.64,6.25,25.82,3.88,3.02,19.36,20.28,46.12,
5.17,0.20,36.66,10.06,4.94,5.06,16.62,12.07,6.97,0.08,1.40,2.75,
7.32,1.26,6.76,8.60,7.62,3.52,9.74,0.40,5.41,2.54,2.69,8.26,0.50,
5.32,5.09,2.09,7.93,12.02,13.08,5.85,7.09,5.32,4.33,2.83,8.37,14.77,
8.53,11.98,1.76,4.40,34.26,2.07,17.12,12.63,7.66,4.18,13.29,23.63,3.25,
7.63,2.87,3.31,2.26,2.69,11.79,5.34,6.93,10.75,13.11,7.39)

##################################
# Discrete: Fixed (non-censored) #
##################################
T1=c(4.5,19.13,14.24,7.87,5.49,9.22,3.82,26.31,2.62,
0.90,21.73,0.51,43.01,0.81,3.36,1.46,17.14,15.96,7.28,
4.33,22.69,2.46,3.48,4.23,6.54,8.65,5.41,2.23,4.34,32.15,
4.87,5.71,7.59,4.51,1.05,9.47,79.05,2.02,4.26,11.25,10.34,
10.66,12.03,2.64,14.74,1.19,8.66,14.83,5.62,18.10,25.74,17.36,
1.35,9.02,6.94,7.26,3.70,3.64,3.57,11.64,6.25,25.82,3.88,3.02,
20.28,46.12,5.17,0.20,36.66,10.06,4.94,5.06,16.62,12.07,6.97,
0.08,1.40,2.75,7.32,1.26,6.76,7.62,3.52,9.74,0.40,2.54,2.69,8.26,
0.50,5.09,2.09,7.93,12.02,13.08,5.85,7.09,5.32,2.83,8.37,14.77,
8.53,11.98,1.76,4.40,34.26,2.07,17.12,12.63,7.66,4.18,13.29,23.63,
3.25,7.63,2.87,3.31,2.26,11.79,5.34,6.93,10.75,13.11,7.39)

# Cencored
T0=c(4.65, 0.87, 24.80, 10.86, 4.70, 3.02 ,19.36, 
8.60, 24.80, 10.86, 4.70,  3.02, 19.36, 8.60, 4.33)

%T0=T[!(T %in% T1)]


#non-censored
#T1=c(278, 317, 327, 342, 354, 361, 370, 380, 395, 401, 431, 438, 482,
# 484, 507,513, 521, 549, 553, 568, 575, 588, 596, 599, 627, 629, 633, 633, 636,
# 641,642, 645, 659, 680, 685, 692, 700, 704, 741, 743, 757, 767,
# 772, 784, 788,790, 790, 793, 798, 823, 825, 830, 838, 846, 852, 853, 860,
# 863, 869, 871,889, 901, 902, 911, 913, 921, 935, 944, 947, 965, 994,
# 999, 1003, 1012,1023, 1045, 1049, 1050, 1051, 1053, 1058, 1069, 1078, 1081, 1087,
#1095, 1103, 1118, 1118, 1137, 1140, 1149, 1186, 1198, 1223, 1227,
#1271, 1283, 1339, 1342, 1357, 1358, 1372, 1373, 1377, 1413, 1436,
#1436, 1444, 1493, 1494, 1496, 1511, 1528, 1538, 1566, 1574, 1586,
#1622, 1757, 1887, 2115)

# Cencored
#T0=c(470, 504, 626, 717, 781, 813, 860, 886, 906, 947, 973, 982,
# 1002, 1015,
#1023, 1069, 1122, 1150, 1182, 1211, 1313, 1332, 1409, 1426, 1476,
#1542, 1577, 1588, 1606, 1683, 1738, 1855, 1911, 1946, 1960, 1969,
#2075, 2078, 2092, 2133, 2241, 2242, 2342, 2367, 2385, 2427)

status=c(rep(1,length(T1)), rep(0,length(T0)))
T=c(T1,T0)

surv.dat=data.frame(T, status)


### MLE ###
# "norm", "lnorm", "exp", "weibull","logis", "llogis", "gompertz", etc

fit=fit_data(data = surv.dat, dist = "weibull",time = "T",censor = "status" )
fit
fit=fit_data(data = surv.dat, dist = "exp",time = "T",censor = "status" )
fit

#attributes(fit)
#mle: weibull
k1=fit$estimate[[1]] #shape
k2=fit$estimate[[2]] #scale
cbind(k1,k2)

# True distribution
curve(pweibull(x, k1,k2) ,xlab = NULL, xlim=c(0,5000),ylim=c(0,1),
ylab="Prob", lwd=4, main="Posterior Beta-Stacy Process vs Exact")
#mtext(paste("n=",n, ", k=",c))
legend(30,0.5,c("Exact", "posterior"), col = c(1,2),text.col = "black", 
lty = c(1, 2),lwd =c(2,2))
###################	
#    Parameters   # 
###################
c=1 # parameters:  c=k
epsilon=0.01
lambda <- c/epsilon
lambda  
rep=5 # distance size
d.prior=numeric(rep)
d.post=numeric(rep)

########################	
#    Prior Distance    # 
########################
for (L in 1:rep){

# Generating log-beta process via Algorithm A
# Step (b)
M <- rpois(1,lambda)
M

# Step (c)
#t <- rexp(M,mle)
t <- rweibull(M, k1 , k2 ) 
J.cont.prior=matrix(0,M,1)
# Step (d)
beta<-function(t){c*exp(-(t/k2)^k1)}# beta(t)


rweibull(M, k1 , k2 ) 


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

#
# cumulative sum of the weights: to compute the distance
#
J.Z.cum=cumsum(J.Z)
J.BS.cum=1-exp(-J.Z.cum)
theta.sort=c(theta.Z,Inf)
# Alogorim C: Steps (i)-(iii)
sum1.prior=0
sum2.prior=0

for (i in 1: length(J.Z.cum)){
sum1.prior=sum1.prior+J.BS.cum[i]*((pweibull(theta.sort [i+1],k1,k2))^2-
(pweibull(theta.sort [i],k1,k2))^2)
sum2.prior=sum2.prior+(J.BS.cum[i])^2*(pweibull(theta.sort [i+1],k1,k2)
-pweibull(theta.sort [i],k1,k2))
}
d.prior[L]=(1/3)-sum1.prior+sum2.prior
lines(theta.Z, J.BS.cum, xlab = NULL,type="s", col="black", lwd=2)

}

############################	
#    Postrior  Distance    # 
############################

for (L in 1:rep){

# Step 1.
# Generating log-beta process via Algorithm B

# Step (a)
########################
# Continuous: censored #
########################
M <- rpois(1,lambda)
M
#t <- rexp(M,mle)
t <- rweibull(M, k1 , k2 ) 
J.cont.post=matrix(0,M,1)
beta.post<-function(t){c*exp(-(t/k2)^k1)+sum(T >= t)}# beta(t)

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
tab=table(T1)
Nt=as.matrix(tab)
T2=unique(T1)
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

# Step 2.
#
# cumulative sum of the weights: to compute the distance
#
J.Z.cum=cumsum(J.Z)
J.BS.cum=1-exp(-J.Z.cum)
theta.sort=c(theta.Z,Inf)

# Alogorim C: Steps (iv)-(vi)
sum1.post=0
sum2.post=0
for (i in 1: length(J.Z.cum)){
sum1.post=sum1.post+J.BS.cum[i]*((pweibull(theta.sort [i+1],k1,k2))^2
-(pweibull(theta.sort [i],k1,k2))^2)
sum2.post=sum2.post+(J.BS.cum[i])^2*(pweibull(theta.sort [i+1],k1,k2)
-pweibull(theta.sort [i],k1,k2))
}
d.post[L]=(1/3)-sum1.post+sum2.post
lines(theta.Z, J.BS.cum, xlab = NULL,type="s", col="red", lwd=2)

}

d.prior=d.prior[d.prior>=0]
d.post=d.post[d.post>=0]


###plots of prior/postrior densities of distance###
#par(mfrow=c(2, 2))
#y_max=20
#d.prior1=density(d.prior)
#d.post1=density(d.post)
#plot(d.prior1, main="Density of distance",lwd = 2, xlim=c(0,3), 
#ylim=c(0,y_max)) 


#lines(d.post1,col = 2, lty = 2, lwd = 2)
#legend(3,y_max/2+1,c("prior", "posterior"), col = c(1,2),text.col = "black", 
#lty = c(1, 2),lwd =c(2,2))
#dev.off()

# Algorithm C (vii)
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
Hz1=(z1/k2)^k1
pvalue=LogRank1(z1,d1,Hz1)$ApproxPvalue2side
#cbind(d.prior,d.post)

###Results####
cbind(RBR,strength,pvalue)


# "norm", "lnorm", "exp", "weibull","logis", "llogis", "gompertz", etc

