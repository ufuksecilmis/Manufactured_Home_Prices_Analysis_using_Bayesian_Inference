# Read Table
House_Prices = read.csv("House_Prices.csv",header=T)
head(House_Prices)
str(House_Prices)

# Check if there is a missing value in each year
colSums(is.na(House_Prices))

# Fill out missing values with columns' means
for (i in 1:ncol(House_Prices)) {
  House_Prices[,i][is.na(House_Prices[,i])] = mean(House_Prices[,i],na.rm = T)
}


colSums(is.na(House_Prices))

Prices_2018 = House_Prices$Prices_2018
Prices_2019 = House_Prices$Prices_2019
Prices_2020 = House_Prices$Prices_2020
Prices_2021 = House_Prices$Prices_2021


#Checking distribution of data

par(mfrow= c(2,2))

hist(Prices_2018,probability = T,col = "blue",main = "Year 2018")
hist(Prices_2019,probability = T,col = "green",main = "Year 2019")
hist(Prices_2020,probability = T,col = "red",main = "Year 2020")
hist(Prices_2021,probability = T,col = "purple",main = "Year 2021")

# Data is right skewed. Log transformation will be applied.


# Log Transformation


log_Prices_2018 = log(Prices_2018)
log_Prices_2019 = log(Prices_2019)
log_Prices_2020 = log(Prices_2020)
log_Prices_2021 = log(Prices_2021)

mean(log_Prices_2018)
mean(log_Prices_2019)
mean(log_Prices_2020)
mean(log_Prices_2021)


# Checking the distribution of the data after log transformation

hist(log_Prices_2018,probability = T,col = "blue",main = "Year 2018")
hist(log_Prices_2019,probability = T,col = "green",main = "Year 2019")
hist(log_Prices_2020,probability = T,col = "red",main = "Year 2020")
hist(log_Prices_2021,probability = T,col = "purple",main = "Year 2021")


# Data is looked symmetric after log transformation


# 2018 - 2019

mean(log_Prices_2018)
# 11.30834
mean(log_Prices_2019)
# 11.3322



#Likelihood Function
like=function(th){
  mu1=th[1]; sig1=th[2]; mu2=th[3]; sig2=th[4]
  prod(dnorm(log_Prices_2018, mean=mu1,sd=sig1))*prod(dnorm(log_Prices_2019,mean=mu2,sd=sig2))
}

like=function(th){
  mu1=th[1]; sig1=th[2]; mu2=th[3]; sig2=th[4]
  sum(dnorm(log_Prices_2018, mean=mu1,sd=sig1))+sum(dnorm(log_Prices_2019,mean=mu2,sd=sig2))
}

like(th0)


#prior Distribution
Prior=function(th){
  mu1=th[1]; sig1=th[2]; mu2=th[3]; sig2=th[4]
  if (sig1<=0 | sig2<=0) return(0)
  dnorm(mu1,11.32,11.32)*dnorm(mu2,11.29,11.29)*dexp(sig1,rate=1/11.32)*dexp(sig2,rate=1/11.29)
}


#posterior
Posterior=function(th){Prior(th)*like(th)}



#starting
mu1=11.31; sig1=11.31; mu2=11.34; sig2=11.34
th0=c(mu1,sig1,mu2,sig2)
nit=100000
results=matrix(0,nrow=nit,ncol=4)
th=th0
results[1,]=th0
for (it in 2:nit){
  Cand=th + rnorm(4,sd=.03)
  ratio=Posterior(Cand)/Posterior(th)
  if (runif(1) < ratio) th=Cand
  results[it,]=th
}
edit(results)
par(mfrow=c(2,1))   
plot(results[,1])
plot(results[,2])
plot(results[,3])
plot(results[,4])

res=results[1e+03:1e+05,]
par(mfrow=c(2,1))
plot(res[,1])
plot(res[,2])
plot(res[,3])
plot(res[,4])
mu1s=results[,1]
sig1s=results[,2]
mu2s=results[,3]
sig2s=results[,4]
par(mfrow=c(2,1))
plot(mu1s-mu2s)
hist(mu1s-mu2s)
mean(mu1s-mu2s<0)
# [1] 0.6526816



# 2019 - 2020


mean(log_Prices_2019)
# 11.28928
mean(log_Prices_2020)
# 11.40545



#Likelihood Function
like=function(th){
  mu1=th[1]; sig1=th[2]; mu2=th[3]; sig2=th[4]
  prod(dnorm(log_Prices_2019, mean=mu1,sd=sig1))*prod(dnorm(log_Prices_2020,mean=mu2,sd=sig2))
}

#prior Distribution
Prior=function(th){
  mu1=th[1]; sig1=th[2]; mu2=th[3]; sig2=th[4]
  if (sig1<=0 | sig2<=0) return(0)
  dnorm(mu1,11.29,11.29)*dnorm(mu2,11.40,11.40)*dexp(sig1,rate=1/11.29)*dexp(sig2,rate=1/11.40)
}


#posterior
Posterior=function(th){Prior(th)*like(th)}



#starting
mu1=11.29; sig1=11.29; mu2=11.40; sig2=11.40
th0=c(mu1,sig1,mu2,sig2)
nit=100000
results=matrix(0,nrow=nit,ncol=4)
th=th0
results[1,]=th0
for (it in 2:nit){
  Cand=th + rnorm(4,sd=.03)
  ratio=Posterior(Cand)/Posterior(th)
  if (runif(1) < ratio) th=Cand
  results[it,]=th
}


edit(results)
par(mfrow=c(2,1))   
plot(results[,1])
plot(results[,2])
plot(results[,3])
plot(results[,4])

res=results[1e+04:1e+05,]
par(mfrow=c(2,1))
plot(res[,1])
plot(res[,2])
plot(res[,3])
plot(res[,4])
mu1s=res[,1]
sig1s=res[,2]
mu2s=res[,3]
sig2s=res[,4]
plot(mu1s-mu2s)
hist(mu1s-mu2s)
mean(mu1s-mu2s<0)
#[1] 0.9804891



# 2020 - 2021


mean(log_Prices_2020)
# [1] 11.40545
mean(log_Prices_2021)
# 11.52889



#Likelihood Function
like=function(th){
  mu1=th[1]; sig1=th[2]; mu2=th[3]; sig2=th[4]
  prod(dnorm(log_Prices_2020, mean=mu1,sd=sig1))*prod(dnorm(log_Prices_2021,mean=mu2,sd=sig2))
}

#prior Distribution
Prior=function(th){
  mu1=th[1]; sig1=th[2]; mu2=th[3]; sig2=th[4]
  if (sig1<=0 | sig2<=0) return(0)
  dnorm(mu1,11.40,11.40)*dnorm(mu2,11.53,11.53)*dexp(sig1,rate=1/11.40)*dexp(sig2,rate=1/11.53)
}


#posterior
Posterior=function(th){Prior(th)*like(th)}



#starting
mu1=11.40; sig1=11.40; mu2=11.53; sig2=11.53
th0=c(mu1,sig1,mu2,sig2)
nit=100000
results=matrix(0,nrow=nit,ncol=4)
th=th0
results[1,]=th0
for (it in 2:nit){
  Cand=th + rnorm(4,sd=.03)
  ratio=Posterior(Cand)/Posterior(th)
  if (runif(1) < ratio) th=Cand
  results[it,]=th
}

edit(results)
par(mfrow=c(2,1))   
plot(results[,1])
plot(results[,2])
plot(results[,3])
plot(results[,4])

res=results[1e+04:1e+05,]
plot(res[,1])
plot(res[,2])
plot(res[,3])
plot(res[,4])
mu1s=res[,1]
sig1s=res[,2]
mu2s=res[,3]
sig2s=res[,4]
plot(mu1s-mu2s)
hist(mu1s-mu2s)
mean(mu1s-mu2s<0)
# 0.9865224

