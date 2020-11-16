#Distribution of returns (aka "bell curve")
library(quantmod)
library(scales)

options(getSymbols.warning4.0 = FALSE)

gold <-getSymbols("GOLDPMGBD228NLBM", src = "FRED", auto.assign = FALSE)
gold <- na.omit(gold)
gold <- gold["1979-12-31/2017-12-31"]
names(gold) <-"TR"
logret <- diff(log(gold$TR))[-1]

##Estimate mu and sigma 

mu<-round(mean(logret),8)
sigma<-round(sd(logret),8)

#VaR (Value at Risk)

VaRisk = round(qnorm(0.05,mu,sigma),6)
round(1000*(exp(VaRisk)-1),6)

#Expected Shortfall
ES <-round(mu-sigma*dnorm(qnorm(0.05,0,1),0,1)/0.05,6)
round(1000*(exp(ES)-1),1)

#Using simulation to calculate VaR and ES
alpha <-0.05
set.seed(123789)
rvec <- rnorm(100000, mu, sigma)
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)

## With replacement 

alpha <-0.05
set.seed(123789)
rvec <- sample(as.vector(logret),100000, replace=TRUE)
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)

#Quiz1

USJP <-getSymbols("DEXJPUS", src = "FRED", auto.assign = FALSE)
USJP <- na.omit(gold)
USJP <- 1/USJP
USJP <- USJP["1979-12-31/2017-12-31"]
names(USJP) <-"TR"
logret_USJP <- diff(log(USJP$TR))[-1]

##Estimate mu and sigma 

mu<-round(mean(logret_USJP),6)
sigma<-round(sd(logret_USJP),6)

#VaR (Value at Risk)

VaRisk = round(qnorm(0.05,mu,sigma),6)
round(1000*(exp(VaRisk)-1),2)

#Expected Shortfall
ES <-round(mu-sigma*dnorm(qnorm(0.01,0,1),0,1)/0.01,6)
round(1000*(exp(ES)-1),2)


#Using simulation to calculate VaR and ES
alpha <-0.01
RNGkind(sample.kind=”Rounding”)
set.seed(123789)
rvec <- rnorm(100000, mu, sigma)
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)

## With replacement 

alpha <-0.01
set.seed(123789)
rvec <- sample(as.vector(logret),100000, replace=TRUE)
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)

#Quiz2

USJP <-getSymbols("DEXJPUS", src = "FRED", auto.assign = FALSE)
USJP <- na.omit(USJP)
USJP <- 1/USJP
USJP <- USJP["1979-12-31/2017-12-31"]
names(USJP) <-"TR"
logret_USJP <- diff(log(USJP$TR))[-1]

##Estimate mu and sigma 

mu<-round(mean(logret_USJP),6)
sigma<-round(sd(logret_USJP),6)

#VaR (Value at Risk)

VaRisk = round(qnorm(0.01,mu,sigma),6)
round(1000*(exp(VaRisk)-1),2)

#Expected Shortfall
ES <-round(mu-sigma*dnorm(qnorm(0.01,0,1),0,1)/0.01,6)
round(1000*(exp(ES)-1),2)


#Using simulation to calculate VaR and ES
alpha <-0.01
RNGkind(sample.kind=”Rounding”)
set.seed(123789)
rvec <- rnorm(100000, mu, sigma)
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)

## With replacement 

alpha <-0.01
set.seed(123789)
rvec <- sample(as.vector(logret),100000, replace=TRUE)
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)

#Quiz3

SZUS <-getSymbols("DEXSZUS", src = "FRED", auto.assign = FALSE)
SZUS <- na.omit(SZUS)
SZUS <- 1/SZUS
SZUS <- SZUS["1979-12-31/2017-12-31"]
names(SZUS) <-"TR"
logret_SZUS <- diff(log(SZUS$TR))[-1]

##Estimate mu and sigma 

mu<-round(mean(logret_SZUS),6)
sigma<-round(sd(logret_SZUS),6)

#VaR (Value at Risk)

VaRisk = round(qnorm(0.01,mu,sigma),6)
round(1000*(exp(VaRisk)-1),2)

#Expected Shortfall
ES <-round(mu-sigma*dnorm(qnorm(0.01,0,1),0,1)/0.01,6)
round(1000*(exp(ES)-1),2)


#Using simulation to calculate VaR and ES
alpha <-0.01
RNGkind(sample.kind=”Rounding”)
set.seed(123789)
rvec <- rnorm(100000, mu, sigma)
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)

## With replacement 

alpha <-0.01
set.seed(123789)
rvec <- sample(as.vector(logret_SZUS),100000, replace=TRUE)
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)

#Quiz4

USAL <-getSymbols("DEXUSAL", src = "FRED", auto.assign = FALSE)
USAL <- na.omit(USAL)
USAL <- USAL["1979-12-31/2017-12-31"]
names(USAL) <-"TR"
logret_USAL <- diff(log(USAL$TR))[-1]

##Estimate mu and sigma 

mu<-round(mean(logret_USAL),6)
sigma<-round(sd(logret_USAL),6)

#VaR (Value at Risk)

VaRisk = round(qnorm(0.01,mu,sigma),6)
round(1000*(exp(VaRisk)-1),2)

#Expected Shortfall
ES <-round(mu-sigma*dnorm(qnorm(0.01,0,1),0,1)/0.01,6)
round(1000*(exp(ES)-1),2)


#Using simulation to calculate VaR and ES
alpha <-0.01
RNGkind(sample.kind=”Rounding”)
set.seed(123789)
rvec <- rnorm(100000, mu, sigma)
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)

## With replacement 

alpha <-0.01
set.seed(123789)
rvec <- sample(as.vector(logret_USAL),100000, replace=TRUE)
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)

