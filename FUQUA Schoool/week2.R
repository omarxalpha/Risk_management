install.packages("moments")
library(quantmod)
library(moments)

gold <-getSymbols("GOLDPMGBD228NLBM", src = "FRED", auto.assign = FALSE)
gold <- na.omit(gold)
gold <- gold["1979-12-31/2017-12-31"]
names(gold) <-"TR"
logret <- diff(log(gold$TR))[-1]

#Non-normaldistributions

##Skewness
rvec <- as.vector(logret)
round(skewness(rvec),2)

##Kurtosis
round(kurtosis(rvec),2)

## Jarque Test

rvec <- as.vector(logret)
JBGold <- jarque.test(rvec)

JBGold$statistic

# Student-t distribution
library(MASS)
rvec <- as.vector(logret)
t.fit <-fitdistr(rvec, "t")
round(t.fit$estimate, 6)

#Estimating VaRandEs for student-t
library(metRology)
alpha <- 0.05
set.seed(123789)
rvec <- rt.scaled(100000, mean=t.fit$estimate[1],
                  sd = t.fit$estimate[2],
                  df = t.fit$estimate[3])

VaR <- quantile(rvec, alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR, 6)
round(ES, 6)

#VaR and ES for multi-dayhprizon

#a)simulated from estimated student-t

alpha <- 0.05
set.seed(123789)
rvec <- rep(0,100000)
for (i in 1:10){
  rvec <- rvec+rt.scaled(100000,mean=t.fit$estimate[1],
                         sd = t.fit$estimate[2],
                         df = t.fit$estimate[3])
  }
VaR <-round(quantile(rvec, alpha),6)
ES <- round(mean(rvec[rvec<VaR]),6)

#b) IID simulation from empirical distribution 

alpha <- 0.05
set.seed(123789)
rvec<-rep(0,100000)

for (i in 1:10) {
  rvec = rvec+sample(as.vector(logret),
                     100000, replace = TRUE)
}
VaR2 = round(quantile(rvec, alpha),6)
ES2 <- round(mean(rvec[rvec<VaR2]),6)

#c) clock simulation fromempirical distribution 
alpha <- 0.05
set.seed(123789)
rvec = rep(0,100000)
rdat = as.vector(logret)
posn <- seq(from=1, to = length(rdat)-9, by = 1)
rpos = sample(posn, 100000, replace=TRUE)
for (i in 1:10) {
  rvec= rvec+rdat[rpos]  
  rpos <-rpos+1
}
VaR3 = round(quantile(rvec, alpha),6)
ES3 = round(mean(rvec[rvec<VaR3]),6)

Tablita <- as.data.frame(matrix(c('Sum of ten 1-day student-t',
                                  'Sum of ten 1-day IID logret',
                                  'Sum of ten consecutive 1-day log ret',
                        VaR,VaR2, VaR3,ES, ES2, ES3), ncol = 3))
names(Tablita) = c('Simulation method', 
                   "VaR","ES")

# Quiz 1
JPUS <-getSymbols("DEXJPUS", src = "FRED", auto.assign = FALSE)
JPUS <- na.omit(JPUS)
JPUS = 1/JPUS
JPUS <- JPUS["1979-12-31/2017-12-31"]
names(JPUS) <-"TR"
logret <- diff(log(JPUS$TR))[-1]

#Non-normaldistributions

##Skewness
rvec <- as.vector(logret)
round(skewness(rvec),2)

##Kurtosis
round(kurtosis(rvec),2)

## Jarque Test

rvec <- as.vector(logret)
JBGold <- jarque.test(rvec)

JBGold$statistic

# Student-t distribution
library(MASS)
rvec <- as.vector(logret)
t.fit <-fitdistr(rvec, "t")
round(t.fit$estimate, 6)

#Estimating VaRandEs for student-t
library(metRology)
alpha <- 0.01
set.seed(123789)
rvec <- rt.scaled(100000, mean=t.fit$estimate[1],
                  sd = t.fit$estimate[2],
                  df = t.fit$estimate[3])

VaR <- quantile(rvec, alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR, 6)
round(ES, 6)

#VaR and ES for multi-dayhprizon

#a)simulated from estimated student-t

alpha <- 0.01
set.seed(123789)
rvec <- rep(0,100000)
for (i in 1:10){
  rvec <- rvec+rt.scaled(100000,mean=t.fit$estimate[1],
                         sd = t.fit$estimate[2],
                         df = t.fit$estimate[3])
}
VaR <-round(quantile(rvec, alpha),6)
ES <- round(mean(rvec[rvec<VaR]),6)

#b) IID simulation from empirical distribution 

alpha <- 0.01
set.seed(123789)
rvec<-rep(0,100000)

for (i in 1:10) {
  rvec = rvec+sample(as.vector(logret),
                     100000, replace = TRUE)
}
VaR2 = round(quantile(rvec, alpha),6)
ES2 <- round(mean(rvec[rvec<VaR2]),6)

#c) clock simulation fromempirical distribution 
alpha <- 0.01
set.seed(123789)
rvec = rep(0,100000)
rdat = as.vector(logret)
posn <- seq(from=1, to = length(rdat)-9, by = 1)
rpos = sample(posn, 100000, replace=TRUE)
for (i in 1:10) {
  rvec= rvec+rdat[rpos]  
  rpos <-rpos+1
}
VaR3 = round(quantile(rvec, alpha),6)
ES3 = round(mean(rvec[rvec<VaR3]),6)

Tablita <- as.data.frame(matrix(c('Sum of ten 1-day student-t',
                                  'Sum of ten 1-day IID logret',
                                  'Sum of ten consecutive 1-day log ret',
                                  VaR,VaR2, VaR3,ES, ES2, ES3), ncol = 3))
names(Tablita) = c('Simulation method', 
                   "VaR","ES")
Tablita

# Quiz 2
JPUS <-getSymbols("DEXUSUK", src = "FRED", auto.assign = FALSE)
JPUS <- na.omit(JPUS)
JPUS <- JPUS["1979-12-31/2017-12-31"]
names(JPUS) <-"TR"
logret <- diff(log(JPUS$TR))[-1]

#Non-normaldistributions

##Skewness
rvec <- as.vector(logret)
round(skewness(rvec),2)

##Kurtosis
round(kurtosis(rvec),2)

## Jarque Test

rvec <- as.vector(logret)
JBGold <- jarque.test(rvec)

JBGold$statistic

# Student-t distribution
library(MASS)
rvec <- as.vector(logret)
t.fit <-fitdistr(rvec, "t")
round(t.fit$estimate, 6)

#Estimating VaRandEs for student-t
library(metRology)
alpha <- 0.01
set.seed(123789)
rvec <- rt.scaled(100000, mean=t.fit$estimate[1],
                  sd = t.fit$estimate[2],
                  df = t.fit$estimate[3])

VaR <- quantile(rvec, alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR, 6)
round(ES, 6)

#VaR and ES for multi-dayhprizon

#a)simulated from estimated student-t

alpha <- 0.01
set.seed(123789)
rvec <- rep(0,100000)
for (i in 1:10){
  rvec <- rvec+rt.scaled(100000,mean=t.fit$estimate[1],
                         sd = t.fit$estimate[2],
                         df = t.fit$estimate[3])
}
VaR <-round(quantile(rvec, alpha),6)
ES <- round(mean(rvec[rvec<VaR]),6)

#b) IID simulation from empirical distribution 

alpha <- 0.01
set.seed(123789)
rvec<-rep(0,100000)

for (i in 1:10) {
  rvec = rvec+sample(as.vector(logret),
                     100000, replace = TRUE)
}
VaR2 = round(quantile(rvec, alpha),6)
ES2 <- round(mean(rvec[rvec<VaR2]),6)

#c) clock simulation fromempirical distribution 
alpha <- 0.01
set.seed(123789)
rvec = rep(0,100000)
rdat = as.vector(logret)
posn <- seq(from=1, to = length(rdat)-9, by = 1)
rpos = sample(posn, 100000, replace=TRUE)
for (i in 1:10) {
  rvec= rvec+rdat[rpos]  
  rpos <-rpos+1
}
VaR3 = round(quantile(rvec, alpha),6)
ES3 = round(mean(rvec[rvec<VaR3]),6)

Tablita <- as.data.frame(matrix(c('Sum of ten 1-day student-t',
                                  'Sum of ten 1-day IID logret',
                                  'Sum of ten consecutive 1-day log ret',
                                  VaR,VaR2, VaR3,ES, ES2, ES3), ncol = 3))
names(Tablita) = c('Simulation method', 
                   "VaR","ES")
Tablita

# Quiz ·
JPUS <-getSymbols("DEXSZUS", src = "FRED", auto.assign = FALSE)
JPUS <- na.omit(JPUS)
JPUS = 1/JPUS
JPUS <- JPUS["1979-12-31/2017-12-31"]
names(JPUS) <-"TR"
logret <- diff(log(JPUS$TR))[-1]

#Non-normaldistributions

##Skewness
rvec <- as.vector(logret)
round(skewness(rvec),2)

##Kurtosis
round(kurtosis(rvec),2)

## Jarque Test

rvec <- as.vector(logret)
JBGold <- jarque.test(rvec)

JBGold$statistic

# Student-t distribution
library(MASS)
rvec <- as.vector(logret)
t.fit <-fitdistr(rvec, "t")
round(t.fit$estimate, 6)

#Estimating VaRandEs for student-t
library(metRology)
alpha <- 0.01
set.seed(123789)
rvec <- rt.scaled(100000, mean=t.fit$estimate[1],
                  sd = t.fit$estimate[2],
                  df = t.fit$estimate[3])

VaR <- quantile(rvec, alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR, 6)
round(ES, 6)

#VaR and ES for multi-dayhprizon

#a)simulated from estimated student-t

alpha <- 0.01
set.seed(123789)
rvec <- rep(0,100000)
for (i in 1:10){
  rvec <- rvec+rt.scaled(100000,mean=t.fit$estimate[1],
                         sd = t.fit$estimate[2],
                         df = t.fit$estimate[3])
}
VaR <-round(quantile(rvec, alpha),6)
ES <- round(mean(rvec[rvec<VaR]),6)

#b) IID simulation from empirical distribution 

alpha <- 0.01
set.seed(123789)
rvec<-rep(0,100000)

for (i in 1:10) {
  rvec = rvec+sample(as.vector(logret),
                     100000, replace = TRUE)
}
VaR2 = round(quantile(rvec, alpha),6)
ES2 <- round(mean(rvec[rvec<VaR2]),6)

#c) clock simulation fromempirical distribution 
alpha <- 0.01
set.seed(123789)
rvec = rep(0,100000)
rdat = as.vector(logret)
posn <- seq(from=1, to = length(rdat)-9, by = 1)
rpos = sample(posn, 100000, replace=TRUE)
for (i in 1:10) {
  rvec= rvec+rdat[rpos]  
  rpos <-rpos+1
}
VaR3 = round(quantile(rvec, alpha),6)
ES3 = round(mean(rvec[rvec<VaR3]),6)

Tablita <- as.data.frame(matrix(c('Sum of ten 1-day student-t',
                                  'Sum of ten 1-day IID logret',
                                  'Sum of ten consecutive 1-day log ret',
                                  VaR,VaR2, VaR3,ES, ES2, ES3), ncol = 3))
names(Tablita) = c('Simulation method', 
                   "VaR","ES")
Tablita

# Quiz 4
JPUS <-getSymbols("DEXUSAL", src = "FRED", auto.assign = FALSE)
JPUS <- na.omit(JPUS)
JPUS <- JPUS["1979-12-31/2017-12-31"]
logret <- diff(log(JPUS$TR))[-1]

#Non-normaldistributions

##Skewness
rvec <- as.vector(logret)
round(skewness(rvec),2)

##Kurtosis
round(kurtosis(rvec),2)

## Jarque Test

rvec <- as.vector(logret)
JBGold <- jarque.test(rvec)

JBGold$statistic

# Student-t distribution
library(MASS)
rvec <- as.vector(logret)
t.fit <-fitdistr(rvec, "t")
round(t.fit$estimate, 6)

#Estimating VaRandEs for student-t
library(metRology)
alpha <- 0.01
set.seed(123789)
rvec <- rt.scaled(100000, mean=t.fit$estimate[1],
                  sd = t.fit$estimate[2],
                  df = t.fit$estimate[3])

VaR <- quantile(rvec, alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR, 6)
round(ES, 6)

#VaR and ES for multi-dayhprizon

#a)simulated from estimated student-t

alpha <- 0.01
set.seed(123789)
rvec <- rep(0,100000)
for (i in 1:10){
  rvec <- rvec+rt.scaled(100000,mean=t.fit$estimate[1],
                         sd = t.fit$estimate[2],
                         df = t.fit$estimate[3])
}
VaR <-round(quantile(rvec, alpha),6)
ES <- round(mean(rvec[rvec<VaR]),6)

#b) IID simulation from empirical distribution 

alpha <- 0.01
set.seed(123789)
rvec<-rep(0,100000)

for (i in 1:10) {
  rvec = rvec+sample(as.vector(logret),
                     100000, replace = TRUE)
}
VaR2 = round(quantile(rvec, alpha),6)
ES2 <- round(mean(rvec[rvec<VaR2]),6)

#c) clock simulation fromempirical distribution 
alpha <- 0.01
set.seed(123789)
rvec = rep(0,100000)
rdat = as.vector(logret)
posn <- seq(from=1, to = length(rdat)-9, by = 1)
rpos = sample(posn, 100000, replace=TRUE)
for (i in 1:10) {
  rvec= rvec+rdat[rpos]  
  rpos <-rpos+1
}
VaR3 = round(quantile(rvec, alpha),6)
ES3 = round(mean(rvec[rvec<VaR3]),6)

Tablita <- as.data.frame(matrix(c('Sum of ten 1-day student-t',
                                  'Sum of ten 1-day IID logret',
                                  'Sum of ten consecutive 1-day log ret',
                                  VaR,VaR2, VaR3,ES, ES2, ES3), ncol = 3))
names(Tablita) = c('Simulation method', 
                   "VaR","ES")
Tablita

