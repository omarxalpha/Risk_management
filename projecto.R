#install.packages("dynlm")
#install.packages("moments")
#install.packages(broom")
#install.packages("FinTS")
library(ggplot2)
library(quantmod)
library(moments)
library(dynlm)
library(broom)
library(FinTS)
AMZN <-getSymbols("AMZN", src = "yahoo", auto.assign = FALSE)

AMZN <- na.omit(AMZN)

plot(AMZN$AMZN.Adjusted)

logret <- ts(diff(log(AMZN$AMZN.Adjusted))[-1])

plot(logret)

hist(logret, breaks = 30)
#Pruebas de normalidad

##Kurtosis
round(kurtosis(vector_ret),2)
##Sesgo
round(skewness(vector_ret),2)
##Prueba de normalidad 
jarque.test(vector_ret)

plot(logret)

acf(logret)
acf(logret^2)

logret_random <- sample(as.vector(logret),size =  length(logret), replace = FALSE)

acf(logret_random^2)

logret_mean = dynlm(logret~1)

summary(logret_mean)

ehatsq = ts(resid(logret_mean)^2)

ARCH_m = dynlm(ehatsq~L(ehatsq))

summary(ARCH_m)

plot(ARCH_m$residuals)

media = mean(logret)
ds = sd(logret)

dnormal <- rnorm(length(logret), mean = media, sd = ds)

round(kurtosis(dnormal),2)

qplot(logret, geom = "density")+
  geom_density(aes(dnormal),stat = 'density', col='red')


vector_ret <- as.vector(logret)






T_capital <- nobs(logret_mean)
q <- length(coef(ARCH_m))-1
Rsq <- glance(ARCH_m)[[1]]
LM <- (T_capital-q)*Rsq
alpha <- 0.05
Chicr <- qchisq(1-alpha, q)
Chicr
LM


ArchTest(logret, lags = 1, demean = TRUE)
