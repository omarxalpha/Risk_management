install.packages("quantmod")

options(getSymbols.warning4.0 = FALSE)

library(quantmod)
wilsh <-getSymbols("WILL5000IND", src = "FRED", auto.assign = FALSE)
wilsh <- na.omit(wilsh)
wilsh <- wilsh["1979-12-31/2017-12-21"]
names(wilsh) <-"TR"

plot(log(wilsh))
head(wilsh, 3)
tail(wilsh, 3)

#Return 

logretW <- diff(log(wilsh$TR))[-1]
round(head(logret, 3),6)

plot(logret)

#Longer period return 
logret_w <-apply.weekly(logret,sum)
round(head(logret_w,3),6)

ret_W<-exp(logret_w)-1
ret_W

#Quiz2

USUK <-getSymbols("DEXUSUK", src = "FRED", auto.assign = FALSE)
USUK <- na.omit(USUK)
USUK <- USUK["1979-12-31/2017-12-21"]
names(USUK) <-"EXUSK"

plot(log(USUK))
head(USUK, 3)
tail(USUK, 3)

#Return 

lret_USUK <- diff(log(USUK$EXUSK^-1))[-1]
round(head(lret_USUK, 3),6)

plot(lret_USUK)

#Longer period return 
logret_w <-apply.weekly(lret_USUK,sum)
logret_m <-apply.monthly(lret_USUK,sum)
logret_q <-apply.quarterly(lret_USUK,sum)
logret_y <-apply.yearly(lret_USUK,sum)

ret_W<-exp(logret_w)-1
ret_m<-exp(logret_m)-1
ret_q<-exp(logret_q)-1
ret_y<-exp(logret_y)-1

round(last(ret_q,1),6)

      
round(last(ret_y,1),6)

#Quiz3


SZUS <-getSymbols("DEXSZUS", src = "FRED", auto.assign = FALSE)
SZUS <- na.omit(SZUS)
SZUS<- SZUS^-1
SZUS <- SZUS["1979-12-31/2017-12-31"]
names(SZUS) <-"EXUSSZ"

plot(log(SZUS))
head(head(SZUS, 3),6)
head(tail(SZUS, 3),6)

#Return 

lret_SZUS <- diff(log(SZUS$EXUSSZ))[-1]
round(head(lret_SZUS, 3),6)

plot(lret_SZUS)

#Longer period return 
logret_w <-apply.weekly(lret_SZUS,sum)
logret_m <-apply.monthly(lret_SZUS,sum)
logret_q <-apply.quarterly(lret_SZUS,sum)
logret_y <-apply.yearly(lret_SZUS,sum)

round(last(logret_q,3),6)

#Discrete return

ret_W<-exp(logret_w)-1
ret_m<-exp(logret_m)-1
ret_q<-exp(logret_q)-1
ret_y<-exp(logret_y)-1

round(head(ret_m,1),6)


round(last(ret_y,1),6)

#Quiz 4

USAL <-getSymbols("DEXUSAL", src = "FRED", auto.assign = FALSE)
USAL <- na.omit(USAL)
USAL <- USAL["1979-12-31/2017-12-31"]
names(USAL) <-"EXUSAL"

plot(log(USAL))
head(head(USAL, 3),6)
head(tail(USAL, 3),6)

#Return 

lret_USAL <- diff(log(USAL$EXUSAL))[-1]
round(head(lret_USAL, 3),6)

plot(lret_USAL)

#Longer period return 
logret_w <-apply.weekly(lret_USAL,sum)
logret_m <-apply.monthly(lret_USAL,sum)
logret_q <-apply.quarterly(lret_USAL,sum)
logret_y <-apply.yearly(lret_USAL,sum)

round(last(logret_q,3),6)

#Discrete return

ret_W<-exp(logret_w)-1
ret_m<-exp(logret_m)-1
ret_q<-exp(logret_q)-1
ret_y<-exp(logret_y)-1

round(head(ret_m,1),6)


round(last(ret_y,1),6)
