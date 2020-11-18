BMX <-getSymbols("BOLSAA.MX", src = "yahoo", auto.assign = FALSE)

BMX <- na.omit(BMX)

plot(BMX$BOLSAA.MX.Adjusted)

logret <- diff(log(BMX$BOLSAA.MX.Adjusted))[-1]

plot(logret)

acf(logret)
acf(abs(logret))
