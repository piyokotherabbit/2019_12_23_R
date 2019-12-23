install.packages("quantmod")
library(quantmod)
library(forecast)
library(tseries)

strtdate <- "2014-09-04"

#fxrates
EUR <- getSymbols("EURUSD=X", from = strtdate, to = Sys.Date())
GBP <- getSymbols("GBPUSD=X", from = strtdate, to = Sys.Date())
CHF <- getSymbols("USDCHF=X", from = strtdate, to = Sys.Date())
EGP <- getSymbols("EURGBP=X", from = strtdate, to = Sys.Date())

#stocks
HSBC <- getSymbols("HSBA.L", from = strtdate, to = Sys.Date())
BNPP <- getSymbols("BNP.PA", from = strtdate , to = Sys.Date())
DB <- getSymbols("DBK.DE", from = strtdate , to = Sys.Date())
CAGC <- getSymbols("ACA.PA", from = strtdate , to = Sys.Date())
SNT <- getSymbols("SAN.MC", from = strtdate , to = Sys.Date())
BARC <- getSymbols("BARC.L", from = strtdate , to = Sys.Date())
SGN <- getSymbols("GLE.PA", from = strtdate , to = Sys.Date())
LYG <- getSymbols("LLOY.L", from = strtdate , to = Sys.Date())
ING <- getSymbols("INGA.AS", from = strtdate , to = Sys.Date())

#indices
FTSE100 <- getSymbols("^FTSE", from = strtdate, to = Sys.Date())
CAC40 <- getSymbols("^FCHI", from = strtdate, to = Sys.Date())
IBEX35 <- getSymbols("^IBEX", from = strtdate, to=Sys.Date())
DAXidx <- getSymbols("^GDAXI", from = strtdate, to = Sys.Date())
AEXidx <- getSymbols("^AEX", from = strtdate, to = Sys.Date())

#bonds
UKcorp <- getSymbols("SLXX.L", from = strtdate, to = Sys.Date())
EUROcorp <- getSymbols("IEAC.L", from = strtdate, to = Sys.Date())

#forecastFX
EURforecast <- auto.arima(`EURUSD=X`$`EURUSD=X.Close`, trace = T)
GBPforecast <- auto.arima(`GBPUSD=X`$`GBPUSD=X.Close`, trace = T)
CHFforecast <- auto.arima(`USDCHF=X`$`USDCHF=X.Close`, trace = T)
EGPforecast <- auto.arima(`EURGBP=X`$`EURGBP=X.Close`, trace = T)

#forecastBanks
HSBAforecast <- auto.arima(HSBA.L$HSBA.L.Close, trace = T)
BNPPforecast <- auto.arima(BNP.PA$BNP.PA.Close, trace = T)
DBforecast <- auto.arima(DBK.DE$DBK.DE.Close, trace = T)
CAGCforecast <- auto.arima(ACA.PA$ACA.PA.Close, trace = T)
SNTforecast <- auto.arima(SAN.MC$SAN.MC.Close, trace = T)
BARCforecast <- auto.arima(BARC.L$BARC.L.Close, trace = T)
SGNforecast <- auto.arima(GLE.PA$GLE.PA.Close, trace = T)
LYGforecast <- auto.arima(LLOY.L$LLOY.L.Close, trace = T)
INGforecast <- auto.arima(INGA.AS$INGA.AS.Close, trace = T)

#forecastIndices
FTSEforecast <- auto.arima(FTSE$FTSE.Close, trace = T)
CAC40forecast <- auto.arima(FCHI$FCHI.Close, trace = T)
IBEX35forecast <- auto.arima(IBEX$IBEX.Close, trace = T)
DAXforecast <- auto.arima(GDAXI$GDAXI.Close, trace = T)
AEXforecast <- auto.arima(AEX$AEX.Close, trace = T)

#forecastBonds
UKcorpforecast <- auto.arima(SLXX.L$SLXX.L.Close, trace = T)
EUROcorpforecast <- auto.arima(IEAC.L$IEAC.L.Close, trace = T)

#FXplotinone
par(mfrow=c(2,2))
plot(forecast(EURforecast,c(50.95),h=360), main = "EUR/USD")
plot(forecast(GBPforecast,c(50.95),h=360), main = "GBP/USD")
plot(forecast(CHFforecast,c(50.95),h=360), main = "USD/CHF")
plot(forecast(EGPforecast,c(50.95),h=360), main = "EUR/GBP")

#BANKSplotinone
par(mfrow=c(3,3))
plot(forecast(HSBAforecast,c(50.95),h=360), main = "HSBC(UK)")
plot(forecast(BNPPforecast,c(50.95),h=360), main = "BNP Paribas(France)")
plot(forecast(DBforecast,c(50.95),h=360), main = "Deutshe Bank(Germany)")
plot(forecast(CAGCforecast,c(50.95),h=360), main = "Credit Agrecole(France)")
plot(forecast(SNTforecast,c(50.95),h=360), main = "Santendar(Spain)")
plot(forecast(BARCforecast,c(50.95),h=360), main = "Barclays(UK)")
plot(forecast(SGNforecast,c(50.95),h=360), main = "Societe General(France)")
plot(forecast(LYGforecast,c(50.95),h=360), main = "Lloyds Banking Group(UK)")
plot(forecast(INGforecast,c(50.95),h=360), main = "ING Groupe(Netherlands)")

#BONDSplotinone
par(mfrow=c(1,2))
plot(forecast(UKcorpforecast,c(50,95),h=360),main = "UK Corporate Bond ETF")
plot(forecast(EUROcorpforecast,c(50,95),h=360), main = "Eurozone Corporate Bond ETF")

#INDICESplotinone
par(mfrow=c(3,2))
plot(forecast(FTSEforecast,c(50,95),h=360),main = "FTSE100(UK)")
plot(forecast(CAC40forecast,c(50,95),h=360), main = "CAC40(France)")
plot(forecast(IBEX35forecast,c(50,95),h=360),main = "IBEX35(Spain)")
plot(forecast(DAXforecast,c(50,95),h=360), main = "DAX(Germany)")
plot(forecast(AEXforecast,c(50,95),h=360), main = "AEX(Netherlands)")

#Bondsprices
par(mfrow=c(1,2))
chart_Series(SLXX.L)
chart_Series(IEAC.L)