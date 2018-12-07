library(readxl);library(xts);library(forecast)
library(tseries); library(utils);library(urca)
library(xts);library(quantmod);library(stats);library(forecast) # XA3
library(fGarch) # JONAS GARCH EXAMPLE
#sti <- "C:/Users/X1/Desktop/FIN/XA4-40-Unit Roots/Quarterly.csv"
#"C:/User/X1/Desktop/FIN/DATA/SEU-INDEX-17oct18.xlsx"

## ------------- IMPORT ------------- ##
sti <- "C:/Users/X1/Desktop/FIN/Exam project/SEU-INDEX-18nov18.xlsx"
sheetNAME <- "INDEX DAILY"
dataTYPE <- c("date", "numeric", "numeric",
              "date", "numeric", "numeric",
              "date", "numeric", "numeric",
              "date", "numeric", "numeric",
              "date", "numeric", "numeric")
data <- read_excel(sti, sheet = sheetNAME, col_types = dataTYPE)
## --------------------------------- ##

## ------------- XTS Object ------------- ##
data.SPXy.xts  <- xts(data$`SPX-LastPrice`, data$`SPX-Date`)[2:3471]
data.SPXr.xts  <- xts(data$`SPX-LogReturn`, data$`SPX-Date`)
data.SPXr.xts  <- na.omit(data.SPXr.xts)
nSPX  <- length(data.SPXr.xts)
## --------------------------------- ##
#View(SEU_INDEX_17oct18) # INSEPCT DATA


# ---- SUMARY STATISTICS OF SPX (S&P500) ---- ##
summary(data.SPXr.xts)
summary(data.SPXy.xts)
nSPX
head(data.SPXr.xts,100)
plot(data.SPXr.xts)
## --------------------------------- ##

# -------------- Initial Inspect (Box Jenkins) ------------- ##
par(mfrow=c(3,1))
plot.ts(data.SPXr.xts,main="SPX",sub="",xlab="",ylab="Log-Return") # Plot 1
Acf(data.SPXr.xts,main="",ylab="ACF",xlab="",ylim=c(-0.1,0.1)) # Plot 1
Pacf(data.SPXr.xts,main="",ylab="PACF",xlab="",ylim=c(-0.1,0.1)) # Plot 1
par(mfrow=c(1,1))
## --------------------------------- ##

# -------------- ADF - Augmented Dickey Fuller Test (log return) ------------- ##
adf.test(data.SPXr.xts,k=4) # text
adf.test(data.SPXr.xts,k=8) # text
adf.test(data.SPXr.xts,k=12) # text
adf.test(data.SPXr.xts,k=16) # text
## CLEARLY NOT STATIONARY (p less than 0.05 for k=4 8 12 16)

# -------------- AIC/BIC TABLE - ARIMA SELECTIOn (p/q max 5) ------------- ##
x=matrix(data=NA, nrow=5+1, ncol=5+1); q = 0; x [[1,1]]= c("q") # Initialize
for (p in c(0:4)){ # Loop
  x [[1,p+2]] = p
  x [[p+2,1]]= paste( "ARIMA(",p,", q )")
  for (q in c(0:4)){
    fit <- Arima(data.SPXr.xts, order=c(p,0,q))
    x [[p+2,q+2]]= round(fit$bic,2)
  }
}
x # Output suggest p=3 q=3 (AIC) p=2 q=3 (BIC)
auto.arima(data.SPXr.xts,ic="bic",max.p = 4, max.q = 4,stepwise = FALSE) # see if the automated procedure choose the same model (see help for details about arguments)
## auto function suggest p=3 q=2 (AIC) p=2 q=0 (BIC)
## Choosing parsimonous: p=2 q=0 (AUTO BIC) --> Excel (Plot 2)
## ------------------------------------------- ##

# -------------- Ljung-Box test on residuals (ARIMA 2,1,0) ------------- ##
fit <- Arima(data.SPXr.xts, order=c(2,0,0))
fitres<-fit$residuals # residuals from estimated model

Box.test(fitres,lag=4,type="Ljung-Box",fitdf=1) # text
Box.test(fitres,lag=11,type="Ljung-Box",fitdf=1) # text
Box.test(fitres,lag=12,type="Ljung-Box",fitdf=1) # text
Box.test(fitres,lag=16,type="Ljung-Box",fitdf=1) # text
# Just barely stationary
## ------------------------------------------- ##

# -------------- ADF - AR(2)/GARCH(1,1) ------------- ##
garch0 <- garchFit(formula = data.SPXr.xts ~ garch(1,1)) 
garch1 <- garchFit(formula = data.SPXr.xts ~ arma(2,0) +garch(1,1)) 
garch2 <- garchFit(formula = data.SPXr.xts ~ arma(3,3) +garch(1,1)) 
garch3 <- garchFit(formula = data.SPXr.xts ~ arma(4,4) +garch(1,1)) 


plot(garch2)
summary(garch0)
summary(garch1)
summary(garch2) # BEST
summary(garch3)


fitres1 <- garch1@residuals # save residuals from your estimated model
fitres2 <- garch2@residuals # save residuals from your estimated model

plot.ts(fitres1)
plot.ts(fitres2)

fitaic1 <- garch1@fit$ics[1]
fitaic2 <- garch2@fit$ics[1]

par(mfrow=c(3,1)) # 650 x 650 for axis readability
Acf(fitres1)
Acf(fitres2)
Acf(fitres2) # (DELETE IN PLOT)
par(mfrow=c(1,1))

Box.test(fitres1,lag=4,type="Ljung-Box",fitdf=1) #perform Ljung-Box test on residuals
Box.test(fitres1,lag=11,type="Ljung-Box",fitdf=1) #perform Ljung-Box test on residuals
Box.test(fitres1,lag=12,type="Ljung-Box",fitdf=1) #perform Ljung-Box test on residuals
Box.test(fitres1,lag=16,type="Ljung-Box",fitdf=1) #perform Ljung-Box test on residuals
Box.test(fitres2^2,lag=4,type="Ljung-Box",fitdf=1) #perform Ljung-Box test on residuals
Box.test(fitres2^2,lag=11,type="Ljung-Box",fitdf=1) #perform Ljung-Box test on residuals
Box.test(fitres2^2,lag=12,type="Ljung-Box",fitdf=1) #perform Ljung-Box test on residuals
Box.test(fitres2^2,lag=16,type="Ljung-Box",fitdf=1) #perform Ljung-Box test on residuals
## ------------------------------------------- ##

# -------------- forecast out-of-sample ------------- ##
#fcast1=forecast(fit1,h=255)
#fcast2=forecast(fit2,h=255)
#fcast2=forecast(fit3,h=255)
#plot(fcast1) # CLEARLY A LAME IDEA
## ------------------------------------------- ##

# -------------- TEST OF CONCEPT ------------- ##
# train.data=data.SPXr.xts[1:(nrow(data.SPXr.xts)-20)]
# test.data=data.SPXr.xts[(nrow(data.SPXr.xts)-19):(nrow(data.SPXr.xts))]
# ARMA202=Arima(train.data,order=c(2,0,2))
# fit202=Arima(data.SPXr.xts,model=ARMA202)
# onestep202=fitted(fit202)
# onestep202=onestep202[(length(onestep202)-19):length(onestep202)]
# plot.zoo(cbind(onestep202,test.data),plot.type = "single", col = c("red", "blue"))
# #DIAGNOSTIC
# MSE202 <- mean((onestep202 - test.data)^2);MSE202*100
# fitres202<-ARMA202$residuals # save residuals from your estimated model
# Acf(fitres202)
# Pacf(fitres202)
# Box.test(fitres202,lag=4,type="Ljung-Box",fitdf=1) #perform Ljung-Box test on residuals
# Box.test(fitres202,lag=8,type="Ljung-Box",fitdf=1) #perform Ljung-Box test on residuals
# Box.test(fitres202,lag=12,type="Ljung-Box",fitdf=1) #perform Ljung-Box test on residuals
# Box.test(fitres202,lag=16,type="Ljung-Box",fitdf=1) #perform Ljung-Box test on residuals
# Pacf(fitres202^2)
# Acf(fitres202^2)
# Box.test(fitres202^2,lag=4,type="Ljung-Box",fitdf=1) #perform Ljung-Box test on residuals
# Box.test(fitres202^2,lag=8,type="Ljung-Box",fitdf=1) #perform Ljung-Box test on residuals
# Box.test(fitres202^2,lag=12,type="Ljung-Box",fitdf=1) #perform Ljung-Box test on residuals
# Box.test(fitres202^2,lag=16,type="Ljung-Box",fitdf=1) #perform Ljung-Box test on residuals
# Clearly not white noise
## ------------------------------------------- ##


# -------------- ARIMA FORECAST (JONAS forecast rolling 1000 steps) ------------- ##
r <- data.SPXr.xts
y <- data.SPXy.xts
model10 <- Arima(r, order = c(1,0,1))
model22 <- Arima(r, order = c(2,0,2))
model33 <- Arima(r, order = c(2,0,2))
forecast10 <- vector()
forecast22 <- vector()
forecast33 <- vector()
for (i in 1:1000){
  r.temp <- r[(1+i):(length(r)-1000+i)]
  model10 <- Arima(r.temp, order = c(1,0,1))
  model22 <- Arima(r.temp, order = c(2,0,2))
  model33 <- Arima(r.temp, order = c(3,0,3))
  forecast10[i] <- predict(model10,1)$pred
  forecast22[i] <- predict(model22,1)$pred
  forecast33[i] <- predict(model33,1)$pred
}
MFE10 <- mean((forecast10 - r[(length(r)-999):length(r)])^2)
MFE22 <- mean((forecast22 - r[(length(r)-999):length(r)])^2)
MFE33 <- mean((forecast33 - r[(length(r)-999):length(r)])^2)
MFE10*100;MFE22*100;MFE33*100
par(mfrow=c(3,1));
plot.zoo(cbind(r[(length(r)-999):length(r)],forecast10),plot.type = "single", col = c("gray80","black"), lwd = c(2,2),xlab="",ylab="",ylim=c(-0.02,0.02));
plot.zoo(cbind(r[(length(r)-999):length(r)],forecast22),plot.type = "single", col = c("gray80","black"), lwd = c(2,2),xlab="",ylab="",ylim=c(-0.02,0.02));
plot.zoo(cbind(r[(length(r)-999):length(r)],forecast33),plot.type = "single", col = c("gray80","black"), lwd = c(2,2),xlab="",ylab="",ylim=c(-0.02,0.02));
par(mfrow=c(1,1))
## ------------------------------------------- ##

# -------------- GARCH FORECAST (KEVIN forecast rolling 1000 steps) ------------- ##
r <- data.SPXr.xts
y <- data.SPXy.xts
fcastm <- vector()
fcastu <- vector()
fcastl <- vector()
for (i in 1:1000){
  r.temp <- r[(1+i):(length(r)-1000+i)]
  garch1 <- garchFit(formula = r.temp ~ arma(1,0)+garch(1,1), data=r.temp)
  fcastm[i] <- predict(garch1,1)$meanForecast
  fcastu[i] <- fcastm[i]+2*predict(garch1,1)$standardDeviation
  fcastl[i] <- fcastm[i]-2*predict(garch1,1)$standardDeviation
  }
par(mfrow=c(3,1));
plot.zoo(cbind(r[(length(r)-999):length(r)],fcastu,fcastl,fcastm),plot.type = "single", col = c("gray80","black","black","black"), lwd = c(2,2,2,1),xlab="",ylab="",ylim=c(-0.02,0.02));
plot.zoo(cbind(r[(length(r)-999):length(r)],fcastu,fcastl,fcastm),plot.type = "single", col = c("gray80","black","black","black"), lwd = c(2,2,2,1),xlab="",ylab="",ylim=c(-0.02,0.02));
plot.zoo(cbind(r[(length(r)-999):length(r)],fcastu,fcastl,fcastm),plot.type = "single", col = c("gray80","black","black","black"), lwd = c(2,2,2,1),xlab="",ylab="",ylim=c(-0.02,0.02));
par(mfrow=c(1,1))
## ------------------------------------------- ##

# -------------- GARCH OUTLIERS ------------- ##
rback <- r[(length(r)-999):length(r)]
length(rback[rback>fcastu])
length(rback[rback<fcastl])
21/1000
## ------------------------------------------- ##
