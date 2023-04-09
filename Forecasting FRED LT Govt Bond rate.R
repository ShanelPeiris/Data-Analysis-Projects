setwd("C:/Users/shane/OneDrive/UC Work files/Time series econometrics/Assignments/Homework 3")
#Clear Console and environment
rm(list=ls())
cat("\014")
if(!is.null(dev.list())) dev.off()

library(tseries)
library(urca)
library(quantmod)
library(dynlm)
library(forecast)
source(file="intord.R")

# FRED - import data
library(fredr)
fredr_set_key("9039c22c956495f44981b011031f7759")

df <- fredr(
  series_id = "IRLTLT01USQ156N",
  observation_start = as.Date("1960-01-01"),
  observation_end = as.Date("2022-10-01"),
  frequency = "q" # Quarterly
)

colnames(df)[3] <- "Rate"

dfts <- ts(df, start=c(1960,1), frequency=4)
plot.ts(dfts)
bondrte <- dfts[,3] 
c_bondrte <- window(bondrte,end=c(2020,4)) # removing 2 years (8 obs)
plot.ts(bondrte, main = "Long-Term Government Bond Yields: 10-year")
abline(v=2021)
plot.ts(c_bondrte)


#Plot Margins
par(mar = c(1, 1, 1, 1))
intord(c_bondrte)
# I1 as per Intord according to ADF test. SD drop suggests same.
# Also note a relatively slow dacay in the ACF

#ADF test with drift and trend
df0t=ur.df(c_bondrte,type="trend",selectlags="BIC")
summary(df0t)
# Fail to reject Tau3, this indicates Unit root is present
# No Drift
# No Trend

### Try De-trend and de-seasonalize to see what happens?
dummies = seasonaldummy(c_bondrte)
res1 = dynlm(c_bondrte~trend(c_bondrte)+dummies+L(c_bondrte,4)+L(c_bondrte,8)+L(c_bondrte,12))
summary(res1)

intord(res1$residuals)
if(!is.null(dev.list())) dev.off()
par(mfrow=c(2,2))
Acf(res1$residuals)
Pacf(res1$residuals)

###################### Differencing #################################
# Look at the ACF/PACF of level, referenced and seasonal differenced
if(!is.null(dev.list())) dev.off()
par(mfrow=c(2,2))
Acf(c_bondrte)
Pacf(c_bondrte) 
# Very slow seasonal decay noted on ACF, with 2 spikes on PACF
# Indicates an AR(2) process

diff_c_bondrte = diff(c_bondrte) 
Acf(diff_c_bondrte)
Pacf(diff_c_bondrte)
# After First Difference, no seasonal spikes are noted
# There are noticable spikes upto lag 18

sd_diff_c_bondrte = diff(diff_c_bondrte,4) # difference with previous year (solve unit root problem on seasonal lags)
Acf(sd_diff_c_bondrte)
Pacf(sd_diff_c_bondrte)

######################## Creating Trend and seasonal dummies ##################

#create a trend
h = 8 #(2 years ahead)
n = length(c_bondrte)
nh = n + h

freq = 4 # quarterly data
tr=(1:n)/freq # in sample trend of size n
trt = (1:nh)/freq # trend for entire sample including forcasting
trf = trt[(n+1):nh] # trend for forecasting period

# create seasonal dummies for entire sample nh
sdum = seasonaldummy(ts(rep(0,n),start=c(1960,1),frequency=4)) # in sample seasonal dummies
sdumt = seasonaldummy(ts(rep(0,nh),start=c(1960,1),frequency=4)) # in sample plus forecasting
sdumf = sdumt[(n+1):nh,] # seasonal dummies for forecasting period



#################### Estimate ARIMA model ####################
arma1 = arima(c_bondrte,order=c(18,1,18),seasonal=c(0,0,0))
arma1
Acf(arma1$residuals)
Pacf(arma1$residuals)
BIC(arma1)
# None of the estimates are significant
# AIC is 316 and BIC is 445
# No Serial correlation in the residuals
# Going to reduce lags and retry

arma2 = arima(c_bondrte,order=c(3,1,3),seasonal=c(0,0,0))
arma2
Acf(arma2$residuals)
Pacf(arma2$residuals)
BIC(arma2)
# AR lag 3 and MA3 not significant
# AIC is 295 and BIC is 319
# no serial correlation in the residuals
# going to Reduce lags further and retry

arma3 = arima(c_bondrte,order=c(2,1,2),seasonal=c(0,0,0))
arma3
Acf(arma3$residuals)
Pacf(arma3$residuals)
BIC(arma3)
# AR3 and MA3 not significant
# AIC is 301 and BIC is 319
# no serial correlation in the residuals
# Going to try reducing lags furthur and also incorporate seasonal lags

arma4 = arima(c_bondrte,order=c(1,1,1),seasonal=c(2,1,1))
arma4
Acf(arma4$residuals)
Pacf(arma4$residuals)
BIC(arma4)
# AR, SAR(1) and SAR(2) are not significant
# AR3 and MA3 not significant
# AIC is 297 and BIC is 324
# Some serial correlation in the residuals
# Seems seasonal lags are not suitable/significant

arma5 = arima(c_bondrte,order=c(1,1,1),seasonal=c(0,0,0))
arma5
Acf(arma5$residuals)
Pacf(arma5$residuals)
BIC(arma5)
# Coefficients are significant
# AIC is 303 and BIC is 313
# There is Serial correlation in the residuals
# Seems to be a good model, will try including seasonal dummies and Trend

#  Best Model but adding Trend and seasonal dummies
xx=cbind(sdum,tr)
armabest = arima(c_bondrte,order=c(2,1,2),seasonal=c(0,0,0),xreg=xx)
armabest
Acf(armabest$residuals)
Pacf(armabest$residuals)
BIC(armabest)
# Trend and Seasonal dummies not significant
# AIC is 306 and BIC is 337
# There is serial correlation in the residuals
# Prev model was better

#  Best Model
armabest = arima(c_bondrte,order=c(2,1,2),seasonal=c(0,0,0))
armabest
Acf(armabest$residuals)
Pacf(armabest$residuals)
BIC(armabest)
# AIC is 301 and BIC is 319
# No serial correlation in the residuals


# Check Auto ARIMA
res <- auto.arima(c_bondrte)
res
Acf(res$residuals)
Pacf(res$residuals)
# Auto Arima AIC is 302 and BIC is 309
# Auto Arima model has Serial correlation in the error term


# diagnostics for residuals - Best Model
par(mar = c(1, 1, 1, 1))
tsdiag(armabest)
# BoX-Ljung Q Statistic
b = Box.test(armabest$residuals,lag = 20, type="Ljung-Box")
b

blt = rep(0,20)
for (i in 1:20){
  b = Box.test(armabest$residuals,lag = i, type="Ljung-Box")
  blt[i]=b$p.value
}
blt
# No Serial correlation detected upto 20 lags

# Out-of-sample forecasting h-steps ahead (dynamic)
# Forecasting using Best Model

if(!is.null(dev.list())) dev.off()
par(mfrow=c(2,2))
fcastbest = predict(armabest,n.ahead=8)
ts.plot(c_bondrte,fcastbest$pred,col=1:2,main="Forecasts" )
ts.plot(bondrte,fcastbest$pred,col=1:2, main="Forecasts vs Actual ")
abline(v=2021)
accuracy(armabest)

yf = fcastbest$pred # forecast values
yfse = fcastbest$se # standard errors of forecasts
#lsales <- (log(dfts))   # household goods and services exp., UK
true <- window(bondrte,start=2021) # Creating the Test dataset
u95 = yf + 2*yfse
l95 = yf - 2*yfse
ff = cbind(yf,u95,l95)
matplot(ff,type='l',col=c(1,3,3),lty=c(1,3,3),main="Forecasts from ARIMA()")
legend("topleft",legend=c("Forecast","upper .95 PI","lower .95 PI"),col=c(1,3,3),lty=c(1,3,3),lwd=1,bty="n",cex=1.1)

fft = cbind(c(c_bondrte,u95),c(c_bondrte,l95),c(c_bondrte,yf),c(c_bondrte,true))
st = length(fft[,1])-20
matplot(fft[st:nh,],type='l',col=c(3,3,2,1),lwd=c(1.5,1.5,3,2),lty=c(3,3,1,1),main="forecasts from ARIMA")
legend("topleft",legend=c("upper .95 PI","lower .95 PI","Forecast","Actual"),col=c(3,3,2,1),lty=1,lwd=c(1.5,1.5,3,2),bty="n",cex=1.1)

ts.plot(true,fcastbest$pred,col=c("black","red"),lty=1:2, main = "Actual vs Forecasted Plot") 
#legend("top",legend=c("Actual","Forecast"),lty=1:2, ncol=2, cex=0.5)
MSE <- mean((true - fcastbest$pred)^2)
MAE <- mean(abs((true - fcastbest$pred)))
MAPE <- 100*mean(abs((true - fcastbest$pred)/true))
cat(" The Mean Square Error of the Model is", MSE)
cat("\n The Mean Absolute Error of the Model is", MAE)
cat("\n The Mean Absolute Percentage Error of the Model is", MAPE)

