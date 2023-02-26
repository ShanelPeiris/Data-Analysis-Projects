#Clear Console and environment
rm(list=ls())
cat("\014")
#Scientific Notation
options("scipen"=99999, digits=3)

#setwd("C:/Users/shane/OneDrive/UC Work files/Time series econometrics/Assignments/Homework 2")

#install.packages("pdfetch")
#install.packages("fredr")
# install.packages("rlang")
library(pdfetch)
library(fredr)
library(tidyverse)
library(quantmod)

### Import data directly from FRED ###
fredr_set_key("9039c22c956495f44981b011031f7759")
# https://fred.stlouisfed.org/series/ROWFDNQ027S#0
# https://fred.stlouisfed.org/series/DFF#0
# https://fred.stlouisfed.org/series/CPILFENS
# https://fred.stlouisfed.org/series/UNRATENSA

fdi <- fredr(
  series_id = "ROWFDNQ027S",
  observation_start = as.Date("1957-01-01"),
  observation_end = as.Date("2022-07-01"),
  frequency = "q" # quarterly
)

fed_ER <- fredr(
  series_id = "DFF",
  observation_start = as.Date("1957-01-01"),
  observation_end = as.Date("2022-07-01"),
  frequency = "q" # quarterly
)

cpi <- fredr(
  series_id = "CPILFENS",
  observation_start = as.Date("1957-01-01"),
  observation_end = as.Date("2022-07-01"),
  frequency = "q" # quarterly
)

unemp <- fredr(
  series_id = "UNRATENSA",
  observation_start = as.Date("1957-01-01"),
  observation_end = as.Date("2022-07-01"),
  frequency = "q" # quarterly
)


#### Cleaning, remaining and merging data ###

colnames(cpi) <- c("date","sereis_id","cpi","cpi_realtime_start","cpi_realtime_end","inflation")
colnames(fdi) <- c("date","sereis_id","fdi","fdi_realtime_start","fdi_realtime_end","fdi_change")
colnames(fed_ER) <- c("date","sereis_id","int_rte","int_rte_realtime_start","int_rte_realtime_end")
colnames(unemp) <- c("date","sereis_id","unemp","unemp_realtime_start","unemp_realtime_end")

# Merge
df <- full_join(cpi,fdi,by="date")
df <- full_join(df,fed_ER,by="date")
df <- full_join(df,unemp,by="date")

df$inf <- Delt((df$cpi)) 
df$fdi_pctchg <- Delt((df$fdi)) 
df <- drop_na(df)
df <- select(df,date,cpi,fdi,int_rte,unemp,inf,fdi_pctchg)

# Define dataset as Time Series
# Df dataset starts from 1957 jan and is quarterly hence
dfts <- ts(df, frequency=4, start= c(1957,4)) 


#### Q2.a - Determine the order of integration of each variable.Report your findings.
source(file="intord.R")

#Plot Margins
par(mar = c(1, 1, 1, 1))

intord(df$inf) # I1 process
intord(df$fdi_pctchg) # I0 process
intord(df$int_rte) # I1 process
intord(df$unemp) # I0 process  but suggest to take first difference

# Modeling inflation by regressing interest rate and unemployment
# fdi percentage change variable dropped from analysis

inf <- dfts[,6]
fdi_pctchg <- dfts[,7]
int_rte <- dfts[,4]
unemp <- dfts[,5]

dinf <- diff(inf)
dint_rte <- diff(int_rte)
dunemp <- diff(unemp)

## Results of urdf test for unemp suggest I take first difference for unemployment as well.
# Test shown below.

plot.ts(dinf)
plot.ts(dint_rte)
plot.ts(unemp)
plot.ts(dunemp)

library(urca)
####### URDF Test for Unemployment ######
#ADF test with no trend and no drift
df0n=ur.df(unemp,type="none",selectlags="BIC")
summary(df0n)
# tau1 -  we have a Unit root

#ADF test with drift no trend  -- same as intord
df0d=ur.df(unemp,type="drift",selectlags="BIC")
summary(df0d)
# tau2 - Have no unit root
# phi1 - Have a drift

#ADF test with drift and trend
df0t=ur.df(unemp,type="trend",selectlags="BIC")
summary(df0t)
# tau3 - Have  unit root
# Phi3 - at 5%, we fail to reject
# Phi2 - Have no Trend at 1% but at 5% have a Trend
###################################################

# Add dummy variables
library(forecast)
seas = seasonaldummy(dinf) # set dummy for the shortest time series

library(dynlm)
r1 <- dynlm(dinf~L(dinf,1:12)+L(dint_rte,0:12)+L(dunemp,0:12)+seas)
summary(r1)

# 11 is the highest lag
r2 <- dynlm(dinf~L(dinf,1:11)+L(dint_rte,0:11)+L(dunemp,0:11)+seas)
summary(r2)

# Base model - Start = 1960.4
rbase <- dynlm(dinf~L(dinf,1:11)+L(dint_rte,0:11)+L(dunemp,0:11)+seas, start=c(1960,4))
summary(rbase)
AIC(rbase)
BIC(rbase)

# dropping insignificant lags for  int_rte and unemp
r4 <- dynlm(dinf~L(dinf,1:11)+L(dint_rte,0:6)+L(dunemp,4)+L(dunemp,4)+seas, start=c(1960,4))
summary(r4)
AIC(r4)
BIC(r4)

# Best Model
# dropping insignificant lags for  int_rte and unemp
rfinal <- dynlm(dinf~L(dinf,1:11)+L(dint_rte,0:6)+L(dunemp,0)+L(dunemp,4)+seas, start=c(1960,4))
summary(rfinal)
AIC(rfinal)
BIC(rfinal)

#################### part C #####################
# Test if Seasonal dummies have effect on FF rate

#constraint model:
res5 = dynlm(dinf~L(dinf,1:11)+L(dint_rte,0:6)+ L(dunemp,0)+L(dunemp,4), start=c(1960,4))
summary(res5)
AIC(res5)
BIC(res5)
# anova test (F test)
anova(res5,rfinal,test="F")

# New Best Model
# dropping insignificant lags for  int_rte and unemp
rfinal_2 <- dynlm(dinf~L(dinf,1:11)+L(dint_rte,0:6)+L(dunemp,0)+L(dunemp,4), start=c(1960,4))
summary(rfinal_2)
AIC(rfinal_2)
BIC(rfinal_2)

# Reject the null. Hence we do not need seasonal dummies

##################### Part d ##########################
#Check serial correlation
# Null hypothesis is that there is no serial corellation
# we want to fail to reject - hence need a p value below 0.05
library(lmtest)
bgtest(rfinal_2, order = 1)
bgtest(rfinal_2, order = 2)
bgtest(rfinal_2, order = 3)
bgtest(rfinal_2, order = 4) # reject the null, hence serial correlation
bgtest(rfinal_2, order = 5)
bgtest(rfinal_2, order = 6)

# compare against Serial correlation in base model at start.

library(lmtest)
bgtest(rbase, order = 1)
bgtest(rbase, order = 2)
bgtest(rbase, order = 3)
bgtest(rbase, order = 4)# reject the null, hence serial correlation
bgtest(rbase, order = 5)
bgtest(rbase, order = 6)

#  serial correlation in both the base model and final model


#################### part E #####################
# Test if dunemp Granger causes dinf

#constrained model:
res6 = dynlm(dinf~L(dinf,1:11)+L(dint_rte,0:6), start=c(1960,4))
summary(res6)
AIC(res6)
BIC(res6)
# anova test (F test)
anova(res6,rfinal_2,test="F")

# Since P-value below 0.05 we reject the null H0. Therefore diff in unemployment 
# does Granger cause the difference in inflation

# Test if lagged dinf Granger causes difference in dinf
#constraint model:
res7 = dynlm(dinf~L(dint_rte,0:6)+L(dunemp,0)+L(dunemp,4), start=c(1960,4))
summary(res7)
AIC(res7)
BIC(res7)
# anova test (F test)
anova(res7,rfinal_2,test="F")

#Since P-value below 0.05.  lagged diff in inflation Granger causes difference 
# in inflation

# Test if difference in interest rate Granger causes difference in inflation
#constraint model:
res8 = dynlm(dinf~L(dinf,1:11)+L(dunemp,0)+L(dunemp,4), start=c(1960,4))
summary(res8)
AIC(res8)
BIC(res8)
# anova test (F test)
anova(res8,rfinal_2,test="F")

#Since P-value below 0.05.  Diff in interest rate Granger causes difference 
# in inflation

###################### part F - Conclusion #######################################

