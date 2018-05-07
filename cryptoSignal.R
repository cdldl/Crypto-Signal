#Set your directory
setwd('C:/Users/cyril/Desktop/Master application/Crypto Signal')
rm(list=ls())

#install libraries
library(xts)
library(data.table)
library(TTR)
library(quantmod)
require(PerformanceAnalytics)
library(parallel)

# Register parallel background
if(Sys.info()['sysname'] == "Windows") {
  library(doParallel)
  registerDoParallel(cores = detectCores())
} else {
  library(doMC)
  registerDoMC(cores = detectCores())
}

# Load functions and data 
source('functionCrypto.R')
all <- fread('crypto-markets.csv')
data <- data.table(all,key="symbol")

# Create Lags
data <- data[,`:=`(ret=c(rep(0,1),diff(log(close),1))),by=symbol]
data$ret[which(is.infinite(data$ret))] = NA
data = data[, lag2:= shift(.SD, 1, 0, "lag"), .SDcols="ret",by='symbol']
data = data[, lag3:= shift(.SD, 2, 0, "lag"), .SDcols="ret",by='symbol']
data = data[, lag4:= shift(.SD, 3, 0, "lag"), .SDcols="ret",by='symbol']
data = data[, lag5:= shift(.SD, 4, 0, "lag"), .SDcols="ret",by='symbol']
data = data[, lag6:= shift(.SD, 5, 0, "lag"), .SDcols="ret",by='symbol']
data = data[, lag7:= shift(.SD, 6, 0, "lag"), .SDcols="ret",by='symbol']

# Keep only if size of each symbol is above 50 occurences
data<- data[,len:=(length(ret)),by=symbol]
data <- data[which(data$len > 50),]

# Create technical indicators
ind = BTIParallel(data)
#ind = readRDS('results techInd2.rds')


# Clean up the crypto names that did not work on technical indicators
names=sort(unique(data$name))
nameToTakeOff=NULL
for( i in 1:length(ind)) {
  cols=dim(ind[[i]])[2]
  if(is.null(cols)) {
    nameToTakeOff=c(nameToTakeOff,names(ind)[i])
    next
  }
  if(dim(ind[[i]])[2]!=77) {
    nameToTakeOff=c(nameToTakeOff,names(ind)[i])
    next
  }
  ind[[i]]= cbind(rep(names(ind)[i],dim(ind[[i]])[1]),as.data.frame(ind[[i]]))
  ind[[i]]= cbind(row.names(ind[[i]]),ind[[i]])
}
whi = which(names(ind) %in% nameToTakeOff)
ind[whi] = NULL
inds = do.call(rbind,ind)
data = data[-which(data$name %in% nameToTakeOff),]

# Merge the lags with technical indictors
data$names = paste0(data$name,'.',data$date)
inds$names = paste0(inds[,2],'.',inds[,1])
data = data[order(data$names),]
inds = inds[order(inds$names),]
data = cbind(data,inds)

# Additional features
data<- data[,vol50:=(c(rollapplyr(ret,50,sd,na.rm=T,align="right",fill=NA))),by=symbol]
data<- data[,closeRatio:=((close - runMin(low,7))/(runMax(high,7) - runMin(low,7))),by=symbol]
data<- data[,ret7:=(runSum(ret,7)),by=symbol]
data<- data[,sd7:=(runSD(ret,7)),by=symbol]
data<- data[,sd30:=(runSD(ret,30)),by=symbol]
data<- data[,ret30:=(runSum(ret,30)),by=symbol]
data<- data[,ret50:=(runSum(ret,50)),by=symbol]
data<- data[,sharpe7:=( (ret7*sqrt(252))/sd7),by=symbol]
data<- data[,sharpe30:=( (ret30*sqrt(252))/sd30),by=symbol]
data$logMarket = log(data$market)
data$logVolume = log(data$volume)
data$med = (data$high + data$low)/2

# Clean up features
data$logMarket[which(is.infinite(data$logMarket))] = NA
data$logVolume[which(is.infinite(data$logVolume))] = NA
data$cmo[which(is.infinite(data$cmo))] = NA
data$ult.osc[which(is.infinite(data$ult.osc))] = NA
data$close_ratio[which(is.infinite(data$close_ratio))] = NA
data$mfi = NULL
data$chaikinad = NULL
data$names = NULL
data$obv = NULL
data[,"row.names(ind[[i]])"] = NULL
data[,"rep(names(ind)[i], dim(ind[[i]])[1])"] = NULL
data=data[order(data$date),]
data = data[complete.cases(data),]

# Create predictors
data = data[, forSharpe7:= shift(.SD, 7, 0, "lead"), .SDcols='sharpe7',by='symbol']
data = data[, forRet:= shift(.SD, 1, 0, "lead"), .SDcols='ret',by='symbol']
data = data[, forRet7:= shift(.SD, 7, 0, "lead"), .SDcols='ret7',by='symbol']
data = data[, forVol:= shift(.SD, 7, 0, "lead"), .SDcols='sd7',by='symbol']

# Check if we have at least 50 coins per day to have robust results
data$date = as.Date(data$date)
minDate = min(as.Date(names(which(table(data$date)>50))))
data = data[which(data$date>=minDate),]
data = data[order(data$date),]

# Split the predictor from the data
names=names(which(sapply(data,class) == "numeric"))
namesFor = names[grep('for',names)]
dataFor = data[,c(colnames(data) %in% namesFor),with=F]
data = data[,-c(colnames(data) %in% namesFor),with=F]

#Get the date
dataDate = data$date

# Normalize features
names=names(which(sapply(data,class) == "numeric"))
for(i in names) {
  name=paste0(i,'Norm')
  dataNorm = data[,(name):=scale(get(i)),by=date]
}
dataNorm = dataNorm[,grep('Norm',colnames(dataNorm)),with=F]


# Normalized Cross sectional regression
signalsforRet = crossSection(dataNorm,'forRet')
signalsforRet7 = crossSection(dataNorm,'forRet7')
signalsforSharpe7 = crossSection(dataNorm,'forSharpe7')
signalsforVol = crossSection(dataNorm,'forVol')

#Signal Stats
statsforRet = stats(signalsforRet)
statsforRet7 = stats(signalsforRet7)
statsforSharpe7 = stats(signalsforSharpe7)
statsforVol= stats(signalsforVol)
