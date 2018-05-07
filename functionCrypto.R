BTIParallel <- function(data) {
  
  grid.param = expand.grid(sort(unique(data$name)))
  fe <- foreach(param = iter(grid.param, by = "row"), 
                .verbose = TRUE, .errorhandling = "pass",  
                .multicombine = TRUE, .maxcombine = max(2, nrow(grid.param)),
                .export=c("xts","build.Technical.Indicators"))
  fe$args <- fe$args[1]
  fe$argnames <- fe$argnames[1]
  
  results <- fe %dopar% {
    param=as.character(param)
    marketRet = data[which(data$name==param),-1:-5]
    marketRet = xts(marketRet,order.by=as.POSIXct(data$date[which(data$name==param)]))
    ind = build.Technical.Indicators(marketRet[,1:4],marketRet[,5])
    ind
  }
  names(results) <-  apply(grid.param,1,paste,collapse=",")
  #saveRDS(results,paste0('/mnt/c/Users/cyril/Desktop/Master application/Crypto signal/','results techInd3.rds'))
  results
}

build.Technical.Indicators <- function(ohlc.xts,volume) {
  
  med = xts(apply(ohlc.xts[,2:3],1,sum)/2,order.by = index(ohlc.xts))
  
  ohlc <- unclass(ohlc.xts)
  firstHlc <- ohlc.xts[,2:4]
  adx <- ADX(firstHlc)
  indicators <- data.frame(adx=adx[,4])
  tryCatch({
    
    indicators$adx.signal <- ifelse(adx[,4] >= 25,ifelse(adx[,1] > adx[,2],1,-1),0)
    Aroon <- aroon(firstHlc[,1:2],20)
    indicators$aroon <- Aroon[,3]
    
    atr <- ATR(firstHlc,14)
    indicators$atr14 <- atr[,2]
    atr <- ATR(firstHlc,50)
    indicators$atr50 <- atr[,2]
    indicators$atr14.50 <-  indicators$atr14 - indicators$atr50
    
    
    bbands <- BBands(firstHlc)
    colnames(bbands)[1:3] <- c("lowBBands","meanBBands","highBBands")
    indicators <- cbind(indicators,coredata(bbands))
    
    indicators$cci <- CCI(firstHlc)
    
    indicators$chaikinVol <- chaikinVolatility(firstHlc[,1:2])
    
    indicators$CloseLocationValue <- CLV(firstHlc)
    
    indicators$cmo <- CMO(med)
    
    # --> 12 more variables
    gmma <- GMMA(med)
    gmma.df <- data.frame(gmma.1=rep(NA,nrow(gmma)))
    for(i in 1:6) gmma.df[,paste("gmma.",i,sep="")] <- gmma[,i]-gmma[,i+6]
    indicators <- cbind(indicators,gmma.df)
    
    kst <- KST(firstHlc[,3])
    indicators$kst <- kst[,1]
    indicators$kst.signal <- kst[,1] - kst[,2]
    
    kst4MA <- KST(med,maType=list(list(SMA),list(EMA),list(DEMA),list(WMA)))
    indicators$kst4MA <- kst4MA[,1]
    indicators$kst4MA.signal <- kst4MA[,1] - kst4MA[,2]
    
    macd <- MACD(med,12,26,9,maType="EMA")
    indicators$macd <- macd[,1]
    indicators$macd.diff <- diff(macd[,1])
    indicators$macd.signal <- macd[,1] -macd[,2]
    
    pbands <- PBands(firstHlc[,3])
    names(pbands) <- c("lowPBands","centerPBands","highPBands")
    indicators <- cbind(indicators,coredata(pbands))
    
    indicators$mom2 <- ROC(med,2)
    indicators$mom3 <- ROC(med,3)
    indicators$mom5 <- ROC(med,5)
    indicators$mom10 <- ROC(med,10)
    indicators$mom15 <- ROC(med,15)
    indicators$mom30 <- ROC(med,30)
    indicators$mom50 <- ROC(med,50)
    
    indicators$rsi2 <- RSI(med,2)
    indicators$rsi4 <- RSI(med,4) 
    indicators$rsi14 <- RSI(med,14)
    
    sar <- SAR(firstHlc[,1:2])
    indicators$sar <- sar
    indicators$ema.3.30 <-   EMA(med, 3) - EMA(med, 30)
    indicators$ema.5.35 <-   EMA(med, 5)   - EMA(med, 35)
    indicators$ema.8.40 <-   EMA(med, 8)   - EMA(med, 40)
    indicators$ema.8.40 <-   EMA(med, 8)   - EMA(med, 40)
    indicators$ema.10.45 <-   EMA(med, 10)   - EMA(med, 45)
    indicators$ema.12.50 <-   EMA(med, 12)   - EMA(med, 50)
    indicators$ema.15.60 <-   EMA(med, 15)   - EMA(med, 60)
    
    T3 <- function(x, n=10, v=1) DEMA(DEMA(DEMA(x,n,v),n,v),n,v)
    indicators$t3 <- T3(med)
    
    stochOSC <- stoch(firstHlc)
    names(stochOSC) <- c("stochOSCfastK","stochOSCfastD","stochOSCslowD")
    indicators$stochOSCfastK <- stochOSC$stochOSCfastK - stochOSC$stochOSCslowD
    indicators$stochOSCfastD <- stochOSC$stochOSCfastD - stochOSC$stochOSCslowD
    indicators$stochOSCfastKD <- stochOSC$stochOSCfastK - stochOSC$stochOSCfastD
    smi <- SMI(firstHlc)
    
    indicators$SMI <- smi[,1]
    indicators$SMI.diff <- diff(smi[,1])
    indicators$SMI.signal <- smi[,1] - smi[,2]
    
    tdi <- TDI(med, n=20)
    indicators$tdi= tdi[,1]
    
    trix <- TRIX(med)
    indicators$trix <- trix[,1]
    indicators$trix.signal <- trix[,1] - trix[,2]
    
    indicators$ult.osc <- ultimateOscillator(firstHlc)
    
    indicators$vhf.close <- VHF(med)
    
    vGK <- volatility(ohlc, calc="garman")
    indicators$vGK <- ifelse(is.nan(vGK),0,vGK)
    
    vParkinson <- c(NA,volatility(ohlc[-1,], calc="parkinson"))
    indicators$vParkinson <- ifelse(is.nan(vParkinson),0,vParkinson)
    
    vRS <- volatility(ohlc, calc="rogers")
    indicators$vRS <- ifelse(is.nan(vRS),0,vRS)
    
    vGk.yz <- volatility(ohlc, calc="gk.yz")
    indicators$vGk.yz <- ifelse(is.nan(vGk.yz),0,vGk.yz)
    summary(indicators$vGk.yz)
    vYang.zhang <- c(NA,volatility(ohlc[-1,], calc="yang.zhang"))
    indicators$vYang.zhang <- ifelse(is.nan(vYang.zhang),0,vYang.zhang)
    
    indicators$ad <- williamsAD(firstHlc)
    
    indicators$stochWPR<- WPR(firstHlc)
    
    
    indicators$mean <- c(rep(NA,20),lag(rollmean(med, k=21, align="right"),1)) #c(rep(NA,20),
    indicators$median <- c(rep(NA,20),lag(rollmedian(med, k=21, align="right"),1))
    indicators$sd <- lag(rollapply(med, width=21, align="right", FUN=sd),1)
    indicators$mad <- lag(rollapply(med, width=21, align="right", FUN=mad),1)
    indicators$skewness <- lag(rollapply(med, width=21, align="right", FUN=skewness),1)
    indicators$kurtosis <- lag(rollapply(med, width=21, align="right", FUN=kurtosis),1)
    
    
    
    #indicators[,(ncol(indicators)-4):ncol(indicators)] <- diff(log(indicators[,(ncol(indicators)-4):ncol(indicators)]))
    
    if(!missing(volume)) {
      
      indicators$chaikinad <- chaikinAD(firstHlc,volume)
      
      indicators$chaikinMoneyFlow <- CMF(firstHlc,volume)
      
      #emv <- EMV(firstHlc[,1:2],volume)
      #indicators <- cbind(indicators,emv)
      
      indicators$mfi <- MFI(firstHlc, volume)
      
      indicators$obv <- OBV(firstHlc[,3], volume)
      
      # Take off some days at the end too
      indicators$evwma.20 <- EVWMA(firstHlc[,3], volume, 20)
    }
  }, error=function(e){})
  indicators <<- indicators 
  indicators <- xts(indicators,order.by=index(ohlc.xts))
  #indicators[,c("priceLag1","priceLag2","priceLag3","priceLag4" ,"priceLag5")]  <- diff(log(indicators[,c("priceLag1","priceLag2","priceLag3","priceLag4" ,"priceLag5")]))
  
  indicators
}

crossSection = function(dataNorm,predictor) {
  dataNorm[,predictor] = dataFor[,list(get(predictor))]
  dataNorm$date = dataDate
  dataNorm = dataNorm[complete.cases(dataNorm),]
  dataNorm= as.data.frame(dataNorm)
  coeff = list()
  for(i in 1:length(unique(dataNorm$date)) ) { #
    #i=1
    date = sort(unique(dataNorm$date))[i]
    dataOfDate = dataNorm[which(dataNorm$date==date),]
    dataOfDate$date= NULL
    coeff[[i]]=lm.cross(dataOfDate,predictor)
  }
as.data.frame(do.call(rbind,coeff))
}

lm.cross = function(data,predictor) {
  reg=NULL
  tryCatch({
    formula <- as.formula(paste0(predictor,'~',paste0(colnames(data)[-grep('for',colnames(data))],collapse='+')))
    reg <- lm(formula,data=data) 
    pvalues = summary(reg)$coefficients[,4]
    
    misNames = colnames(data)[-which(colnames(data)[-grep('for',colnames(data))] %in% names(pvalues))]
    misPvalues = as.vector(rep(NA,length=length(misNames)))
    names(misPvalues) = misNames
    pvalues = c(pvalues,misPvalues)[sort(names(c(pvalues,misPvalues)))]
    names(pvalues) = paste0(names(pvalues),'.pval')
    }, error=function(e){})
  c(reg$coefficients,r.squared=summary(reg)$r.squared,pvalues) #[sort(row.names(coefficients(summary(reg))))]
}

shiftCol = function(col,shift,n.shift) {
  for(i in 1:n.lags) {
    name = paste0(shift,i)
    data = data[, (name):= shift(.SD, i, 0, shift), .SDcols=col]
  }
}

stats = function(data) {
  pvals = data[,grep('.pval',colnames(data))]
  signals = data[,-grep('.pval',colnames(data))]
  sharpeSignal = sort(abs(apply(signals,2,mean,na.rm=T)/apply(signals,2,sd,na.rm=T)))
  signs = sign(apply(signals,2,mean,na.rm=T)/apply(signals,2,sd,na.rm=T))[names(sharpeSignal)]
  sharpeSignalDir = sharpeSignal * signs
  for(i in 1:ncol(pvals)) pvals[which(is.nan(pvals[,i])),i] = NA
  pvals = sort(apply(pvals,2,mean,na.rm=T))
  list(sharpeSignalDir=sharpeSignalDir,pvals=pvals,r.squared=mean(signals$r.squared,na.rm=T))
}
