model.formula<- function(formula, data=list(), ...)
{
  mf <- model.frame(formula=formula, data=data)
  X <- model.matrix(attr(mf, "terms"), data=mf)
  y <- model.response(mf)
  return(list(X=X,y=y))
}

model.formula1<- function(formula, data=list(), ...)
{
  mf <- model.frame(formula=formula, data=data)
  Z <- model.matrix(attr(mf, "terms"), data=mf)
  x <- model.response(mf)
  return(list(Z=Z,x=x))
}

SumDiagNormal<- function(betadraw,sigmasqdraw){
  Summary<- summary(betadraw)
  GewekeTest<- geweke.diag(betadraw)
  RafteryTest<- raftery.diag(betadraw,q=0.5,r=0.025,s = 0.95)
  HeidelTest<- heidel.diag(betadraw)
  return(list(Summary = Summary, Geweke.Test = GewekeTest, Raftery.Test = RafteryTest, Heidel.Test = HeidelTest))
}

SumDiagLogit<- function(betadraw){
  Summary<- summary(betadraw)
  GewekeTest<- geweke.diag(betadraw)
  RafteryTest<- raftery.diag(betadraw,q=0.5,r=0.025,s = 0.95)
  HeidelTest<- heidel.diag(betadraw)
  return(list(Summary = Summary, Geweke.Test = GewekeTest, Raftery.Test = RafteryTest, Heidel.Test = HeidelTest))
}

SumDiagProbit<- function(betadraw,R,burnin,keep){
  Goodbetadraw<-Draws(betadraw,burnin,keep)
  attributes(Goodbetadraw)$class = c("mcmc")
  attributes(Goodbetadraw)$mcpar = c(burnin+keep, R, keep)
  Summary<- summary(Goodbetadraw)
  GewekeTest<- geweke.diag(Goodbetadraw)
  RafteryTest<- raftery.diag(Goodbetadraw,q=0.5,r=0.025,s = 0.95)
  HeidelTest<- heidel.diag(Goodbetadraw)
  return(list(Summary = Summary, Geweke.Test = GewekeTest, Raftery.Test = RafteryTest, Heidel.Test = HeidelTest))
}

SumDiagMultProbit<- function(betadraw,sigmadraw,R,burnin,keep){
  Goodbetadraw<-Draws(betadraw,burnin,keep)
  attributes(Goodbetadraw)$class = c("mcmc")
  attributes(Goodbetadraw)$mcpar = c(burnin+keep, R, keep)
  SummaryDependentVar<- summary(Goodbetadraw)
  GewekeTestDependentVar<- geweke.diag(Goodbetadraw)
  RafteryTestDependentVar<- raftery.diag(Goodbetadraw[,],q=0.5,r=0.025,s = 0.95)
  HeidelTestDependentVar<- heidel.diag(Goodbetadraw)
  Goodsigmadraw<-Draws(sigmadraw,burnin,keep)
  attributes(Goodsigmadraw)$class = c("mcmc")
  attributes(Goodsigmadraw)$mcpar = c(burnin+keep, R, keep)
  SummaryCovMatrix<- summary(Goodsigmadraw)
  GewekeTestCovMatrix<- geweke.diag(Goodsigmadraw)
  RafteryTestCovMatrix<- raftery.diag(Goodsigmadraw[,-1],q=0.5,r=0.025,s = 0.95)
  HeidelTestCovMatrix<- heidel.diag(Goodsigmadraw)
  return(list(SummaryLocationCoef = SummaryDependentVar, GewekeTestLocationCoef = GewekeTestDependentVar, RafteryTestLocationCoef = RafteryTestDependentVar, HeidelTestLocationCoef = HeidelTestDependentVar, SummaryCovMatrix = SummaryCovMatrix, GewekeTestCovMatrix = GewekeTestCovMatrix, RafteryTestCovMatrix = RafteryTestCovMatrix, HeidelTestCovMatrix = HeidelTestCovMatrix))
}

SumDiagMultLogit<- function(betadraw,R,burnin,keep){
  Goodbetadraw<-Draws(betadraw,burnin,keep)
  attributes(Goodbetadraw)$class = c("mcmc")
  attributes(Goodbetadraw)$mcpar = c(burnin+keep, R, keep)
  Summary<- summary(Goodbetadraw)
  GewekeTest<- geweke.diag(Goodbetadraw)
  RafteryTest<- raftery.diag(Goodbetadraw,q=0.5,r=0.025,s = 0.95)
  HeidelTest<- heidel.diag(Goodbetadraw)
  return(list(Summary = Summary, Geweke.Test = GewekeTest, Raftery.Test = RafteryTest, Heidel.Test = HeidelTest))
}


SumDiagOprobit<- function(betadraw,cutdraw,R,burnin,keep){
  Goodbetadraw<-Draws(betadraw,burnin,keep)
  attributes(Goodbetadraw)$class = c("mcmc")
  attributes(Goodbetadraw)$mcpar = c(burnin+keep, R, keep)
  SummaryLocation<- summary(Goodbetadraw)
  GewekeTestLocation<- geweke.diag(Goodbetadraw)
  RafteryTestLocation<- raftery.diag(Goodbetadraw[,],q=0.5,r=0.025,s = 0.95)
  HeidelTestLocation<- heidel.diag(Goodbetadraw)
  Goodcutdraw<-Draws(cutdraw,burnin,keep)
  attributes(Goodcutdraw)$class = c("mcmc")
  attributes(Goodcutdraw)$mcpar = c(burnin+keep, R, keep)
  SummaryCut<- summary(Goodcutdraw)
  GewekeTestCut<- geweke.diag(Goodcutdraw)
  RafteryTestCut<- raftery.diag(Goodcutdraw[,-1],q=0.5,r=0.025,s = 0.95)
  HeidelTestCut<- heidel.diag(Goodcutdraw)
  return(list(SummaryLocation = SummaryLocation, GewekeTestLocation = GewekeTestLocation, RafteryTestLocation = RafteryTestLocation, HeidelTestLocation = HeidelTestLocation, SummaryCut = SummaryCut, GewekeTestCut = GewekeTestCut, RafteryTestCut = RafteryTestCut, HeidelTestCut = HeidelTestCut))
}

SumDiagNegBin<- function(betadraw,R,burnin,keep){ #There is an issue with alpha
  Goodbetadraw<-Draws(betadraw,burnin,keep)
  attributes(Goodbetadraw)$class = c("mcmc")
  attributes(Goodbetadraw)$mcpar = c(burnin+keep, R, keep)
  SummaryLocation<- summary(Goodbetadraw)
  GewekeTestLocation<- geweke.diag(Goodbetadraw)
  RafteryTestLocation<- raftery.diag(Goodbetadraw[,],q=0.5,r=0.025,s = 0.95)
  HeidelTestLocation<- heidel.diag(Goodbetadraw)
  # Goodalphadraw<-Draws(alphadraw,burnin,keep)
  # attributes(Goodalphadraw)$class = c("mcmc")
  # attributes(Goodalphadraw)$mcpar = c(burnin+keep, R, keep)
  # SummaryDispersion<- summary(Goodalphadraw)
  # GewekeTestDispersion<- geweke.diag(Goodalphadraw)
  # RafteryTestDispersion<- raftery.diag(Goodalphadraw[],q=0.5,r=0.025,s = 0.95)
  # HeidelTestDispersion<- heidel.diag(Goodalphadraw)
  # return(list(SummaryLocation = SummaryLocation, GewekeTestLocation = GewekeTestLocation, RafteryTestLocation = RafteryTestLocation, HeidelTestLocation = HeidelTestLocation, SummaryDispersion = SummaryDispersion, GewekeTestDispersion = GewekeTestDispersion, RafteryTestDispersion = RafteryTestDispersion, HeidelTestDispersion = HeidelTestDispersion))
  return(list(SummaryLocation = SummaryLocation, GewekeTestLocation = GewekeTestLocation, RafteryTestLocation = RafteryTestLocation, HeidelTestLocation = HeidelTestLocation))
}

SumDiagTobit<- function(betadraw){
  Summary<- summary(betadraw)
  GewekeTest<- geweke.diag(betadraw)
  RafteryTest<- raftery.diag(betadraw,q=0.5,r=0.025,s = 0.95)
  HeidelTest<- heidel.diag(betadraw)
  return(list(Summary = Summary, Geweke.Test = GewekeTest, Raftery.Test = RafteryTest, Heidel.Test = HeidelTest))
}

SumDiagQuantile<- function(betadraw){
  Summary<- summary(betadraw)
  GewekeTest<- geweke.diag(betadraw)
  RafteryTest<- raftery.diag(betadraw,q=0.5,r=0.025,s = 0.95)
  HeidelTest<- heidel.diag(betadraw)
  return(list(Summary = Summary, Geweke.Test = GewekeTest, Raftery.Test = RafteryTest, Heidel.Test = HeidelTest))
}

SumDiagMultiReg<- function(betadraw,sigmasqdraw){
  SummaryLocation<- summary(betadraw)
  GewekeTestLocation<- geweke.diag(betadraw)
  RafteryTestLocation<- raftery.diag(betadraw,q=0.5,r=0.025,s = 0.95)
  HeidelTestLocation<- heidel.diag(betadraw)
  SummaryScale<- summary(sigmasqdraw)
  GewekeTestScale<- geweke.diag(sigmasqdraw)
  RafteryTestScale<- raftery.diag(sigmasqdraw,q=0.5,r=0.025,s = 0.95)
  HeidelTestScale<- heidel.diag(sigmasqdraw)
  return(list(SummaryLocation = SummaryLocation, GewekeTestLocation = GewekeTestLocation, RafteryTestLocation = RafteryTestLocation, HeidelTestLocation = HeidelTestLocation, SummaryScale = SummaryScale, GewekeTestScale = GewekeTestScale, RafteryTestScale = RafteryTestScale, HeidelTestScale = HeidelTestScale))
}

SumDiagSUR<- function(betadraw,sigmasqdraw,R,burnin,keep){
  Goodbetadraw<-Draws(betadraw,burnin,keep)
  attributes(Goodbetadraw)$class = c("mcmc")
  attributes(Goodbetadraw)$mcpar = c(burnin+keep, R, keep)
  SummaryLocation<-summary(Goodbetadraw)
  GewekeTestLocation<- geweke.diag(Goodbetadraw)
  RafteryTestLocation<- raftery.diag(Goodbetadraw,q=0.5,r=0.025,s = 0.95)
  HeidelTestLocation<- heidel.diag(Goodbetadraw)
  Goodsigmasqdraw<-Draws(sigmasqdraw,burnin,keep)
  attributes(Goodsigmasqdraw)$class = c("mcmc")
  attributes(Goodsigmasqdraw)$mcpar = c(burnin+keep, R, keep)
  SummaryScale<- summary(Goodsigmasqdraw)
  GewekeTestScale<- geweke.diag(Goodsigmasqdraw)
  RafteryTestScale<- raftery.diag(Goodsigmasqdraw,q=0.5,r=0.025,s = 0.95)
  HeidelTestScale<- heidel.diag(Goodsigmasqdraw)
  return(list(SummaryLocation = SummaryLocation, GewekeTestLocation = GewekeTestLocation, RafteryTestLocation = RafteryTestLocation, HeidelTestLocation = HeidelTestLocation, SummaryScale = SummaryScale, GewekeTestScale = GewekeTestScale, RafteryTestScale = RafteryTestScale, HeidelTestScale = HeidelTestScale))
}

SumDiagInstVar<- function(betadraw,deltadraw,gammadraw,Sigmadraw,R,burnin,keep){
  Goodbetadraw<-Draws(as.matrix(betadraw),burnin,keep)
  attributes(Goodbetadraw)$class = c("mcmc")
  attributes(Goodbetadraw)$mcpar = c(burnin+keep, R, keep)
  SummaryEndogenousVar<- summary(Goodbetadraw)
  GewekeTestEndogenousVar<- geweke.diag(Goodbetadraw)
  RafteryTestEndogenousVar<- raftery.diag(Goodbetadraw,q=0.5,r=0.025,s = 0.95)
  HeidelTestEndogenousVar<- heidel.diag(Goodbetadraw)
  Gooddeltadraw<-Draws(deltadraw,burnin,keep)
  attributes(Gooddeltadraw)$class = c("mcmc")
  attributes(Gooddeltadraw)$mcpar = c(burnin+keep, R, keep)
  SummaryInstruments<- summary(Gooddeltadraw)
  GewekeTestInstruments<- geweke.diag(Gooddeltadraw)
  RafteryTestInstruments<- raftery.diag(Gooddeltadraw,q=0.5,r=0.025,s = 0.95)
  HeidelTestInstruments<- heidel.diag(Gooddeltadraw)
  Goodgammadraw<-Draws(as.matrix(gammadraw),burnin,keep)
  attributes(Goodgammadraw)$class = c("mcmc")
  attributes(Goodgammadraw)$mcpar = c(burnin+keep, R, keep)  
  SummaryExogenousVars<- summary(Goodgammadraw)
  GewekeTestExogenousVars<- geweke.diag(Goodgammadraw)
  RafteryTestExogenousVars<- raftery.diag(Goodgammadraw,q=0.5,r=0.025,s = 0.95)
  HeidelTestExogenousVars<- heidel.diag(Goodgammadraw)
  GoodSigmadraw<-Draws(Sigmadraw,burnin,keep)
  attributes(GoodSigmadraw)$class = c("mcmc")
  attributes(GoodSigmadraw)$mcpar = c(burnin+keep, R, keep)  
  SummaryCovMatrix<- summary(GoodSigmadraw)
  GewekeTestCovMatrix<- geweke.diag(GoodSigmadraw)
  RafteryTestCovMatrix<- raftery.diag(GoodSigmadraw,q=0.5,r=0.025,s = 0.95)
  HeidelTestCovMatrix<- heidel.diag(GoodSigmadraw)
  return(list(SummaryEndogenousVar = SummaryEndogenousVar, GewekeTestEndogenousVar = GewekeTestEndogenousVar, RafteryTestEndogenousVar = RafteryTestEndogenousVar, HeidelTestEndogenousVar = HeidelTestEndogenousVar, SummaryInstruments = SummaryInstruments, GewekeTestInstruments = GewekeTestInstruments, RafteryTestInstruments = RafteryTestInstruments, HeidelTestInstruments = HeidelTestInstruments,
              SummaryExogenousVars = SummaryExogenousVars, GewekeTestExogenousVars = GewekeTestExogenousVars, RafteryTestExogenousVars = RafteryTestExogenousVars, HeidelTestExogenousVars = HeidelTestExogenousVars, SummaryCovMatrix = SummaryCovMatrix, GewekeTestCovMatrix = GewekeTestCovMatrix, RafteryTestCovMatrix = RafteryTestCovMatrix, HeidelTestCovMatrix = HeidelTestCovMatrix))
}

SumDiagBVProbit<- function(betadraw,sigmadraw,R,burnin,keep,p){
  Goodbetadraw<-Draws(betadraw,burnin,keep)
  attributes(Goodbetadraw)$class = c("mcmc")
  attributes(Goodbetadraw)$mcpar = c(burnin+keep, R, keep)
  SummaryLocation<-summary(Goodbetadraw)
  GewekeTestLocation<- geweke.diag(Goodbetadraw)
  RafteryTestLocation<- raftery.diag(Goodbetadraw[,],q=0.5,r=0.025,s = 0.95)
  HeidelTestLocation<- heidel.diag(Goodbetadraw)
  ind<-seq(1,p^2,p+1)
  GoodSigmadraw<-Draws(sigmadraw,burnin,keep)
  attributes(GoodSigmadraw)$class = c("mcmc")
  attributes(GoodSigmadraw)$mcpar = c(burnin+keep, R, keep)  
  SummaryCovariance<- summary(GoodSigmadraw)
  GewekeTestCovariance<- geweke.diag(GoodSigmadraw)
  RafteryTestCovariance<- raftery.diag(GoodSigmadraw[,-ind],q=0.5,r=0.025,s = 0.95)
  HeidelTestCovariance<- heidel.diag(GoodSigmadraw)
  return(list(SummaryLocation = SummaryLocation, GewekeTestLocation = GewekeTestLocation, RafteryTestLocation = RafteryTestLocation, HeidelTestLocation = HeidelTestLocation, SummaryCovariance = SummaryCovariance, GewekeTestCovariance = GewekeTestCovariance, RafteryTestCovariance = RafteryTestCovariance, HeidelTestCovariance = HeidelTestCovariance))
}

SumDiagHier<- function(betadraw){
  #saveRDS(betadraw, file = "my_bd.rds")
  betadraw=betadraw[,!stringr::str_detect(colnames(betadraw),"^b\\.")]
  Summary<- summary(betadraw)
  GewekeTest<- geweke.diag(betadraw)
  RafteryTest<- raftery.diag(betadraw,q=0.5,r=0.025,s = 0.95)
  HeidelTest<- heidel.diag(betadraw)
  return(list(Summary = Summary, Geweke.Test = GewekeTest, Raftery.Test = RafteryTest, Heidel.Test = HeidelTest))
}

SumDiagBayBoots<- function(betadraw){
  Summary<- summary(betadraw)
  GewekeTest<- geweke.diag(betadraw)
  RafteryTest<- raftery.diag(betadraw,q=0.5,r=0.025,s = 0.95)
  HeidelTest<- heidel.diag(betadraw)
  return(list(Summary = Summary, Geweke.Test = GewekeTest, Raftery.Test = RafteryTest, Heidel.Test = HeidelTest))
}


Plot<- function(betadraw){
  hist(betadraw, breaks=20,freq=FALSE, xlab="Parameter", main="", col="lightgreen")
  lines(density(betadraw,na.rm = TRUE), col="red", lwd=2)
  abline(h = NULL, v = c(quantile(betadraw,c(0.025, 0.975))), col = "purple", lwd=2)
  text(quantile(betadraw,c(0.025)),y=1, "Quantile 2.5%", col = "black", adj = c(0,-0.5), cex=0.75)
  text(quantile(betadraw,c(0.975)),y=1, "Quantile 97.5%", col = "black", adj = c(0,-0.5), cex=0.75)
  abline(h = NULL, v = c(quantile(betadraw,c(0.5))), col = "red", lwd=3)
  abline(h = NULL, v = mean(betadraw), col = "blue", lwd=2)
  legend("topleft",inset=.05,cex = 0.75,c("Median","Mean"),horiz=TRUE,lty=c(1,1),lwd=c(1,1),col=c("red","blue"),bg="grey96")
}

Plot.trace<- function(betadraw){traceplot(mcmc(betadraw), main = "Trace Plot", xlab="Iteration", ylab= "Parameter", col= "blue")}
Plot.corr<- function(betadraw){autocorr.plot(mcmc(betadraw), main = "Autocorrelation", col="blue")}
