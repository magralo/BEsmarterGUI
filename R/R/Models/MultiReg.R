MultiReg<-function(Data, m, k, Prior, Mcmc){
  
  pandterm = function(message) {
    print(Data)
    print(m)
    print(k)
    print(Prior)
    print(Mcmc)
    stop(message, call. = FALSE)
  }
  
  if (missing(Data)) {
    pandterm("Requires Data argument -- list of Y and X")
  }
  if (missing(m)) {
    pandterm("Requires number of endogeneous varaibles")
  }
  if (missing(k)) {
    pandterm("Requires number of exogeneous varaibles")
  }
  
  if ((m+k) != ncol(Data)) {
    pandterm("Check data set! Dimensions do not agree")
  }
  
  Y = as.matrix(Data[,1:m])
  X = as.matrix(Data[,(m+1):(m+k)])
  
  if (missing(Prior)) {
    betabar=rep(0,k*m)
    Bbar=matrix(betabar,ncol=m)
    A=diag(rep(.01,k))
    nu=m-1
    V=nu*diag(m)
  }
  else {
    if (is.null(Prior$Bbar)) {
      Bbar = matrix(betabar,ncol=m)
    }
    else {
      Bbar = Prior$Bbar
    }
    if (is.null(Prior$A)) {
      A = diag(rep(.01,k))
    }
    else {
      A = Prior$A
    }
    if (is.null(Prior$nu)) {
      nu=m-1
    }
    else {
      nu = Prior$nu
    }
    if (is.null(Prior$V)) {
      V=nu*diag(m)
    }
    else {
      V = Prior$V
    }
  }
  
  if (missing(Mcmc)) {
    pandterm("Requires Mcmc argument")
  }
  else {
    if (is.null(Mcmc$R)) {
      pandterm("Requires Mcmc element R")
    }
    else {
      R = Mcmc$R+Mcmc$burnin
    }
    if (is.null(Mcmc$keep)) {
      keep = 1
    }
    else {
      keep = Mcmc$keep
    }
    if (is.null(Mcmc$burnin)) {
      burnin = 0
    }
    else {
      burnin = Mcmc$burnin
    }
  }
  
  betadraw=matrix(double(floor(R/keep)*k*m),ncol=k*m)
  Sigmadraw=matrix(double(floor(R/keep)*m*m),ncol=m*m)
  
  # withProgress(message = 'Making calculations', value = 0, {
  for (rep in 1:R)
  {out=rmultireg(Y,X,Bbar,A,nu,V)

  if (rep%%keep == 0) {
    mkeep = rep/keep
    betadraw[mkeep, ] = out$B
    Sigmadraw[mkeep,] = out$Sigma
  }
  
  # incProgress(1/rep, detail = paste('Doing iteration', rep))
  }
  # })
  
  betadraw=betadraw[-c(1:ceiling(burnin/keep)),]
  Sigmadraw=Sigmadraw[-c(1:ceiling(burnin/keep)),]
  attributes(betadraw)$class = c("mcmc")
  attributes(betadraw)$mcpar = c(burnin+keep, R, keep)
  attributes(Sigmadraw)$class = c("mcmc")
  attributes(Sigmadraw)$mcpar = c(burnin+keep, R, keep)
  return(list(betadraw = betadraw, Sigmadraw = Sigmadraw))
}
