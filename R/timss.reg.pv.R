timss.reg.pv <-
function(x, pvlabel="ASRREA", by, data) {
  
  reg.pv.input <- function(x, pvlabel=pvlabel, data) {
  # PV labels
  pvnames <- paste(pvlabel, "0", 1:5, sep="")
  # List of formulas for each PV
  regform <- lapply(pvnames, function(i) paste(i, "~", paste(x, collapse="+")))
  # Replicate weighted coefficients for sampling error (PV1 only)
  Coefrpv1 <- sapply(1:75, function(i) coefficients(lm(formula=as.formula(regform[[1]]), data=data, 
  weights=ifelse(data[["JKZONE"]] == i, 2*data[["TOTWGT"]]*data[["JKREP"]], data[["TOTWGT"]]))))
  # Total weighted coefficient for each PV for imputation (between) error
  Regpv <- lapply(regform, function(i) summary(lm(formula=i, data=data, weights=data[["TOTWGT"]])))
  Coefpv <- sapply(Regpv, function(i) i[["coefficients"]][, 1])
  R2 <- mean(sapply(Regpv, function(i) i[["r.squared"]]))
  # Mean total coefficients (across PVs)
  Coeftot <- apply(Coefpv, 1, mean)
  # Sampling error (variance within)
  Varw <- apply((Coefrpv1-Coefpv[,1])^2, 1, sum)
  # Imputation error (variance between)
  Varb <- (1+1/5)*apply(Coefpv, 1, var)
  CoefSE <- (Varw+Varb)^(1/2)
  CoefT <- Coeftot/CoefSE
  # Reg Table
  RegTab <- round(data.frame("Estimate"=Coeftot, "Std. Error"=CoefSE, "t value"=CoefT, check.names=F),2)
  return(RegTab)
  }

  # If by not supplied, calculate for the complete sample    
  if (missing(by)) { 
    return(reg.pv.input(x=x, pvlabel=pvlabel, data=data)) 
  } else {
    return(lapply(split(data, data[by]), function(i) reg.pv.input(x=x, pvlabel=pvlabel, data=i)))
  }
}
