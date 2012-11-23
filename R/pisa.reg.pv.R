pisa.reg.pv <-
function(x, pvlabel="READ", by, data) {
  
  reg.pv.input <- function(x, pvlabel=pvlabel, data) {
    # PV labels
    pvnames <- paste("PV", 1:5, pvlabel, sep="")
    # List of formulas for each PV
    regform <- lapply(pvnames, function(i) paste(i, "~", paste(x, collapse="+")))
    # Replicate weighted coefficients for sampling error (5 PVs)
    Coefrpv <- lapply(regform, function(k) sapply(1:80, function(i) 
    coefficients(lm(formula=as.formula(k), data=data, 
    weights=data[[paste("W_FSTR", i , sep="")]]))))
    # Total weighted coefficient for each PV for imputation (between) error
    Regpv <- lapply(regform, function(i) summary(lm(formula=i, data=data, weights=data[["W_FSTUWT"]])))
    Coefpv <- sapply(Regpv, function(i) i[["coefficients"]][, 1])
    R2 <- mean(sapply(Regpv, function(i) i[["r.squared"]]))
    # Mean total coefficients (across PVs)
    Coeftot <- apply(Coefpv, 1, mean)
    # Sampling error (variance within)
    Varw <- apply(0.05*sapply(1:5, function(i) apply((Coefrpv[[i]]-Coefpv[,i])^2, 1, sum)), 1, mean)
    # Imputation error (variance between)
    Varb <- (1/4)*apply(sapply(1:5, function(i) (Coefpv[, i] - Coeftot)^2), 1, sum)
    CoefSE <- (Varw+(1+1/5)*Varb)^(1/2)
    CoefT <- Coeftot/CoefSE
    # Reg Table
    RegTab <- round(data.frame("Estimate"=Coeftot, "Std. Error"=CoefSE, "t value"=CoefT, check.names=F),3)
    return(RegTab)
  }
  
  # If by not supplied, calculate for the complete sample    
  if (missing(by)) { 
    return(reg.pv.input(x=x, pvlabel=pvlabel, data=data)) 
  } else {
    return(lapply(split(data, data[by]), function(i) reg.pv.input(x=x, pvlabel=pvlabel, data=i)))
  }
}
