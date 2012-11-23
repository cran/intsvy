pirls.rho.pv <-
function(variable, pvlabels, by, data) {
  rho.pv.input <- function(variable, pvlabels, data) {
    
    # Correlation of 2 PVs (variable should be missing)
    
    if (length(pvlabels)==2 & missing(variable)) {
        
    # PV names
    pvnames <- lapply(pvlabels, function(x) paste(x, "0", 1:5, sep=""))
    # Complete dataset (listwise deletion)
    data <- na.omit(data[c(unlist(pvnames), "TOTWGT", "JKREP", "JKZONE")])
    # Replicate weighted correlations for PV1 (sampling error)
    rhopvrp <- lapply(1:75, function(i) cov.wt(x=data[c(pvnames[[1]][1], pvnames[[2]][1])], cor=T, 
               wt=ifelse(data[["JKZONE"]] == i, 2*data[["TOTWGT"]]*data[["JKREP"]], data[["TOTWGT"]]))[[5]])
    # Total weighted correlation for imputation variance
    rhopvtot <- lapply(1:5, function(i) cov.wt(x=data[c(pvnames[[1]][i],pvnames[[2]][i])], cor=T, 
                 wt= data[["TOTWGT"]])[[5]])
    # Sampling variance, imputation variance, and SEs
    varw <- Reduce("+", lapply(rhopvrp, function(x) (x - rhopvtot[[1]])^2))
    varb <- (1+1/5)* apply(simplify2array(rhopvtot), c(1, 2), var) # slower, but Reduce(var) fails
    rhose <- (varw+varb)^(1/2)
    # Mean total weighted correlation
    rhotot <- Reduce("+", rhopvtot)/length(rhopvtot)
    # Combined rhos and SEs
    rhomat <- round(do.call(cbind, lapply(1:ncol(rhotot), function(x) t(rbind(rhotot[,x], rhose[, x])))), 5)
    # Format and print
    colnames(rhomat) <- unlist(lapply(1:2, function(i) 
    c(paste(pvlabels, "Rho", sep=" ")[i], paste(pvlabels, "SE", sep=" ")[i])))
    return(round(rhomat, 3))
    } else {
    
    # Correlation of no PV with PV
    # PV names
    pvnames <- paste(pvlabels, "0", 1:5, sep="")
    # Complete dataset (listwise deletion)
    data <- na.omit(data[c(variable, pvnames, "TOTWGT", "JKREP", "JKZONE")])
    # Replicate weighted correlations for PV1 (sampling error)
    rhopvrp <- lapply(1:75, function(i) cov.wt(x=data[c(variable, pvnames[1])], cor=T, 
                 wt=ifelse(data[["JKZONE"]] == i, 2*data[["TOTWGT"]]*data[["JKREP"]], data[["TOTWGT"]]))[[5]])
    # Total weighted correlation for imputation variance
    rhopvtot <- lapply(pvnames, function(i) cov.wt(x=data[c(variable,i)], cor=T, wt= data[["TOTWGT"]])[[5]])
    # Sampling variance, imputation variance, and SEs
    varw <- Reduce("+", lapply(rhopvrp, function(x) (x - rhopvtot[[1]])^2))
    varb <- (1+1/5)* apply(simplify2array(rhopvtot), c(1, 2), var) # slower, but Reduce(var) fails
    rhose <- (varw+varb)^(1/2)
    # Mean total weighted correlation
    rhotot <- Reduce("+", rhopvtot)/length(rhopvtot)
    # Combined rhos and SEs
    rhomat <- round(do.call(cbind, lapply(1:ncol(rhotot), function(x) t(rbind(rhotot[,x], rhose[, x])))), 5)
    # Format and print
    colnames(rhomat) <- unlist(lapply(1:2, function(i) 
    c(paste(c(variable, pvlabels), "Rho", sep=" ")[i], paste(c(variable, pvlabels), "SE", sep=" ")[i])))
    return(round(rhomat, 3))
    }
  }
# If by not supplied, calculate for complete sample    
if (missing(by)) { 
return(rho.pv.input(variable=variable, pvlabels=pvlabels, data=data))
  } else {
    if (length(pvlabels)==2 & missing(variable)) {
    return(lapply(split(data, data[by]), function(x) 
    rho.pv.input(pvlabels=pvlabels, data=x)))
    } else {
      return(lapply(split(data, data[by]), function(x) 
      rho.pv.input(variable=variable, pvlabels=pvlabels, data=x)))
    }
  }  
}
