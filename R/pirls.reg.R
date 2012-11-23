pirls.reg <-
function(regform, by, data) { 

  reg.input <- function(regform, data) {
  # Replicate weights coefficients for sampling error
  Coefrp <- sapply(1:75, function(i) coefficients(lm(formula= as.formula(regform), data=data, weights= 
  ifelse(data[["JKZONE"]] == i, 2*data[["TOTWGT"]]*data[["JKREP"]], data[["TOTWGT"]]))))
  
  # Total weighted coefficients
  Reg <- summary(lm(formula=as.formula(regform), data=data, weights=data[["TOTWGT"]]))
  Coeftot <- Reg$coefficients[,1]
  # R-squared
  R2 <- Reg$r.squared
  # Sampling error
  CoefSE <- apply((Coefrp-Coeftot)^2, 1, sum)^(1/2)
  # T-value
  CoefT <- Coeftot/CoefSE
  # Reg Table
  RegTab <- round(data.frame("Estimate"=Coeftot, "Std. Error"=CoefSE, "t value"=CoefT, check.names=F),2)
  return(RegTab)
  }
# If by not supplied, calculate for the complete sample    
if (missing(by)) { 
  return(reg.input(regform=regform, data=data)) 
  } else {
    return(lapply(split(data, data[by]), function(x) reg.input(regform=regform, data=x)))
  }
}
