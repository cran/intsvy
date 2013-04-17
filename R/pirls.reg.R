pirls.reg <-
function(y, x, by, weight="TOTWGT", data, export=FALSE, name= "output", folder=getwd()) { 

  regform <- paste(y, "~", paste(x, collapse="+"))
  
  reg.input <- function(y, x, weight, data) {
  # Replicate weights coefficients for sampling error
  Coefrp <- sapply(1:max(data[["JKZONE"]]), function(i) coefficients(lm(formula= as.formula(regform), data=data, weights= 
  ifelse(data[["JKZONE"]] == i, 2*data[[weight]]*data[["JKREP"]], data[[weight]]))))
  
  # Total weighted coefficients
  Reg <- summary(lm(formula=as.formula(regform), data=data, weights=data[[weight]]))
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
  output <- reg.input(y=y, x=x, weight=weight, data=data)
  } else {
    output <- lapply(split(data, data[by]), function(x) reg.input(y=y, x=x, weight=weight, data=x))
  }
  
  if (export)  {
    write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
  }
  
  return(output)
}
