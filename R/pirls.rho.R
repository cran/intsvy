pirls.rho <-
function(variables, by, data) {
  rho.input <- function(variables, data) {
  # Remove cases listwise
  data <- na.omit(data[c(variables, "TOTWGT", "JKREP", "JKZONE")]) 
  # Fifth element is correlation matrix
  rhorp <- lapply(1:75, function(i) cov.wt(data[variables],
  wt=ifelse(data[["JKZONE"]] == i, 2*data[["TOTWGT"]]*data[["JKREP"]], data[["TOTWGT"]]), cor=T)[[5]])
  rhotot <- cov.wt(data[variables], wt=data[["TOTWGT"]], cor=T)[[5]]
  # SE formula
  rhose <- Reduce("+", lapply(rhorp, function(x) (x-rhotot)^2))^(1/2) 
  # Combined rhos and SEs
  rhomat <- round(do.call(cbind, lapply(1:ncol(rhotot), function(x) t(rbind(rhotot[,x], rhose[, x])))), 3)
  colnames(rhomat) <- unlist(lapply(1:length(variables), function(x) 
  c(paste(variables, "Rho", sep=" ")[x], paste(variables, "SE", sep=" ")[x])))
  return(round(rhomat, 6))
  }
  # If by not supplied, calculate for the complete sample    
  if (missing(by)) { 
    return(rho.input(variables=variables, data=data)) }
  else {
    return(lapply(split(data, data[by]), function(x) rho.input(variables=variables, data=x)))
  }
}
