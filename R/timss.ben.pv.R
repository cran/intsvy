timss.ben.pv <-
function(pvlabel, cutoff = c(400, 475, 550, 625), by, data) {
  pv.ben.input <- function(pvlabel, cutoff, data) {
  # PV variable names
  pvname <- paste(pvlabel, "0", 1:5, sep='')
  
  # Replicate weighted percentages PV1 (sampling error)
  tabpv1 <- sapply(1:length(cutoff), function(z) sapply(1:75, function(x) 
    percent(data[[pvname[1]]]>=cutoff[z], weights= ifelse(data[["JKZONE"]] == x, 
    2*data[["TOTWGT"]]*data[["JKREP"]], data[["TOTWGT"]]), na.rm = TRUE, total=F)))
  
  # Total weighted %s PV1-5 
  tabpv <- sapply(1:length(cutoff), function(z) sapply(pvname, function(x) 
    percent(data[[x]]>=cutoff[z], weights=data[["TOTWGT"]], na.rm=TRUE, total=F)))
  
  # Sampling error within (PV1), between PV error, and total (se)
  tabpvw <- sapply(1:length(cutoff), function(y) sum(sapply(1:75, function(x) 
            (tabpv1[x,y]-tabpv[1,y])^2)))
  tabpvb <- (1+1/5)*apply(tabpv, 2, var)
  tabse <- round((tabpvw+tabpvb)^(1/2), 2)
  
  # Total %
  tabtot <- round(apply(tabpv, 2, mean), 2)
  
  # Result
  result <- data.frame("Benchmark"=paste(rep("At or above", length(cutoff)), cutoff), 
            "Percentage"=tabtot, "Std. err."= tabse, check.names=F)
  return(result)
  }

# If by not supplied, calculate for the complete sample    
if (missing(by)) { 
  return(pv.ben.input(pvlabel=pvlabel, cutoff=cutoff, data=data))
} else {
  return(ddply(data, by, function(x) pv.ben.input(pvlabel=pvlabel, cutoff=cutoff, data=x)))
  }
}
