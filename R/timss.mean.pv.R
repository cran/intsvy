timss.mean.pv <-
function(pvlabel, by, data) {
    pv.input <- function(pvlabel= "ASRREA", data) {
    # PV variable names
    pvnames <- paste(pvlabel, "0", 1:5, sep="")
    # Replicate weight means of PV1 (sampling error)
    read1m <- sapply(1:75, function(x) weighted.mean(data[[pvnames[[1]]]], ifelse(data[["JKZONE"]] == x, 
    2*data[["TOTWGT"]]*data[["JKREP"]], data[["TOTWGT"]]), na.rm = TRUE))
    # Grand mean of 5 PVs (imputation variance)
    readm <- sapply(pvnames, function(x) weighted.mean(data[[x]], data[["TOTWGT"]], na.rm = TRUE))
    # Sampling variance (1st PV); imputation variance; SEs
    varw <- sum((read1m-readm[1])^2); varb <- (1+1/5)*var(readm); readse <-(varw+varb)^(1/2)
    result <- data.frame("Mean"= mean(readm), "Std.err."= readse)
    return(round(result, 2))
  }
  # If by no supplied, calculate for the complete sample    
  if (missing(by)) { 
  return(pv.input(pvlabel=pvlabel, data=data))
  } else {
    return(ddply(data, by, function(x) pv.input(data=x, pvlabel=pvlabel)))
  }
}
