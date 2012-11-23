timss.mean <-
function(variable, by, data) {
  mean.input <- function(variable, data) {
  # Replicate weight means (sampling error)
  meanrp <- sapply(1:75, function(x) weighted.mean(as.numeric(data[[variable]]), ifelse(data[["JKZONE"]] == x, 
             2*data[["TOTWGT"]]*data[["JKREP"]], data[["TOTWGT"]]), na.rm = TRUE))
  # Total weighted mean                                                                      
  meantot <- weighted.mean(as.numeric(data[[variable]]), data[["TOTWGT"]], na.rm = TRUE)
  # Standard error (sampling eror) 
  meanse <- (sum((meanrp-meantot)^2))^(1/2)
  result <- data.frame("Mean"= meantot, "Std.err."= meanse)
  return(round(result, 2))
  }
  # If by no supplied, calculate for the complete sample    
  if (missing(by)) { 
  return(mean.input(variable=variable, data=data))
  } else {
  return(ddply(data, by, function(x) mean.input(data=x, variable=variable)))
  }
}
