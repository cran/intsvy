pisa.ben.pv <-
function(pvlabel, cutoff = c(262.04, 334.75, 407.47, 480.18, 552.89, 625.61, 698.32), by, weight= "W_FSTUWT", data, 
                         export=FALSE, name= "output", folder=getwd()) {
  pv.ben.input <- function(pvlabel, cutoff, weight, data) {
    # PV variable names
    pvnames <- paste("PV", 1:5, pvlabel, sep="")
    
    # Replicate weight means of 5 PVs (sampling error)

    incut <- lapply(pvnames, function(x) sapply(2:length(cutoff), function(z) 
      data[[x]] < cutoff[z] & data[[x]] >= cutoff[z-1]))
    
    tabpva <- sapply(pvnames, function(k) sapply(1:80, function(i) percent(data[[k]]<cutoff[1], 
              weights=  data[[paste("W_FSTR", i , sep="")]], na.rm = TRUE, total=FALSE)))
    
    
    tabpvb <- lapply(1:5, function(k) sapply(1:(length(cutoff)-1), function(x) 
               sapply(1:80, function(i) percent(incut[[k]][, x], 
               weights=  data[[paste("W_FSTR", i , sep="")]], na.rm = TRUE, total=FALSE))))

    tabpvc <- sapply(pvnames, function(k) sapply(1:80, function(i) percent(data[[k]]>=cutoff[length(cutoff)], 
               weights=  data[[paste("W_FSTR", i , sep="")]], na.rm = TRUE, total=FALSE)))
    
    
    tabpvw <- lapply(1:5, function(x) cbind(tabpva[,x], tabpvb[[x]], tabpvc[,x]))
    

    # Total weighted %s PV1-5 

    tabpva <- sapply(pvnames, function(x) percent(data[[x]] < cutoff[1], 
              weights= data[[weight]], na.rm = TRUE, total=F))

  
    tabpvb <- sapply(1:length(incut), function(x) sapply(1:ncol(incut[[1]]), function(z) 
              percent(incut[[x]][, z], weights= data[[weight]], na.rm = TRUE, total=FALSE)))

    tabpvc <- sapply(pvnames, function(x) percent(data[[x]] >= cutoff[length(cutoff)], 
              weights= data[[weight]], na.rm = TRUE, total=F))
    
    
    tabpvb <- rbind(tabpva, tabpvb, tabpvc)
    
    # Mean of means (the one is reported)
    
    tabtot <- apply(tabpvb, 1, mean)
    
    # Sampling error, between PV error, and total (se)
    
  
    varw <- apply(sapply(1:5, function(w) sapply(1:nrow(tabpvb), function(i) 
    0.05*sum((tabpvw[[w]][, i] - tabpvb[i , w])^2))), 1, mean)
           
    varb <- (1/(5-1)*apply(sapply(1:5, function(x) sapply(1:nrow(tabpvb), function(i) (tabpvb[i, x]-tabtot[i])^2)), 1, sum))
    

    tabse <-(varw+(1+1/5)*varb)^(1/2)
    
  
    # Result
    result <- data.frame("Benchmarks"= c(paste("Below", cutoff[1]), paste(rep("from", length(cutoff) -1), cutoff[1:length(cutoff)-1], 
              "to less than", cutoff[2:length(cutoff)]), paste("Above", cutoff[length(cutoff)])),
                   "Percentage"=round(tabtot, 2), "Std. err."= round(tabse,2), check.names=F)
    
    return(result)
  }
  
  # If by not supplied, calculate for the complete sample    
  if (missing(by)) { 
    output <- pv.ben.input(pvlabel=pvlabel, cutoff=cutoff, weight=weight, data=data)
  } else {
    output <- ddply(data, by, function(x) pv.ben.input(pvlabel=pvlabel, cutoff=cutoff, weight=weight, data=x))
  }
  
  if (export)  {
    write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
  }
  
  return(output)
}
