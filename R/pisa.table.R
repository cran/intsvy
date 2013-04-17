pisa.table <-
function(variable, by, data, export=FALSE, name= "output", folder=getwd()) {
  table.input <- function(variable, data) {
    # Replicate weighted %s (sampling error)
    tabrp <- sapply(1:80, function(i) percent(as.factor(as.numeric(data[[variable]])), total=FALSE, 
             weights=  data[[paste("W_FSTR", i , sep="")]], na.rm=T))     
    
    # Total weighted %                                                                      
    tabtot <- round(percent(as.factor(as.numeric(data[[variable]])), weights= data[["W_FSTUWT"]], na.rm = TRUE, total=F), 2)
    # Standard error
    tabse <- round(sapply(1:length(table(as.numeric(data[[variable]]))), function(x)
      (0.05*sum(sapply(1:80, function(y) (tabrp[x,y]-tabtot[[x]])^2)))^(1/2)), 2)
    result <- data.frame(table(as.numeric(data[[variable]])), "Percentage"=as.numeric(tabtot), "Std.err."= tabse)
    names(result)[1] <- variable # var label for table, otherwise prints "Var1"
    return(result)
  }
  # If by not supplied, calculate for complete sample    
  if (missing(by)) { 
    output <- table.input(variable=variable, data=data)
  } else {
    for (i in by) {
      data[[c(i)]] <- as.character(data[[c(i)]])
    }
    output <- ddply(data, by, function(x) table.input(data=x, variable=variable))
  }
  
  if (export)  {
    write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
  }
  
  return(output)
}
