timss.table <-
function(variable, by, data, weight="TOTWGT", export=FALSE, name= "output", folder=getwd()) {
  table.input <- function(variable, data, weight) {
    # Replicate weighted %s (sampling error)
    tabrp <- sapply(1:max(data[["JKZONE"]]), function(x) percent(data[[variable]], total=FALSE, weights= ifelse(data[["JKZONE"]] == x, 
              2*data[[weight]]*data[["JKREP"]], data[[weight]]), na.rm = TRUE))
    
    # Total weighted %                                                                      
    tabtot <- percent(data[[variable]], weights= data[[weight]], na.rm = TRUE, total=F)
    # Standard error
    tabse <- sapply(1:length(table(data[[variable]])), function(x)
      sum(sapply(1:max(data[["JKZONE"]]), function(y) (tabrp[x,y]-tabtot[[x]])^2))^(1/2))
    result <- data.frame(table(data[[variable]]), "Percentage"=round(as.numeric(tabtot), 2), 
              "Std.err."= round(tabse, 2))
    names(result)[1] <- variable # var label for table, otherwise prints "Var1"
    return(result)
  }
  # If by not supplied, calculate for complete sample    
  if (missing(by)) { 
    output <-table.input(variable=variable, weight=weight, data=data)
  } else {
    output <- ddply(data, by, function(x) table.input(data=x, weight=weight, variable=variable))
  }
  
  if (export)  {
    write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
  }
  
  return(output)
}
