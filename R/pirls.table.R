pirls.table <-
function(variable, by, data) {
  table.input <- function(variable, data) {
  # Replicate weighted %s (sampling error)
  tabrp <- sapply(1:75, function(x) percent(data[[variable]], total=F, weights= ifelse(data[["JKZONE"]] == x, 
           2*data[["TOTWGT"]]*data[["JKREP"]], data[["TOTWGT"]]), na.rm = TRUE))
  
  # Total weighted %                                                                      
  tabtot <- round(percent(data[[variable]], weights= data[["TOTWGT"]], na.rm = TRUE, total=F), 3)
  # Standard error
  tabse <- round(sapply(1:length(table(data[[variable]])), function(x)
  sum(sapply(1:75, function(y) (tabrp[x,y]-tabtot[[x]])^2))^(1/2)), 3)
  result <- data.frame(table(data[[variable]]), "Percentage"=as.numeric(tabtot), "Std.err."= tabse)
  names(result)[1] <- variable # var label for table, otherwise prints "Var1"
  return(result)
  }
# If by not supplied, calculate for complete sample    
if (missing(by)) { 
 return(table.input(variable=variable, data=data))
 } else {
return(ddply(data, by, function(x) table.input(data=x, variable=variable)))
 }
}
