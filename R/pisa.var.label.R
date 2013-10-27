pisa.var.label <-
function(folder=getwd(), name="Variable labels", output=getwd()) {
  
  # Retrieve file name
  files.all <- lapply(c("INT_ST", "INT_PA", "INT_SC"), function(x) list.files(folder, 
        full.names= TRUE, pattern=paste("^", x, ".*.sav$", sep=""), recursive=TRUE))
    
  if (sum(sapply(files.all, length))==0){
    stop(paste("cannot locate the original files in", folder))
  }
  
  # Add names to list
  list.name <- substr(files.all, nchar(files.all) - 18, nchar(files.all) - 13)
  names(files.all) <- file.names[file.names[["Abv"]] %in% list.name, "Instrument"]
  
  # Retrieve var labels
  var.label <- lapply(files.all, function(x) description(spss.system.file(x[[1]])))
  
  # Participating countries (from school file)
  country <- unique(as.data.frame(adj.measlev(spss.system.file(grep("INT_SC", files.all, value=T))[, c("CNT", "COUNTRY")])))
  
  # Participating countries in dataset (must be all)
  country.list <- pisa.country[pisa.country[["ISO"]] %in% country$CNT, ]
  rownames(country.list) <-NULL
  
  # setdiff(country$CNT, pisa.country$ISO) must be zero
    
  var.label[[length(files.all)+1]] <- country.list
  names(var.label)[length(var.label)] <-"Participating countries"
    
  # Print labels in list and text file
  capture.output(var.label, file=file.path(output, paste(name, ".txt", sep="")))
  cat('The file "', paste(name, ".txt", sep=""), '" in directory "', output, '" contains the variable labels of the complete dataset', sep=' ', "\n")
  return(var.label)
}
