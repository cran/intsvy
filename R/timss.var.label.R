timss.var.label <-
function(folder=getwd()) {  
  
  # All file names of datasets in lists (student, home, school)
  files.all <- lapply(c("bsg", "bcg", "btm", "bts"), function(x) list.files(folder, 
                                                                            full.names= T, pattern=paste("^", x, ".*.sav$", sep=""), recursive=T))
  
  # Variable labels
  var.label <- list("Student background and achievement" = description(spss.system.file(files.all[[1]][1])), 
                    "School data" = description(spss.system.file(files.all[[2]][1])), 
                    "Math teacher"= description(spss.system.file(files.all[[3]][1])),
                    "Science teacher"= description(spss.system.file(files.all[[4]][1])))
  
  # Print labels in list and text file
  print(var.label)
  capture.output(var.label, file=file.path(folder,"Variable labels.txt"))
  cat("The file 'Variable labels.txt' in the directory", folder, "contains the variable labels of the complete dataset", sep=" ")
}
