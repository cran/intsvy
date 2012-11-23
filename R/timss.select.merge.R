timss.var.label <- function(folder=getwd()) {  
  
  # All file names of datasets in lists (student, home, school)
  files.all <- lapply(c("bsg", "bcg", "btm", "bts"), function(x) list.files(folder, 
                                                                            full= T, pattern=paste("^", x, ".*.sav$", sep=""), recursive=T))
  
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

# Data select and merge
timss.select.merge <- function(folder=getwd(), countries, student, school, math.teacher, science.teacher) {
  
  # All file names of datasets in lists (student, home, school)
  files.all <- lapply(c("bsg", "bcg", "btm", "bts"), function(x) list.files(folder, 
                                                                            full.names= T, pattern=paste("^", x, ".*\\.sav$", sep=""), recursive=T))
  
  # Country abbrevation labels from existing file names (datasets)
  cntlab <- toupper(unique(unlist(lapply(files.all, function(x) substr(x, nchar(x)-8, nchar(x)-6))))) 
  
  # External data (locate in data folder) from userguide: Georgia ISO FEO was incorrect and changed to GEO
  
  # setdiff(cntlab, country.ug$ISO) needs be zero! all elements in data labels are in userguide
  
  # Countries in the datasets and userguide
  country.list <- country.ug[country.ug$ISO %in% intersect(country.ug$ISO, cntlab), ]
  
  
  # If no countries are selected: seleect all
  if (missing(countries)) { 
    countries=cntlab
  }
  
  # If countries are entered numerically, change to ISO labels for file selection (next)
  if (is.numeric(countries)) {
    countries=country.list[country.list$Code %in% countries, "ISO"]
  }
  
  # Selected files for selected countries (1st/last time countries argument used)
  files.select <- lapply(files.all, function(y) sapply(countries, function(x) y[substr(y, 
                                                                                       nchar(y)-8, nchar(y)-6)==tolower(x)])) 
  
  # Student achievement and background data
  student.data <- do.call("rbind",                                               # Merge [[1]] student
                  lapply(files.select[[1]], function(y) 
                  read.spss(y, use.value.labels=F, to.data.frame=T)
                 [c("IDCNTRY", "IDSCHOOL", "IDCLASS", "IDSTUD", "JKREP",         # IDs/Weights/PVs
                  "JKZONE", "HOUWGT", "SENWGT", "TOTWGT",      
                  "BSMMAT01", "BSMMAT02", "BSMMAT03", "BSMMAT04", "BSMMAT05",                               
                  "BSSSCI01", "BSSSCI02", "BSSSCI03", "BSSSCI04", "BSSSCI05", student)]))
  
  # memisc creates NAs, also read.spss with labels
  
  
  # Create country label variable (not possible to add labels to numeric factor, see to do list)
  student.data$IDCNTRYL <- factor(student.data$IDCNTRY,  
                levels=country.list[country.list$Code %in% unique(student.data$IDCNTRY), "Code"] ,        
                labels= country.list[country.list$Code %in% unique(student.data$IDCNTRY), "Country"])
  
  # table(student.data$IDCNTRYL, student.data$IDCNTRY) test of equality!
  
  # School data
  school.data <- do.call("rbind",                                                 # Merge [[2]] school
        lapply(files.select[[2]], function(y) 
        read.spss(y, use.value.labels=F, to.data.frame=T)[c("IDCNTRY", "IDSCHOOL", "SCHWGT", school)]))                                                                     # Selected
  
  
  # We convert data.set to data.frame for "rbind", with that, we lose data.set functionalities like 
  # codebook, show, etc, but we gain merging functions both rbind and merge, which do not work for
  # data.sets.
  
  # Merge 
  timss.all <- merge(student.data, school.data, by=c("IDCNTRY", "IDSCHOOL"), suffixes=c(".st", ".sc") )
  
  # Merge test
  
  # lapply(split(mergestsc, mergestsc[c("IDCNTRY", "IDSCHOOL")]), function(x) var(x$BCDSRMI, na.rm=T))
  
  # Math teacher data
  #math.data <- do.call("rbind",                                                    # Merge [[2]] school
  #lapply(files.select[[3]], function(y) 
  #read.spss(y, use.value.labels=F, to.data.frame=T)[c("IDCNTRY", "IDSCHOOL", 
  #"ITCOURSE", "IDTEACH", "IDLINK", "IDGRADE", math.teacher)]))                                                                     # Selected
  
  # Science teacher
  #science.data <- do.call("rbind",                                                 # Merge [[2]] school
  #lapply(files.select[[3]], function(y) 
  #read.spss(y, use.value.labels=F, to.data.frame=T)[c("IDCNTRY", "IDSCHOOL", science.teacher)]))                                                                     # Selected
  
  # Merging
  #mergesm <- merge(mergestsc, math.data, by=c("IDCNTRY", "IDSCHOOL"))
  #timss.all <- merge(mergesm, science.data, by=c("IDCNTRY", "IDSCHOOL"))
  
  return(timss.all)
}


