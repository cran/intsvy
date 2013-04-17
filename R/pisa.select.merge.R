pisa.select.merge <-
function(folder=getwd(), countries, student=c(), parent, school) {
  
  if (missing(student) & missing(parent) & missing(school)) {
    stop("no variables are selected")
    }
  
  files.all <- lapply(c("INT_ST", "INT_PA", "INT_SC"), function(x) list.files(folder, 
               full.names= TRUE, pattern=paste("^", x, ".*.sav$", sep=""), recursive=TRUE))
  
  if (sum(sapply(files.all, length))==0){
    stop(paste("cannot locate the original files in", folder))
  }
  
  list.name <- substr(files.all, nchar(files.all) - 14, nchar(files.all) - 10)
  names(files.all) <- list.name
  
  if (missing(countries)) {
    countries <- unique(spss.system.file(files.all[["STQ09"]])[, c("CNT")][])
  }
    
  
  # Participating countries
  
  country <- as.data.frame(adj.measlev(spss.system.file(files.all[[3]])[, c("CNT", "COUNTRY")]))
  
  # Participating countries in dataset (must be all)
  country.list <- pisa.country[pisa.country$ISO %in% unique(country$CNT) , ]
  rownames(country.list) <-NULL
    
  # Have to use spss.system.file, otherwise read.spss crashes
  
  # Student data (need for school and parent data too)
  
  if (!missing(student) | !missing(parent)) {
  
  if (is.null(files.all[["STQ09"]])) {
    stop("cannot locate student data file")
  }
    
     
  pisa.student <- spss.system.file(files.all[["STQ09"]])
  names(pisa.student) <- toupper(names(pisa.student))
  
  student.data <- pisa.student[pisa.student["CNT"] %in% countries, 
                  c("CNT", "COUNTRY", "OECD", "SCHOOLID", "STIDSTD",  
                   names(pisa.student)[grep("^PV", names(pisa.student))], student,
                   names(pisa.student)[grep("^W_F", names(pisa.student))])]
  
  student.data <- as.data.frame(adj.measlev(student.data))
  
  }
  
  
  # Parental questionnaire
  
  if (!missing(parent)) {
    
    if (is.null(files.all[["PAQ09"]])) {
      stop("cannot locate parental questionnaire data file")
    }
    
    
    pisa.parent <- spss.system.file(files.all[["PAQ09"]])
    names(pisa.parent) <- toupper(names(pisa.parent))
    
    parent.data <- pisa.parent[pisa.parent["CNT"] %in% countries, 
                                 c("CNT", "COUNTRY", "OECD", "SCHOOLID", "STIDSTD", parent)]
    
    parent.data <- as.data.frame(adj.measlev(parent.data))
    
  }
  
  
  # School data
  
  if (!missing(school)) {
    
    if (is.null(files.all[["SCQ09"]])) {
      stop("cannot locate school data file")
    }
    
    
    pisa.school <- spss.system.file(files.all[["SCQ09"]])
    names(pisa.school) <- toupper(names(pisa.school))
    
    school.data <- pisa.school[pisa.school["CNT"] %in% countries, 
                   c("CNT", "COUNTRY", "OECD", "SCHOOLID", school, "W_FSCHWT")]
    
    school.data <- as.data.frame(adj.measlev(school.data))
    
  }
  
  
  # Merging data depending on existing datasets/arguments
  
  # Student data available
  
  if (!missing(student) & missing(parent) & missing(school)) {
    pisa.all <- student.data
  }
  
  if (!missing(student) & !missing(parent) & missing(school)) {
    pisa.all <- merge(student.data, parent.data, all.x=TRUE, by=c("COUNTRY", "SCHOOLID", "STIDSTD"))
    pisa.all <- pisa.all[, -c(grep("*.y", names(pisa.all)))]
    names(pisa.all) <- gsub("*.x", "", names(pisa.all))
  }
  
  if (!missing(student) & missing(parent) & !missing(school)) {
    pisa.all <- merge(student.data, school.data, all.x=TRUE, by=c("COUNTRY", "SCHOOLID"))
    pisa.all <- pisa.all[, -c(grep("*.y", names(pisa.all)))]
    names(pisa.all) <- gsub("*.x", "", names(pisa.all))
  }
  
  if (!missing(student) & !missing(parent) & !missing(school)) {
    pisa.all <- merge(student.data, parent.data, all.x=TRUE, by=c("COUNTRY", "SCHOOLID", "STIDSTD"))
    pisa.all <- merge(pisa.all, school.data, by=c("COUNTRY", "SCHOOLID"))
    pisa.all <- pisa.all[, -c(grep("*.y", names(pisa.all)))]
    names(pisa.all) <- gsub("*.x", "", names(pisa.all))
  }
  
  # Parent data available
  
  if (missing(student) & !missing(parent) & missing(school)) {
    pisa.all <- merge(student.data, parent.data, by=c("COUNTRY", "SCHOOLID", "STIDSTD"))
    pisa.all <- pisa.all[, -c(grep("*.y", names(pisa.all)))]
    names(pisa.all) <- gsub("*.x", "", names(pisa.all))
  }
  
  if (missing(student) & !missing(parent) & !missing(school)) {
    pisa.all <- merge(school.data, parent.data, by=c("COUNTRY", "SCHOOLID"))
    pisa.all <- pisa.all[, -c(grep("*.y", names(pisa.all)))]
    names(pisa.all) <- gsub("*.x", "", names(pisa.all))
  }
  
  # School data available
  
  if (missing(student) & missing(parent) & !missing(school)) {
    pisa.all <- school.data
  }
  
  
  # Create country label variable (not possible to add labels to numeric factor, see to do list)
  pisa.all$IDCNTRYL <- factor(pisa.all$CNT,  
             levels=country.list[country.list$ISO %in% unique(pisa.all$CNT), "ISO"] ,        
             labels= country.list[country.list$ISO %in% unique(pisa.all$CNT), "Country"])
  
  return(pisa.all)
}
