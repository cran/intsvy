pirls.select.merge <- function(folder=getwd(), countries, student, home, school) {
  
  # All file names of datasets in lists (student, home, school)
  files.all <- lapply(c("asg", "ash", "acg"), function(x) list.files(folder, 
               full.names= T, pattern=paste("^", x, ".*.sav$", sep=""), recursive=T))
  
  # Country abbrevation labels from existing file names (datasets)
  cntlab <- toupper(unique(unlist(lapply(files.all, function(x) substr(x, nchar(x)-8, nchar(x)-6))))) 
  
  # External data (locate in data folder) from userguide: Georgia ISO FEO was incorrect and changed to GEO

  # setdiff(cntlab, country$ISO) needs be zero! all elements in data labels are in userguide
  
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
  student.data <- do.call("rbind",                                          # Merge [[1]] student
  lapply(lapply(files.select[[1]], function(y) spss.system.file(y)),        # Each dataset
  function(x) as.data.frame(x[ , c(                                         # Variable selection
  "IDCNTRY", "IDSCHOOL", "IDCLASS", "IDSTUD", "JKREP",                      # IDs/Weights/PVs
  "JKZONE", "HOUWGT", "SENWGT", "TOTWGT",      
  "ASRREA01", "ASRREA02", "ASRREA03", "ASRREA04", "ASRREA05",                               
  "ASRINF01", "ASRINF02", "ASRINF03", "ASRINF04", "ASRINF05",
  "ASRLIT01", "ASRLIT02", "ASRLIT03", "ASRLIT04", "ASRLIT05",
   student)])))                                                              # Selected
  
  
  # Create country label variable (not possible to add labels to numeric factor, see to do list)
  student.data$IDCNTRYL <- factor(student.data$IDCNTRY,  
  levels=country.list[country.list$Code %in% unique(student.data$IDCNTRY), "Code"] ,        
  labels= country.list[country.list$Code %in% unique(student.data$IDCNTRY), "Country"])
  
  # table(student.data$IDCNTRYL, student.data$IDCNTRY) test of equality!
  
  # Home background data
  home.data <- do.call("rbind",                                                   # Merge [[2]] home
  lapply(lapply(files.select[[2]], function(y) spss.system.file(y)),              # Each dataset
  function(x) as.data.frame(x[ , c(                                               # Variable selection
  "IDCNTRY", "IDSTUD",                                                            # IDs
  home)])))                                                                       # Selected
  
  # School data
  school.data <- do.call("rbind",                                                 # Merge [[3]] school
  lapply(lapply(files.select[[3]], function(y) spss.system.file(y)),              # Each dataset
  function(x) as.data.frame(x[, c(                                                # Variable selection
  "IDCNTRY", "IDSCHOOL", "SCHWGT",                                                # IDs/Weight
  school)])))                                                                     # Selected
  
  # We convert data.set to data.frame for "rbind", with that, we lose data.set functionalities like 
  # codebook, show, etc, but we gain merging functions both rbind and merge, which do not work for
  # data.sets.
  
  # Merge 
  pirls.student <- merge(student.data, home.data, by=c("IDCNTRY", "IDSTUD"), suffixes=c(".st", ".hm"))
  pirls.all <- merge(pirls.student, school.data, by=c("IDCNTRY", "IDSCHOOL"), suffixes=c(".st", ".sc") )
  
  # Merge test
  
  # data.sorted <- pirls.all[order(IDCNTRY, IDSTUD), ]
  # junk1 <- by(data.sorted, paste(data.sorted$IDCNTRY, data.sorted$IDSCHOOL),  function(x) 
  # c(sd(as.numeric(x$ACDGASR), na.rm=T), sd(as.numeric(x$ACDGCMP), na.rm=T), 
  # sd(as.numeric(x$ACDGPPSS), na.rm=T)))
  # sum(sapply(junk1, function(x) sum(x)), na.rm=T)==0
  # SUCCESSFUL!, school variables are constant within schools
  
  return(pirls.all)
}
