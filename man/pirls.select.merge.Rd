\name{pirls.select.merge}
\alias{pirls.select.merge}
\title{Select and merge data}
\description{pirls.select.merge selects and merges data from PIRLS 2006 downloaded from the IEA data repository
}
\usage{
pirls.select.merge(folder = getwd(), countries, student, home, school)
}
\arguments{
  \item{folder}{
Directory path where the data are located. The data could be organized within folders but it should not be duplicated.
}
  \item{countries}{
The selected countries, supplied with the abbreviation (e.g., countries=c("AUT", "BGR") or codes (countries=c(40, 100)). If no countries are selected, all are selected.
}
  \item{student}{
The data labels for the selected student variables. 
}
  \item{home}{
The data labels for the selected home background variables.
}
  \item{school}{
The data labels for the selected school variables.
}
}
\value{
pirls.select.merge returns a data frame with the selected data from PIRLS 2006.
}
\author{
Daniel Caro
}

\seealso{
timss.select.merge
}
\examples{
\dontrun{

## Using country abbreviations
pirls.data <- pirls.select.merge(folder= "/home/eldani/Work/R work/PIRLS/Data",
              countries= c("AUT", "BGR", "TWN", "ZAF", "ESP", "SWE", "TTO", "MKD", "USA"),
              student= c("ITSEX", "ITLANG", "ASBGBOOK", "ASDAGE", "ASDHHER", "ASBGTOC5", "ASDGTHC2", "ASBGTSP4"),
              home= c("ASDHEHLA", "ASDHEDUP", "ASDHPEMP", "ASDHPATR", "ASDHOCCP"),
              school= c("ACDGASR", "ACDGCMP", "ACDGPPSS"))

# Using country ISO codes
pirls.data <- pirls.select.merge(folder= "/home/eldani/Work/R work/PIRLS/Data",
              countries= c(276, 504, 554, 643, 752),
              student= c("ITSEX", "ITLANG", "IDBOOK", "ASDAGE", "ASDHHER", "ASDGTHC2"),
              home= c("ASDHEHLA", "ASDHEDUP", "ASDHPEMP", "ASDHPATR", "ASDHOCCP"),
              school= c("ACDGASR", "ACDGCMP", "ACDGPPSS"))

}


## The function is currently defined as
function (folder = getwd(), countries, student, home, school) 
{
    files.all <- lapply(c("asg", "ash", "acg"), function(x) list.files(folder, 
        full.names = T, pattern = paste("^", x, ".*.sav$", sep = ""), 
        recursive = T))
    cntlab <- toupper(unique(unlist(lapply(files.all, function(x) substr(x, 
        nchar(x) - 8, nchar(x) - 6)))))
   country.list <- country.ug[country.ug$ISO \%in\% intersect(country.ug$ISO, 
        cntlab), ]
    if (missing(countries)) {
        countries = cntlab
    }
    if (is.numeric(countries)) {
        countries = country.list[country.list$Code \%in\% countries, 
            "ISO"]
    }
    files.select <- lapply(files.all, function(y) sapply(countries, 
        function(x) y[substr(y, nchar(y) - 8, nchar(y) - 6) == 
            tolower(x)]))
    student.data <- do.call("rbind", lapply(lapply(files.select[[1]], 
        function(y) spss.system.file(y)), function(x) as.data.frame(x[, 
        c("IDCNTRY", "IDSCHOOL", "IDCLASS", "IDSTUD", "JKREP", 
            "JKZONE", "HOUWGT", "SENWGT", "TOTWGT", "ASRREA01", 
            "ASRREA02", "ASRREA03", "ASRREA04", "ASRREA05", "ASRINF01", 
            "ASRINF02", "ASRINF03", "ASRINF04", "ASRINF05", "ASRLIT01", 
            "ASRLIT02", "ASRLIT03", "ASRLIT04", "ASRLIT05", student)])))
    student.data$IDCNTRYL <- factor(student.data$IDCNTRY, levels = country.list[country.list$Code \%in\% 
        unique(student.data$IDCNTRY), "Code"], labels = country.list[country.list$Code \%in\% 
        unique(student.data$IDCNTRY), "Country"])
    home.data <- do.call("rbind", lapply(lapply(files.select[[2]], 
        function(y) spss.system.file(y)), function(x) as.data.frame(x[, 
        c("IDCNTRY", "IDSTUD", home)])))
    school.data <- do.call("rbind", lapply(lapply(files.select[[3]], 
        function(y) spss.system.file(y)), function(x) as.data.frame(x[, 
        c("IDCNTRY", "IDSCHOOL", "SCHWGT", school)])))
    pirls.student <- merge(student.data, home.data, by = c("IDCNTRY", 
        "IDSTUD"), suffixes = c(".st", ".hm"))
    pirls.all <- merge(pirls.student, school.data, by = c("IDCNTRY", 
        "IDSCHOOL"), suffixes = c(".st", ".sc"))
    return(pirls.all)
  }
}

