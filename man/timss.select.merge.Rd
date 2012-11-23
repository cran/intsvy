\name{timss.select.merge}
\alias{timss.select.merge}
\title{Select and merge data}
\description{
timss.select.merge selects and merges data from TIMSS 2007 downloaded from the IEA data repository
}
\usage{
timss.select.merge(folder = getwd(), countries, student, school, math.teacher, science.teacher)
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
  \item{school}{
The data labels for the selected school variables.
}
  \item{math.teacher}{
The data labels for the math teacher data.
}
  \item{science.teacher}{
The data labels for the science teacher data.
}
}
\value{
timss.select.merge returns a data frame with the selected data from PIRLS 2006.
}

\author{
Daniel Caro
}


\seealso{
pirls.select.merge
}
\examples{
\dontrun{

timss.data <- timss.select.merge(folder="/home/eldani/Work/R work/TIMSS/Data",
              student =c("BSDGEDUP", "ITSEX", "BSDAGE"), countries=c("DZA", "ARM", "AUS", "BHR"),
              school =c("BCDSRMI", "BCDGPPSC"))
}



## The function is currently defined as
function (folder = getwd(), countries, student, school, math.teacher, 
    science.teacher) 
{
    files.all <- lapply(c("bsg", "bcg", "btm", "bts"), function(x) list.files(folder, 
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
    student.data <- do.call("rbind", lapply(files.select[[1]], 
        function(y) read.spss(y, use.value.labels = F, to.data.frame = T)[c("IDCNTRY", 
            "IDSCHOOL", "IDCLASS", "IDSTUD", "JKREP", "JKZONE", 
            "HOUWGT", "SENWGT", "TOTWGT", "BSMMAT01", "BSMMAT02", 
            "BSMMAT03", "BSMMAT04", "BSMMAT05", "BSSSCI01", "BSSSCI02", 
            "BSSSCI03", "BSSSCI04", "BSSSCI05", student)]))
    student.data$IDCNTRYL <- factor(student.data$IDCNTRY, levels = country.list[country.list$Code \%in\% 
        unique(student.data$IDCNTRY), "Code"], labels = country.list[country.list$Code \%in\% 
        unique(student.data$IDCNTRY), "Country"])
    school.data <- do.call("rbind", lapply(files.select[[2]], 
        function(y) read.spss(y, use.value.labels = F, to.data.frame = T)[c("IDCNTRY", 
            "IDSCHOOL", "SCHWGT", school)]))
    timss.all <- merge(student.data, school.data, by = c("IDCNTRY", 
        "IDSCHOOL"), suffixes = c(".st", ".sc"))
    return(timss.all)
  }
}

