\name{pisa.select.merge}
\alias{pisa.select.merge}
\title{
Select and merge data
}
\description{
pisa.select.merge selects and merges data from TIMSS G4.
}
\usage{
pisa.select.merge(folder = getwd(), countries, student = c(), parent, school)
}
\arguments{
  \item{folder}{
Directory path where the data are located. The data could be organized within folders but it should not be duplicated.
}
  \item{countries}{
The selected countries, supplied with the abbreviation (e.g., countries=c("DEU", "NOR"). If no countries are selected, all are selected.
}
  \item{student}{
The data labels for the selected student variables. 
}
  \item{parent}{
The data labels for the selected parental variables.
}
  \item{school}{
The data labels for the selected school variables.
}
}

\value{
pisa.select.merge returns a data frame with the selected data from PISA.
}

\seealso{
timssg4.select.merge, timssg8.select.merge, pirls.select.merge
}
\examples{
\dontrun{
pisa <- pisa.select.merge(folder=getwd(), 
                          countries = c("DEU", "NOR", "GBR", "SGP"), 
                          student = c("HOMEPOS", "ESCS", "PARED", "HISEI", "ST01Q01", "ST04Q01", "ST20Q10"),
                          parent = c("READRES", "PARINVOL", "CURSUPP"), 
                          school = c("RESPCURR", "RESPRES",  "SCMATEDU", "STUDBEHA")))
}
}

