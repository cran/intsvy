\name{pirls.log}
\alias{pirls.log}
\title{Logistic regression analysis}
\description{pirls.log performs logistic regression analysis for an observed depedent variable 
(NOT for plausible values)
}
\usage{
pirls.log(y, x, by, data, export = FALSE, 
name = "output", folder = getwd())
}
\arguments{
  \item{y}{
Label for dependent variable
}
  \item{x}{
Data labels of independent variables (e.g., x = c("ASDHEHLA", "ITSEX") ).
}
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="IDCNTRYL"), but could be 
any other categorical variable.   
}
  \item{data}{
An R object, normally a data frame, containing the data from PIRLS.
}
  \item{export}{
A logical value. If TRUE, the output is exported to a file in comma-separated value format (.csv) 
that can be opened from LibreOffice or Excel. 
}
  \item{name}{
The name of the exported file.
}
  \item{folder}{
The folder where the exported file is located.
}
}

\value{
pirls.log prints a data frame with coefficients, standard errors, t-values, and odds ratios. 
Results are stored in a list object of class "intsvy.reg". 
}

\seealso{
timss.log, pisa.log
}
\examples{
\dontrun{

pisa$SKIP[!(pisa$ST09Q01 =="None" & pisa$ST115Q01 == "None")] <- 1
pisa$SKIP[pisa$ST09Q01 =="None" & pisa$ST115Q01 == "None"] <- 0

pisa$LATE[!pisa$ST08Q01=="None"] <- 1
pisa$LATE[pisa$ST08Q01=="None"] <- 0

pisa.log(y="SKIP", x="LATE", by="IDCNTRYL", data = pisa)

}}