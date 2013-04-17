\name{pisa.reg.pv}
\alias{pisa.reg.pv}
\title{Regression analysis with plausible values}
\description{
pisa.reg.pv performs standard linear regression analysis (OLS) while handling plausible values and replicate weights.
}
\usage{
pisa.reg.pv(x, pvlabel = "READ", by, data, export = FALSE, name = "output", folder = getwd())
}

\arguments{
  \item{x}{
Data labels of independent variables.
}
  \item{pvlabel}{
The label corresponding to the achievement variable, for example, "READ", for overall reading performance.
}
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="IDCNTRYL"), but could be any other categorical variable. 	
}
  \item{data}{
An R object, normally a data frame, containing the data from PISA.
}
  \item{export}{
A logical value. If TRUE, the output is exported to a file in comma-separated value format (.csv) that can be opened from LibreOffice or Excel. 
}
  \item{name}{
The name of the exported file.
}
  \item{folder}{
The folder where the exported file is located.
}
}

\value{
pisa.reg.pv returns a data frame with coefficients, standard errors and t-values. If "by" is specified, results are reported in a list. 
}

\seealso{
timss.reg.pv, pirls.reg.pv
}
\examples{
\dontrun{
pisa.mean.pv(pvlabel = "READ", by = c("IDCNTRYL", "ST04Q01"), data = pisa)
}
}

