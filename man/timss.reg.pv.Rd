\name{timss.reg.pv}
\alias{timss.reg.pv}
\title{Regression analysis with plausible values}
\description{
timss.reg.pv performs linear regression analysis (OLS) with plausible values and replicate weights.
}
\usage{
timss.reg.pv(x, pvlabel, by, 
data, std=FALSE, export = FALSE, name = "output", folder = getwd())
}

\arguments{
  \item{x}{
Data labels of independent variables.
}
  \item{pvlabel}{
The names of columns corresponding to the achievement plausible scores.
}
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="IDCNTRYL"), but could be any other categorical variable. 	
}
  \item{data}{
An R object, normally a data frame, containing the data from TIMSS.
}
  \item{std}{
A logical value. If TRUE standardised regression coefficients are calculated.
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
timss.reg.pv prints a data.frame with regression results (i.e., coefficients, standard errors, t-values, R-squared) and stores  different regression output including residuals, replicate coefficients, variance within and between, and the regression data.frame in a list object of class "intsvy.reg". 
}

\seealso{
pirls.reg.pv, pisa.reg.pv
}
\examples{
\dontrun{
timss8g$SEX[timss8g$ITSEX=="BOY"]=1
timss8g$SEX[timss8g$ITSEX=="GIRL"]=0
timss.reg.pv(pvlabel= paste0("BSMMAT0", 1:5), by=c("IDCNTRYL"), x="SEX", data=timss8g)
}
}
