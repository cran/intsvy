\name{pisa.ben.pv}
\alias{pisa.ben.pv}
\title{PISA proficiency levels}
\description{Calculates percentage of students at each proficiency level defined by PISA. Or at proficiency levels provided by the useR.
}
\usage{
pisa.ben.pv(pvlabel, cutoff = c(262.04, 334.75, 407.47, 480.18, 552.89, 625.61, 698.32), by, weight= "W_FSTUWT", data, export = FALSE, name = "output", folder = getwd())
}
\arguments{
  \item{pvlabel}{
The label corresponding to the achievement variable, for example, "READ", for overall reading performance.
}
  \item{cutoff}{
The cut-off points for calculating the percentage of students. The default are the proficiency levels in PISA
}
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="IDCNTRYL"), but could be any other categorical variable. 	
}
  \item{weight}{
The label for total weight, "W_FSTUWT", in PISA. 	
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
pisa.ben.pv returns a data frame with the percentage of students at each proficiency level and its corresponding standard error.
}

\seealso{
timss.ben.pv, pirls.ben.pv
}
\examples{
\dontrun{
# Table I.2.1 p.194 International Report 2009 Volume I
pisa.ben.pv(pvlabel="READ", by="IDCNTRYL", data=pisa)
}
}
