\name{intsvy.ben.pv}
\alias{intsvy.ben.pv}
\title{Performance international benchmarks and proficiency levels}
\description{
intsvy.ben.pv calculates the percentage of students performing at or above the cut-off points (scores) given by the useR. The default are the benchmarks established by official reports.
}
\usage{
intsvy.ben.pv(pvnames,  by, cutoff, data, atlevel=FALSE, export = FALSE, name = "output", 
  folder = getwd(), config)
}

\arguments{
  \item{pvnames}{
The names of columns corresponding to the achievement plausible scores, for example, paste0("PV",1:10,"MATH") for PISA
}
  \item{cutoff}{
The cut-off points for the assessment benchmarks (e.g., cutoff= c(357.77, 420.07, 482.38, 544.68, 606.99, 669.30)). }
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="IDCNTRYL"), but could be any other categorical variable. 	
}
  \item{data}{
An R object, normally a data frame, containing the data from PIRLS.
}
 \item{atlevel}{
A logical value. If TRUE, percentages at each level are calculated. Otherwise (FALSE), percentages at or above levels are reported.
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
  \item{config}{
Object with configuration of a given study. Should contain the slot `prefixes` with prefixes of filenames with the student, home, school, and teacher data.
}
}

\value{
pirls.ben.pv returns a data frame with the percentage of students at or above the benchmark and the corresponding standard error.
}

\seealso{
timss.ben.pv, pirls.ben.pv, pisa.ben.pv
}
\examples{
\dontrun{
pisa.ben.pv(pvlabel= paste0("PV",1:10,"MATH") for PISA, by="CNT", 
data=pisa, atlevel = TRUE)

intsvy.ben.pv(pvnames= paste0("PV",1:10,"MATH") for PISA by="CNT", 
data=pisa, atlevel= TRUE, config=pisa_conf)

piaac.ben.pv(pvlabel= paste0("PVLIT", 1:10), by="CNTRYID", data=piaac)

intsvy.ben.pv(pvnames= paste0("PVLIT", 1:10), by="CNTRYID", data=piaac, 
config=piaac_conf)

timss.ben.pv(pvlabel= paste0("BSMMAT0", 1:5), by="IDCNTRYL", data=timss4)

intsvy.ben.pv(pvnames= paste0("BSMMAT0", 1:5), by="IDCNTRYL", data=timss4, 
config=timss4_conf)
}
}