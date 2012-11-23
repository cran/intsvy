\name{timss.ben.pv}
\alias{timss.ben.pv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates percentage of students at or above a benchmark
}
\description{
timss.ben.pv calculates the percentage of students performing at or above the levels of achievement given by the useR. It produces the results for the international PIRLS/TIMSS benchmarks by default
}
\usage{
timss.ben.pv(pvlabel, cutoff = c(400, 475, 550, 625), by, data)
}
\arguments{
  \item{pvlabel}{
The label corresponding to the achievement variable, e.g., "BSMMAT" for overall reading performance.
}
  \item{cutoff}{
The cut-off points or benchmarks for calculating the percentage of students. The default are the PIRLS/TIMSS international benchmarks: cutoff = c(400, 475, 550, 625)
}
  \item{by}{
The variable label defining the grouping, usually the countries (by="IDCNTRY"), but could be any other categorical variable. 	
}
  \item{data}{
An R object, normally a data frame, containing the data from TIMSS.
}
}
\value{
timss.ben.pv returns a data frame with the percentage of students at or above the benchmark and the corresponding standard error.
}
\author{
Daniel Caro
}

\seealso{
pirls.ben.pv
}
\examples{
\dontrun{
# Exhibit 2.16, p31 User Guide
timss.ben.pv(pvlabel="BSMMAT", by="IDCNTRYL", data=timss.data)}



## The function is currently defined as
function (pvlabel, cutoff = c(400, 475, 550, 625), by, data) 
{
    pv.ben.input <- function(pvlabel, cutoff, data) {
        pvname <- paste(pvlabel, "0", 1:5, sep = "")
        tabpv1 <- sapply(1:length(cutoff), function(z) sapply(1:75, 
            function(x) percent(data[[pvname[1]]] >= cutoff[z], 
                weights = ifelse(data[["JKZONE"]] == x, 2 * data[["TOTWGT"]] * 
                  data[["JKREP"]], data[["TOTWGT"]]), na.rm = TRUE, 
                total = F)))
        tabpv <- sapply(1:length(cutoff), function(z) sapply(pvname, 
            function(x) percent(data[[x]] >= cutoff[z], weights = data[["TOTWGT"]], 
                na.rm = TRUE, total = F)))
        tabpvw <- sapply(1:length(cutoff), function(y) sum(sapply(1:75, 
            function(x) (tabpv1[x, y] - tabpv[1, y])^2)))
        tabpvb <- (1 + 1/5) * apply(tabpv, 2, var)
        tabse <- round((tabpvw + tabpvb)^(1/2), 2)
        tabtot <- round(apply(tabpv, 2, mean), 2)
        result <- data.frame(Benchmark = paste(rep("At or above", 
            length(cutoff)), cutoff), Percentage = tabtot, `Std. err.` = tabse, 
            check.names = F)
        return(result)
    }
    if (missing(by)) {
        return(pv.ben.input(pvlabel = pvlabel, cutoff = cutoff, 
            data = data))
    }
    else {
        return(ddply(data, by, function(x) pv.ben.input(pvlabel = pvlabel, 
            cutoff = cutoff, data = x)))
    }
  }
}

