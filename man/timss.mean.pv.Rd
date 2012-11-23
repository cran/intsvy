\name{timss.mean.pv}
\alias{timss.mean.pv}
\title{Calculates mean performance}
\description{
timss.mean.pv uses five plausible values to calculate mean performance and its standard error}
\usage{
timss.mean.pv(pvlabel, by, data)
}
\arguments{
  \item{pvlabel}{
The label corresponding to the achievement variable, e.g., "BSMMAT" for overall reading performance.
}
  \item{by}{
The variable label defining the grouping, usually the countries (by="IDCNTRY"), but could be any other categorical variable. 
}
  \item{data}{
An R object, normally a data frame, containing the data from PIRLS.
}
}
\value{
timss.mean.pv returns a data frame with the mean and standard errors.
}
  \author{
Daniel Caro
}

\seealso{
pirls.mean.pv
}
\examples{
\dontrun{
## Exhibit 2.8, p.22 User Guide 2007
timss.mean.pv(pvlabel="BSMMAT", by=c("IDCNTRYL", "ITSEX"), data=timss.data)}




## The function is currently defined as
function (pvlabel, by, data) 
{
    pv.input <- function(pvlabel = "ASRREA", data) {
        pvnames <- paste(pvlabel, "0", 1:5, sep = "")
        read1m <- sapply(1:75, function(x) weighted.mean(data[[pvnames[[1]]]], 
            ifelse(data[["JKZONE"]] == x, 2 * data[["TOTWGT"]] * 
                data[["JKREP"]], data[["TOTWGT"]]), na.rm = TRUE))
        readm <- sapply(pvnames, function(x) weighted.mean(data[[x]], 
            data[["TOTWGT"]], na.rm = TRUE))
        varw <- sum((read1m - readm[1])^2)
        varb <- (1 + 1/5) * var(readm)
        readse <- (varw + varb)^(1/2)
        result <- data.frame(Mean = mean(readm), Std.err. = readse)
        return(round(result, 2))
    }
    if (missing(by)) {
        return(pv.input(pvlabel = pvlabel, data = data))
    }
    else {
        return(ddply(data, by, function(x) pv.input(data = x, 
            pvlabel = pvlabel)))
    }
  }
}

