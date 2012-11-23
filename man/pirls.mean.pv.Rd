\name{pirls.mean.pv}
\alias{pirls.mean.pv}
\title{Calculates mean performance}
\description{
pirls.mean.pv uses five plausible values to calculate mean performance and its standard error}
\usage{
pirls.mean.pv(pvlabel, by, data)
}
\arguments{
  \item{pvlabel}{
The label corresponding to the achievement variable, e.g., "ASRREA" for overall reading performance.
}
  \item{by}{
The variable label defining the grouping, usually the countries (by="IDCNTRY"), but could be any other categorical variable. 
}
  \item{data}{
An R object, normally a data frame, containing the data from PIRLS.
}
}
\value{
pirls.mean.pv returns a data frame with the mean and standard errors.
}
\author{
Daniel Caro
}
\seealso{
timss.mean.pv, pisa.mean.pv
}
\examples{
\dontrun{
## Replicating Exhibit 3.10 in User Guide 2006, p.64
table(as.numeric(pirls.data$ASBGTOC5), pirls.data$ASBGTOC5)
pirls.data$NEWTOCS[as.numeric(pirls.data$ASBGTOC5) ==1 ] <- "EVERY DAY OR ALMOST EVERY DAY"
pirls.data$NEWTOCS[as.numeric(pirls.data$ASBGTOC5) ==2 ] <- "ONCE OR TWICE A WEEK"
pirls.data$NEWTOCS[as.numeric(pirls.data$ASBGTOC5) ==3 | as.numeric(pirls.data$ASBGTOC5) ==4 ] <- "TWICE A MONTH OR LESS"

table(pirls.data$NEWTOCS)

pirls.mean.pv(pvlabel="ASRREA", by=c("IDCNTRYL", "NEWTOCS"), data=pirls.data)}


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

