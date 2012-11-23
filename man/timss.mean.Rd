\name{timss.mean}
\alias{timss.mean}
\title{
Calculates the mean
}
\description{
Calculates the mean for an observed variable (NOT one with plausible values).
}
\usage{
timss.mean(variable, by, data)
}
\arguments{
  \item{variable}{
A variable label. The mean is calculated for this variable.
}
  \item{by}{
The variable label defining the grouping, usually the countries (by="IDCNTRY"), but could be any other categorical variable. 
}
  \item{data}{
An R object, normally a data frame, containing the data from TIMSS.
}
}
\value{
timss.mean returns a data frame including the means and standard errors.
}
\author{
Daniel Caro
}

\seealso{
pirls.mean
}
\examples{
\dontrun{
## Exhibit 2.5, p.19 User Guide 2007
timss.mean(variable="BSDAGE", by="IDCNTRYL", data=timss.data)}



## The function is currently defined as
function (variable, by, data) 
{
    mean.input <- function(variable, data) {
        meanrp <- sapply(1:75, function(x) weighted.mean(as.numeric(data[[variable]]), 
            ifelse(data[["JKZONE"]] == x, 2 * data[["TOTWGT"]] * 
                data[["JKREP"]], data[["TOTWGT"]]), na.rm = TRUE))
        meantot <- weighted.mean(as.numeric(data[[variable]]), 
            data[["TOTWGT"]], na.rm = TRUE)
        meanse <- (sum((meanrp - meantot)^2))^(1/2)
        result <- data.frame(Mean = meantot, Std.err. = meanse)
        return(round(result, 2))
    }
    if (missing(by)) {
        return(mean.input(variable = variable, data = data))
    }
    else {
        return(ddply(data, by, function(x) mean.input(data = x, 
            variable = variable)))
    }
  }
}

