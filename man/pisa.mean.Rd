\name{pisa.mean}
\alias{pisa.mean}
\title{Calculates the mean}
\description{
Calculates the mean for an observed variable (NOT one with plausible values).
}
\usage{
pisa.mean(variable, by, data)
}
\arguments{
  \item{variable}{
A variable label. The mean is calculated for this variable (e.g., variable="ESCS") 
}
  \item{by}{
The variable label defining the grouping, usually the countries (by="CNT"), but could be any other categorical variable. 
}
  \item{data}{
An R object, normally a data frame, containing the data from PISA.
}
}
\value{
pisa.mean returns a data frame including the means and standard errors
}
\author{
Daniel Caro
}

\seealso{
pirls.mean, timss.mean
}
\examples{
\dontrun{
## Replicating Table II.1.1, p. 152 in International Report 2009
pisa.mean(variable="ESCS", by="CNT", data=student.data)}


## The function is currently defined as
function (variable, by, data) 
{
    mean.input <- function(variable, data) {
        meanrp <- achmrp <- sapply(1:80, function(i) weighted.mean(as.numeric(data[[variable]]), 
            data[[paste("W_FSTR", i, sep = "")]], na.rm = TRUE))
        meantot <- weighted.mean(as.numeric(data[[variable]]), 
            data[["W_FSTUWT"]], na.rm = TRUE)
        meanse <- (0.05 * sum((meanrp - meantot)^2))^(1/2)
        result <- data.frame(Mean = meantot, Std.err. = meanse)
        return(round(result, 3))
    }
    if (missing(by)) {
        return(mean.input(variable = variable, data = data))
    }
    else {
        for (i in by) {
            data[[c(i)]] <- as.character(data[[c(i)]])
        }
        return(ddply(data, by, function(x) mean.input(data = x, 
            variable = variable)))
    }
  }
}

