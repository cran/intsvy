\name{pisa.mean.pv}
\alias{pisa.mean.pv}
\title{Mean for plausible values
}
\description{
pisa.mean.pv uses five plausible values to calculate mean performance and its standard erro
}
\usage{
pisa.mean.pv(pvlabel, by, data)
}
\arguments{
  \item{pvlabel}{
The label corresponding to the achievement variable, e.g., "READ" for overall reading performance.
}
  \item{by}{
The variable label defining the grouping, usually the countries (by="CNT"), but could be any other categorical variable. 
}
  \item{data}{
An R object, normally a data frame, containing the data from PISA.
}
}
\value{
pisa.mean.pv returns a data frame with the mean and standard errors.
}
\author{
Daniel Caro
}
\seealso{
timss.mean.pv
}
\examples{
\dontrun{
## Replicating Figure I.2.16 p. 56 International Report 2009
pisa.mean.pv(pvlabel = "READ", by = "CNT", data = student.data)}


## The function is currently defined as
function (pvlabel, by, data) 
{
    pv.input <- function(pvlabel = "READ", data) {
        pvnames <- paste("PV", 1:5, pvlabel, sep = "")
        achmrp <- sapply(pvnames, function(k) sapply(1:80, function(i) weighted.mean(data[[k]], 
            data[[paste("W_FSTR", i, sep = "")]], na.rm = TRUE)))
        achmtot <- sapply(pvnames, function(x) weighted.mean(data[[x]], 
            data[["W_FSTUWT"]], na.rm = TRUE))
        achtot <- mean(achmtot)
        varw <- mean(sapply(1:5, function(i) 0.05 * sum((achmrp[, 
            i] - achmtot[i])^2)))
        varb <- (1/(5 - 1)) * sum(sapply(1:5, function(i) (achmtot[i] - 
            achtot)^2))
        achse <- (varw + (1 + 1/5) * varb)^(1/2)
        result <- data.frame(Mean = achtot, Std.err. = achse)
        return(round(result, 3))
    }
    if (missing(by)) {
        return(pv.input(pvlabel = pvlabel, data = data))
    }
    else {
        for (i in by) {
            data[[c(i)]] <- as.character(data[[c(i)]])
        }
        return(ddply(data, by, function(x) pv.input(data = x, 
            pvlabel = pvlabel)))
    }
  }
}

