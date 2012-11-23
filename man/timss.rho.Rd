\name{timss.rho}
\alias{timss.rho}
\title{
Correlation matrix
}
\description{
timss.rho produces a correlations matrix for observed variables (NOT for plausible values)
}
\usage{
timss.rho(variables, by, data)
}
\arguments{
  \item{variables}{
Data labels for the variables in the correlation matrix (e.g., variables=c("ASRREA01", "ASDAGE") )
}
  \item{by}{
The variable label defining the grouping, usually the countries (by="IDCNTRY"), but could be any other categorical variable. 
}
  \item{data}{
An R object, normally a data frame, containing the data from TIMSS.
}
}
\value{
pirls.rho returns a matrix including correlation and standard error values}

\author{
Daniel Caro
}

\seealso{
pirls.rho
}
\examples{
\dontrun{
timss.rho(variables=c("BSMMAT01", "BSDGEDUP"), data=timss.data)
}


## The function is currently defined as
function (variables, by, data) 
{
    rho.input <- function(variables, data) {
        data <- na.omit(data[c(variables, "TOTWGT", "JKREP", 
            "JKZONE")])
        rhorp <- lapply(1:75, function(i) cov.wt(data[variables], 
            wt = ifelse(data[["JKZONE"]] == i, 2 * data[["TOTWGT"]] * 
                data[["JKREP"]], data[["TOTWGT"]]), cor = T)[[5]])
        rhotot <- cov.wt(data[variables], wt = data[["TOTWGT"]], 
            cor = T)[[5]]
        rhose <- Reduce("+", lapply(rhorp, function(x) (x - rhotot)^2))^(1/2)
        rhomat <- round(do.call(cbind, lapply(1:ncol(rhotot), 
            function(x) t(rbind(rhotot[, x], rhose[, x])))), 
            3)
        colnames(rhomat) <- unlist(lapply(1:length(variables), 
            function(x) c(paste(variables, "Rho", sep = " ")[x], 
                paste(variables, "SE", sep = " ")[x])))
        return(round(rhomat, 6))
    }
    if (missing(by)) {
        return(rho.input(variables = variables, data = data))
    }
    else {
        return(lapply(split(data, data[by]), function(x) rho.input(variables = variables, 
            data = x)))
    }
  }
}

