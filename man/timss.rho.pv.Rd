\name{timss.rho.pv}
\alias{timss.rho.pv}
\title{
Two-way weighted correlation
}
\description{
pirls.rho.pv calculates the correlation and standard error among two achievement variables each based on 5 plausible values or one achievement variable and an observed variable (i.e., with observed scores rather than plausible values).
}
\usage{
timss.rho.pv(variable, pvlabels, by, data)
}
\arguments{
  \item{variable}{
A data label for the observed variable (e.g., variable="ASDAGE") 
}
  \item{pvlabels}{
One or two labels describing the achievement variables (e.g., pvlabels = c("ASRLIT", "ASRINF") )
}
  \item{by}{
The variable label defining the grouping, usually the countries (by="IDCNTRY"), but could be any other categorical variable. 
}
  \item{data}{
An R object, normally a data frame, containing the data from TIMSS.
}
}
\value{
timss.rho.pv returns a matrix with correlations and standard errors.
}

\author{
Daniel Caro
}


\seealso{
pirls.rho.pv
}
\examples{
\dontrun{
timss.rho.pv(variable="BSDGEDUP", pvlabel="BSMMAT", by="IDCNTRYL", data=timss.data)
}


## The function is currently defined as
function (variable, pvlabels, by, data) 
{
    rho.pv.input <- function(variable, pvlabels, data) {
        if (length(pvlabels) == 2 & missing(variable)) {
            pvnames <- lapply(pvlabels, function(x) paste(x, 
                "0", 1:5, sep = ""))
            data <- na.omit(data[c(unlist(pvnames), "TOTWGT", 
                "JKREP", "JKZONE")])
            rhopvrp <- lapply(1:75, function(i) cov.wt(x = data[c(pvnames[[1]][1], 
                pvnames[[2]][1])], cor = T, wt = ifelse(data[["JKZONE"]] == 
                i, 2 * data[["TOTWGT"]] * data[["JKREP"]], data[["TOTWGT"]]))[[5]])
            rhopvtot <- lapply(1:5, function(i) cov.wt(x = data[c(pvnames[[1]][i], 
                pvnames[[2]][i])], cor = T, wt = data[["TOTWGT"]])[[5]])
            varw <- Reduce("+", lapply(rhopvrp, function(x) (x - 
                rhopvtot[[1]])^2))
            varb <- (1 + 1/5) * apply(simplify2array(rhopvtot), 
                c(1, 2), var)
            rhose <- (varw + varb)^(1/2)
            rhotot <- Reduce("+", rhopvtot)/length(rhopvtot)
            rhomat <- round(do.call(cbind, lapply(1:ncol(rhotot), 
                function(x) t(rbind(rhotot[, x], rhose[, x])))), 
                5)
            colnames(rhomat) <- unlist(lapply(1:2, function(i) c(paste(pvlabels, 
                "Rho", sep = " ")[i], paste(pvlabels, "SE", sep = " ")[i])))
            return(round(rhomat, 3))
        }
        else {
            pvnames <- paste(pvlabels, "0", 1:5, sep = "")
            data <- na.omit(data[c(variable, pvnames, "TOTWGT", 
                "JKREP", "JKZONE")])
            rhopvrp <- lapply(1:75, function(i) cov.wt(x = data[c(variable, 
                pvnames[1])], cor = T, wt = ifelse(data[["JKZONE"]] == 
                i, 2 * data[["TOTWGT"]] * data[["JKREP"]], data[["TOTWGT"]]))[[5]])
            rhopvtot <- lapply(pvnames, function(i) cov.wt(x = data[c(variable, 
                i)], cor = T, wt = data[["TOTWGT"]])[[5]])
            varw <- Reduce("+", lapply(rhopvrp, function(x) (x - 
                rhopvtot[[1]])^2))
            varb <- (1 + 1/5) * apply(simplify2array(rhopvtot), 
                c(1, 2), var)
            rhose <- (varw + varb)^(1/2)
            rhotot <- Reduce("+", rhopvtot)/length(rhopvtot)
            rhomat <- round(do.call(cbind, lapply(1:ncol(rhotot), 
                function(x) t(rbind(rhotot[, x], rhose[, x])))), 
                5)
            colnames(rhomat) <- unlist(lapply(1:2, function(i) c(paste(c(variable, 
                pvlabels), "Rho", sep = " ")[i], paste(c(variable, 
                pvlabels), "SE", sep = " ")[i])))
            return(round(rhomat, 3))
        }
    }
    if (missing(by)) {
        return(rho.pv.input(variable = variable, pvlabels = pvlabels, 
            data = data))
    }
    else {
        if (length(pvlabels) == 2 & missing(variable)) {
            return(lapply(split(data, data[by]), function(x) rho.pv.input(pvlabels = pvlabels, 
                data = x)))
        }
        else {
            return(lapply(split(data, data[by]), function(x) rho.pv.input(variable = variable, 
                pvlabels = pvlabels, data = x)))
        }
    }
  }
}

