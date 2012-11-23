\name{timss.reg.pv}
\alias{timss.reg.pv}
\title{
Regression analysis for plausible values
}
\description{
pisa.reg.pv performs standard linear regression analysis (OLS) while handling plausible values and replicate weights.
}
\usage{
timss.reg.pv(x, pvlabel = "ASRREA", by, data)
}
\arguments{
  \item{x}{
Data labels of independent variables.
}
  \item{pvlabel}{
The label corresponding to the achievement variable.
}
  \item{by}{
The variable label defining the grouping, usually the countries (by="IDCNTRY"), but could be any other categorical variable. 
}
  \item{data}{
An R object, normally a data frame, containing the data from TIMSS.
}
}
\value{
timss.reg.pv returns a data frame with coefficients, standard errors and t-values. If "by" is specified, results are reported in a list. 
}

\author{
Daniel Caro
}

\seealso{
pirls.reg.pv
}
\examples{
\dontrun{
## Exhibit 2.13, p.27
timss.reg.pv(pvlabel="BSMMAT", by=c("IDCNTRYL"), x="SEX", data=timss.data)}


## The function is currently defined as
function (x, pvlabel = "ASRREA", by, data) 
{
    reg.pv.input <- function(x, pvlabel = pvlabel, data) {
        pvnames <- paste(pvlabel, "0", 1:5, sep = "")
        regform <- lapply(pvnames, function(i) paste(i, "~", 
            paste(x, collapse = "+")))
        Coefrpv1 <- sapply(1:75, function(i) coefficients(lm(formula = as.formula(regform[[1]]), 
            data = data, weights = ifelse(data[["JKZONE"]] == 
                i, 2 * data[["TOTWGT"]] * data[["JKREP"]], data[["TOTWGT"]]))))
        Regpv <- lapply(regform, function(i) summary(lm(formula = i, 
            data = data, weights = data[["TOTWGT"]])))
        Coefpv <- sapply(Regpv, function(i) i[["coefficients"]][, 
            1])
        R2 <- mean(sapply(Regpv, function(i) i[["r.squared"]]))
        Coeftot <- apply(Coefpv, 1, mean)
        Varw <- apply((Coefrpv1 - Coefpv[, 1])^2, 1, sum)
        Varb <- (1 + 1/5) * apply(Coefpv, 1, var)
        CoefSE <- (Varw + Varb)^(1/2)
        CoefT <- Coeftot/CoefSE
        RegTab <- round(data.frame(Estimate = Coeftot, `Std. Error` = CoefSE, 
            `t value` = CoefT, check.names = F), 2)
        return(RegTab)
    }
    if (missing(by)) {
        return(reg.pv.input(x = x, pvlabel = pvlabel, data = data))
    }
    else {
        return(lapply(split(data, data[by]), function(i) reg.pv.input(x = x, 
            pvlabel = pvlabel, data = i)))
    }
  }
}

