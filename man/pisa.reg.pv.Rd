\name{pisa.reg.pv}
\alias{pisa.reg.pv}
\title{
Regression analysis for plausible values.
}
\description{
pisa.reg.pv performs standard linear regression analysis (OLS) while handling plausible values and replicate weights.
}
\usage{
pisa.reg.pv(x, pvlabel = "READ", by, data)
}
\arguments{
  \item{x}{
Independent variables.
}
  \item{pvlabel}{
Data label for plausible value.
}
  \item{by}{
The variable label defining the grouping, usually the countries (by="CNT"), but could be any other categorical variable. 
}
  \item{data}{
An R object, normally a data frame, containing the data from PISA.
}
}
\value{
pisa.reg.pv returns a data frame with coefficients, standard errors and t-values. If "by" is specified, results are reported in a list. 
}
\author{
Daniel Caro
}

\seealso{
pirls.reg.pv, timss.reg.pv
}
\examples{
\dontrun{
pisa.reg.pv(pvlabel="READ", x="ST04Q01", data=student.data)
}



## The function is currently defined as
function (x, pvlabel = "READ", by, data) 
{
    reg.pv.input <- function(x, pvlabel = pvlabel, data) {
        pvnames <- paste("PV", 1:5, pvlabel, sep = "")
        regform <- lapply(pvnames, function(i) paste(i, "~", 
            paste(x, collapse = "+")))
        Coefrpv <- lapply(regform, function(k) sapply(1:80, function(i) coefficients(lm(formula = as.formula(k), 
            data = data, weights = data[[paste("W_FSTR", i, sep = "")]]))))
        Regpv <- lapply(regform, function(i) summary(lm(formula = i, 
            data = data, weights = data[["W_FSTUWT"]])))
        Coefpv <- sapply(Regpv, function(i) i[["coefficients"]][, 
            1])
        R2 <- mean(sapply(Regpv, function(i) i[["r.squared"]]))
        Coeftot <- apply(Coefpv, 1, mean)
        Varw <- apply(0.05 * sapply(1:5, function(i) apply((Coefrpv[[i]] - 
            Coefpv[, i])^2, 1, sum)), 1, mean)
        Varb <- (1/4) * apply(sapply(1:5, function(i) (Coefpv[, 
            i] - Coeftot)^2), 1, sum)
        CoefSE <- (Varw + (1 + 1/5) * Varb)^(1/2)
        CoefT <- Coeftot/CoefSE
        RegTab <- round(data.frame(Estimate = Coeftot, `Std. Error` = CoefSE, 
            `t value` = CoefT, check.names = F), 3)
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

