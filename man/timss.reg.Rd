\name{timss.reg}
\alias{timss.reg}
\title{
Regression analysis
}
\description{
pisa.mean performs standard linear regression analysis (OLS) for an observed depedent variable (NOT for plausible values).
}
\usage{
timss.reg(regform, by, data)
}
\arguments{
  \item{regform}{
A regression formula (e.g., "ASBGBOOK ~ ITSEX").
}
  \item{by}{
The variable label defining the grouping, usually the countries (by="IDCNTRY"), but could be any other categorical variable. 
}
  \item{data}{
An R object, normally a data frame, containing the data from TIMSS.
}
}
\value{
timss.reg returns a data frame with coefficients, standard errors and t-values. If "by" is specified, results are reported in a list.
}

\author{
Daniel Caro
}


\seealso{
pirls.reg
}
\examples{
\dontrun{
# Exhibit 2.11
timss.data$SEX[timss.data$ITSEX==2]=1
timss.data$SEX[timss.data$ITSEX==1]=0

timss.reg(regform="BSDAGE ~ SEX", by=c("IDCNTRYL"), data=timss.data)
timss.mean(variable="BSDAGE", by=c("IDCNTRYL", "ITSEX"), data=timss.data)
}




## The function is currently defined as
function (regform, by, data) 
{
    reg.input <- function(regform, data) {
        Coefrp <- sapply(1:75, function(i) coefficients(lm(formula = as.formula(regform), 
            data = data, weights = ifelse(data[["JKZONE"]] == 
                i, 2 * data[["TOTWGT"]] * data[["JKREP"]], data[["TOTWGT"]]))))
        Reg <- summary(lm(formula = as.formula(regform), data = data, 
            weights = data[["TOTWGT"]]))
        Coeftot <- Reg$coefficients[, 1]
        R2 <- Reg$r.squared
        CoefSE <- apply((Coefrp - Coeftot)^2, 1, sum)^(1/2)
        CoefT <- Coeftot/CoefSE
        RegTab <- round(data.frame(Estimate = Coeftot, `Std. Error` = CoefSE, 
            `t value` = CoefT, check.names = F), 2)
        return(RegTab)
    }
    if (missing(by)) {
        return(reg.input(regform = regform, data = data))
    }
    else {
        return(lapply(split(data, data[by]), function(x) reg.input(regform = regform, 
            data = x)))
    }
  }
}

