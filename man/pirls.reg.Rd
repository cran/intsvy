\name{pirls.reg}
\alias{pirls.reg}
\title{Regression analysis}
\description{pirls.mean performs standard linear regression analysis (OLS) for an observed depedent variable (NOT for plausible values).
}
\usage{
pirls.reg(regform, by, data)
}
\arguments{
  \item{regform}{
A regression formula (e.g., "ASBGBOOK ~ ITSEX").
}
  \item{by}{
The variable label defining the grouping, usually the countries (by="IDCNTRY"), but could be any other categorical variable. 
}
  \item{data}{
An R object, normally a data frame, containing the data from PIRLS.
}
}
\value{
pirls.reg returns a data frame with coefficients, standard errors and t-values. If "by" is specified, results are reported in a list.
}
\author{
Daniel Caro
}


\seealso{
timss.reg
}
\examples{
\dontrun{
# Replicating Exhibit 3.13 in User Guide 2006, p. 68

# Recode ASBGBOOK
table(as.numeric(pirls.data$ASBGBOOK), pirls.data$ASBGBOOK)
pirls.data$BOOK[as.numeric(pirls.data$ASBGBOOK)==1] <- 5
pirls.data$BOOK[as.numeric(pirls.data$ASBGBOOK)==2] <- 18
pirls.data$BOOK[as.numeric(pirls.data$ASBGBOOK)==3] <- 63
pirls.data$BOOK[as.numeric(pirls.data$ASBGBOOK)==4] <- 151
pirls.data$BOOK[as.numeric(pirls.data$ASBGBOOK)==5] <- 251
table(pirls.data$BOOK)

# Exhibit 3.13 p.68 User Guide
pirls.reg(regform= "BOOK ~ ITSEX", by="IDCNTRYL", data=pirls.data)
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

