\name{pirls.mean}
\alias{pirls.mean}
\title{Calculates the mean}
\description{Calculates the mean for an observed variable (NOT one with plausible values)
}
\usage{
pirls.mean(variable, by, data)
}
\arguments{
  \item{variable}{
A variable label. The mean is calculated for this variable (e.g., variable="ASDAGE") 
}
  \item{by}{
The variable label defining the grouping, usually the countries (by="IDCNTRY"), but could be any other categorical variable. 
}
  \item{data}{
An R object, normally a data frame, containing the data from PIRLS.
}
}
\value{
pirls.mean returns a data frame including the means and standard errors.
}
\author{
Daniel Caro
}


\seealso{
timss.mean, pisa.mean
}
\examples{
\dontrun{
## Replicating Exhibit 5.10 in p.162 of User Guide 2006

# Number of hours spent reading stories or articles in books or magazines outside of school
pirls.data$TIME[as.numeric(pirls.data$ASBGTSP4) ==1] <-5.5
pirls.data$TIME[as.numeric(pirls.data$ASBGTSP4) ==2] <-4
pirls.data$TIME[as.numeric(pirls.data$ASBGTSP4) ==3] <-2
pirls.data$TIME[as.numeric(pirls.data$ASBGTSP4) ==4] <-0.5
pirls.data$TIME[as.numeric(pirls.data$ASBGTSP4) ==5] <-0

table(pirls.data$ASBGTSP4, pirls.data$TIME)

pirls.mean(variable="TIME", by="IDCNTRYL", data=pirls.data)}


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

