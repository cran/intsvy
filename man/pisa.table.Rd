\name{pisa.table}
\alias{pisa.table}
\title{Frequency table}
\description{
pirls.table produces a frequency table for a categorical variable printing percentages and standard errors.
}
\usage{
pisa.table(variable, by, data)
}
\arguments{
  \item{variable}{
The data label with the variable to be analyzed.
}
  \item{by}{
The variable label defining the grouping, usually the countries (by="CNT"), but could be any other categorical variable. 
}
  \item{data}{
An R object, normally a data frame, containing the data from PISA.
}
}
\value{
pisa.table returns a data frame with percentages and standard errors.
}
\author{
Daniel Caro
}


\seealso{
pirls.table, timss.table
}
\examples{
\dontrun{
## Replicating Table A2.4b in International Report 2009
pisa.table(variable="ST01Q01", by=c("CNT", "ST04Q01"), data=student.data)
}


## The function is currently defined as
function (variable, by, data) 
{
    table.input <- function(variable, data) {
        tabrp <- sapply(1:80, function(i) percent(as.factor(as.numeric(data[[variable]])), 
            total = F, weights = data[[paste("W_FSTR", i, sep = "")]], 
            na.rm = T))
        tabtot <- round(percent(as.factor(as.numeric(data[[variable]])), 
            weights = data[["W_FSTUWT"]], na.rm = TRUE, total = F), 
            3)
        tabse <- round(sapply(1:length(table(as.numeric(data[[variable]]))), 
            function(x) (0.05 * sum(sapply(1:80, function(y) (tabrp[x, 
                y] - tabtot[[x]])^2)))^(1/2)), 3)
        result <- data.frame(table(as.numeric(data[[variable]])), 
            Percentage = as.numeric(tabtot), Std.err. = tabse)
        names(result)[1] <- variable
        return(result)
    }
    if (missing(by)) {
        return(table.input(variable = variable, data = data))
    }
    else {
        for (i in by) {
            data[[c(i)]] <- as.character(data[[c(i)]])
        }
        return(ddply(data, by, function(x) table.input(data = x, 
            variable = variable)))
    }
  }
}

