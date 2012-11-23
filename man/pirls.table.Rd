\name{pirls.table}
\alias{pirls.table}
\title{Frequency table}
\description{
pirls.table produces a frequency table for a categorical variable printing percentages and standard errors.
}
\usage{
pirls.table(variable, by, data)
}
\arguments{
  \item{variable}{
The data label with the variable to be analyzed.
}
  \item{by}{
The variable label defining the grouping, usually the countries (by="IDCNTRY"), but could be any other categorical variable. 
}
  \item{data}{
An R object, normally a data frame, containing the data from PIRLS.
}
}
\value{
pirls.table returns a data frame with percentages and standard errors.
}
\author{
Daniel Caro
}

\seealso{
timss.table, pisa.table
}
\examples{
\dontrun{
pirls.table(variable="ITSEX", by=c("IDCNTRYL", "ASDHPATR"), data=pirls.data)
}


## The function is currently defined as
function (variable, by, data) 
{
    table.input <- function(variable, data) {
        tabrp <- sapply(1:75, function(x) percent(data[[variable]], 
            total = F, weights = ifelse(data[["JKZONE"]] == x, 
                2 * data[["TOTWGT"]] * data[["JKREP"]], data[["TOTWGT"]]), 
            na.rm = TRUE))
        tabtot <- round(percent(data[[variable]], weights = data[["TOTWGT"]], 
            na.rm = TRUE, total = F), 3)
        tabse <- round(sapply(1:length(table(data[[variable]])), 
            function(x) sum(sapply(1:75, function(y) (tabrp[x, 
                y] - tabtot[[x]])^2))^(1/2)), 3)
        result <- data.frame(table(data[[variable]]), Percentage = as.numeric(tabtot), 
            Std.err. = tabse)
        names(result)[1] <- variable
        return(result)
    }
    if (missing(by)) {
        return(table.input(variable = variable, data = data))
    }
    else {
        return(ddply(data, by, function(x) table.input(data = x, 
            variable = variable)))
    }
  }
}

