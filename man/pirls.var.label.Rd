\name{pirls.var.label}
\alias{pirls.var.label}
\title{Data labels}
\description{
pirls.var.labels prints and saves variable labels in a text file
}
\usage{
pirls.var.label(folder = getwd())
}
\arguments{
  \item{folder}{
Directory path where the data are located. The data could be organized within folders but it should not be duplicated.
}
}
\value{
pirls.var.label returns a list with variable labels for the student, home, and school data. The output is also printed on a text sheet located in "folder".
}
\author{
Daniel Caro
}

\seealso{
timss.var.label
}
\examples{
\dontrun{
pirls.var.label(folder= getwd())
}


## The function is currently defined as
function (folder = getwd()) 
{
    files.all <- lapply(c("asg", "ash", "acg"), function(x) list.files(folder, 
        full.names = T, pattern = paste("^", x, ".*.sav$", sep = ""), 
        recursive = T))
    var.label <- list(`Student background and achievement` = description(spss.system.file(files.all[[1]][1])), 
        `Home background` = description(spss.system.file(files.all[[2]][1])), 
        `School variables` = description(spss.system.file(files.all[[3]][1])))
    print(var.label)
    capture.output(var.label, file = file.path(folder, "Variable labels.txt"))
    cat("The file 'Variable labels.txt' in the directory", folder, 
        "contains the variable labels of the complete dataset", 
        sep = " ")
  }
}

