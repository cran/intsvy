\name{pisa.var.label}
\alias{pisa.var.label}
\title{
Data labels
}
\description{
pisa.var.labels prints and saves variable labels and names of participating countries in a text file
}
\usage{
pisa.var.label(folder = getwd(), name = "Variable labels", output = getwd())
}

\arguments{
  \item{folder}{
Directory path where the PISA data are located. The data could be organized within folders but it should not be duplicated.
}
   \item{name}{
Name of output file.
}
  \item{output}{
Folder where output file is located.
}
}

\value{
pisa.var.label returns a list with variable labels for the student, parent, and school data. 
}


\seealso{
timssg4.var.label, timssg8.var.label, pirls.var.label
}
\examples{
\dontrun{
pisa.var.label(folder= getwd())
}
}
