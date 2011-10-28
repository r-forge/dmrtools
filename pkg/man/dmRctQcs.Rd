\name{dmRctQcs}
\alias{dmRctQcs}
\title{Creates multiple plots showing summary of Qcs for
  each center}
\description{This function creates multiple graphs 
  showing summary report of external Qcs from each center
}
\usage{
dmRctQcs( shMatrix, colNames = NULL)
}
\arguments{
\item{shMatrix}{Matrix input with header }
\item{colNames}{Use this column names instead of Matrix column headers}
}
\seealso{
 \code{\link{dmRbarPlot}}, 
          and \code{\link{xtable}}
}
\examples{
require("xtable")
library("dmRTools")
data("dmRToolsData")
dmRctQcs(ctQcsDat)
#give colnames
#dmRctQcs(ctQcsDat,colNames)
}
\author{Zekai  Otles}
