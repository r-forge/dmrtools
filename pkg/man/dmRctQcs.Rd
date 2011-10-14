\name{dmRctQcs}
\alias{dmRctQcs}
\title{Creates multiple plots showing summary of Qcs for
  each center}
\description{This function creates multiple graphs 
  showing summary report of external Qcs from each center
}
\usage{
dmRctQcs(shMatrix,colNames,isImageSave=TRUE,tmpDir,layoutOpt)
}
\arguments{
\item{shMatrix}{Matrix input with header }
\item{colNames}{Use this column names instead of Matrix column headers}
\item{isImageSave}{Output image saved as postscript file when is TRUE, output as psFile }
\item{tmpDir}{tmpDir=NULL  Unix system uses /tmp directory in WINDOWS
  give  C:/temp/ forward slashes used and last one is required}
\item{layoutOpt}{layoutOpt is layout option default is 3 figure in a page}
}
\seealso{
 \code{\link{dmRbarPlot}}, 
          and \code{\link{xtable}}
}
\examples{
library("dmRTools")
data("dmRToolsData")
dmRctQcs(ctQcsDat)
#same as
dmRctQcs(ctQcsDat,isImageSave=FALSE,tmpDir=NULL)
#or
#give colnames, and dir names for image to saved
#dmRctQcs(ctQcsDat,colNames,isImageSave=TRUE,tmpDir=tmpDir)
}
\author{Zekai  Otles}
