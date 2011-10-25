\name{dmRlegend}
\alias{dmRlegend}
\title{Add legends to plots}
\description{
  This function modified by Zekai Otles from original legend function from graphics package,
 see details of legend function for usage and arguments 
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988).
  \emph{The New S Language}.  Wadsworth & Brooks/Cole.

  Murrell, P. (2005). \emph{ R graphics}. Chapman & Hall/CRC Press.
}
\seealso{
\code{\link{dmRbarPlot}}, \code{\link{dmRlegScales}},
  \code{\link{matplot}}, \code{\link{matpoints}}, 
  \code{\link{plot}}, \code{\link{barplot}},  \code{\link{legend}}, and
  \code{\link{graphics}} 
}
\examples{
#Matrix is one dimensional matrix multiple columns
#shMatrix  colName1 colName2 colName3 ...
#           val1    val2     val3     ...

library("dmRTools")
data(dmRToolsData)
xLabel<-"Site Number"
yLabel<-c("Normalized Number of Records, QCs, and Resolved QCs ","Normalized QC ages")
dmRbarPlot(ctQcsDat[,2:4],xlab=xLabel,ylab=yLabel,titleOfPlot="test",shadedOrNot=FALSE,beside=FALSE)


dmRlegend(1,100,colnames(ctQcsDat)[2:4])

}
\keyword{legend}
