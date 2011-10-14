\name{dmRbarPlot}
\alias{dmRbarPlot}
\title{Creates simple barPlot or stack bars for data in matrix format
      }
\description{This function creates barplot/histogram given data in matrix format with one row, it produces stack bar matrix with multiple rows.
}
\usage{
 dmRbarPlot(shMatx,xlab,ylab,titleOfPlot,shadedOrNot,beside,secondPlot)
}
\arguments{
\item{shMatx}{shMatx information Matrix }
\item{titleOfPlot}{Title of Plot}
\item{xlab}{Labels of X-axis}
\item{ylab}{Labels of Y-axis}
\item{shadedOrNot}{multiple options-shadedOrNot of stacked bars}
\item{beside}{multiple options-bars is next to each other or not}
\item{secondPlot}{plot actual number of Qcs or Not}
}
\seealso{
\code{\link{barplot}}, \code{\link{mtext}},
\code{\link{dmRbarProps}}, \code{\link{dmRcrfBarPlot}},
\code{\link{dmRctQcs}}, 
 and \code{\link{dmRlegend}}
}
\examples{
library("dmRTools")
data(dmRToolsData)
xLabel<-"Site Number"
yLabel<-c("Normalized Number of Records, QCs, and Resolved QCs ","Normalized QC ages")
dmRbarPlot(ctQcsDat[,2:4],xlab=xLabel,ylab=yLabel,titleOfPlot="test",shadedOrNot=FALSE,beside=FALSE)

}
\author{Zekai  Otles}
