\name{dmRcrfBarPlot}
\alias{dmRcrfBarPlot}
\title{Creates barplot for data in matrix format}
\description{This function creates barplot given data in matrix format,
  it is used to display number of incoming CRFs per month 
}
\usage{
dmRcrfBarPlot(shMatrix,titleOfPlot=NULL,ylab=NULL,isColor=FALSE)
}
\arguments{
\item{shMatrix}{shMatrix information Matrix }
\item{titleOfPlot}{Title of Plot}
\item{ylab}{Label of Y-axis}
\item{isColor}{color plot or not}
}
\seealso{
\code{\link{dmRcrfs}}, \code{\link{text}}, \code{\link{mtext}}, and \code{\link{barplot}}
}
\examples{
library(dmRTools)
#shMatrix is one dimensional matrix multiple columns

shMatrix<-matrix(c(6,73,4,6,175),1,5)
colnames(shMatrix)<-c("Apr","May","Jun","Aug","Sep")

dmRcrfBarPlot(shMatrix,titleOfPlot="Number of Incoming CRFs per Month",ylab="\% of Incoming CRFs",isColor=TRUE)

}
\author{Zekai  Otles}
