\name{dmRbarProps}
\alias{dmRbarProps}
\title{ Returns the colors, angles, and densities to be used in the stack bar}
\description{This function returns properties like colors, angles and densities to be used in
stack bar plot}
\usage{
dmRbarProps(nPairs,isColor=FALSE)
}
\arguments{
\item{nPairs}{npairs the array length for the dens. angles, and color }
\item{isColor}{color plot or not}
}
\seealso{ \code{\link{dmRbarPlot}} 
 }
\examples{
nPairs<-5
dmRbarProps(nPairs)
}
\author{Zekai  Otles}
