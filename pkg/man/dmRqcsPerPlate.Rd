\name{dmRqcsPerPlate}
\alias{dmRqcsPerPlate}
\title{ Creates Stack Bar to Display Qcs per Plate}
\description{This function creates the stackbar to display number of Qcs per plate in terms of problem type 
 This code is modified to display number of Qcs plot along with normalized Qcs per Plate
 }
\usage{
dmRqcsPerPlate(shMatrix,xCol,yCols,xlab,ylab,titleOfPlots,shadedOrNot,isImageSave,tmpDir,layoutOpt)
}
\arguments{
\item{shMatrix}{shMatrix information Matrix }
\item{xCol}{x Column Number for the Data-CRF Names }
\item{yCols}{ Column Numbers-for Qc categories }
\item{xlab}{X axis Labels }
\item{ylab}{Y axis Labels }
\item{titleOfPlots}{Title of the Plots }
\item{shadedOrNot}{Shaded or solid filled bars  default no argument
  means filled bars }
\item{isImageSave}{image Save option, default is FALSE, no image saved}
\item{tmpDir}{dir name where the image will be saved, it needs final slashes }
\item{layoutOpt}{layoutOpt is layout option default is 2 figure in a page}

}
\seealso{ \code{\link{dmRbarPlot}}
}
\examples{
library("dmRTools")
data("dmRToolsData")
dmRqcsPerPlate(qcsPerPlateDat)
#or
dmRqcsPerPlate(qcsPerPlateDat,xCol=1,yCols=c(4,9))
dmRqcsPerPlate(qcsPerPlateDat,xCol=1,yCols=c(4,9),
	xlab=c("Plate Number","Plate Number"),
	ylab=c("\% QCs","Number Of QCs"),
	titleOfPlots=c("Normalized QCs Per Plate"," Number of QCs Per Plate"))
#or
dmRqcsPerPlate(qcsPerPlateDat,xCol=1,yCols=c(4,9),
	xlab=c("Plate Number","Plate Number"),
	ylab=c("\% QCs","Number Of QCs"),
	titleOfPlots=c("Normalized QCs Per Plate"," Number of QCs Per Plate"),
	isImageSave=TRUE,tmpDir="/tmp/")
}
\author{Zekai Otles}
