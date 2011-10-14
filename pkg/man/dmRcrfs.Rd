\name{dmRcrfs}
\alias{dmRcrfs}
\title{Creates Time Plot and Bar Plot for Incoming CRFs}
\description{This function creates  time plot for incoming CRFS
}
\usage{
dmRcrfs(sh,freq,isImageSave,imageType,imageSize,tmpDir,layoutOpt)
}
\arguments{
\item{sh}{sh  incoming crf data}
\item{freq}{freq  is  incomig CRFs frequescy, could be weekly, daily, Monthly }
\item{isImageSave}{image Save option, default is FALSE, no image saved}
\item{tmpDir}{dir Name where the image will be saved }
\item{layoutOpt}{layoutOpt is layout option default is 2 figure in a page}
\item{imageType}{imageType is imageType default is a ps }
\item{imageSize}{imageSize is imageSize default is a whatever }
}

\seealso{
 \code{\link{chron}},
         \code{\link{matplot}} , \code{\link{matpoints}} ,
	and
         \code{\link{dmRcrfBarPlot}}
}
\examples{
library("dmRTools")
data("dmRToolsData")
#same as
dmRcrfs(crfsWkDat,freq="weekly",isImageSave=FALSE,tmpDir=NULL)
#or
dmRcrfs(crfsDlyDat,freq="daily",isImageSave=FALSE,tmpDir=NULL)
#or
dmRcrfs(crfsMonDat,freq="monthly",isImageSave=FALSE,tmpDir=NULL)
#or
dmRcrfs(crfsWkDat,freq="weekly",layoutOpt=matrix(c(1,0,2,0),2,2,byrow=TRUE))

}
\author{Zekai  Otles}
