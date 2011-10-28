\name{dmRcenterMap}
\alias{dmRcenterMap}
\title{Creates google map image with the normalized acrual data
      }
\description{
	This function uses google map package to plot acrual data on google maps. 

}
\usage{
	dmRcenterMap(centerData=centerData,outFile=NULL,tmpDir=NULL)

}
\arguments{
\item{centerData}{centerData is data frame which gives center number,location,num of Subjects, and
	latitude (north is positive) and longtitude (east is positive) (in degrees).
	}


\item{outFile}{outFile is default is NULL, but it can be assigned for .png file}
\item{tmpDir}{tmDir is default is /tmp in UNIX C:\\temp on windows, but it can be assigned }
}
\references{
Loecher, M., (2010). 
\emph{ Plotting on Google Static Maps in R }.  Sense Networks, markus@sensenetworks.com
}
\seealso{
\code{\link{RgoogleMaps}}, \code{\link{GDALinfo}} and \code{\link{sp}}
}

\examples{
	library(dmRTools)
	require(RgoogleMaps)
	require(rgdal)
	require(sp)
	data(dmRToolsData)
	#following will produce centerMap.png in tmp dir.
	dmRcenterMap(centerDat)
}
\author{Zekai  Otles}