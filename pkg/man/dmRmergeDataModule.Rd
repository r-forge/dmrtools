\name{dmRmergeDataModule}
\alias{dmRmergeDataModule}
\title{Merges study data with visit map}
\description{This function prepares data for merging visit map file and study data}
\usage{
dmRmergeDataModule(visMapDat = visMapDat, studyDat = studyDat,
                 baselineMat = baselineMat, dateFormat = dateFormat)
}
\arguments{
\item{visMapDat}{Vist map data }
\item{studyDat}{study data}
\item{baselineMat}{baselineMat subject baseline data in matrix format}
\item{dateFormat}{dateFormat is study date format, European format dates in "d/m/y",  }
}
\seealso{ \code{\link{dmRvisitSchedule}}
}
\examples{
library("dmRTools")
data("dmRToolsData")
baselineMat<-subset(visScheduleDat,visitNo==subset(visitMapDat,typeOfVisit=="B")$visitNo)
dmRmergeDataModule(visMapDat=visitMapDat,studyDat=visScheduleDat, baselineMat=baselineMat,dateFormat="d/m/y")

}
\author{Zekai  Otles}
