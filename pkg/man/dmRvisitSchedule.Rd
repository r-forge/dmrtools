\name{dmRvisitSchedule}
\alias{dmRvisitSchedule}
\title{Creates visit schedule summary tables and pie charts}
\description{This function creates  visit schedule summary table and pie charts; this function also utilizes the subject data and visit map to construct the table and bar charts}
\usage{
dmRvisitSchedule( visMapDat = visMapDat, studyDat = studyDat,
                 typeOfVisits = typeOfVisits, dateFormat = dateFormat,
                 colPlt = NULL, rowPlt = NULL)
}
\arguments{
\item{visMapDat}{visMapDat is visit map data }
\item{studyDat}{studyDat is study data }
\item{typeOfVisits}{type of visits}
\item{dateFormat}{dateFormat is study date format, European format dates in "d/m/y",  }
\item{colPlt}{plot column data of Matrix, if it is TRUE}
\item{rowPlt}{plot row data of Matrix, if it is TRUE}
}
\seealso{ \code{\link{dmRmergeDataModule}}, \code{\link{dmRfreqTable}}, and 
\code{\link{sqldf}}
}
\examples{
require("chron")
require("sqldf")
library("dmRTools")
data("dmRToolsData")
#examples from dmRToolsData
dmRvisitSchedule(visMapDat=visitMapDat,studyDat=visScheduleDat,typeOfVisits=NULL,dateFormat="d/m/y")
#or
dmRvisitSchedule(visMapDat=visitMapDat,studyDat=visScheduleDat,typeOfVisits=NULL,dateFormat="d/m/y",colPlt=TRUE)
#or
dmRvisitSchedule(visMapDat=visitMapDat,studyDat=visScheduleDat,typeOfVisits=NULL,dateFormat="d/m/y",rowPlt=TRUE)

}
\author{Zekai  Otles}
