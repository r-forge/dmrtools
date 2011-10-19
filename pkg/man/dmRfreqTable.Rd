\name{dmRfreqTable}
\alias{dmRfreqTable}
\title{Display summary visit tables and pie charts }
\description{This function displays summary visit tables  and produces summary tables and pie charts for quick and comprehensible view}
\usage{
dmRfreqTable(mat = mat, pieChartsOrNot = pieChartsOrNot, colorPie =
                 NULL, colPlt = NULL, rowPlt = NULL)

}
\arguments{
\item{mat}{mat is matrix format of comprehensive visit table }
\item{pieChartsOrNot}{pieChartsOrNot to plot pie chart or not} 
\item{colorPie}{pie chart default is gray, but it this  argument is TRUE  then plots pie chart in color}
\item{colPlt}{plot column data of Matrix, if it is TRUE}
\item{rowPlt}{plot row data of Matrix, if it is TRUE}
}
\seealso{ \code{\link{ftable}}, \code{\link{dmRvisitSchedule}}, \code{\link{dmRcolPercentsPie}} 
and \code{\link{dmRrowPercentsPie}}, 

}
\examples{
library("dmRTools")
data("dmRToolsData")
baselineMat<-subset(visScheduleDat,visitNo==subset(visitMapDat,typeOfVisit=="B")$visitNo)

aOutX<-dmRmergeDataModule(visMapDat=visitMapDat,studyDat=visScheduleDat, 
			    baselineMat=baselineMat,dateFormat="d/m/y")
tab1<-as.table(ftable(aOutX[c("CRFlabel","indicator")]))
Total<-colSums(tab1)
ts<-rbind(tab1,Total)
Total<-rowSums(ts)
ts<-cbind(ts,Total)

dmRfreqTable(mat=ts, pieChartsOrNot=TRUE,colorPie=TRUE,colPlt=TRUE)
#or
dmRfreqTable(mat=ts, pieChartsOrNot=TRUE,colorPie=TRUE,rowPlt=TRUE)
}
\author{Zekai  Otles}
