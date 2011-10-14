\name{dmRrowPercentsPie}
\alias{dmRrowPercentsPie}
\title{display summary visit tables rows in pie charts }
\description{This function displays rows summary of visit table in pie charts for quick and comprehensible view}
\usage{
dmRrowPercentsPie(mat,colorPie=NULL)
}
\arguments{
\item{mat}{mat is matrix format of comprehensive visit table }
\item{colorPie}{pie chart default is gray, but it this  argument is TRUE  then plots pie chart in color}
}
\seealso{ \code{\link{dmRcolPercentsPie}},\code{\link{dmRfreqTable}},and \code{\link{dmRvisitSchedule}}
}
\examples{
library("dmRTools")
data("dmRToolsData")
baselineMat<-subset(visScheduleDat,visitNo==subset(visitMapDat,typeOfVisit=="B")$visitNo)
aOutX<-dmRmergeDataModule(visMapDat=visitMapDat,studyDat=visScheduleDat, baselineMat=baselineMat,dateFormat="d/m/y")
tab1<-as.table(ftable(aOutX[c("CRFlabel","indicator")]))
Total<-colSums(tab1)
ts<-rbind(tab1,Total)
Total<-rowSums(ts)
ts<-cbind(ts,Total)
mat<-as.matrix(ts)
dmRrowPercentsPie(mat=mat,colorPie=TRUE)

}
\author{Zekai  Otles}
