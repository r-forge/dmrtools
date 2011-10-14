\name{dmRcolPercentsPie}
\alias{dmRcolPercentsPie }
\title{display summary visit tables columns in pie charts }
\description{This function displays the columns of summary visit table in pie charts for quick and comprehensible view}
\usage{
	dmRcolPercentsPie(mat=mat,colorPie=NULL)
}
\arguments{
\item{mat}{mat is matrix format of comprehensive visit table }
\item{colorPie}{pie chart default is gray, but it this  argument is TRUE  then plots pie chart is in color}
}
\seealso{ \code{\link{dmRrowPercentsPie}},\code{\link{dmRfreqTable}},and \code{\link{dmRvisitSchedule}}
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
mat<-as.matrix(ts)
dmRcolPercentsPie(mat=mat,colorPie=TRUE)
}
\author{Zekai  Otles}
