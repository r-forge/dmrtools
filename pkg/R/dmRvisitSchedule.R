## Copyright (C) 2011 Frontier Science & Technology
## This program is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## These functions are distributed in the hope that they will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## The text of the GNU General Public License, version 2, is available
## as http://www.gnu.org/copyleft or by writing to the Free Software
## Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
##


#######################################
# function dmRvisitSchedule
#######################################
#This function tabulate the visit schedule  
#
#written by zekai otles <otles@fstrf-wi.org>
"dmRvisitSchedule"<-function(visMapDat=visMapDat,studyDat=studyDat,
typeOfVisits=typeOfVisits,dateFormat=dateFormat,colPlt=NULL,rowPlt=NULL){
  require("xtable")
  require("chron")
  library("dmRTools")





#AggDatMat is for agregating all the Data, this might be problem for the larger
#Dataset, but I will use this and see what is going to happen


  baseline<-subset(visMapDat,typeOfVisit=="B")
  preBaseline<-subset(visMapDat,typeOfVisit=="P")
  termination<-subset(visMapDat,typeOfVisit=="T")
  scheduled<-subset(visMapDat,typeOfVisit=="S")

  
  
  
  if(is.null(typeOfVisits))typeOfVisits<-c("B","P","T","S")

     

	visMapDat<-subset(visMapDat,typeOfVisit %in% typeOfVisits)


	dataOut<-subset(studyDat,visitNo %in% visMapDat[["visitNo"]])

		#determine baseline visitNo
		visNo<-subset(visMapDat,typeOfVisit=="B")$visitNo

             baselineMat<-subset(dataOut,visitNo==visNo)

		#also exclude Baseline Data

             studyDat<-subset(dataOut,visitNo!=visNo)


  #next merge the baseline, study and visitmap 

#merge Data Module
aOutX<-dmRmergeDataModule(visMapDat=visMapDat,studyDat=studyDat, baselineMat=baselineMat,dateFormat)



   tab1<-as.table(ftable(aOutX[c("CRFlabel","indicator")]))

 
   Total<-colSums(tab1)
    ts<-rbind(tab1,Total)
   Total<-rowSums(ts)
   ts<-cbind(ts,Total)



dmRfreqTable(mat=ts, pieChartsOrNot=TRUE,colorPie=TRUE,colPlt=colPlt,rowPlt=rowPlt)
  a<-xtable(ts,caption="Visit Scheduling")

  b<-xtable(ts,caption="Summary Table",digits=0)
  list(aOutX=aOutX,ts=ts)   #return these values
}
