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
#WRITTEN BY ZEKAI OTLES

#######################################
# function dmRmergeDataModule
#######################################
#This function merge  the data objcets in matrix format to be used
#by visit schedule , it is nothing more than moduler programming
#
#written by zekai otles <otles@fstrf-wi.org>

"dmRmergeDataModule"<-function(visMapDat=visMapDat,studyDat=studyDat,
		baselineMat=baselineMat,dateFormat=dateFormat){
  #AggDatMat is for actual  subject Data
  #Baseline Plate information stored in the baselineMat for the subjects

  require("chron")
  require("sqldf")
  library("dmRTools")

  

#first join
aggData<-sqldf("select a.* ,b.visitDueDate as visitDueDate,b.overdueDays as
overDueDays, b.labelOfPlate as CRFlabel  from studyDat as a, visMapDat as b where a.visitNo=b.visitNo")
#second join

aggData<-sqldf("select a.* ,b.VisitDate as baselineVisitDate
from aggData as a, baselineMat as b where a.SubjectID=b.SubjectID")


#after sqldf some of aggData variables become factor variables, then some arithmetic does not work!
aggData$VisitDate<-as.character(ordered(aggData$VisitDate)) 
aggData$baselineVisitDate<-as.character(ordered(aggData$baselineVisitDate)) 
aggData$overDueDays<-as.numeric(array(aggData$overDueDays)) 


  newRowIndx<-0    #When all the criteria meets increase the index numbe

    #determine subjects baseline information
  numRows<-dim(aggData)[1]
     #subject baseline matrix
     for(rowIndx in 1:numRows){  #loop_aggData



#baseline Visit
     x<-chron(as.character(aggData$baselineVisitDate[rowIndx]),format=dateFormat)

             
             #Target Visit Date
             targetDt<-chron(x+as.numeric(aggData$visitDueDate[rowIndx]),format=dateFormat)
             aggData$targetVisitDate[rowIndx]<-as.character(targetDt)
             #Target Start Date
             targetStrDt<-chron(targetDt-as.numeric(aggData$overDueDays[rowIndx]),
			format=dateFormat)
             aggData$targetStrDt[rowIndx]<-as.character(targetStrDt)
             #Target End Date
             targetEndDt<-chron(targetDt+as.numeric(aggData$overDueDays[rowIndx]),
			format=dateFormat)
             aggData$targetEndDate[rowIndx]<-as.character(targetEndDt)

              indicator<-NA
              #determine the indicator for the visit
             actVisDt<-as.character(aggData$VisitDate[rowIndx])
              if(!is.na(actVisDt) & nchar(actVisDt) >= 6){

                actVisDt<-dates(actVisDt,format=dateFormat)

                   if(targetStrDt<=actVisDt && actVisDt<=targetEndDt){
                      indicator<-"OK"}
                   else if(targetStrDt>actVisDt && targetEndDt>actVisDt){
                      #actual visit occurred before Targeted date
                      indicator<-"Out Of Window-(Early Visit)"
                    }
                    else if(targetStrDt<actVisDt && targetEndDt<actVisDt){
                      #actual visit occurred later than  Targeted date
                      indicator<-"Out Of Window-(Late Visit)"
                    }

                }else {
                     #actual visit has missing Date
                     indicator<-"Missing Plate-Late Visit"
                }

                   #window indicator
                   aggData$indicator[rowIndx]<-indicator



     }# end of for_rowIndx


    return(aggData)

}

