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
# function dmRrowPercentsPie
#######################################
#This function used  in the the freq table which is frequency matrix
#which produces row percents from freq Table 
#written by zekai otles <otles@fstrf-wi.org>
"dmRrowPercentsPie"<-function(mat=mat,colorPie=NULL){
  require("chron")
  library("dmRTools")

 if(is.null(colorPie))colorPie<-FALSE

 #shMat is matrix representation of the visit Map file
 shMat<-as.matrix(mat)
 numFigInRow<-2
 numRows<-as.numeric(dim(shMat)[1])
 numCols<-as.numeric(dim(shMat)[2])
 #I have decided to have 2 rows and 2 cols figure listing
 #opar<-par(mfrow=c(ceiling((numRows-1)/numFigInRow),numFigInRow),mex=0.8)
 opar<-par(mfrow=c(2,numFigInRow),mex=0.8)

 numColsM<-numCols-1
 numRowsM<-numRows-1
 grayScales<-array("",dim=max(1,numRows))
 if(colorPie){
 colScales<-array(rep(c("green","blue","red","yellow","orange")),dim=max(numCols,numRows))
 }
 #dynamic gray scales
       lastIndx<-length(grayScales)
       grayScales[1]<-"white"
       grayScales[lastIndx]<-"black"
       lastIndxM<-lastIndx-1
       increment<-as.integer(100/lastIndx)
        for(grayIndx in 2:lastIndxM){
               grayScales[grayIndx]<-paste("gray",as.integer((grayIndx-1)*increment),sep="")
        }

  
        for(colsIndx in 1:numColsM){
          #to avoid not a number issue "NaN"
        if(shMat[numRows,colsIndx]!=0){    #if block shMat_numRows
          x<-format(as.numeric(shMat[1:numRowsM,colsIndx]/shMat[numRows,colsIndx]),nsmall=2,digits=2,trim=TRUE)
          #just to skip those values pie chart because pi function does not like value=0
          selIndx<-which(as.numeric(x)!=0)
          x<-x[selIndx]
          labels<-rownames(shMat)[selIndx]
#          #dynamic gray scales
#                 lastIndx<-length(selIndx)
#                 grayScales[1]<-"white"
#                 grayScales[lastIndx]<-"black"
#                 lastIndxM<-lastIndx-1
#                 increment<-as.integer(100/lastIndx)
#                 for(grayIndx in 2:lastIndxM){
#                     grayScales[grayIndx]<-paste("gray",as.integer((grayIndx-1)*increment),sep="")
#                     
#                     
#                 }

	      if(!colorPie){
		pie(as.numeric(x),labels=labels,main=colnames(shMat)[colsIndx],col=grayScales)
	      }else{
		pie(as.numeric(x),labels=labels,main=colnames(shMat)[colsIndx],col=colScales)
	      }

        }#end of if block for shMat_numRows
      }
       par(opar)

}
