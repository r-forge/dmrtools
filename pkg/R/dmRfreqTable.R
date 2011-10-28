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
# function dmRfreqTable.
#######################################
#This function tabulate the freq table from frequency matrix
#matrix already includes column sums and row sums
#written by zekai otles <otles@fstrf-wi.org>
"dmRfreqTable"<-function(mat=mat,pieChartsOrNot=pieChartsOrNot,colorPie=NULL,colPlt=NULL,rowPlt=NULL){
  require("chron")
  library("dmRTools")

 if(is.null(colorPie))colorPie<-FALSE

  shMat<-as.matrix(mat)
      numCols<-as.numeric(dim(shMat)[2])
      numRows<-as.numeric(dim(shMat)[1])

      numFreqRows<-4 

      matOut<-NULL
      newRowIndx<-0
      for(rowIndx in 1:numRows){
           newRowIndx<-newRowIndx+1
           matOut<-rbind(matOut,shMat[rowIndx,])
           rownames(matOut)[newRowIndx]<-rownames(shMat)[rowIndx]
           matOut<-rbind(matOut,format(as.numeric(100.00*as.numeric(shMat[rowIndx,]/shMat[numRows,numCols])),nsmall=2,digits=2,trim=TRUE))
           newRowIndx<-newRowIndx+1
           rownames(matOut)[newRowIndx]<-"       Cumulative %"
           matOut<-rbind(matOut,format(as.numeric(100.00*as.numeric(shMat[rowIndx,]/shMat[rowIndx,numCols])),nsmall=2,digits=2,trim=TRUE))
           newRowIndx<-newRowIndx+1
           rownames(matOut)[newRowIndx]<-"       Row_wise  %"
           newRowIndx<-newRowIndx+1
           matOut<-rbind(matOut,format(as.numeric(100.00*as.numeric(shMat[rowIndx,]/shMat[numRows,])),nsmall=2,digits=2,trim=TRUE))
           rownames(matOut)[newRowIndx]<-"       Column_wise  %"
           
           list(matOut=matOut)
      }

      if(pieChartsOrNot){
        #already identifed the number of the plates to be plotted, last Row show the Total
        #plot columns percents in pie charts
        if(!is.null(colPlt) && colPlt==TRUE){
		dmRcolPercentsPie(mat=shMat,colorPie=colorPie)
	}
        if(!is.null(rowPlt) && rowPlt==TRUE){
        #plot rows percents in pie charts
       		dmRrowPercentsPie(mat=shMat,colorPie=colorPie)
	}

      }
}
       
