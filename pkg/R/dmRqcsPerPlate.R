## Copyright (C) 2011 Frontier Science & Technology
##
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
# Function dmRqcsPerPlate 
#######################################
#This function plots  QCS per plate as a stack bar default is gray shading
#different pattern style filling is also an option
#This function written by Zekai Otles <otles@fstrf-wi.org>
#


dmRqcsPerPlate<-function(shMatrix=shMatrix,xCol=NULL,yCols=NULL,
	xlab=NULL,ylab=NULL,
	titleOfPlots=NULL,shadedOrNot=FALSE){
  require("chron")
  scaleCex=0.5



nRows<-length(shMatrix)
#	#write a matrix for number of occuorrence of type of QCS
	shMat<-shMatrix
        numCol<-dim(shMat)[2]
	if(is.null(xCol)){xCol<-1}
	if(is.null(yCols)){yCols<-c(4,numCol)}
	numColX<-numCol-(yCols[1]-1)
	#First 3 column are not used for plotting, description, plate numbers, etc
	crfsFact<-levels(factor(as.integer(shMat[,xCol])))
	shMatx<-matrix(0,nrow=length(crfsFact),ncol=numCol-(yCols[1]-1))
	colnames(shMatx)<-colnames(shMat)[yCols[1]:yCols[2]]
	rownames(shMatx)<-crfsFact

	for(levIndx in 1:length(crfsFact)){
	x<-array(0,numColX)
	  for(rowIndx in 1:dim(shMat)[1]){
          	shMat[rowIndx,xCol]<-as.integer(shMat[rowIndx,xCol])
		if(shMat[rowIndx,xCol]==as.integer(crfsFact[levIndx])){
                for(colIndx in yCols[1]:numCol){
                  shMat[rowIndx,colIndx]<-as.integer(shMat[rowIndx,colIndx])
		  ix<-colIndx-(yCols[1]-1)
                  x[ix]<-as.integer(x[ix])+as.integer(shMat[rowIndx,colIndx])
                  }#end of colIndx
                }#end of if

          }#end of row Indx
                for(indx in 1:numColX){
		  shMatx[levIndx,indx]<-x[indx]
	        }#end of indx
	}#end of levIndx



	if(is.null(xlab)){xlab<-c("Plate Number","Plate Number")}
	if(is.null(ylab)){ylab<-c("% QCs","Number Of QCs")}
	if(is.null(titleOfPlots)){titleOfPlots<-c("Normalized QCs Per Plate"," Number of QCs Per Plate")}
     dmRbarPlot(shMatx=shMatx,xlab=xlab,ylab=ylab,titleOfPlot=titleOfPlots,
		shadedOrNot=c(TRUE,TRUE),
                beside=c(FALSE,TRUE),secondPlot=TRUE)
      list(matrixOut=shMatx)

}



