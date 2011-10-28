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
#function   dmRctQcs
#######################################
#This function plots summary report of external QC notes for each  center
#function modified 06/06/2011 allows the R output save as ps file
#when image saved as ps, it does not produce results on R output
#this is done to allow images embedded in open office document
#
#written by zekai otles,  <otles@fstrf-wi.org>
dmRctQcs<-function(shMatrix,colNames=NULL){
require("xtable")



#prepare this as a matrix
  if(!is.null(colNames)){
    colnames(shMatrix)<-colNames
  }


	
              
#%of records clean on Arrrival or after cleaning up
#number of Total records, QCs and aresolved QCs
        nRows<-dim(shMatrix)[1]
#plot Total records, total Qcs and Number of Resolved Qcs

for(pltIndx in 1:3){

#         rownames(sshMatrix)<-rownames(shMatrix)
	if(pltIndx==1){
	if(exists("sshMatrix")){rm("sshMatrix")}
         sshMatrix<-shMatrix[,c(2:4)]
         colnames(sshMatrix)<-colnames(shMatrix)[c(2:4)]
#
	}else if(pltIndx==2){
	#here is QCs rate and %Resolved QCS
	if(exists("sshMatrix")){rm("sshMatrix")}
         sshMatrix<-shMatrix[,c(3:4)]
	colnames(sshMatrix)<-c("Num QCs","% Resolved QCS")

	#is.na(sshMatrix)<-0
	}else{
	#last index
	if(exists("sshMatrix")){rm("sshMatrix")}
         sshMatrix<-shMatrix[,c(3,5:9)]
         colnames(sshMatrix)<-colnames(shMatrix)[c(3,5:9)]

	}

         legendsNames<-colnames(sshMatrix)
	xLabel<-"Site Number"
	yLabel<-ifelse(pltIndx<3,"% Num. Rec., QCs, Res. Qcs","QC ages")
        titleOfPlot<-ifelse(pltIndx<3,"Normalized Number of Records, QCs, and Resolved QCs ","Normalized QC ages")
        crfsFact<-levels(factor(as.integer(shMatrix[,1])))
	    rownames(sshMatrix)<-crfsFact

             dmRbarPlot(shMatx=sshMatrix,xlab=xLabel,ylab=yLabel,titleOfPlot=titleOfPlot,shadedOrNot=FALSE,beside=FALSE)
}#end of pltIndx

#        
#    shMatrix[nRows,1]<-"Total"
#    colnames(shMatrix)<-gsub("%","\% ",colnames(shMatrix))
#   
   if(nRows !=1){
      ts1<-xtable(shMatrix,caption="Summary Report of Records")
      ts1Matrix<-shMatrix
   }else{
	
      ts1<-xtable(t(shMatrix[1,]),caption="Summary Report of Records")
      ts1Matrix<-shMatrix[1,]
	}
  

   list(shMatrix=shMatrix,matrixOut1=ts1Matrix)
}



