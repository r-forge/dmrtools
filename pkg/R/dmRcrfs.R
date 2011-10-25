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
#function dmRcrfs
#######################################
#This function plots number of incoming CRF against time and
#number of incoming CRFs it can be different format daily, monthly weekly
#written by zekai otles <otles@fstrf-wi.org>


dmRcrfs<-function(sh,freq="daily",isImageSave=FALSE,imageType=NULL,imageSize=NULL,tmpDir=NULL,layoutOpt=NULL){
  library("chron")

if(is.null(tmpDir)){
	if(.Platform$OS.type=="windows"){
		tmpDir<-"C:/temp/"
	}else{
	tmpDir<-"/tmp/"}
}




  dateValues<-array(1:length(sh$year))
  numOfRows<-length(sh$year)
  if(freq=="weekly"){
	for(i in 1:numOfRows){
	    dateValues[[i]]<-dmRwkToDate(sh$year[i],sh$week[i])
  	}
    }else {
	for(i in 1:numOfRows){
	    dateValues[[i]]<-paste(paste(sh$month[i],sh$day[i],sep="/"),sh$year[i],sep="/")
  	}
    }
  sh$dates<-dateValues
  days<-dates(sh$dates)
  sh$months<-months(days)

  if(is.null(layoutOpt)){
  	layout(matrix(c(1,2),2,1,byrow=TRUE))
  }else{
  	layout(layoutOpt)}

  if(!is.null(imageSize)){
	  width=imageSize[1]
	  height=imageSize[2]
	  units=imageSize[3]
	  imageSizeSet<-TRUE
   }
   if(isImageSave){
	   if(is.null(imageType)){
		   suffix<-"ps"
		   }else{
		   suffix<-imageType
	   }

	   if(is.null(layoutOpt)){
			psFile<-paste(tmpDir,paste("dmRcrfPerWk_out",suffix,sep="."),sep="")
	   }else{
			psFile<-paste(tmpDir,paste("dmRcrfPerWk_out%03d",suffix,sep="."),sep="")
	   }

	  if(imageType=="ps"){
		if(!imageSizeSet){
			postscript(file=psFile)
		}else{
			postscript(file=psFile,width=width,height=height)
		}

	   }
	  if(imageType=="png"){
		if(!imageSizeSet){
			png(file=psFile)
		}else{
			png(file=psFile,width=width,height=height,units=units)
		}
	   }
        }else{
          psFile<-NULL
        }
  


  matplot(days,sh$incoming,type="l",xaxt="n",xlab="Months",col="blue",ylab="Number of Incoming CRFs ",ylim=c(1,max(sh$incoming)),main="Number of Incoming CRFs")
 matpoints(days,sh$incoming,pch=25)
  axis.Date(1, at=seq(min(days), max(days),by="months"),format="%b%Y")

  numCol<-length(levels(factor(sh$months)))
  shMatrix<-matrix(0,nrow=1,ncol=numCol)

  colnames(shMatrix)<-levels(factor(sh$months))

	for(iColIndx in 1:numCol){
		temp<-sh[sh$months==colnames(shMatrix)[[iColIndx]],]
                shMatrix[1,iColIndx]<-sum(temp$incoming)
          }
dmRcrfBarPlot(shMatrix,titleOfPlot="Number of Incoming CRFs per Month",ylab="% of Incoming CRFs",isColor=TRUE)

 if(isImageSave){dev.off()}
   list(matrixOut=sh,binData=shMatrix,image=psFile)
	
}



