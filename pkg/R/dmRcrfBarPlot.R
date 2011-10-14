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
# Function dmRcrfBarPlot 
#######################################
#This function creates barplot 
 #written by zekai otles <otles@fstrf-wi.org>

"dmRcrfBarPlot" <- function(shMatrix,titleOfPlot=NULL,ylab=NULL,isColor=FALSE)
{
  library('dmRTools');
#color option to the graph added after Discussion with the other people

  if(isColor){
    colorArray<-c("blue","red","green","yellow","orange","magenta","black","gray")
  }else{
    colorArray<-NULL
  }




barValsLoc=30	#it locates the bar values location maxVal/barValsLoc

scaleCex=0.5
# Label bars with their values
sumVal<-sum(shMatrix[1,])
perVals<- 100*(shMatrix[1,]/sumVal)
maxPerVal<-max(perVals)
barVals<-array(1:length(perVals))

if(!is.na(maxPerVal)){

#identify the ymax for the ylimits
if(maxPerVal<=91){
  #set the ticks for next 10th value
    yLimMax<-10*ceiling(maxPerVal/10)
  
  }else{yLimMax<-100}


yOffSet=1.5

#give space to Write last Value


deltaY<-yOffSet+1.5*scaleCex
if((yLimMax -maxPerVal)<deltaY){yLimMax<-yLimMax+deltaY}


bp<-barplot(perVals,ylim=c(0,yLimMax),col=colorArray[1],ylab=ylab)

for(i in 1: length(barVals)){
    barVals[[i]]<-sprintf("%2.2f",perVals[i])
}

box()
text(x=bp, y=yOffSet+perVals ,cex=(1.5*scaleCex), labels=barVals)


#mtext(titleOfPlot,side=3,line =3, at=titleLoc,cex=3*scaleCex, font = 2,adj=0.5) 
mtext(titleOfPlot,side=3,line =1, cex=2*scaleCex, font = 2,adj=0.5) 

mtext(paste("n = ",shMatrix[1,], sep=""),side=1,line=0,at=bp,cex=1.0*scaleCex)
}
}



