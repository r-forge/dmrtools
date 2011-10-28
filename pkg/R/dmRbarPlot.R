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
# Function dmRbarPlot
#######################################
#This function plots  various bar plots, single histogram, stacked bar,
#with bar stacked on each other or bars side of each other
#different pattern style filling is also an option
#This function written by Zekai Otles <otles@fstrf-wi.org>

dmRbarPlot<-function(shMatx=shMatx,xlab=xlab,ylab=ylab,titleOfPlot,shadedOrNot,beside,secondPlot=FALSE){
  require("chron")
  library("dmRTools")
  scaleCex=0.5




    ##If this plot used for each indiviual plate, we will use dmRcrfBarPlot
    if(dim(shMatx)[1]==1){
      #use CRFBar plot
      dmRcrfBarPlot(shMatx,titleOfPlot=titleOfPlot[1])

    }else{#more than one columns

    rowSumValues<-rowSums(shMatx)
    #numCol<-length(colnames(shMatx))
    numCol<-dim(shMatx)[2]
    if(max(shMatx)>0){
          shMatx[,1:numCol]<-100*shMatx[,1:numCol]/max(rowSums(shMatx))
    }
         
    #barPs<-dmRbarProps(numCol)
    barPs<-dmRbarProps(numCol,isColor=TRUE)

     #find the RowSums
     rSums<-rowSums(shMatx)
     minIndex<-which.min(rSums)
     maxIndex<-which.max(rSums)
     if(minIndex !=1 & minIndex!=length(rSums)){
     minRegion1<-rSums[minIndex]+rSums[minIndex+1]
     minRegion2<-rSums[minIndex]+rSums[minIndex-1]

           if(minRegion1>minRegion2) {
             minUseIndx<-minIndex-1}
           else{
             minUseIndx<-minIndex+1}
     }#end of minUseIndx

     #default x justify is left
       xJustify<-0    #left Justified
  
     if(minIndex==1){
       minUseIndx<-1
       xJustify<-0    #left Justified
     }

     if(minIndex==length(rSums)){
       minUseIndx<-length(rSums)
       xJustify<-1    #right Justified
     }

        numShMatx<-max(rowSumValues)*shMatx/100
        if(minUseIndx>3){yIndex<-minUseIndx-2}
         else{yIndex<-1}

	numLegCols=ceiling(length(colnames(numShMatx))/3)
	lengthCol<-length(colnames(numShMatx))

    if(shadedOrNot[1]){
          bp<-barplot(t(shMatx),beside=beside[1],ylim=c(0,100),ylab=ylab[1],xlab=xlab[1],
                  col=barPs$col,density=barPs$dens,angle=barPs$angl)
		  #numLegCols=ceiling(length(colnames(shMatx))/3)
                  dmRlegend(bp[minIndex],95,colnames(numShMatx),xjust=xJustify,yjust=1,
                  fill=barPs$col,angle=barPs$angl,density=barPs$dens,bty="n")
    }
     else{
        bp<-barplot(t(shMatx),beside=beside[1],ylim=c(0,100),ylab=ylab[1],xlab=xlab[1],col=barPs$col)
        dmRlegend(bp[minIndex],95,colnames(numShMatx),xjust=xJustify,yjust=1,
                  fill=barPs$col,bty="n")
     }

        box()
        mtext(titleOfPlot[1],side=3,line =1, cex=1.5*scaleCex, font = 2,adj=0.5) 

        mtext(paste("n = ",rowSumValues, sep=""),side=1,line=0,at=bp,cex=1.0*scaleCex)

	#plot actual number of Qcs or Not -
	if(secondPlot) {

        #number of Qc values

	if(shadedOrNot[2]){
          bpM<-barplot(t(numShMatx),beside=beside[2],ylim=c(0,max(rowSumValues)),ylab=ylab[2],xlab=xlab[2],
                  col=barPs$col,density=barPs$dens,angle=barPs$angl)
                  dmRlegend(bpM[minIndex],max(rowSumValues),colnames(numShMatx),xjust=xJustify,yjust=1,
                  fill=barPs$col[1:lengthCol],angle=barPs$angl[1:lengthCol],density=barPs$dens[1:lengthCol],
			bty="n",ncol=numLegCols)
	  }else{

          bpM<-barplot(t(numShMatx),beside=beside[2],ylim=c(0,max(rowSumValues)),ylab=ylab[2],xlab=xlab[2],
                  col=barPs$col)
                  dmRlegend(bpM[minIndex],max(rowSumValues),colnames(numShMatx),xjust=xJustify,yjust=1,
                  fill=barPs$col[1:lengthCol],bty="n",ncol=numLegCols)
	  }
                  #dmRlegend(bpM[1,minUseIndx],max(rowSumValues),colnames(numShMatx),xjust=xJustify,yjust=1,
                  #fill=barPs$col,angle=barPs$angl,density=barPs$dens)
        #box()
        mtext(titleOfPlot[2],side=3,line =2, cex=1.5*scaleCex, font = 2,adj=0.5) 

        #mtext(paste("n = ",rowSumValues, sep=""),side=1,line=0,at=bpM,cex=1.0*scaleCex)
        #mtext("n refers numbers of Qcs",side=1,line=2,cex=1.0*scaleCex)
	} #endof SecondPlot

  }#end of else for more than one plates

}

