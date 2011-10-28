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
#


#######################################
# Function dmRcenterMap
#######################################
#The purpose of these function to
#plot center accrual data actual google maps
#This function written by Zekai Otles otles@fstrf-wi.org

"dmRcenterMap"<-function(centerData=centerData,outFile=NULL,tmpDir=NULL){
require(RgoogleMaps)
require(rgdal)
require(sp)
if(is.null(outFile))outFile="centerMap.png"

if(is.null(tmpDir))tmpDir="/tmp"

if(is.null(tmpDir)){
  if(.Platform$OS.type=="windows"){
		tmpDir<-paste("C:","temp",sep=.Platform$file.sep)
	}else{
	tmpDir<-"/tmp"}
}

outFile<-paste(tmpDir,outFile,sep=.Platform$file.sep)
mat<-centerData
out<-MapBackground(lat=mat$Latitude, lon=mat$Longtitude, destfile=outFile, NEWMAP = TRUE, myTile, zoom = NULL, size = c(320,320), GRAYSCALE = FALSE )
total<-sum(mat$NumOfSubjects)
normValues=10*(mat$NumOfSubjects/total)

png(outFile)
temp<-PlotOnStaticMap(lat = mat$Latitude, lon = mat$Longtitude,cex=normValues,pch=20,col='red',add=FALSE)
dev.off()
}