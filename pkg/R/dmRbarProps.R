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
# Function dmRbarProps 
######################################### 
#
## This function returns properties like angles and densities
## for Barplot to use
##
#This function written by Zekai Otles <otles@fstrf-wi.org>
"dmRbarProps"<-function(nPairs,isColor=FALSE){
 library("dmRTools") 

 nPairs<nPairs+1

 

angles<-array(0,dim=nPairs)
densities<-array(0,dim=nPairs)
 if(isColor){
 #need to have dynamic color
   colors<-array(c("blue","red","green","yellow","orange","magenta","purple","cyan","black","gray"),dim=nPairs)
 }else{
   colors<-gray(seq(0.1,0.9,len=nPairs))
}

 

#determine the delta Angle for lines
delAngle<-2*90/nPairs  #angle incremental unit
den<-30              #density initial values 
denIncr<-100/nPairs
if(nPairs>5){
  delAngle<-delAngle
}

for(i in 1:nPairs){
  if(i >2) {
    inAng<-angles[i-2]+delAngle}
  else{
    inAng<-delAngle
  }

  if(i%%2 !=0){
    angles[i]<-inAng
    densities[i]<-den
  }else{
    angles[i]<-(angles[i-1]+90)
    densities[i]<-densities[i-1]
    den<-denIncr+densities[i] #increase 1 unit
  }

     
}#end of for loop

#this part is modified on 1/11/2007 to better represent the stack bar  plot

if(nPairs>2){
  #always second box white
  #recycle previous values
   recycCol<-colors[-1]
  colors[2]<-gray(1)
  colors[3:nPairs]<-recycCol[1:length(recycCol)-1]
   recycDen<-densities[-1]
  densities[2]<-0
  densities[3:nPairs]<-recycDen[1:length(recycDen)-1]
}

if(nPairs>4){
  #always second fourth box black
  colors[4]<-gray(0)
  densities[4]<-100
}

list(angl=angles,dens=densities,cols=colors)

}
