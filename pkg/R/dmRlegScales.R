# Copyright (C) 2011 Frontier Science & Technology
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
# Function dmRlegScales 
#######################################
#This functions used in the dmRlegend function 
#written by zekai otles <otles@fstrf-wi.org>
##
##
"dmRlegScales" <-
	function (par){

		x<-par("mfrow")
		scaleCex<-1.0/sum(x)
	maxY<-max(par("yaxp"))-2
	minY<-min(par("yaxp"))+2
        legendBoxScale<-0.25
        if(scaleCex<1){
          legendBoxScale<-scaleCex*legendBoxScale}
	list(scaleCex=scaleCex,maxY=maxY,minY=minY,legendBoxScale=legendBoxScale)
	
	
}
