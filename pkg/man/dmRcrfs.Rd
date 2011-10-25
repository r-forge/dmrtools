\name{dmRcrfs}
\alias{dmRcrfs}
\title{Creates Time Plot and Bar Plot for Incoming CRFs}
\description{This function creates  time plot for incoming CRFS
}
\usage{
dmRcrfs(sh,freq)
}
\arguments{
\item{sh}{sh  incoming crf data}
\item{freq}{freq  is  incomig CRFs frequescy, could be weekly, daily, Monthly }
}

\seealso{
 \code{\link{chron}},
         \code{\link{matplot}} , \code{\link{matpoints}} ,
	and
         \code{\link{dmRcrfBarPlot}}
}
\examples{
library("dmRTools")
data("dmRToolsData")
#same as
dmRcrfs(crfsWkDat,freq="weekly")
#or
dmRcrfs(crfsDlyDat,freq="daily")
#or
dmRcrfs(crfsMonDat,freq="monthly")
#or
dmRcrfs(crfsWkDat,freq="weekly")

}
\author{Zekai  Otles}
