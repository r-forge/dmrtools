\name{dmRwkToDate}
\alias{dmRwkToDate}
\title{ Converts Week of the Year to Calendar Date}
\description{This function converts week of the year to the calendar date }
\usage{
dmRwkToDate(year,wk)
}
\arguments{
\item{year}{year value as 4 digits integer }
  \item{wk}{week value as 2 digits integer}
}
\seealso{ \code{\link{dmRcrfs}}
}
\examples{
library("dmRTools")
require("chron")
dateValue<-dmRwkToDate(1998,52)
}
\author{Zekai  Otles}
