\name{visitMapDat}
\docType{data}
\alias{visitMapDat}
\title{visit Map Information for the study }
\description{
This dataset gives visit schdeule information for the study, data is data frame
and contains following variables
visitNo as int
typeOfVisits (basic visit types) are  B (for baseline), C(conditional), O( optional)
P (pre-baseline), S( scheduled) and T (Termination) this termination taken from DataFax system
labelOfPlate  is label for the CRF or plate
visitDueDate is date field when CRFs supposed to arrive
overdueDays is number of days when the scheuled CRFs will be counted as overdue
}
\usage{
data("dmRToolsData")
visitMapDat}
\format{data frame}
\author{ZekaiOtles}
\keyword{datasets}