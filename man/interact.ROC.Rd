\name{interact.ROC}
\alias{interact.ROC}
\title{Interactively draw a ROC curve with your data}
\description{
Draw a ROC curve with the user data, interactively, sliding a button, and watch
how changes at the cutoff, correlate with the changes of Sensitivity and Specificity
while building the ROC curve itself.
}
\usage{
interact.ROC(gold, test)
}
\arguments{
  \item{gold}{A column in a dataframe or a vector indicating the classification
   by the reference test. Must be coded either as 0 - without target disease -
   or 1 - with the target disease.}
  \item{test}{A column in a dataframe or a vector indicating the test under
   study (index test) results. Must be numeric.}
}
\details{
interact.ROC is a call from roc.demo{TeachingDemo}. The difference is that
 interact.ROC allow the input data be displayed as usually is in diagnostic
 studies, a column with the test result and the other with the reference standard
 results. Inside this function the "ask" option - which
 controls the "next" button to see the next graph - is turned off. To turn it on
 again later, type ask=TRUE. The test must have a rationale that higher values
 of the index test belong to those with the target disease and those with lower
 values belong to those without the target disease. If this is not the case, the
 suggestion is to transform the tests results by multiplying it by -1 before
 running interact.ROC.
}
\value{
A ROC graph with a sliding button to interact and see the Sensitivity and
Specificity changes.
}
\references{JA Knotterus, The Evidence Based Clinical Diagnosis; BMJBooks, 2002}
\author{Pedro Brasil - \email{pedro.brasil@ipec.fiocruz.br}}
\note{Bug reports, malfunctioning, or suggestions for further improvements can
 be sent, preferentially, through the epidemiologic R list.
}
\seealso{\code{\link{diagnosis}},\code{\link{ROC}},\link[epicalc]{ROC},\link[TeachingDemos]{roc.demo}}
\examples{
data(rocdata)
attach(rocdata)
interact.ROC(Gold,test2)
}
\keyword{dynamic}
\keyword{iplot}
