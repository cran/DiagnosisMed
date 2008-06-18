\name{plot.diag}
\alias{plot.diag}
\title{Nomogram from 2x2 table diagnostic test}
\description{
This function draw a simple nomogram from an object created by diagnosis or 
diagnosisI
}
\usage{
\method{plot}{diag}(x, print = FALSE, ...)
}
\arguments{
  \item{x}{An object from diagnosis or diagnosisI}
  \item{print}{If TRUE, plot.diag will print in the output window the table 
  with all pre-test and its correspondig post-test probabilities. Default is
  FALSE}
  \item{\dots}{Futher generic options for plot function}
}
\details{
 plot.diag will draw a very simple nomogram as many examples from wikipedia 
 \url{http://en.wikipedia.org/wiki/Nomogram}. This is not a generic nomogram 
 as shown in many evidenced based medicine texts, because this one shows only 
 pre-test and post-test variabilities with a fixed likelihood ratio. This 
 likelihood is a statistic from an object created by diagnosis or diagnosisI.
 To use it, draw, with a rule, a vertical line from a desired pre-test 
 probability, and to find the corresponding post-test probability, draw a 
 horizontal line from the intersection of the curve and the vertical line.
}
 
\references{JA Knotterus, The Evidence Based Clinical Diagnosis; BMJBooks, 2002}
\author{Pedro Brasil; \email{pedro.brasil@ipec.fiocruz.br}}
\seealso{\code{\link{diagnosis}},\link[Design]{nomogram}}
\examples{
#this will draw a ROC curve
mytest<-diagnosisI(364,22,17,211, print=TRUE, plot=TRUE)
#this is different, and will draw a nomogram
mytest<-diagnosisI(364,22,17,211)
plot(mytest)
#draw a nomogram and print it's table
mytest<-diagnosisI(364,22,17,211)
plot(mytest,print=TRUE)
}
\keyword{univar}
\keyword{htest}
\keyword{aplot}
