\name{print.diag}
\alias{print.diag}
\title{Print results from diagnosis function in the output window}
\description{
This functions prints in the output window the statistics from diagnosis and
diagnosisI functions.
}
\usage{
\method{print}{diag}(x, ...)
}
\arguments{
  \item{x}{An object created by diagnosis or diagnosisI.}
  \item{\dots}{Further generic print options.}
}
\author{Pedro Brasil - \email{pedro.brasil@ipec.fiocruz.br}}
\seealso{\code{\link{diagnosis}}}
\examples{
mytest<-diagnosisI(364,22,17,211, print=FALSE)
print(mytest)
# the same as
diagnosisI(364,22,17,211)
}
\keyword{univar}
\keyword{htest}
