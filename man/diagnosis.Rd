\name{diagnosis}
\alias{diagnosis}
\alias{diagnosisI}
\title{ Diagnostic test accuracy evaluation }
\description{
  diagnosis estimate sensitivity, specificity, predictive values, likelihood 
  ratios, area under ROC curve and other validity measures for binary diagnostic
   test evaluation.
}
\usage{
diagnosis(gold, test1, print = TRUE, plot = FALSE)
diagnosisI(TP,FN,FP,TN,print = TRUE, plot = FALSE)
}
\arguments{
  \item{gold}{The reference test (gold stanrd)}
  \item{test1}{The index test (test under evaluation)}
  \item{TP}{A number representing True Positives from a 2x2 table}
  \item{FN}{A number representing False Negatives from a 2x2 table}
  \item{FP}{A number representing False Positives from a 2x2 table}
  \item{TN}{A number representing True Negatives from a 2x2 table}
  \item{print}{If TRUE, diagnosis will print in the output window the 
   statistics resulted from the 2x2 table. Default is TRUE.}
  \item{plot}{If TRUE, diagnosis will plot a ROC curve of the test under
   evaluation. This plot may later be edited, as any ohter plot, with title,
   legends etc. Default is FALSE.}
}
\details{
In diagnosis, the values entered must be two variables in a dataframe. The 
first one the gold standard and the second the index test. These two 
variables must be coded either as numeric - 0 for negative and 1 for a 
positive test - or with the words "positive" and "negative". Never code the 
variables as TRUE or FALSE; diagnosis will level this categories uncorrectly. 
diagnosisI executes the very same statistics, but here, the values of a 2x2 
table are inputed. TP is true positive; TN is true negative; FP is false 
positive and FN is false negative. All confidence limits rely on binomial 
distribuition, which does not give result outside [0:1] such as normal 
distribution or assymptotic theory.
}
\value{
  2x2 table, Sample size, Prevalence, Sensitivity, Specificity, Predictive
  values, Likelihood ratios, Diagnostic odds ratio, Error trade off, Error rate,
  Accuracy, Area under ROC cruve and confidence intervals.
  }
\references{JA Knotterus, The Evidence Based Clinical Diagnosis; BMJBooks, 2002}
\author{Pedro Brasil - \email{pedro.brasil@ipec.fiocruz.br}}
\seealso{\code{\link{plot.diag}}, \link[epicalc]{roc.from.table}, \link[epitools]{binom.conf.int}, \link[epibaxis]{sensSpec}}
\examples{
# simulating a dataset
mygold<-c("negative","positive","negative","negative","positive",
  "negative","positive","negative","negative","positive","negative","positive")
mytest<-c("negative","negative","negative","positive","positive",
  "negative","positive","negative","negative","positive","negative","positive")
center<-rep(1:3,4)  
data<-cbind(mygold,mytest,center)
for(i in 1:6) {data<-rbind(data,data)}
data<-as.data.frame(data)
data[2,2]<-"positive"
data[12,2]<-"positive"
#attaching dataset
attach(data)
#removing unwanted objects
rm(mygold,mytest,center)
# running the function
test<-diagnosis(mygold,mytest,print=FALSE)
print(test)
# the same as
diagnosis(mygold,mytest)
# draw a nomogram from this test
test<-diagnosis(mygold,mytest,print=FALSE)
plot(test)
#different from - draw a ROC curve from this test
diagnosis(mygold,mytest,plot=TRUE,print=FALSE)
#inserting values from a 2x2 table
diagnosisI(364,22,17,211)
}
\keyword{univar}
\keyword{htest}
