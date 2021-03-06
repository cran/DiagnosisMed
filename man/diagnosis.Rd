\name{diagnosis}
\alias{diagnosis}
\alias{summary.diag}
\alias{print.diag}
\alias{plot.diag}
\title{Diagnostic test accuracy evaluation}
\description{\command{diagnosis} estimate sensitivity, specificity, predictive values, likelihood ratios, area under ROC curve and other validity measures for binary diagnostic test evaluation. It accepts as input either columns from a dataset or vectors, a 2 x 2 table or numbers representing true positives, false negatives, false positives and true negatives. \command{plot} for \command{diagnosis} draw a simple nomogram and \command{summary} for \command{diagnosis} creates an output in a table format that allows the output to be easily exported to a spreadsheet.
}
\usage{
diagnosis(a,b,c,d, CL = 0.95, print = TRUE, plot = FALSE)
\method{plot}{diag}(x, print = FALSE, ...)
\method{print}{diag}(x, ...)
\method{summary}{diag}(object, ...)
}
\arguments{
  \item{a}{A number representing True Positives from a 2x2 table. Also, TP could either a 2x2 table (see below) or a column in a dataset representing the reference standard.}
  \item{b}{A number representing False Negatives from a 2x2 table. If TP is a column in a dataset FN should also be a columns in a dataset, however representing the index test.}
  \item{c}{A number representing False Positives from a 2x2 table}
  \item{d}{A number representing True Negatives from a 2x2 table}
  \item{print}{If TRUE, diagnosis will print in the output window the statistics resulted from the 2x2 table. For plot, this will print in the output window a table with all pre-test and its corresponding post-test probabilities.}
  \item{plot}{There are two options of plot. If plot is called in \command{diagnosis}, then a ROC curve of the test under evaluation plotted. If plot is called from an object storing diagnosis output (see example) than a nomogram is plotted. These plots may later be edited, as any other plot, with title, legends etc. Default is FALSE.}
  \item{x}{For plot and print functions, x is an object assigned with diagnois output.}
  \item{CL}{Confidence limits for confidence intervals. Must be a numeric value between 0 and 1. Default is 0.95.}
  \item{object}{For summry function, 'object' is an object assigned with diagnois output.}
  \item{...}{Other options passed to \link[base]{print}, \link[graphics]{plot.default} or \link[base]{summary}.}
}
\details{
In \command{diagnosis}, the values entered must be eather two variables in a data frame, a 2 x 2 table or numbers corresponding to 2 x 2 table cells. If vectors or columns from a dataset, the first one should be the gold standard and the second should be the index test. These two variables must be coded either as numeric - 0 for negative and 1 for a positive test - or with the words "positive" and "negative" or "presence" and "absence". In a older version, there was diagnosisI function that was replace by diagnosis function. The values of a 2 x 2 table can be inputted as: TP is true positive; TN is true negative; FP is false positive and FN is false negative. Sensitivity, Specificity, Predictive values and Accuracy confidence limits rely on binomial distribution, which does not give result outside [0:1] such as normal distribution or asymptotic theory. DOR, Likelihood  ratios and Youden J index confidence limits rely on normal approximation (Wald method for likelihoods). The AUC (area under the ROC curve) is estimated by trapezoidal method (see below). Also, this functions have a summary function wich creates a matrix as a result (identical to the default print option) wich allows to easily export the results to a spreadsheet or to a odt file (with OdfWeave) in a table format (see example). If the input is a 2 x 2 table it should be formated as: 

\tabular{cll}{
\tab TN \tab FN \cr
\tab FP \tab TP \cr
}

\command{plot.diag} will draw a very simple nomogram as many examples from wikipedia \url{http://en.wikipedia.org/wiki/Nomogram}. This is not a generic nomogram as shown in many evidenced based medicine texts, because this one shows only pre-test and post-test variations with a fixed positive likelihood ratio. This likelihood is a statistic from an object created by \command{diagnosis} function. Its usage is the same as applying the Bayes theorem where the pre-test odds times positive likelihood ratio equals the pos-test odd (transforming the odds to probabilities). To use it, draw, with a rule, a vertical line from a desired pre-test  probability, and to find the corresponding post-test probability, draw a horizontal line from the intersection of the curve and the vertical line toward the vertical axis.
}                                                   

\value{
  A 2x2 table from which the validity measures are calculated.
  \item{Sample size}{The number of subjects analyzed.}
  \item{Prevalence}{The proportion classified as with the target condition by the
        reference standard}
  \item{Sensitivity}{the probability of the test to correctly classify subjects
        with the target condition (TP/(TP+FN))}
  \item{Specificity}{the probability of the test to correctly classify subjects
        without the target condition (TN/(TN+FP))}
  \item{Predictive values}{the probabilities of being with (positive predictive
        value) (TP/(TP+FP)) or without (negative predictive value) the target
         condition given a test result (TN/(TN+FN)).}
  \item{Likelihood ratios}{the probability of test a result in people with the
       target condition, divided by the probability of the same test result in
       people without the target condition (PLR = Se/(1-Sp); NLR = (1-Sp)/Se)}
  \item{Diagnostic odds ratio}{represents the overall discrimination of a
        dichotomous test, and is equivalent to the ratio of PLR and NLR.}
  \item{Error trade off}{Is the amount of false positives traded with false negatives
        for each decision threshold; here expressed as an odd - for binary results
        there is only one threshold}
  \item{Error rate}{Expresses how many errors we make when we diagnose patients with an abnormal test result as diseased, and those with a normal test result as non-diseased ((FP+FN)/sample size).}
  \item{Accuracy}{overall measure that express the capacity of the test to correctly classify subjects with and without the target condition ((TP+TN)/(sample size))}
  \item{Area under ROC curve}{overall measure of accuracy - here the method is the trapezoidal. It gives identical results as (Se+SP)/2.}
  }
\references{

Knotterus. The Evidence Based Clinical Diagnosis; BMJBooks, 2002.

Xiou-Hua Zhou, Nancy A Obuchowsky, Donna McClish. Statistical Mehods in diagnostic Medicine; Wiley, 2002.

Simel D, Samsa G, Matchar D (1991). Likelihood ratios with confidence: Sample size estimation for diagnostic test studies. Journal of Clinical Epidemiology 44: 763 - 770
}
\author{Pedro Brasil - \email{diagnosismed-list@lists.r-forge.r-project.org}}
\note{Bug reports, malfunctioning, or suggestions for further improvements or contributions can be sent, preferentially, through the DiagnosisMed email list, or R-Forge website \url{https://r-forge.r-project.org/projects/diagnosismed/}.
}
\seealso{\code{\link{LRgraph}}, \code{\link{ROC}},\code{\link{LRgraph}},\link[epitools]{binom.conf.int},
 \link[epibasix]{sensSpec},\link[epiR]{epi.tests},\link[Design]{nomogram},\link[epiR]{epi.nomogram}}
\examples{
# Simulating a dataset
mydata <- as.data.frame(rbind(
          cbind(rep(c("positive"),18),rep(c("negative"),18)),
          cbind(rep(c("positive"),72),rep(c("positive"),72)),
          cbind(rep(c("negative"),25),rep(c("positive"),25)),
          cbind(rep(c("negative"),149),rep(c("negative"),149))                    
          ))
colnames(mydata) <- c('culture','serology')
# A little description of the data set to check if it is ok!
str(mydata)
# Attaching the data set and checking 
attach(mydata)

# Running the diagnosis analysis
diagnosis(culture,serology)

#Simulating a table
mytable <- matrix(c(149,18,25,72), nrow = 2, ncol=2, byrow=TRUE,
               dimnames = list(enzyme=c('absent','present'),
                               citology=c('absent','present')))
# Running analysis from a 2 x 2 table
diagnosis(mytable)

#Inserting values as isolated numbers 
diagnosis(72,18,25,149)

#---------------------------------
# Export results to a spreadsheet:
#---------------------------------

# Assing diagnosis to an object
mytest <- diagnosis(364,22,17,211,print=FALSE)
# Assign the summary to an object
mt.sum <- summary(mytest) 
# Export to a spreadsheet using csv format - could also work to text with OdfWeave export.
# write.csv(mt.sum,'MytestResults.csv',quote = F,na='')


# Draw a nomogram from a test
plot(mytest)

rm(mydata,mytable,mytest,mt.sum)

}
\keyword{univar}
\keyword{htest}

