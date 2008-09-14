\name{ROC}
\alias{ROC}
\title{Draw a ROC curve, estimate good cut-offs and compute validity measures
       for each cut-off}
\description{
  Draw a non-parametric (empirical) ROC curve and compute test sensitivity,
  specificity, predictive values and likelihood ratios (and respective confidence
  limits) for each decision threshold. Estimate good decision threshold by a
  variety of methods.
}
\usage{
ROC(gold,
    test,
    CL = 0.95,
    Cost = 1,
    Prevalence = 0,
    Plot = TRUE,
    Plot.point = "Min.ROC.Dist",
    Print.full = FALSE,
    Print = TRUE)
}
\arguments{
  \item{gold}{The reference standard. A column in a dataframe indicating the classification
        by the reference test. The reference standard must have two levels: must be
        coded either as 0 - without target disease - or 1 - with the target disease;
        or could be coded \link[base]{as.factor} with the words "negative" - without target disease - and
        "positive"  - with the target disease.}
  \item{test}{The index test or test under evaluation. A column in a dataframe or
        indicating the test results in a continuous scale. May also work with discrete
        ordinal scale.}
  \item{CL}{Confidence limit. The limits of the confidence interval. Must be coded
        as number in a range from 0 to 1. Default value is 0.95}
  \item{Cost}{Cost = cost(FN)/cost(FP). MCT will be used to estimate a
        good cut-off. It is a value in a range from 0 to infinite.
        Could be financial cost or a health outcome with the perception that FN are
        more undesirable than FP (or the other way around). This item will run
        into MCT (misclassification cost term) - (1-prevalence)*(1-Sp)+Cost*prevalence(1-Se).
        Cost=1 means FN and FP have even cost. Cost = 0.9 means FP are 10 percent
        more costly. Cost = 0.769 means that FP are 30 percent more costly. Cost =
        0.555 means that FP are 80 percent more costly. Cost = 0.3 means that FP
        are 3 times more costly. Cost = 0.2 means that FP are 5 times more costly.
        }
  \item{Prevalence}{Prevalence of the disease in the population who the test will
        be performed. If left 0 (the default value), this will be replaced by the
        disease prevalence in the sample. This values will be used in the MCT and
        Efficiency formulas to estime good cut-offs.}
  \item{Plot}{If FALSE, the ROC curve plot will not be displayed. Default is TRUE.}
  \item{Plot.point}{The method of best cut-off estimation which will be displayed
        at ROC curve as a dot. Default is "Min.ROC.Dist". Possible options are:
        \item{"Max.Accuracy"}{- the cut-off which maximize the accuracy;}
        \item{"Max.DOR"}{- the cut-off which maximize the diagnostic odds ratio;}
        \item{"Error.rate"}{- the cut-off which minimizes the error rate;}
        \item{"Max.Accuracy.area"}{- the cut-off which maximize the accuracy area;}
        \item{"Max.Sens+Spec"}{- the cut-off which maximize the sum of sensitivity
              with specificity;}
        \item{"Max.Youden"}{- the cut-off which maximize the Youden index;}
        \item{"Se=Sp"}{- the cut-off which Sensitivity is equal to Specificity;}
        \item{"Min.ROC.Dist"}{- the cut-off which minimize the distance between
              the curve and the upper left corner of the graph;}
        \item{"Max.Efficiency"}{- the cut-off which maximize the efficiency;}
        \item{"Min.MCT"}{- the cut-off which minimize the misclassification cost term.}
  }
  \item{Print.full}{If TRUE, a table with sensitivity, specificity, predictive values
        and likelihood ratios (and respective confidence limits) for each decision
        threshold will be displayed.}
  \item{Print}{If FALSE, no results (detailed below in vlaues section) will be displayed on the
        output window. Default is TRUE}
}
\details{
   Tests results matching the cut-off values will be considered a positive test.
   ROC assumes that subjects with higher values of the test are with the target
   condition and those with lower values are without the target condition. Tests
   the have a behavior like glucose (middle values are supposed to be normal and
   extreme values are supposed to be abnormal) and immunefluorescence (lower
   values - higher dilutions - are suppose to be abnormal) will not be correctly
   analyzed. In the latter example, multiplying the test results by -1 or other
   transformation before analysis could make it work. The result table with the
   Print.full option, has more columns than can be shown in the screen. R
   automatically  shows these columns below, therefore one has to be careful when
   relating the corresponding lines.
   \item{1}{Diagnostic odds ratio: }
           \eqn{DOR = (TP*TN)/(FN*FP); the same as: DOR = PLR/NLR}
   \item{2}{Accuracy area: }
           \eqn{AA = (TP*TN)/((TP+FN)*(FP+TN))}
   \item{3}{Youden index: }
           \eqn{Y = Se+Sp-1; the same as: Y = Se-FPR}
   \item{4}{Minimum ROC distance: }
           \eqn{m ROC Dis = (Sp-1)^2+(1-Se)^2}
   \item{5}{Efficiency: }
           \eqn{Ef = Se*prevalence+(1-prevalence)*Sp}
   \item{6}{Misclassification Cost Term: }
           \eqn{MCT = (1-prevalence)*(1-Sp)+(cost(FN)/cost(FP))*prevalence*(1-Se)}
}
\value{
  \item{pop.prevalence}{The disease prevalence informed by the user. If not
        informed, it will be the same as the sample prevalence.}
  \item{sample.prevalence}{The disease prevalence in the sample}
  \item{sample.size}{The number of subjects analyzed}
  \item{test.summary}{A table showing the quintiles, mean and standard deviation
        of overall test results, test results from those with the target condition
        and without the target condition}
  \item{AUC.summary}{A table showing the AUC estimated by DeLong method (trapezoidal)
        and its confidence limits.}
  \item{test.best.cutoff}{A table showing the best cut-offs estimated by methods
        described above, its corresponding sensitivity, specificity and positive
        likelihood ratio (and their confidence limits)}
}
\references{
  \item{1}{Knotterus. The Evidence Based Clinical Diagnosis; BMJBooks, 2002.}
  \item{2}{Xiou-Hua Zhou, Nancy A Obuchowsky, Donna McClish. Statistical Mehods
          in diagnostic Medicine; Wiley, 2002.}
  \item{3}{Simel D, Samsa G, Matchar D (1991). Likelihood ratios with confidence:
           Sample size estimation for diagnostic test studies. Journal of Clinical
           Epidemiology 44: 763 - 770}
  \item{4}{S.B. Cantor, C.C. Sun, G. Tortolero-Luna, R. Richards-Kortum, and
           M. Follen. (1999) A comparison of C/B ratios from studies using receiver
           operating characteristic curve analysis. Journal of Clinical Epidemiology,
            52(9):885-892.}
  \item{5}{Greiner, M. (1996) Two-graph receiver operating characteristic (TG-ROC):
           update version supports optimisation of cut-off values that minimise
           overall misclassification costs. J.Immunol.Methods 191:93-94.}
}
\author{Pedro Brasil - \email{pedro.brasil@ipec.fiocruz.br}}
\note{Bug reports, malfunctioning, or suggestions for further improvements can
 be sent, preferentially, through the epidemiologic R list.
}
\seealso{\code{\link{diagnosis}},\code{\link{interact.ROC}},\link[PresenceAbsence]{optimal.thresholds},
         \link[epicalc]{roc.from.table},\link[ROCR]{prediction}}
\examples{
# loading a dataset
data(tutorial)
# attaching a dataset
attach(tutorial)
# The reference standard is not in the correct format
# Recoding the reference standard to "positive" & "negative"
tutorial$Gold2<-as.factor(ifelse(Gold=="pos","positive","negative"))
# attaching the data set with the modifications
attach(tutorial)
# A little discription of the data set to check if it is ok!
str(tutorial)
# Running ROC analysis wtih the satandard options
ROC(Gold2,Test_B)
}
\keyword{iplot}
\keyword{univar}
\keyword{htest}
