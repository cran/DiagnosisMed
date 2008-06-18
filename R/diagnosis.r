diagnosis <- function(gold,test1,print=TRUE,plot=FALSE)
#require(epitools)
#require(epicalc)
{
# to do ... 
#insert confidence intervals for AUC and PLR
# insert option for different confidence intervals
# by option (multicenter)
# test with 3 categories (indeterminate results)
#   testef <- as.factor(teste)
#   if(nlevels(testef)  2)
#                  {
                   # analysis this way
#                   }
tab<-table(test1,gold,dnn=c(deparse(substitute(test1)),deparse(substitute(gold))))
test<-c("negative","positive")
gold.standard<-c("negative","positive")
dimnames(tab) <- list("test" = test, "gold standard" = gold.standard)
tabmarg<-addmargins(tab)
#dnn is option of table command - specifies the names of row and column
TN<-tab[1,1]
FN<-tab[1,2]
FP<-tab[2,1]
TP<-tab[2,2]
# sample size
n<-sum(tab)
# prevalence
p<-(TP+FN)/n
# sensitivity and confidence interval
Se<-TP/(TP+FN)
Se.ci<-as.numeric(binom.wilson(TP, TP+FN, conf.level = 0.95)[4:5])
# especificity and confidence interval
Sp<-TN/(FP+TN)
Sp.ci<-as.numeric(binom.wilson(TN, FP+TN, conf.level = 0.95)[4:5])
# positive and negative likelyhood ratios
PLR<-Se/(1-Sp)
#PLR.ci<-
NLR<-(1-Se)/Sp
#NLR.ci<-
# accuracy
accu<-(TP+TN)/n
accu.ci<-as.numeric(binom.wilson(TP+TN, n, conf.level = 0.95)[4:5])
# positive and negative predictive values 
PPV<-TP/(TP+FP)
PPV.ci<-as.numeric(binom.wilson(TP, TP+FP, conf.level = 0.95)[4:5])
NPV<-TN/(TN+FN)
NPV.ci<-as.numeric(binom.wilson(TN, TN+FN, conf.level = 0.95)[4:5])
# diagnostic odds ratio
OR<-oddsratio(tab) 
DOR<-OR$measure[2,1]
#DOR<-(TP*TN)/(FP*FN)
DOR.ciI<-OR$measure[2,2]
DOR.ciS<-OR$measure[2,3]
# error rate and error trade
#ER<-((FN/(FN+TN))*p)+(((FP/(FP+TP))*(TN+FP))
ER<-(FN+FP)/n
ER.ci<-as.numeric(binom.wilson(FN+FP, n, conf.level = 0.95)[4:5])
ET<-(FN/FP)
# pre-test and pos-test odds (to do)
# area under ROC curve
if(plot==TRUE)
  {ROC<-roc.from.table(tab, graph = TRUE)}
if(plot==FALSE)
  {ROC<-roc.from.table(tab, graph = FALSE)}
AUC<-ROC$auc
# results evaluations
retval <- list(tabmarg=tabmarg,n=n,accu=accu,accu.ci=accu.ci,p=p,Se=Se,Se.ci=Se.ci,Sp=Sp,
          Sp.ci=Sp.ci,PLR=PLR,NLR=NLR,PPV=PPV,PPV.ci=PPV.ci,NPV=NPV,
          NPV.ci=NPV.ci,DOR=DOR,DOR.ciI=DOR.ciI,DOR.ciS=DOR.ciS,AUC=AUC,ET=ET,ER=ER,ER.ci=ER.ci)
class(retval) <- "diag"
if(print==TRUE)  {print(retval)}
invisible(retval)
}