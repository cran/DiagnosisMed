interact.ROC<-function(gold,test){
  # require(TeachingDemos)
  # require(tcltk)
  ask=FALSE
  i.ROC<-cbind(test,gold)
  without<-subset(i.ROC, subset=gold==0, select=test, drop = FALSE)
  with<-subset(i.ROC, subset=gold==1, select=test, drop = FALSE)
  roc.demo(x = without, y = with)
}
