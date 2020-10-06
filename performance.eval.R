performance.eval <-function(PostP,de.true,TopG,decreasing=TRUE)
{
m=length(TopG);
FDRo=rep(0,m); TPRo=rep(0,m);FPRo=rep(0,m);FDRo=rep(0,m);TNRo=rep(0,m);
FNRo=rep(0,m); ERo=rep(0,m); #auc<-rep(0,m);paucp2<-rep(0,m)
TP<-rep(0,m);FP<-rep(0,m);TN<-rep(0,m);FN<-rep(0,m)
for (ii in 1:m) 
{
Porder<-sort(PostP, decreasing, index.return=TRUE)
Top_pIdx<-Porder$ix

TP[ii] <- length(intersect(Top_pIdx[1:TopG[ii]], which(de.true==T)));
FP[ii] <- length(intersect(Top_pIdx[1:TopG[ii]], which(de.true==F)));
TN[ii] <- length(intersect(Top_pIdx[-(1:TopG[ii])], which(de.true==F)));
FN[ii] <- length(intersect(Top_pIdx[-(1:TopG[ii])], which(de.true==T)));
TPRo[ii]<-TP[ii]/(TP[ii]+FN[ii]);       #True positive rate
if((TP[ii]+FN[ii])==0){TPRo[ii]=0}
TNRo[ii]<-TN[ii]/(TN[ii]+FP[ii]);       #True negative rate
if((TN[ii]+FP[ii])==0){TNRo[ii]=0}
FPRo[ii]<-FP[ii]/(TN[ii]+FP[ii]);        #False positive rate
if((TN[ii]+FP[ii])==0){FPRo[ii]=0}
FNRo[ii]<-FN[ii]/(TP[ii]+FN[ii]);        #False negative rate
if((TP[ii]+FN[ii])==0){FNRo[ii]=0}
FDRo[ii] <-FP[ii]/(TP[ii]+FP[ii]);       #False Discovery rate
if((TP[ii]+FP[ii])==0){FDRo[ii]=0};
ERo[ii]<-(FP[ii]+FN[ii])/length(PostP); #MisClassification Rate
}
R1 <- rocdemo.sca(rbinom(40,1,.3), rnorm(40), dxrule.sca, 
      caseLabel="new case", markerLabel="demo Marker" )
R1@sens<-TPRo; #Sensitivity
if (length(which(FPRo>0))>0) {FPRo<-FPRo}else{FPRo[length(FPRo)]<-0.00001} 
R1@spec<-(1-FPRo)
R1@cuts<-as.numeric(c("Inf",sort(seq(0,0.99999,length=length(PostP)),decreasing=T)))
auc<-AUC(R1); # AUC
paucp2<- pAUC(R1,0.2)# pAUC upto FPR<=0.2
list(TP=TP,TN=TN,FP=FP,FN=FN,R1=R1,TPR=TPRo,TNR=TNRo,FPR=FPRo,FNR=FNRo,
     FDR=FDRo,ER=ERo,AUC2=auc,pAUC2=paucp2)
}
