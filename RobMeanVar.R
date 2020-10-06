RobMeanVar <-function(xx)
#===========================================================
# ROBUST PARAMETER ESTIMATION USING BETA-DIVERGENCE METHOD #
#===========================================================
  {
    X<-xx
    Mo <- median(xx, na.rm = TRUE)
    misInx<-which(is.na(xx)==T)
    if (length(misInx)){
    xx<-xx[-misInx]
    }

   res1<-CalcMeanVar(xx,Mo)

#===========We modified missing data by robust mean================

for (i2 in 1:length(X))
     { if (is.na(X[i2])==T){
          X[i2] <- res1$MM}
}

xxx<-X

res2<-CalcMeanVar(xxx)
Wt<-res2$WW
Mo<-res2$MM
Vo<-res2$VV

###### Threshold Estimation based on sample observation

  alpha=0.1
  delta0=min(Wt)+alpha*(max(Wt)-min(Wt))

  if (min(Wt)<delta0) # if minimum weight for an observation less than thresold, 
                       # will take robust mean and variance otherwise classical mean and variance threshold 
     {M=Mo; V=Vo}
  else{M=mean(xxx); V=var(xxx)*(length(xxx)-1)/length(xxx)} # if no outlier
out<-as.integer(which(Wt<delta0))
Wt.out<-Wt[out]

#===========We can modified data by replacing unusual 
#           obserbations with robust mean/median of samples===================
for (i1 in 1:length(Wt))
     { if(Wt[i1] < delta0){
          xxx[i1] <- M}
}

return(list(xx=xxx,mu=M, Var=V, sd=sqrt(V),out=out,Wt=Wt,
       Wt.out=Wt.out,out.Thr=delta0))
}
