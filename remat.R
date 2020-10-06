remat<-function(Datao,cl){

lab <- unique(cl)

Dat1=Datao[,which(cl == lab[1])]
Dat2=Datao[,which(cl == lab[2])]

betawt1<-xx1<-matrix(0,nrow(Dat1),ncol(Dat1)); 

for (p1 in 1:nrow(Dat1))
{ 
xx1[p1,]<-RobMeanVar(Dat1[p1,])$xx
betawt1[p1,]<-RobMeanVar(Dat1[p1,])$Wt
}

betawt2<-xx2<-matrix(0,nrow(Dat2),ncol(Dat2)); 

for (p2 in 1:nrow(Dat2))
{ 
xx2[p2,]<-RobMeanVar(Dat2[p2,])$xx
betawt2[p2,]<-RobMeanVar(Dat2[p2,])$Wt
}

up_xx<-cbind(xx1,xx2)
betawt<-cbind(betawt1,betawt2)
smallestbetawt<-apply(betawt,1,min)

return(list(remat=up_xx,betawt=smallestbetawt))
}







