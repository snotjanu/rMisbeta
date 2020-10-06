OutMisDat<-function(xx,pctOut=NULL,pctMis=NULL)
{
Datao=xx
if (sum(pctOut)>0){
nOut=round(pctOut*dim(Datao)[1],0)
OutInx<-sample(1:dim(Datao)[1])[1:nOut]
for (jj in 1:length(OutInx)){
OUT<-runif(1, min = 5+max(abs(Datao[OutInx[jj],])), max =10+max(abs(Datao[OutInx[jj],])))
Datao[OutInx[jj],sample(1:dim(Datao)[2])[1]]<-OUT
}
}

if (sum(pctMis)>0){
nMis=round(pctMis*dim(Datao)[1],0)
MisInx=sample(1:dim(Datao)[1])[1:nMis]
for (jj in 1:length(MisInx)){
Datao[MisInx[jj],sample(1:dim(Datao)[2])[1]]<-NA
}
}
return(Datao)
}







