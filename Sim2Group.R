Sim2Group <-
function(ng,n1,n2,var0=0.1,pde=0.05)
{
sz<-c(n1,n2);
TrueDE <-rep(c(1,0),c(ng*pde,(ng-(ng*pde))))
    mu1 <- runif(ng, min = 2, max =5)
    mu2.de <- runif(ng, min = 2, max =5)#
    mu2 <- mu1
    DEt<-which(TrueDE==1)
    DEmu1<-DEt[1:round((ng*pde)/2)]
    DEmu2<-DEt[(length(DEmu1)+1):ng*pde]
    mu1[DEmu1] <- mu1[DEmu1]+3
    mu2[DEmu2] <- mu2.de[DEmu2]+3
    DataMat<-matrix(NA,ng,sum(sz))

    for (jj in 1:length(TrueDE))
      {
        DataMat[jj,]<-c(mu1[jj]+rnorm(sz[1],0,var0), 
                        mu2[jj]+rnorm(sz[2],0,var0))
      }
    return(list(outmat=DataMat, DEtrue=TrueDE))
}
