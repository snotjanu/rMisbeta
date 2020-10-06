CalcMeanVar<-function(xx,Mo=median(xx)){
    Vo <- 1
   Beta<-0.2;
    DiffTol = 0.0001;
    DiffNorm = +10000;
    Iter = 0;
      while (DiffNorm > DiffTol)
      {
      Wt <- exp(-(Beta*(xx-Mo)^2)/(2*Vo))  
      Wt[which(Wt==0)]=0.000001
      while((sum(Wt<0.2)/length(Wt))>0.5)
      {Beta<-Beta-0.02;
      if(Beta<=0){Beta=.001}
      Wt <- exp(-(Beta*(xx-Mo)^2)/(2*Vo)) # Calculation of weights for each observation
      Wb<-sum(Wt<0.2)/length(Wt)
#      cat("Beta=",Beta,"\n")
#      cat("Wb=",Wb,"\n")
      }
      M.new <-  sum(Wt*xx)/sum(Wt) # Robust mean function
      V.new <- sum(Wt*(xx-Mo)^2)/sum(Wt) # Robust variance function
      DiffNorm <- sqrt(sum((M.new-Mo)^2))+sqrt(sum((V.new-Vo)^2))
      Mo = M.new
      Vo = V.new
      sd = sqrt(V.new)
      Iter = Iter + 1 
      } 
#END of while

return(list(MM=Mo, VV=Vo,WW=Wt))
}
