r <- .15

powers<-seq(20,800,15)
propor <-numeric(length(powers))
ratio1 <-numeric(length(powers))
ratio2 <-numeric(length(powers))

xerror<-0.5
yerror<-0.5
n_sims<- 4000

for (j in 1:length(powers))  {

  
  sims<-array(0,c(n_sims,4))
  for (i in 1:n_sims) {
    x <- rnorm(powers[j],0,1)
    y <- r*x + rnorm(powers[j],0,1)
    xx<-lm(y~x)
    sims[i,1:2]<-summary(xx)$coefficients[2,1:2]
    x<-x + rnorm(powers[j],0,xerror)
    y<-y + rnorm(powers[j],0,yerror)
    xx<-lm(y~x)
    sims[i,3:4]<-summary(xx)$coefficients[2,1:2]
  }
  
  # find significant observations (t test > 2) and then check proportion
 
  temp <- sims[abs(sims[,1]/sims[,2])> 2,]
  temp2 <- temp[temp[,1] > r,] 
  ratio1[j] = length(temp2[,3])/length(temp[,3])  
  
  temp <- sims[abs(sims[,3]/sims[,4])> 2,]
  temp2 <- temp[temp[,3] > r,]
  ratio2[j] = length(temp2[,3])/length(temp[,3])
  print(ratio1[j])
  print(ratio2[j])
}

plot(powers,ratio2,type="l",xlab="Sample Size",ylab="Proportion of significant results that overestimate true effect",col="blue")
lines(powers, ratio1, col = "red")
legend(400, 1, legend=c("Without measurement error", "With measurement error"),
       col=c("red", "blue"), lty=1:2, cex=1)
