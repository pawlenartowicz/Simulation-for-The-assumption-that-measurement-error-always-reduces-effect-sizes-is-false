r <- .15

powers<-seq(20,320,15)
len = length(powers)
propor <-numeric(len)
ratio1 <-numeric(len)
ratio2 <-numeric(len)
meanRS1 <-numeric(len)
meanRS2 <-numeric(len)

xerror<-0.5
yerror<-0.5
n_sims<- 8000

for (j in 1:len)  {

  
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
  meanRS1[j] <- mean(abs(temp[, 1]))
  ratio1[j] = length(temp2[,3])/length(temp[,3])  
  
  temp <- sims[abs(sims[,3]/sims[,4])> 2,]
  temp2 <- temp[temp[,3] > r,]
  meanRS2[j] <- mean(abs(temp[, 3]))
  ratio2[j] = length(temp2[,3])/length(temp[,3])
  
  print(j/len)
}

# plot with proportion
plot(powers,ratio1,type="l",xlab="Sample Size",ylab="Proportion of significant results that overestimate true effect",col="blue")
lines(powers, ratio2, col = "red")
legend(200, 1, legend=c("Without measurement error", "With measurement error"),
       col=c("blue", "red"), lty=1:2, cex=1)


# plot with mean effect size
plot(powers,meanRS1,type="l",xlab="Sample Size",ylab="Mean effect size",col="blue")
lines(powers, meanRS2, col = "red")
legend(200, 0.5, legend=c("Without measurement error", "With measurement error"),
       col=c("blue", "red"), lty=1:2, cex=1)
