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
n_sims<- 2000

for (j in 1:len)  {
  
  # the main difference is that error is now added to x or y in different scenarios
  
  sims<-array(0,c(n_sims,4))
  for (i in 1:n_sims) {
    # error on Y
    x <- rnorm(powers[j],0,1)
    y <- r*x + rnorm(powers[j],0,1)
    y_bis <- y + rnorm(powers[j],0,yerror)
    xx<-lm(y_bis~x)
    sims[i,1:2]<-summary(xx)$coefficients[2,1:2]
    # error in x
    x_bis <-x + rnorm(powers[j],0,xerror)
    y<-y
    xx<-lm(y~x_bis)
    sims[i,3:4]<-summary(xx)$coefficients[2,1:2]
  }
  
  # find significant observations (t test > 2) and then check proportion
  
  temp <- sims[abs(sims[,1]/sims[,2])> 2,]
  
  # I put here 'real overall effect' instead of 'real effect in sample'
  temp2 <- temp[temp[,1] > r,]
  meanRS1[j] <- mean(abs(temp[, 1]))
  
  temp <- sims[abs(sims[,3]/sims[,4])> 2,]
  temp2 <- temp[temp[,3] > r,]
  meanRS2[j] <- mean(abs(temp[, 3]))
  
  print(j/len)
}


# plot with mean effect size
plot(powers,meanRS1,type="l",xlab="Sample Size",ylab="Mean effect size",col="blue")
lines(powers, meanRS2, col = "red")
legend(200, 0.5, legend=c("Effect size with y error", "Effect size with x error"),
       col=c("blue", "red"), lty=1:2, cex=1)
