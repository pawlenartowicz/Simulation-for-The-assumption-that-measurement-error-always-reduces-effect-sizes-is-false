r <- .15

propor <-numeric(31)
powers<-seq(50,3050,100)

xerror<-0.5
yerror<-0.5

for (j in 1:31)  {
  
  sims<-array(0,c(1000,4))
  for (i in 1:1000) {
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
  
  temp<-sims[abs(sims[,3]/sims[,4])> 2,]
  
  propor[j] <- table((abs(temp[,3]/temp[,4])> abs(temp[,1]/temp[,2])))[2]/length(temp[,1])
  
  print(j)
}

plot(powers,propor,type="l",xlab="Sample Size",ylab="Prop where error slope greater",col="blue")
