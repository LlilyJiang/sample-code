############# Kanghui Jiang  kj2381 HW2################


####### 2 ##########

###(1)
classify <- function(S,z){
  
  c <- as.vector(sign(S %*% z))
  return(c)
}

###(2)

perceptrain <- function(S,y){
  k=1  #k is the number of current interation
  z<-matrix(1,nrow=ncol(S),ncol=1)
  Gradient_Cp<-matrix(0,nrow=ncol(S),ncol=1)
  Z_history<-z
  while(1){ 
    z<-z-(1/k)*Gradient_Cp  #update z from z and Cp of last iteration
    if(k>1){
      Z_history <- cbind(Z_history,z)  #last col of Z_history is the optimal z
    }
    fx <- classify(S,z)
    i=1
    Cp=0
    Gradient_Cp<-matrix(0,nrow=ncol(S),ncol=1)
    while(i<=nrow(S)){    #for each row in S.  
      if(fx[i]==y[i]){
        I=0
      }
      else{
        I=1
      }  
      Cp <- Cp + I*(S[i,]%*%z)
      print(Gradient_Cp)
      print(S[i,])
      Gradient_Cp <- Gradient_Cp + I*(-y[i])*t(S)[,i]
      i<-i+1
    }
    
    if(Cp==0){  
      return (list(z=z,Z_history=Z_history))  ###last column represents the z which reaches 0 
      break
    }

    k<-k+1
      
  }
}


###(3)

z3=matrix(runif(3),nrow=3,ncol=1)
z3[3,]=-1

data<-fakedata(z3,100)
ZH<-perceptrain(data$S,data$y)
fx.train<-classify(data$S,ZH$z)
mean(data$y==fx.train) # the correct rate for train set is 100%

test<-fakedata(ZH$z,100)
fx.test<-classify(test$S,ZH$z)
mean(test$y==fx.test)# the correct rate for test set is 100%


###(4)
#for test data
plot(test$S[,1],test$S[,2],col=test$y+2)
abline(-ZH$z[3]/ZH$z[2],-ZH$z[1]/ZH$z[2])

# for train data
plot(data$S[,1],data$S[,2],col=data$y+2)
par(new=TRUE)
i=1
# plot Z_history in iterations(including z)
while(i<=ncol(ZH$Z_history)){
  m<-ZH$Z_history[,i]
  par(new=TRUE)
  print(m)
  abline(-m[3]/m[2],-m[1]/m[2],col=i)
  i<-i+1
}

#plot z
abline(-ZH$z[3]/ZH$z[2],-ZH$z[1]/ZH$z[2],lwd=c(5,5),col=8)

legend(locator(),locator(),c(1:ncol(ZH$Z_history)), lty=c(1,1),lwd=c(2,2),col=c(1:ncol(ZH$Z_history)))




############ 3 ##############
##### (1)
library(e1071)

uspsdata <- read.table('uspsdata.txt',header = F)
uspscl <- read.table('uspscl.txt',header = F)

n.test=sample(1:200,200*0.2)

test.data<-uspsdata[n.test,]
train.data<-uspsdata[-n.test,]

test.uspscl<-uspscl[n.test,]
train.uspscl<-uspscl[-n.test,]
#test.label<-factor(test.uspscl,c(-1,1))
#train.label<-factor(train.uspscl,c(-1,1))

dat=data.frame(x=train.data, y=as.factor(train.uspscl))
dat.test=data.frame(x=test.data, y=as.factor(test.uspscl))

### (a) linear ###
cost_seq=10^(seq(-3,1,0.2))
tune.out1=tune(svm,y~.,data=dat,kernel="linear",ranges=list(cost=cost_seq))
summary(tune.out1)
bestmod1=tune.out1$best.model   ###can know the bestmod in this case
plot(tune.out1$performances[,'cost'],tune.out1$performances[,'error'])


### (b) kernel ###
cost_seq=10^(seq(-3,1,0.2))
gamma_seq=10^(seq(-4,-1,1))

tune.out=tune(svm,y~.,data=dat,kernel="radial",ranges=list(cost=cost_seq,gamma=gamma_seq))
summary(tune.out)
bestmod=tune.out$best.model   ###can know the bestmod in this case
#plot(tune.out$performances[,'cost'],tune.out$performances[,'error'])
perf<-tune.out$performances

i=1
while(i<=length(gamma_seq)){
  perf.bygamma<-perf[perf[,'gamma']==gamma_seq[i],]
  kernel.cost<-perf.bygamma$cost
  kernel.error<-perf.bygamma$error
  plot(kernel.cost,kernel.error,type='l',col=i)
  par(new=TRUE)  
  i<-i+1
}
legend(locator(),locator(), gamma_seq, lty=c(1,1),lwd=c(2.5,2.5),col=c(1,2,3,4))

######## (2)

linear.pred=predict(bestmod1,newdata=dat.test)
mean(linear.pred==test.uspscl)   ###correct rate
mean(linear.pred!=test.uspscl)   ###error rate

kernel.pred=predict(bestmod,newdata=dat.test)
mean(kernel.pred==test.uspscl)
mean(kernel.pred!=test.uspscl)
