######## Kanghui Jiang . kj2381 . STAT4400 . HW3 ######

### 1 ###

AdaBoost<-function(X,y,M){
  n<-dim(X)[1] #number of datapoints
  d<-dim(X)[2] #dimension of X
  w<-rep(1/n,n)
  alphas<-matrix(0,M,1)
  all.pars<-rep(list(list()),M) #store all the values of pars. col->j, theta, m
  
  
  for(m in 1:M){
    pars <- train(X,w,y)
    all.pars[[m]]<-pars
    
    label<- Classify(X,pars)
    misclass_sum=0
    
    for(i in 1:n){
      if(y[i]!=label[i]){ misclass_sum<-misclass_sum+w[i]}
    }
    err<-misclass_sum/sum(w)
    
    alphas[m,]= log((1-err)/err)
    
    for(i in 1:n){
      if(y[i]!=label[i]){
        w[i]<-w[i]*(1-err)/err
      }
    }
  }
  result<-list(all.pars=all.pars,alphas=alphas)
  return(result)
  
}

agg_class<-function(X,alpha,allPars){
  n<-dim(X)[1] 
  M<-dim(alpha)[1]
  agg.label<-matrix(0,nrow=n,ncol=M) ###each row is for one data entry.(including all classification result for all 1~M weak learners)
  for(m in 1:M){
    labels <- Classify(X,allPars[[m]])
    agg.label[,m]<-agg.label[,m]+as.matrix(labels)
  }
  
  f=sign(agg.label%*%alpha)

  return (f)
}
#M=5
#ada=AdaBoost(X,Y,M)
#f=agg_class(X,ada$alphas,ada$all.pars)  #testing

### 2 ###

train<-function(X,w,y){
  n<-dim(X)[1] #number of datapoints
  d<-dim(X)[2]
  
  Err<-rep(0,d)
  theta<-rep(0,d)
  all.m<-rep(0,d)
  
  for(j in 1:d){
    sorted.index<-order(X[,j])
    s.X<-X[sorted.index,j]
    s.y<-y[sorted.index]
    #m stores the value to be chosen for x>t 1 or -1
    #when t=n
    misc<-(s.y+1)/2   ###classify x<t to be -1.all y=+1 is misclassified
    m=1
    #classfying x>t to be 1 is wrong
    if(mean(misc)>=0.5){
      m = -1
      misc=-misc
    }
    else{
      m= 1
    }
    werr.opt=(w%*%misc)/sum(w)
    t.opt=n
    m.opt=m
    
    for(t in (n-1):1){
      if(s.X[t]!=s.X[t+1]){  ##if same value, just skip
        
        misc[1:t]<-(s.y[1:t]+1)/2  ###for x<=t,all y=+1 is misclassified
        misc[(t+1):n]<-(-s.y[(t+1):n]+1)/2 ###for x>t,all y=-1 is misclassified
        
        #classfying x>t to be 1 is wrong
        if(mean(misc)>0.5){
          m= -1
          misc=-misc
        }
        else{
          m= 1
        }
        werr.temp<-(w%*%misc)/sum(w)
        
        if(werr.temp<werr.opt){
          werr.opt<-werr.temp
          t.opt<-t
          m.opt<-m
        }
      }
    }
    Err[j]<-werr.opt
    theta[j]<-t.opt
    all.m[j]<-m.opt
  }
  
  minErr<-min(Err)
  OPT.j<-min(which(Err==minErr))
  OPT.t<-theta[OPT.j]
  OPT.m<-all.m[OPT.j]
  
  pars<-list(j=OPT.j,theta=OPT.t,m=OPT.m)
  return(pars)
  
}

Classify<-function(X,pars){
  label<-(2*(X[,pars$j]>pars$theta)-1)*pars$m  #at dimension j,if x>t, then label it as m
  return (label)
}


X<-read.table('uspsdata.txt')
Y<-read.table('uspscl.txt')[,1]


###3###

n<-dim(X)[1]

index<-sample.int(n)
M.max=100  #max number of weak learners

train_error=matrix(0,M.max,5)
test_error=matrix(0,M.max,5)
for(M in 1:M.max){
  
  for(cv in 1:5){
    test.index<-index[(n*(cv-1)/5):(n*cv/5)]
    
    test.set=X[test.index,]
    train.set=X[-test.index,]
    test.Y=Y[test.index]
    train.Y=Y[-test.index]
    
    ada1<-AdaBoost(train.set,train.Y,M)
    f.train=agg_class(train.set,ada1$alphas,ada1$all.pars)
    train_error[M,cv]=mean(f.train!=train.Y)
    
    ada2<-AdaBoost(test.set,test.Y,M)
    f.test=agg_class(test.set,ada$alphas,ada$all.pars)
    test_error[M,cv]=mean(f.test!=test.Y)
  }
  
}

###4###


plot(seq(1:M.max),rowMeans(train_error))
plot(seq(1:M.max),rowMeans(test_error))

