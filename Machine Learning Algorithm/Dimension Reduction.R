############################## 1 #######################################
######## for n=40 ############
###Generate datasets random from [0,1]. N=100
N=1;
D1<-matrix(NA, nrow = 40, ncol = 100);
while(N<=100){
  x<-runif(40);
  D1[,N]<-x;
  N<-N+1;
}


###find the smallest m that can satisfy delta<0.5
m=2
while(m<=40){
  A1<-matrix(rnorm(m*40)/sqrt(m),nrow=m,ncol=40)
  R1<-A1%*%D1 # the reduced matrix
  
  delta1=0 #the sum of error of alpha for all pairs
  s=1
  while(s<=20){
    x<-sample.int(100, 2)
    alpha1_true=dist(t(D1[,x]),method = "euclidean") #calculate the euclidean distance between the randomly selected pair of points
    alpha1_red=dist(t(R1[,x]),method = "euclidean")
    delta1<-delta1+abs(alpha1_red-alpha1_true)
    s<-s+1
  }
  if(delta1/20<=0.5){
    print(paste0("The smallest qualified m should be: ", m))
    print(paste0("The corresponding qualified average delta is ",delta1/20))
    break
  }
  m<-m+1
}

#change 40 to 80, we can get the result for n=80 similarly
######## for n=80 ############
N=1;
D2<-matrix(NA, nrow = 80, ncol = 100);
while(N<=100){
  x<-runif(80);
  D2[,N]<-x;
  N<-N+1;
}

m=2
while(m<=80){
  A1<-matrix(rnorm(m*80)/sqrt(m),nrow=m,ncol=80)
  R1<-A1%*%D2 # the reduced matrix
  
  delta1=0 #the sum of error of alpha for all pairs
  s=1
  while(s<=20){
    x<-sample.int(100, 2)
    alpha1_true=dist(t(D2[,x]),method = "euclidean") #calculate the euclidean distance between the randomly selected pair of points
    alpha1_red=dist(t(R1[,x]),method = "euclidean")
    delta1<-delta1+abs(alpha1_red-alpha1_true)
    s<-s+1
  }
  if(delta1/20<=0.5){
    print(paste0("The smallest qualified m should be: ", m))
    print(paste0("The corresponding qualified average delta is ",delta1/20))
    break
  }
  m<-m+1
}


######## for n=100 ############
N=1;
D3<-matrix(NA, nrow = 100, ncol = 100);
while(N<=100){
  x<-runif(100);
  D3[,N]<-x;
  N<-N+1;
}


m=2
while(m<=100){
  A1<-matrix(rnorm(m*100)/sqrt(m),nrow=m,ncol=100)
  R1<-A1%*%D3 # the reduced matrix
  
  delta1=0 #the sum of error of alpha for all pairs
  s=1
  while(s<=20){
    x<-sample.int(100, 2)
    alpha1_true=dist(t(D3[,x]),method = "euclidean") #calculate the euclidean distance between the randomly selected pair of points
    alpha1_red=dist(t(R1[,x]),method = "euclidean")
    delta1<-delta1+abs(alpha1_red-alpha1_true)
    s<-s+1
  }
  if(delta1/20<=0.5){
    print(paste0("The smallest qualified m should be: ", m))
    print(paste0("The corresponding qualified average delta is ",delta1/20))
    break
  }
  m<-m+1
}


############################## 2 #######################################


#######Circulant#######

n=100

Diag<-diag(sample(c(-1,1),n,replace=TRUE,prob=c(0.5,0.5)))
m=2
while(m<=n){
  ###Calculate the circulant matrix
  P.circ<-matrix(0, nrow = m, ncol = n)
  P.circ[1,]<-rnorm(n) #first row 
  
  i=2
  while(i<=m){
    P.circ[i,1]<-P.circ[i-1,n] #the fisrt element of each row equals the last element of previous row
    j=2
    while(j<=n){
      P.circ[i,j]<-P.circ[i-1,j-1]
      j<-j+1
    }
    i<-i+1
  }
  
  R1<-(P.circ%*%Diag/sqrt(m))%*%D3  # the reduced matrix 
  
  delta1=0 #the sum of error of alpha for all pairs
  s=1
  while(s<=20){
    x<-sample.int(100, 2)
    alpha1_true=dist(t(D3[,x]),method = "euclidean") #calculate the euclidean distance between the randomly selected pair of points
    alpha1_red=dist(t(R1[,x]),method = "euclidean")
    delta1<-delta1+abs(alpha1_red-alpha1_true)
    s<-s+1
  }
  if(delta1/20<=0.5){
    print(paste0("The smallest qualified m should be: ", m))
    print(paste0("The corresponding qualified average delta is ",delta1/20))
    break
  }
  #print(m)
  #print(delta1/20)
  m<-m+1
}


#######Toeplitz#######

n=100

Diag<-diag(sample(c(-1,1),n,replace=TRUE,prob=c(0.5,0.5)))
m=2
while(m<=n){
  ###Calculate the toeplitz matrix
  P.toep<-matrix(0, nrow = m, ncol = n)
  P.toep[1,]<-rnorm(n) #first row to be g  
  P.toep[,1]<-rnorm(m) #first column to be g
  i=2
  while(i<=m){
    j=2
    while(j<=n){
      P.toep[i,j]<-P.toep[i-1,j-1]
      j<-j+1
    }
    i<-i+1
  }
  
  R1<-(P.toep/sqrt(m))%*%Diag%*%D3  # the reduced matrix 
  delta1=0 #the sum of error of alpha for all pairs
  s=1
  while(s<=20){
    x<-sample.int(100, 2)
    alpha1_true=dist(t(D3[,x]),method = "euclidean") #calculate the euclidean distance between the randomly selected pair of points
    alpha1_red=dist(t(R1[,x]),method = "euclidean")
    delta1<-delta1+abs(alpha1_red-alpha1_true)
    s<-s+1
  }
  if(delta1/20<=0.5){
    print(paste0("The smallest qualified m should be: ", m))
    print(paste0("The corresponding qualified average delta is ",delta1/20))
    break
  }
  m<-m+1
}


#######Hankel#######

n=100

Diag<-diag(sample(c(-1,1),n,replace=TRUE,prob=c(0.5,0.5)))
m=2
while(m<=n){
  ###Calculate the hankel matrix
  P.hank<-matrix(0, nrow = m, ncol = n)
  P.hank[1,]<-rnorm(n) #first row to be g  
  P.hank[,n]<-rnorm(m) #last column to be g
  i=2
  while(i<=m){
    j=1
    while(j<n){
      P.hank[i,j]<-P.hank[i-1,j+1]
      j<-j+1
    }
    i<-i+1
  }
  
  R1<-(P.hank/sqrt(m))%*%Diag%*%D3  # the reduced matrix 
  delta1=0 #the sum of error of alpha for all pairs
  s=1
  while(s<=20){
    x<-sample.int(100, 2)
    alpha1_true=dist(t(D3[,x]),method = "euclidean") #calculate the euclidean distance between the randomly selected pair of points
    alpha1_red=dist(t(R1[,x]),method = "euclidean")
    delta1<-delta1+abs(alpha1_red-alpha1_true)
    s<-s+1
  }
  if(delta1/20<=0.5){
    print(paste0("The smallest qualified m should be: ", m))
    print(paste0("The corresponding qualified average delta is ",delta1/20))
    break
  }
  m<-m+1
}
