##### Problem1 #####

# create dataset

U<-matrix(NA, nrow = 7, ncol = 2)
U[,1]<-runif(7,0,20)
U[,2]<-runif(7,0,20)

Std<-matrix(NA, nrow = 7, ncol = 2)
Std[,1]<-sqrt(runif(7,0.5,1))
Std[,2]<-sqrt(runif(7,0.5,1))

D<-matrix(NA, nrow = 400, ncol = 2)
X<-matrix(NA, nrow = 7, ncol = 2)
for(k in 1:400){
  for(i in 1:7){
    for(j in 1:2){
      X[i,j]<-rnorm(1,mean=U[i,j],sd=Std[i,j])
    }
  }
  M<-colSums(X)  #M is one generated point
  D[k,]<-M
}


# implement Lloyd's Algorithm
Centers<-matrix(NA, nrow = 7, ncol = 2) #the initialized random center
r<-sample.int(400,7)
Centers<-D[r,]  


I_num=50
Cost<-matrix(0,nrow=1,ncol=I_num) #the cost function value for each iteration
All_Center<-list()
All_Center[[1]]<-Centers


for(i in 1:I_num){
  Clusters<-list()
  for(k in 1:7){
    Clusters[[k]]<-matrix(data=,nrow=0,ncol=2)
  }
  for(n in 1:400){
    distance<-matrix(NA, nrow = 7, ncol = 1)
    for(j in 1:7){
      distance[j,]<-(dist(rbind(Centers[j,],D[n,])))^2  #calculate the distance from the n th point to all the current centers
    }
    min.index<-which.min(distance)
    Cost[1,i]<-Cost[1,i]+min(distance)
    Clusters[[min.index]]<-rbind(Clusters[[min.index]],D[n,])
    #Clusters[[min.index]][[length(Clusters[[min.index]])+1]]<-D[n,]  # assign the point to its cluster
    #All_Center[[length(All_Center)+1]]<-
  }
  for(k in 1:7){
    Centers[k,]<-colMeans(Clusters[[k]])
  }
  
  All_Center[[length(All_Center)+1]]<-Centers
}

#calculate the distance from centers in Iterations i to the centers in ultimate iterations
Center_dist<-matrix(data=0,nrow=I_num+1,ncol=1)
for(i in 1:I_num){
  diff<-All_Center[[i]]-All_Center[[I_num+1]]
  Center_dist[i,]<-sum(diff^2)
}

plot(1:(I_num+1),Center_dist)

for(i in 1:I_num){
  if(Center_dist[i,]==0){
    con.rate<-i-1  #the rate of convergance
    break
  }
}

# kmeans++ seeding
Centers_plus<-matrix(data=, nrow = 7, ncol = 2) #the initialized random center
r<-sample.int(400,1)
Centers_plus[1,]<-D[r,]
l=1
P<-matrix(0,nrow=400,ncol=1)
All.dist<-matrix(0,nrow=400,ncol=1)

for(l in 1:6){
  for(n in 1:400){
    distance<-matrix(data=1000, nrow = 7, ncol = 1)
    for(j in 1:l){
      distance[j,]<-(dist(rbind(Centers_plus[j,],D[n,])))^2 #calculate the distance from the n th point to all the current centers
    }
    All.dist[n,]<-min(distance)
  }
  P<-cumsum(All.dist/colSums(All.dist))  #P is cdf
  r<-runif(1)
  
  #find next center
  if(r<=P[1]){
    next_C<-1
  }
  for(n in 2:400){
    if(r>P[n-1]&&r<=P[n]){
      next_C<-n
      break
    }
  }
  Centers_plus[l+1,]<-D[next_C,]
}

###run Lloyd's algorithm based on seeding from kmeans++
Centers<-Centers_plus

I_num=50
Cost2<-matrix(0,nrow=1,ncol=I_num) #the cost function value for each iteration
All_Center<-list()
All_Center[[1]]<-Centers


for(i in 1:I_num){
  Clusters<-list()
  for(k in 1:7){
    Clusters[[k]]<-matrix(data=,nrow=0,ncol=2)
  }
  for(n in 1:400){
    distance<-matrix(NA, nrow = 7, ncol = 1)
    for(j in 1:7){
      distance[j,]<-(dist(rbind(Centers[j,],D[n,])))^2  #calculate the distance from the n th point to all the current centers
    }
    min.index<-which.min(distance)
    Cost2[1,i]<-Cost2[1,i]+min(distance)
    Clusters[[min.index]]<-rbind(Clusters[[min.index]],D[n,])
    
  }
  for(k in 1:7){
    Centers[k,]<-colMeans(Clusters[[k]])
  }
  
  All_Center[[length(All_Center)+1]]<-Centers #center for next iteration.
}

#calculate the distance from centers in Iterations i to the centers in ultimate iterations
Center_dist<-matrix(data=0,nrow=I_num+1,ncol=1)
for(i in 1:I_num){
  diff<-All_Center[[i]]-All_Center[[I_num+1]]
  Center_dist[i,]<-sum(diff^2)
}

plot(1:(I_num+1),Center_dist)

for(i in 1:I_num){
  if(Center_dist[i,]==0){
    con.rate.plus<-i-1
    break
  }
}


############ 2 #############
#generate U
U=matrix(0,1000,3)
for(j in 1:3){
  for(i in 1:1000){
    U[i,j]<-rnorm(1)
  }
}

mu=colMeans(U)
sigma<-apply(U, 2, sd)

for(j in 1:3){
  for(i in 1:1000){
    U[i,j]<-(U[i,j]-mu[j])/(sqrt(1000)*sigma[j])
  }
}


x_d<-matrix(0,3,1)


g_d<-list()
D<-list() #stores the 4 datasets
g_std<-c(0.1,0.2,0.3,10)
for(k in 1:4){
  g_d[[k]]<-matrix(data=,nrow=1000,ncol=100)
  D[[k]]<-matrix(data=,nrow=1000,ncol=100)
  for(j in 1:100){
    for(i in 1:1000){
      g_d[[k]][i,j]<-rnorm(1,1,g_std[k])
    }
    for(l in 1:3){x_d[l,1]=rnorm(1)}
  D[[k]][,j]<-U%*%x_d+g_d[[k]][,j]
  }
}


r<-list() #stores the eigenvevectors and eigenvalues of 4 datasets
for(k in 1:4){
  r[[k]]<-eigen((D[[k]]%*%t(D[[k]]))/100)
}

V<-list() #stores the eigenvectors
for(k in 1:4){
  V[[k]]<-r[[k]]$vectors[,1:3]   #the first three eigenvectors of each dataset
  #normalize
  #mu.v=colMeans(V[[k]])
  #sigma.v<-apply(V[[k]], 2, sd)
  
  #for(j in 1:3){
  #  for(i in 1:1000){
  #    V[[k]][i,j]<-(V[[k]][i,j]-mu.v[j])/(sqrt(1000)*sigma.v[j])
  #  }
  #}
}

### for euclidean distance in original space
D.Dist<-list() #stores the distance matrix for each D
Avg.Dist<-list()
for(k in 1:4){
  D.Dist[[k]]<-dist(t(D[[k]]))
  Avg.Dist[[k]]<-mean(D.Dist[[k]])
}

###transform 1000-dim D into 3-dim W
W<-list()
for(k in 1:4){
  W[[k]]<-t(V[[k]])%*%D[[k]]
}


### for euclidean distance after transforming D into W
W.Dist<-list() #stores the distance matrix for each W
Avg.Dist.W<-list()
for(k in 1:4){
  W.Dist[[k]]<-dist(t(W[[k]]))
  Avg.Dist.W[[k]]<-mean(W.Dist[[k]])
}

error<-list()
for(k in 1:4){
  error[[k]]<-abs((Avg.Dist.W[[k]]-Avg.Dist[[k]]))/Avg.Dist[[k]]
}


