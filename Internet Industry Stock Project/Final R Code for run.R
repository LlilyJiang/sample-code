setwd("C:/Users/sony/Downloads/Business Analytics/Data Sets")
amzn=read.csv("YELP.csv")  ##file name has to be set
amzn=na.omit(amzn)
Date=as.Date(amzn$Date,format="%m/%d/%Y")
tot_month=85   ##tot_month has to be set
start_month=7
monthly_data=vector("list",length=tot_month)
j=1
i=length(Date)
while(j<=tot_month) {                             #Arrange data into monthly basis
  temp=NULL
  current_month=months(Date[i])
  while(months(Date[i])==current_month) {
    temp=rbind(temp,amzn[i,])
     i=i-1
     if(i==0)  break
  }
  monthly_data[[j]]=temp
  j=j+1
}

monthly_pred=vector("list",length=tot_month)    #list to record the prediction and model
monthly_var=vector("list",length=tot_month)
for (pred.month in start_month:tot_month) {           #pred.month is the month to be predicted
   var_num=ncol(monthly_data[[1]])-2
   subset_var=c(1:var_num)
   var_best=rep(F,var_num)
   best_model=vector("list",length=var_num)    #record the best model for every number of variables
   best_model_error=rep(0,var_num)
   for(i in 1:var_num) {     #i is the number of variables
      ave.error=c(1:var_num)
      for(k in 1:var_num) {     #k is the new variable to be added
         if (var_best[k]) ave.error[k]=10^7
         else {
             var_best[k]=T
             current_var=subset_var[var_best]
             var_best[k]=F
             test.error=rep(10^10,4)
             train=monthly_data[[pred.month-6]]
             for (j in (pred.month-5):(pred.month-2)) {           #Cross Validation
                train=rbind(train,monthly_data[[j]])
                test=monthly_data[[j+1]]
                train.X=data.frame(train[,current_var+1])
                test.X=data.frame(rep(1,length(test[,1])),test[,current_var+1])
                train.Y=train$Predicted.Return
                test.Y=test$Predicted.Return
                lm.fit=lm(train.Y~.,data=train.X)
                if(!is.na(sum(lm.fit$coefficients))) {
                lm.pred=as.matrix(test.X)%*%as.matrix(lm.fit$coefficients)
                test.error[j-pred.month+6]=mean((lm.pred-test.Y)^2)
                }
             }
         ave.error[k]=mean(test.error)       #Calculate test error for every new added variable
         }
      }
      var_best[which.min(ave.error)]=T      #Add one variable with minimum average test error
      best_model[[i]]=subset_var[var_best]+1
      best_model_error[i]=min(ave.error)
   }
   #Use the best model to test current month
 
   train=rbind(monthly_data[[pred.month-6]],monthly_data[[pred.month-5]],monthly_data[[pred.month-4]],monthly_data[[pred.month-3]],monthly_data[[pred.month-2]],monthly_data[[pred.month-1]])
   test=monthly_data[[pred.month]]
   best_num=which.min(best_model_error[round(var_num/3):round(var_num*2/3)])+round(var_num/3)-1
   train.X=data.frame(train[,best_model[[best_num]]])
   train.Y=train$Predicted.Return
   test.X=data.frame(rep(1,length(test[,1])),test[,best_model[[best_num]]])
   test.Y=test$Predicted.Return
   lm.best=lm(train.Y~.,data=train.X)
   monthly_pred[[pred.month]]=as.matrix(test.X)%*%as.matrix(lm.best$coefficients)
   monthly_var[[pred.month]]=best_model[[best_num]]
   rownames(monthly_pred[[pred.month]])=monthly_data[[pred.month]]$Date
}
   amzn_var=NULL
   for (month in start_month:tot_month) amzn_var=rbind(amzn_var,as.data.frame(names(monthly_data[[1]])[monthly_var[[month]]]))
   amzn_pred=NULL
   for (month in start_month:tot_month) amzn_pred=rbind(amzn_pred,monthly_pred[[month]])
   write.csv(amzn_pred,"yelp prediction.csv")  ##file name has to be set
   write.csv(amzn_var,"yelp model.csv")  ##file name has to be set
   
   #cbind(monthly_pred[[month]],monthly_data[[month]]$Predicted.Return) to find prediction
   #names(monthly_data[[1]])[monthly_var[[month]]] to find best model
   #for (j in 1:length(monthly_var)) print(length(monthly_var[[j]]))
   #table(monthly_pred[[month]]>0,monthly_data[[month]]$Predicted.Return>0)
