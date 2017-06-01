

pred.fun=function(train.x){
  
  #======================================================
  set.seed(100)
  num = c(1:nrow(train.x))
  n1 = sample( num ,nrow(train.x)*0.5)		
  n2=num[-n1]
  
  sub.train	= train.x[n1,] 
  sub.test  	= train.x[n2,]
  
  sub.dtrain	= xgb.DMatrix(
    data = as.matrix(
      subset(sub.train,select = -c(Response) ) 
    ), 
    label= sub.train$Response,
    missing=NA )
  
  xgb_params=list( 	
    objective="reg:linear",
    booster="gbtree",
    eta= 0.1, 
    max_depth= 1, 
    colsample_bytree= 0.7,
    subsample = 0.7)
  
  set.seed(100)
  best.nrounds = 10 # xgb_cv$best_iteration 
  xgb_model <- xgb.train(data = sub.dtrain,
                         params = xgb_params,
                         #watchlist = list(train = dtrain),
                         nrounds = best.nrounds,
                         #verbose = 1,
                         #print.every.n = 5
  )
  gc()
  
  sub.dtrain2	= xgb.DMatrix(
    data = as.matrix(
      subset(sub.train,select = -c(Response) ) 
    ), missing=NA )
  
  sub.dtest2	= xgb.DMatrix(
    data = as.matrix(
      subset(sub.test,select = -c(Response) ) 
    ), missing=NA)
  gc()
  #========================================
  print("xgb can find features automatically")
  feature = xgb.importance(feature_names = colnames(sub.train), 
                           model = xgb_model)
  
  feature[1:5,]
  xgb.plot.importance(feature)
  #========================================
  pred1 <-predict(xgb_model,sub.dtrain2)
  
  pred1[pred1>0.5]=1
  pred1[pred1<0.5]=0
  
  temp1 = table(sub.train$Response,pred1)
  print( "confusion matrix of train data" )
  print(temp1)
  value1 = mcc.evaluation.fun(temp1)
  print( "Matthews correlation coefficient of train data" )
  print(value1)
  
  #========================================
  pred2 <-predict(xgb_model,sub.dtest2)
  
  pred2[pred2>0.5]=1
  pred2[pred2<0.5]=0
  
  temp2 = table(sub.test$Response,pred2)
  print( "confusion matrix of test data" )
  print(temp2)
  value2 = mcc.evaluation.fun(temp2)
  print( "Matthews correlation coefficient of test data" )
  print(value2)
  #------------------------------------------
}	

#function
#算得分
mcc.evaluation.fun=function(tem){
  tp = tem[1,1] %>% as.integer64(.)
  fn = tem[1,2] %>% as.integer64(.)
  fp = tem[2,1] %>% as.integer64(.)
  tn = tem[2,2] %>% as.integer64(.)
  up = tp*tn-fp*fn
  down = sqrt( (tp+fp)*(tp+fn)*(tn+fp)*(tn+fn) )
  
  return( up/down )
}


library(data.table)
library(xgboost)
library(glmnet)
library(dplyr)
library(bit64)


setwd('D:\\kaggle_Production_Line')
#setwd('/media/linsam/10FE4529FE450884/kaggle_Production_Line')


train.numeric	=fread("train_numeric.csv")
train.date	=fread("train_date.csv")

print(object.size(train.numeric),units="GB")
print(object.size(train.date),units="GB")

set.seed(100)
num = sample(nrow(train.numeric),200000)
train.numeric	= train.numeric[num,]
train.date	= train.date[num,]
gc()

merge.train	= merge(train.numeric,train.date, 
                    all.x = TRUE, by = c("Id") )
rm(train.numeric,train.date)
gc()
#Response Id
train.id = merge.train[,.(Id)]
train.x = subset(merge.train,select = -c(Id) )
rm(merge.train)
gc()

#-------------------------------------------------------
pred.fun(train.x)
gc()
print("https://en.wikipedia.org/wiki/Matthews_correlation_coefficient")
print("transform variables")
colnames(train.x)[3]
train.x$L0_S0_F2 = train.x$L0_S0_F2 * 5
train.x$L0_S0_F4 = train.x$L0_S0_F4^2
gc()

pred.fun(train.x)










