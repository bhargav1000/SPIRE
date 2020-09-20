library(plyr)
library(dplyr)
library(beepr)
library(xgboost)
library(pROC)
library(stringr)
library(readxl)
library(xgboost)
library(DiagrammeR)


build=read.csv("path/to/data")
oot=read.csv("path/to/data")

# I cannot add the dataset because it contains highly sensitive information
build=na.omit(build)
oot=na.omit(oot)

# Adding whether monthly income is greater than 20k or not
build$mi_gt_20k=ifelse(build$monthly_income>20000,1,0)
oot$mi_gt_20k=ifelse(oot$monthly_income>20000,1,0)

check=build 

oot_check=oot 


## Fintech App variables
# High Risk Apps
# App Flag
check$flag1=ifelse((check$app21==1 | check$app22==1 | check$app2==1),1,0)

oot_check$flag1=ifelse((oot_check$app21==1 | oot_check$app22==1 | oot_check$app2==1),1,0)

# App Count
target=c('id','app21','app22','app2','app33')
test1 = check %>% select(target) %>% group_by(id) %>% 
  mutate(count1=ifelse((app21==1 | app22==1 | app2==1),(app21+app22+app2+app33),0)) %>% 
  select(c("id", "count1"))

check=join(check, test1, by="id", type='left', match='first')


test2 = oot_check %>% select(target) %>% group_by(id) %>% 
  mutate(count1=ifelse((app21==1 | app22==1 | app2==1),(app21+app22+app2+app33),0)) %>% 
  select(c("id", "count1"))

oot_check=join(oot_check, test2, by="id", type='left', match='first')
beep()

# Medium Risk Apps
# App flag
check$flag2=ifelse((check$app5==1 | check$app4==1 | check$app1==1),1,0)

oot_check$flag2=ifelse((oot_check$app5==1 | oot_check$app4==1 | oot_check$app1==1),1,0)

# App Count
target=c('id', 'app5', 'app4', 'app1', 'app33')
test1 = check %>% select(target) %>% group_by(id) %>% 
  mutate(count2=ifelse((app5==1 | app4==1 | app1==1),(app5+app4+app1+app33),0)) %>% 
  select(c("id", "count2"))

check=join(check, test1, by="id", type='left', match='first')

test2 = oot_check %>% select(target) %>% group_by(id) %>% 
  mutate(count2=ifelse((app5==1 | app4==1 | app1==1),(app5+app4+app1+app33),0)) %>% 
  select(c("id", "count2"))

oot_check=join(oot_check, test2, by="id", type="left", match='first')


## Lifestyle Variables

# Travel Variables
check$flag3=ifelse((check$app7==1 | check$app8==1),1,0)

oot_check$flag3=ifelse((oot_check$app7==1 | oot_check$app8==1),1,0)


# App Count
target=c('id', 'app7', 'app8')
test1 = check %>% select(target) %>% group_by(id) %>% mutate(count3=app7+app8) %>%
  select(c('id', 'count3'))

check=join(check,test1,by="id",type='left',match='first')

test2 = oot_check %>% select(target) %>% group_by(id) %>% mutate(count3=app7+app8) %>%
  select(c('id', 'count3'))

oot_check=join(oot_check,test2,by="id",type="left",match="first")


# Shopping Variables
check$flag4=ifelse((check$app9==1 | check$app10==1 | check$app11==1),1,0)

oot_check$flag4=ifelse((oot_check$app9==1 | oot_check$app10==1 | oot_check$app11==1),1,0)

# App Count
target=c('id','app9','app10','app11')
test1 = check %>% select(target) %>% group_by(id) %>%
  mutate(count4=app9+app10+app11) %>% select(c("id", "count4"))

check=join(check, test1, by="id", type='left', match='first')

test2 = oot_check %>% select(target) %>% group_by(id) %>%
  mutate(count4=app9+app10+app11) %>% select(c("id", "count4"))

oot_check=join(oot_check, test2, by="id", type='left', match='first')


# Mutual Funds Apps
# App Flag
check$flag5=ifelse((check$app12==1 | check$app13==1 | check$app14==1),1,0)

oot_check$flag5=ifelse((oot_check$app12==1 | oot_check$app13==1 | oot_check$app14==1),1,0)

# App Count
target=c('id','app12','app13','app14')
test1 = check %>% select(target) %>% group_by(id) %>% mutate(count5=app12+app13+app14) %>%
  select(c('id','count5'))

check=join(check, test1, by='id', type='left', match='first')

test2 = oot_check %>% select(target) %>% group_by(id) %>% mutate(count5=app12+app13+app14) %>%
  select(c('id','count5'))

oot_check=join(oot_check, test2, by='id', type='left', match='first')


# Social Apps
# App Flag
check$flag6=ifelse((check$app16==1 | check$app18==1 | check$app17==1),1,0)

oot_check$flag6=ifelse((oot_check$app16==1 | oot_check$app18==1 | oot_check$app17==1),1,0)

target=c('id','app16','app17','app18')
test1 = check %>% select(target) %>% group_by(id) %>% mutate(count6=app16+app17+app18) %>%
  select(c('id','count6'))

check=join(check, test1, by='id', type='left', match='first')

test2 = oot_check %>% select(target) %>% group_by(id) %>% mutate(count6=app16+app17+app18) %>%
  select(c('id','count6'))

oot_check=join(oot_check, test2, by='id', type='left', match='first')


check_copy=check
oot_check_copy=oot_check
# Downsampling dataset
dev_data=check # Run model on everyone
set.seed(345)
dt=sort(sample(nrow(dev_data), nrow(dev_data)*.3))
train=dev_data[-dt,]
test=dev_data[dt,]


train_data=train %>% select(c("app1","app2","app3","app4","app5","condition1","condition2",
                              "app6","app7","app8","app9",
                              "app10","app11","app12","app13","app14",
                              "app15","app16","app17","app18","app19",
                              "flag1","count1",
                              "flag2","count2",
                              "flag3","count3",
                              "flag4","count4",
                              "flag5","count5","flag6",
                              "count6", "deliquency"))

# %>% select(c("app1","app2","app3","app4","app5",
#                               "app21","app22","app6","app7","app8","app9",
#                               "app10","app11","app12","app13","app14",
#                               "app15","app16","app17","app18","app19",
#                               "flag1","count1",
#                               "flag2","count2",
#                               "flag3","count3",
#                               "flag4","count4",
#                               "flag5","count5","flag6",
#                               "count6", "deliquency"))

test_data=test %>% select(c("app1","app2","app3","app4","app5","condition1","condition2",
                            "app6","app7","app8","app9",
                            "app10","app11","app12","app13","app14",
                            "app15","app16","app17","app18","app19",
                            "flag1","count1",
                            "flag2","count2",
                            "flag3","count3",
                            "flag4","count4",
                            "flag5","count5","flag6",
                            "count6", "deliquency"))

# oot_data=oot_check %>% select(c('app1','app2','app3','app4','app20',
#                                 'app5','app21','app22','app23','app24',
#                                 'app6','app7','app8','app25','app26',
#                                 'app27','app28','app29','app9','app30',
#                                 'app10','isajio','app11','app31','app32',
#                                 'app12','app13','app15','condition1','sub_flag1',
#                                 'sub_count1','sub_flag2', 'sub_count2',
#                                 'flag4', 'count4', 'deliquency'))

oot_data=oot_check %>% select(c("app1","app2","app3","app4","app5","condition1","condition2",
                                "app6","app7","app8","app9",
                                "app10","app11","app12","app13","app14",
                                "app15","app16","app17","app18","app19",
                                "flag1","count1",
                                "flag2","count2",
                                "flag3","count3",
                                "flag4","count4",
                                "flag5","count5","flag6",
                                "count6", "deliquency"))

# Matrix Conversion
dtrain <- xgb.DMatrix(data=as.matrix(train_data[-which(names(train_data)=="deliquency")]), label=train_data$deliquency)
dtest <- xgb.DMatrix(data=as.matrix(test_data[-which(names(test_data)=="deliquency")]), label=test_data$deliquency)
doot_data <- xgb.DMatrix(data=as.matrix(oot_data[-which(names(oot_data)=="deliquency")]), label=oot_data$deliquency)

# Model building
watchlist <- list(train=dtrain, test=dtest)

{
  eta=0.1
  base_score=0.5
  eval_metric=c('auc')
  gamma=9
  sample_type="weighted"
  normalize_type="tree"
  rate_drop=0.3
  skip_drop=0.5
  max_depth=5
  subsample=0.8
  colsample_bytree=0.6
  colsample_bylevel=0.6
  min_child_weight=7
  
  
  param <- list(booster='dart', 
                objective="binary:logistic", 
                eta=eta, 
                base_score=base_score, 
                eval_metric=eval_metric, 
                gamma=gamma,
                sample_type=sample_type, 
                normalize_type=normalize_type, 
                rate_drop=rate_drop, 
                skip_drop=skip_drop,
                min_child_weight=min_child_weight)
  
  xgboost.cv=xgb.cv(param = param, data=dtrain, nfold=5, nrounds=2000, early_stopping_rounds = 100,
                    max_depth=max_depth, subsample=subsample, colsample_bytree=colsample_bytree, 
                    colsample_bylevel=colsample_bylevel)
  
  plot(xgboost.cv[["evaluation_log"]][["train_auc_mean"]], type='l', ylab='mean auc',xlab='Iterations',
       main="Train vs Test AUC")
  lines(xgboost.cv[["evaluation_log"]][["test_auc_mean"]], col='red', type='l')
  legend("bottomright",
         legend=c("Train", "Test"),
         col=c("black","red"),
         lty=1,
         cex=1.0,
         pt.cex=1.5,
         inset=c(0.1, 0.1)
  )
}

set.seed(345)
model <- xgb.train(data=dtrain,
                   booster='dart',
                   max.depth = max_depth,
                   sample_type='weighted',
                   normalize_type='tree',
                   nround = 800,
                   objective = "binary:logistic",
                   eval_metric="auc",
                   early_stopping_rounds = 100,
                   watchlist=watchlist,
                   eta=eta,
                   base_score=base_score,
                   subsample=subsample,
                   rate_drop=rate_drop,
                   skip_drop=skip_drop,
                   # one_drop=1,
                   colsample_bytree=colsample_bytree,
                   colsample_bylevel=colsample_bylevel,
                   min_child_weight=min_child_weight,
                   gamma=gamma)

# model <- xgb.cv(data=dtrain,
#                 max_depth=3,
#                 objective="binary:logistic",
#                 metrics=list("error", "auc"),
#                 eta=0.03,
#                 nrounds=1000,
#                 nfold=3
#                 )

pred <- predict(model, dtest)

# err <- mean(as.numeric(pred > 0.5) != test$deliquency)
# print(paste("test-error=", err))


xgb.plot.multi.trees(feature_names = names(train_data), 
                     model = model)

importance_matrix <- xgb.importance(names(train_data), model = model)

# feature importance
xgb.plot.importance(importance_matrix)

# ROC Curves

# Test Data
plot(roc(test$deliquency, pred), main="ROC Curve - Test Data")
roc(test$deliquency, pred)

# OOT Data
oot_preds <- predict(model, doot_data)
roc(oot_data$deliquency, oot_preds)
beep()

roc(train$deliquency, predict(model, dtrain))
roc(test$deliquency, pred)
roc(oot_data$deliquency, oot_preds)


plot(roc(oot_data$deliquency, oot_preds), col='red', main="ROC Curve")
lines(roc(test_data$deliquency, pred))
lines(roc(train_data$deliquency, predict(model, dtrain)), col='blue')
legend("bottomright",
       legend=c("Test", "OOT", "Train"),
       col=c("black","red", "blue"),
       lty=1, 
       cex=1.0,
       pt.cex=1.5,
       inset=c(0.1, 0.1)
)






# Sloping
train_data$ypred=predict(model, dtrain, type='response')
test_data$ypred=predict(model, dtest, type='response')
oot_data$ypred=predict(model, doot_data, type='response')

{
  deciles=4
  
  train_data$calc_deciles=ntile(train_data$ypred, deciles)
  test_data$calc_deciles=ntile(test_data$ypred, deciles)
  oot_data$calc_deciles=ntile(oot_data$ypred, deciles)
  
  train_summary = train_data %>% group_by(calc_deciles) %>% summarise(bad_rate=sum(`deliquency`)/n(),
                                                                      group="train")
  test_summary = test_data %>% group_by(calc_deciles) %>% summarise(bad_rate=sum(`deliquency`)/n(),
                                                                    group="test")
  oot_summary = oot_data %>% group_by(calc_deciles) %>% summarise(bad_rate=sum(`deliquency`)/n(),
                                                                  group="oot")
  
  final_summary = rbind(train_summary, test_summary, oot_summary)
  
  # Plotting
  ggplot(aes(x=calc_deciles, y=bad_rate, group=group, col=group),data=final_summary) +
    geom_line() +
    geom_point() +
    xlab("SPIRE Segments") + 
    ylab("Bad Rate") +
    ggtitle("SPIRE XGBoost") +
    scale_y_continuous(labels = scales::percent)
}







