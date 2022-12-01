#import data set
data <- read.csv("H:/Data analytics project 1 4th yr/data.csv")

#libraries
library(randomForest)
library(caTools)
library(vip)
library(caret)
library(psych)
library(naniar) #summary of missing values
library(pdp)
library(ggplot2)


#make this example reproducible
set.seed(1)

#converting character data into factors
data['Class'] = as.factor(data$Class)
data['satisfaction'] = as.factor(data$satisfaction)

#splitting data set into train and validation
#use 80% of dataset as training set and 20% as test set
sample <- sample.split(data$Age, SplitRatio = 0.8)
train  <- subset(data, sample == TRUE)
valid   <- subset(data, sample == FALSE)

#performing defualt random forest
s1= Sys.time()
rf <-randomForest(as.factor(satisfaction)~.,data=train, ntree=50)
s2=Sys.time()

rf_train_pred = predict(rf,train,type = 'class')
confusionMatrix(rf_train_pred,train$satisfaction)

rf_valid_pred = predict(rf,valid,type = 'class')
confusionMatrix(rf_valid_pred,valid$satisfaction)



#performing defualt random forest
s3= Sys.time()
rf1 <-randomForest(as.factor(satisfaction)~.,data=train, ntree=100)
s4=Sys.time()


rf1_train_pred = predict(rf1,train,type = 'class')
confusionMatrix(rf1_train_pred,train$satisfaction)

rf1_valid_pred = predict(rf1,valid,type = 'class')
confusionMatrix(rf1_valid_pred,valid$satisfaction)

View(train)
vip(rf1,geom = "point")
sd(train$boarding)
#######################################################################

#try to fit random forest with less variables

nw_train = train[c('boarding','Class','service','comfort',
                   
                   'Type.of.Travel','Convenience','satisfaction')]

nw_valid = valid[c('boarding','Class','service','comfort',
                   
                   'Type.of.Travel','Convenience','satisfaction')]


nw_rf <-randomForest(as.factor(satisfaction)~.,data=nw_train, ntree=100
                     )

nw_pred = predict(nw_rf,nw_train,type = 'class')
confusionMatrix(nw_pred,nw_train$satisfaction)

nw_valid_pred = predict(nw_rf, nw_valid, type = 'class')
confusionMatrix(nw_valid_pred,nw_valid$satisfaction)

#hyperparameter tuning

tuneRF(nw_train[,-7], as.factor(nw_train[,7]), ntreeTry=100, stepFactor=0.5,
       improve=0.05 )


tu_rf <-randomForest(as.factor(satisfaction)~.,data=nw_train, ntree=100
                     ,mtry = 4 )


tu_pred = predict(tu_rf,nw_train,type = 'class')
confusionMatrix(tu_pred,nw_train$satisfaction)

tu_valid_pred = predict(tu_rf,nw_valid,type= 'class')
confusionMatrix(tu_valid_pred,nw_valid$satisfaction)

#######################################################################

#test set results
df <- read.csv("H:/Data analytics project 1 4th yr/test.csv")

dt = df[,3:25]

#missing values summary
miss_var_summary(dt)
dt = na.omit(dt)
miss_var_summary(dt)

#duplicate values
sum(duplicated(dt))

#loading factor analysis 
ds.fa3=readRDS("H:/Data analytics project 1 4th yr/factor.rda")

fs <- factor.scores(dt[,7:20], ds.fa3)
fs <- fs$scores 
df <- cbind(dt[,1:6],fs,dt[,21:23]) 

#renaming columns
names(df)[7:10] = c("comfort","service","Convenience","boarding")
df = df[c('boarding','Class','service','comfort',
          'Type.of.Travel','Convenience','satisfaction')]

df['Class']= as.factor(df$Class)
df['satisfaction'] = as.factor(df$satisfaction)
tu_test_pred = predict(tu_rf,df,type = 'class')
confusionMatrix(tu_test_pred,as.factor(df$satisfaction))

str(nw_train)
write.csv(nw_train,"D:\\advance analysis\\n_train.csv", row.names = FALSE)


#variable importance plot 
vip(tu_rf)


partialPlot(x=tu_rf, pred.data=df, x.var=Convenience)

partialPlot(x=tu_rf, pred.data=df, x.var=Class, which.class="satisfied")

cls_bor = partial(tu_rf,pred.var = c('comfort','Class'),chull = TRUE, 
                  which.class = 'satisfied' )
cls_bor =autoplot(cls_bor, contour = TRUE, 
                legend.title = "Partial\ndependence")

grid.arrange(cls_bor)
