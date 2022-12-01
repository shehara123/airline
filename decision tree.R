#import data set
data <- read.csv("H:/Data analytics project 1 4th yr/data.csv")

#libraries
library(rpart)
library(rpart.plot)
library(caTools)
library(vip)
library(caret)
library(mlr)

#make this example reproducible
set.seed(1)


#splitting data set into train and validation
#use 80% of dataset as training set and 20% as test set
sample <- sample.split(data$Age, SplitRatio = 0.8)
train  <- subset(data, sample == TRUE)
valid   <- subset(data, sample == FALSE)


d.tree = rpart(as.factor(satisfaction) ~ ., 
               data=train, 
               method = 'class')

rpart.plot(d.tree,type = 2,varlen=50,faclen=1,tweak=1.5, compress = FALSE)

summary(d.tree)

vip(ds.tree,num_features = 13)

d.pred = predict(d.tree,train,type = 'class')

confusionMatrix(d.pred,as.factor(train$satisfaction))


#decision tree on selected variables

nw_train = train[c('Class','boarding','Type.of.Travel','service','Flight.Distance'
                   ,'comfort','Convenience','Customer.Type','satisfaction')]
nw_valid = valid[c('Class','boarding','Type.of.Travel','service','Flight.Distance'
                   ,'comfort','Convenience','Customer.Type','satisfaction')]

ds.tree = rpart(as.factor(satisfaction) ~.,
               data=nw_train, 
               method = 'class')


ds.pred = predict(ds.tree,nw_train,type = 'class')
confusionMatrix(ds.pred,as.factor(train$satisfaction))


ds.valid = predict(ds.tree,nw_valid,type = 'class') #accuracy on valiatioin set
confusionMatrix(ds.valid,as.factor(nw_valid$satisfaction))


