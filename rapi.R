#rf=readRDS("D:/advance analysis/RF.rda")
#data <- read.csv("H:/Data analytics project 1 4th yr/data.csv")
#data['Class'] = as.factor(data$Class)
#data['satisfaction'] = as.factor(data$satisfaction)
#nw_train = data[c('boarding','Class','service','comfort',
                   
#'Type.of.Travel','Convenience','satisfaction'
#tu_test_pred = predict(rf,nw_train,type = 'class')

#tu_test_pred[1]

library(plumber)
library(randomForest)

#* @apiTitle airline
#* @param boarding:numeric
#* @param Class:str
#* @param service:numeric
#* @param comfort:numeric
#* @param Type.of.Travel:str
#* @param Convenience:numeric
#* @post / classification


function(boarding,Class,service,comfort,Type.of.Travel,Convenience){
  boarding+service
}


