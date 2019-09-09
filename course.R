library(caret)
train = read.csv("C:\\Users\\skhristo110016\\Downloads\\pml-training.csv", na.strings=c("NA","#DIV/0!",""))
test = read.csv("C:\\Users\\skhristo110016\\Downloads\\pml-testing.csv", na.strings=c("NA","#DIV/0!",""))


#remove first column
train = train[,2:length(colnames(train))]
test = test[,2:length(colnames(test))]
#change to mean NA
for(i in 1:ncol(train)){
  train[is.na(train[,i]), i] <- mean(train[,i], na.rm = TRUE)
}



for(i in 1:ncol(test )){
  test [is.na(test [,i]), i] <- mean(train [,i], na.rm = TRUE)
}

# remove what cannot be fixed

train = train[, colSums(is.na(train)) == 0] 
test = test[, colSums(is.na(test )) == 0] 

#create partition
inTrain = createDataPartition(train$classe, p=0.70, list=F)
train = train[inTrain, ]
valid = train[-inTrain, ]

# cross validation
control.parms <- trainControl(method="cv", 5)
model <- train(classe ~ ., data=train, method="rf", trControl=control.parms, ntree=30)
print(model)
# out of sample error
valid_predict = predict(model, valid)
out_of_sample_error =  1 - as.numeric(confusionMatrix(valid$classe, valid_predict)$overall[1])
print(out_of_sample_error)
results = predict(model,test)
print(results)