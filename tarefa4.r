library(pROC)

# reproducibility:
set.seed(0)

# loading the data:
data <- read.csv('C:/Users/alega/Documents/Mestrado Stats/ML/tarefa_3/dadosTarefa3.csv', sep=';')

# basic exploration:
head(data)
dim(data)
summary(data)

# variables classes:
sapply(data, class)

# variables types:
binnary <- c('Sex','FamilyHx','Married','remission')
factor <- c('DID','SmokingHx')
not_cont <- c(binnary, factor)

# selecting only continuous variables:
data_cont <- data[, -which(names(data) %in% not_cont)]
# data tem alguns valores faltantes, removendo eles:
sum(is.na(data_cont))
data_cont <- na.omit(data_cont)

library(caret)

data <- na.omit(data)

train_percent <- 0.8
train_ind <- createDataPartition(data[,'DID'], p=train_percent, list=FALSE)

dim(data[train_ind,]) # 74 observacoes
dim(data[-train_ind,]) # 17 observacoes

# k-fold:
k = 5

# define training control
train_control <- trainControl(method="cv", number=5)

method = 'svmLinear'
method = 'gbm'

# DID

data[,'DID'] = as.factor(data$DID)

model <- train(DID ~ ., data=data[train_ind,], trControl=train_control, method=method)
summary(model)
acc_ins = max(model$results['Accuracy'])
pred = predict(model, data[-train_ind,])
acc_oos = mean(pred == data[-train_ind,'DID'])
paste('acc_ins =',acc_ins,'acc_oos =',acc_oos)

model$bestTune

# ajuste do parâmetro K:
plot(model)

# matriz de confusão:
confusionMatrix(pred, data[-train_ind,'DID'])

# curva ROC e AUC
pROC_obj <- roc(data[-train_ind,'DID'],as.numeric(pred),
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)


# remission

data[,'remission'] = as.factor(data$remission)

model <- train(remission ~ ., data=data[train_ind,], trControl=train_control, method=method)
summary(model)
acc_ins = max(model$results['Accuracy'])
pred = predict(model, data[-train_ind,])
acc_oos = mean(pred == data[-train_ind,'remission'])
paste('acc_ins =',acc_ins,'acc_oos =',acc_oos)

model$bestTune

# ajuste do parâmetro K:
plot(model)

# matriz de confusão:
confusionMatrix(pred, data[-train_ind,'remission'])

# curva ROC e AUC
pROC_obj <- roc(data[-train_ind,'remission'],as.numeric(pred),
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)


# SmokingHx

data[,'SmokingHx'] = as.factor(data$SmokingHx)

model <- train(SmokingHx ~ ., data=data[train_ind,], trControl=train_control, method=method)
summary(model)
acc_ins = max(model$results['Accuracy'])
pred = predict(model, data[-train_ind,])
acc_oos = mean(pred == data[-train_ind,'SmokingHx'])
paste('acc_ins =',acc_ins,'acc_oos =',acc_oos)

model$bestTune

# ajuste do parâmetro K:
plot(model)

# matriz de confusão:
confusionMatrix(pred, data[-train_ind,'SmokingHx'])

# curva ROC e AUC
pROC_obj <- roc(data[-train_ind,'SmokingHx'],as.numeric(pred),
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)
