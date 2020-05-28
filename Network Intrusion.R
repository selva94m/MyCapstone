df=read.csv(file.choose())
df1=df
head(df)
str(df1)
df1$target.class =as.factor(df$target.class)
df1$protocol_type_udp =as.factor(df1$protocol_type_udp)
df1$land =as.factor(df1$land)
df1$logged_in=as.factor(df1$logged_in)
df1$root_shell=as.factor(df1$root_shell)
df1$su_attempted=as.factor(df1$su_attempted)
df1$is_guest_login=as.factor(df1$is_guest_login)
df2=df1[ ,c(1,4,5,7,28,26)]
head(df2)
df1=read.csv(file.choose())
str(df1)
df1$target=df$target.class
df1$target=as.factor(df1$target)
str(df1)

library(caTools)
set.seed(144)
spl = sample.split(df1$target, SplitRatio = 0.7)
Train = subset(df1, spl == TRUE)
Test = subset(df1, spl == FALSE)
df1$protocol_type_icmp = NULL
library(caret)
library(mlr)


## Making Train and Test Task

TrainTask= makeClassifTask(data=Train, target = "target", positive = "1")
TestTask= makeClassifTask(data=Test, target = "target", positive = "1")

## Logistic Regression

logistic.part3= makeLearner("classif.logreg", predict.type = "response")
logistic.part3Model=train(logistic.part3, TrainTask)
logistic.part3Predict=predict(logistic.part3Model, TestTask)

confusionMatrix(logistic.part3Predict$data$truth, 
                logistic.part3Predict$data$response, positive = "1")

## SVM

svm.part3= makeLearner("classif.ksvm", predict.type = "response")
svm.part3Model=train(svm.part3, part3TrainTask)
svm.part3Predict=predict(svm.part3Model, part3TestTask)

confusionMatrix(svm.part3Predict$data$truth, svm.part3Predict$data$response, positive = "Delay")


## GBM

## Logistic Regression

gbm.part3= makeLearner("classif.gbm", predict.type = "response")
gbm.part3Model=train(gbm.part3, part3TrainTask)
gbm.part3Predict=predict(gbm.part3Model, part3TestTask)

confusionMatrix(gbm.part3Predict$data$truth, gbm.part3Predict$data$response, positive = "Delay")


## Random Forest

rf.part3= makeLearner("classif.randomForest", predict.type = "response")
rf.part3Model=train(rf.part3, TrainTask)
rf.part3Predict=predict(rf.part3Model, TestTask)

confusionMatrix(rf.part3Predict$data$truth, rf.part3Predict$data$response, positive = "1")

## Tuning Random Forest

rf_param = makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 500),
  makeIntegerParam("mtry", lower = 3, upper = 10),
  makeIntegerParam("nodesize", lower = 10, upper = 50)
)


rancontrol = makeTuneControlRandom(maxit = 50L)
set_cv = makeResampleDesc("CV",iters = 3L)
tuned.rf = tuneParams(learner = rf.part3, resampling = set_cv, task = part3TrainTask, par.set = rf_param, control = rancontrol, measures = acc)

tuned.rfLearner = setHyperPars(rf.part3, par.vals = tuned.rf$x)
tuned.rfModel = train(tuned.rfLearner, part3TrainTask)
tuned.rfPredict = predict(tuned.rfModel, part3TestTask)


confusionMatrix(tuned.rfPredict$data$truth, tuned.rfPredict$data$response, positive = "Delay")


## ROC and AUC

rf.learner2= makeLearner("classif.randomForest", predict.type = "prob")
rf.model2=train(rf.learner2, part3TrainTask)
rf.predict2=predict(rf.model2, part3TestTask)
df2 = generateThreshVsPerfData(rf.predict2, measures = list(fpr, tpr, mmce))
plotROCCurves(df2)
performance(rf.predict2,auc)
plotThreshVsPerf(df2


