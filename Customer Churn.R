getwd()
DF <- read.csv("C:/BABI/Capstone/EDA/ChurnFinal_v1.0.csv",header = TRUE,na.strings = c("","NA"," "))
churnDF <- DF

library(MASS)
library(DMwR)

dim(churnDF)
str(churnDF)
names(churnDF)
summary(churnDF)
head(churnDF)
tail(churnDF)

churnDF$Zip.Code <- as.factor(churnDF$Zip.Code)
churnDF$Churnstatus <- as.factor(churnDF$Churnstatus)
summary(churnDF$Churnstatus)
summary(churnDF$Zip.Code)
table(churnDF$Churnstatus)
prop.table(table(churnDF$Churnstatus))

unique_churnDF <- unique(churnDF)
dim(unique_churnDF)
table(unique_churnDF$Churnstatus)
prop.table(table(unique_churnDF$Churnstatus))
summary(unique_churnDF)
head(unique_churnDF)
write.csv(unique_churnDF,file = "C:/BABI/Capstone/EDA/unique_churn.csv",na="NA")

# Data Cleaning

impute_churnDF <- unique_churnDF
list_na <- colnames(impute_churnDF)[ apply(impute_churnDF, 2, anyNA) ]
str(impute_churnDF$Referred.a.Friend)
summary(impute_churnDF$Referred.a.Friend)
head(impute_churnDF$Referred.a.Friend)
impute_churnDF$Referred.a.Friend[is.na(impute_churnDF$Referred.a.Friend)] <- ifelse(!is.na(impute_churnDF$Number.of.Referrals) &
                                                    (impute_churnDF$Number.of.Referrals > 0 ), 
                                             "Yes",
                                             "No")
                                             
str(impute_churnDF$Offer)
summary(impute_churnDF$Offer)
impute_churnDF$Offer[is.na(impute_churnDF$Offer)] <- "None"

str(impute_churnDF$Internet.Service)
summary(impute_churnDF$Internet.Service)
impute_churnDF$Internet.Service[is.na(impute_churnDF$Internet.Service)] <- "Yes"

str(impute_churnDF$Online.Security)
summary(impute_churnDF$Online.Security)
impute_churnDF$Online.Security[is.na(impute_churnDF$Online.Security)] <- "No"

str(impute_churnDF$Device.Protection.Plan)
summary(impute_churnDF$Device.Protection.Plan)
impute_churnDF$Device.Protection.Plan[is.na(impute_churnDF$Device.Protection.Plan)] <- "Yes"

str(impute_churnDF$Online.Backup)
summary(impute_churnDF$Online.Backup)
impute_churnDF$Online.Backup[is.na(impute_churnDF$Online.Backup)] <- "No"


str(impute_churnDF$Premium.Tech.Support)
summary(impute_churnDF$Premium.Tech.Support)
impute_churnDF$Premium.Tech.Support[is.na(impute_churnDF$Premium.Tech.Support)] <- "No"

str(impute_churnDF$Streaming.TV)
summary(impute_churnDF$Streaming.TV)
impute_churnDF$Streaming.TV[is.na(impute_churnDF$Streaming.TV)] <- "No"
impute_churnDF$Streaming.Movies[is.na(impute_churnDF$Streaming.Movies)] <- "No"
impute_churnDF$Streaming.Music[is.na(impute_churnDF$Streaming.Music)] <- "No"

summary(impute_churnDF$Country)
impute_churnDF$Country[is.na(impute_churnDF$Country)] <- "India"


str(impute_churnDF$Under.30)
summary(impute_churnDF$Under.30)
impute_churnDF$Under.30 <- as.factor(ifelse(!is.na(impute_churnDF$Age) &
                                                  (impute_churnDF$Age < 30 ), 
                                                     "Yes",
                                                     "No")
                                      )

str(impute_churnDF$Senior.Citizen)
summary(impute_churnDF$Senior.Citizen)
impute_churnDF$Senior.Citizen <- as.factor(ifelse(!is.na(impute_churnDF$Age) &
                                              (impute_churnDF$Age >= 65 ), 
                                            "Yes",
                                            "No")
                                           )
estimate_mode <- function(x) {
  d <- density(x,na.rm = TRUE)
  d$x[which.max(d$y)]
}

str(impute_churnDF$Total.Refunds)
summary(impute_churnDF$Total.Refunds)
impute_churnDF$Total.Refunds[is.na(impute_churnDF$Total.Refunds)] <- estimate_mode(impute_churnDF$Total.Refunds)


str(impute_churnDF$Number.of.Dependents)
summary(impute_churnDF$Number.of.Dependents)
impute_churnDF$Number.of.Dependents[is.na(impute_churnDF$Number.of.Dependents)] <- estimate_mode(impute_churnDF$Number.of.Dependents)

str(impute_churnDF$Number.of.Referrals)
summary(impute_churnDF$Number.of.Referrals)
impute_churnDF$Number.of.Referrals[is.na(impute_churnDF$Number.of.Referrals)] <- estimate_mode(impute_churnDF$Number.of.Referrals)


summary(impute_churnDF)
impute_churnDF$Zip.Code <- as.factor(impute_churnDF$Zip.Code)
impute_churnDF$Churnstatus <- as.factor(impute_churnDF$Churnstatus)
table(impute_churnDF$Churnstatus)
prop.table(table(impute_churnDF$Churnstatus))
list_na <- colnames(impute_churnDF)[ apply(impute_churnDF, 2, anyNA) ]

write.csv(impute_churnDF,file = "C:/BABI/Capstone/EDA/impute_churn.csv",na="NA")
str(impute_churnDF)
dim(impute_churnDF)
summary(impute_churnDF)


### Data Cleaning by Imputation Done

smote_churnDF <- NULL
test_df <- NULL
train_df <- NULL
smote_churnDF <- read.csv("C:/BABI/Capstone/EDA/df_smoted.csv",header = TRUE,na.strings = c(""," ",NA))

str(smote_churnDF)
dim(smote_churnDF)
summary(smote_churnDF)
table(smote_churnDF$Churnstatus)
prop.table(table(smote_churnDF$Churnstatus))

smote_churnDF$Zip.Code <- as.factor(smote_churnDF$Zip.Code)
smote_churnDF$Churnstatus <- as.factor(smote_churnDF$Churnstatus)

## SMOTE complete, done through Python code


numeric.smote_churnDF <- Filter(is.numeric,smote_churnDF)
names(numeric.smote_churnDF)
factor.smote_churnDF <- Filter(is.factor,smote_churnDF)
names(factor.smote_churnDF)

cor(numeric.smote_churnDF)
library(corrplot)
corrplot(cor(numeric.smote_churnDF),type = "lower")
library(RColorBrewer)
#plot(numeric.churnDF)
#attach(churnDF)

## ************************************
## ggplot: Box Plots
## ************************************
library(ggplot2)
ggplot(smote_churnDF, aes(x = smote_churnDF$Tenure.in.Months)) +       ## Simple Box Plot - Midsize has high variance
  geom_histogram(aes(y=..density..),binwidth = 4,colour = "black", fill = "white") +
  geom_density(alpha=.3, fill = "#FF6666")
  
library(RColorBrewer)
boxplot(smote_churnDF$Tenure.in.Months ~ smote_churnDF$Churnstatus, data=smote_churnDF, horizontal = TRUE,
        xlab="Tenure in Months", ylab="Churn", main="Churn vs Tenure",
        col=brewer.pal(5, "Set3"))

boxplot(smote_churnDF$Age ~ smote_churnDF$Churnstatus, data=smote_churnDF, horizontal = TRUE,
        xlab="Age", ylab="Churn", main="Churn vs Age",
        col=brewer.pal(5, "Set3"))

boxplot(smote_churnDF$Total.Charges ~ smote_churnDF$Churnstatus, data=smote_churnDF, horizontal = TRUE,
        xlab="Total Charges", ylab="Churn", main="Churn vs Charges",
        col=brewer.pal(5, "Set3"))

boxplot(smote_churnDF$Avg.Monthly.GB.Download ~ smote_churnDF$Churnstatus, data=smote_churnDF, horizontal = TRUE,
        xlab="Monthly Download", ylab="Churn", main="Churn vs Download",
        col=brewer.pal(5, "Set3"))


ggplot(smote_churnDF, aes(y = smote_churnDF$Tenure.in.Months, y = smote_churnDF$Churnstatus)) +       ## Coloured Box Plot
  geom_boxplot()

ggplot(smote_churnDF, aes(y = smote_churnDF$Tenure.in.Months, x = smote_churnDF$Churnstatus, fill = Churnstatus)) +       ## Coloured Box Plot
  geom_boxplot()

ggplot(churnDF, aes(y = smote_churnDF$Tenure.in.Months, x = smote_churnDF$Churnstatus, fill = Churnstatus)) +       ## Coloured Box Plot without Legend
  geom_boxplot() +
  guides(fill = FALSE)

ggplot(smote_churnDF, aes(y = smote_churnDF$Tenure.in.Months, x = smote_churnDF$Churnstatus, fill = Churnstatus)) +       ## Coloured Box Plot with Flipped Coordinates
  geom_boxplot() +
  guides(fill = FALSE) +
  coord_flip()

#######################################################################################
# DATA MODELLING
#######################################################################################
# 1) Logistic Regression
#######################################################################################
library(caret)
library(mlr)
library(caTools)

train_df <- smote_churnDF
clean_df <- read.csv("C:/BABI/Capstone/EDA/impute_churn.csv",header = TRUE,na.strings = c(""," ",NA))
str(clean_df)
clean_df$Zip.Code <- as.factor(clean_df$Zip.Code)
clean_df$Churnstatus <- as.factor(clean_df$Churnstatus)
clean_df <- na.omit(clean_df)
dim(clean_df)

set.seed(999)
spl = sample.split(clean_df$Churnstatus, SplitRatio = 0.3)
test_df = subset(clean_df, spl == TRUE)
write.csv(test_df,file = "C:/BABI/Capstone/EDA/test_churn.csv",na="NA")

test_df <- NULL
test_df <- read.csv("C:/BABI/Capstone/EDA/test_churn.csv",header = TRUE,na.strings = c(""," ",NA))
test_df[1] <- NULL
test_df$Zip.Code <- as.factor(test_df$Zip.Code)
test_df$Churnstatus <- as.factor(test_df$Churnstatus)
str(train_df)
str(test_df)
dim(test_df)
dim(train_df)
names(test_df)
table(train_df$Churnstatus)
table(test_df$Churnstatus)
summarizeColumns(test_df)


logit=glm(Churnstatus~.,data=train_df[,-c(1:2,7,9,23,25:28,36:41)],family=binomial)

library(lmtest)
lrtest(logit)

library(pscl)
pR2(logit)
summary(logit)

library(car)
vif(logit)

# ROC Curve - Receiver Operating Characteristic Curve
library(Deducer)
rocplot(logit,AUC=TRUE)

#install.packages("InformationValue")

#prediction <- predict(logit, newdata=subset(test_df,select=-c(1:2,7,9,23,25:28,36:41)),
#                                            type="response")
prediction <- predict(logit, newdata=test_df,type="response")
#library(InformationValue)
# masks caret library called
#optCutOff <- optimalCutoff(test_df$Churnstatus, prediction)[1] 
#detach("package:InformationValue", unload=TRUE)
cutoff <- NULL
#cutoff <- floor(prediction+round(optCutOff))
cutoff <- floor(prediction+0.5)
predictedresponse<-as.factor(cutoff)
misClasificError <- mean(predictedresponse != test_df$Churnstatus)
confusionMatrix(predictedresponse,test_df$Churnstatus)

# Generate mlt tasks
trainTask= makeClassifTask(data=train_df, target = "Churnstatus", positive = "1")
testTask= makeClassifTask(data=test_df, target = "Churnstatus", positive = "1")

# Identify important and significant features

#install.packages("FSelector")
library(FSelector)
im_feat <- generateFilterValuesData(trainTask, method = c("FSelector_information.gain","FSelector_chi.squared"))
plotFilterValues(im_feat,n.show = 10)

trainTask = dropFeatures(task = trainTask,
                         features = c("Customer.ID","Referred.a.Friend",
                                      "Avg.Monthly.Long.Distance.Charges",
                                      "Monthly.Charge",
                                      "Total.Refunds","Total.Extra.Data.Charges",
                                      "Total.Long.Distance.Charges","Total.Revenue",
                                      "Country","State","City","Zip.Code","Churn.Category",
                                      "Churn.Reason"))

testTask = dropFeatures(task = testTask,
                        features = c("Customer.ID","Referred.a.Friend",
                                     "Avg.Monthly.Long.Distance.Charges",
                                     "Monthly.Charge",
                                     "Total.Refunds","Total.Extra.Data.Charges",
                                     "Total.Long.Distance.Charges","Total.Revenue",
                                     "Country","State","City","Zip.Code","Churn.Category",
                                     "Churn.Reason"))

#######################################################################################
## 1 ) Logistic Regression
#######################################################################################

impVar<-varImp(logit) # from catools

logistic.Learn <- NULL
logistic.Learn <- makeLearner("classif.logreg", predict.type = "prob")#,predict.threshold = 0.7)
logistic.Model <- train(logistic.Learn, trainTask)
logistic.Predict <- predict(logistic.Model,testTask)
confusionMatrix(logistic.Predict$data$truth, logistic.Predict$data$response, positive = "1",
                mode = "everything")
logistic.roc_df = generateThreshVsPerfData(logistic.Predict, measures = list(fpr, tpr, mmce))
plotROCCurves(logistic.roc_df)
plotThreshVsPerf(logistic.roc_df)
#plotLearningCurve(generateLearningCurveData(logistic.Learn,trainTask))
performance(logistic.Predict,measures = mlr::auc)

#makeMeasure("1a",aggr="mmse", properties = "classif",minimize = TRUE)
#rdesc = makeResampleDesc(test_df)
#bmr <- benchmark(logistic.Learn,trainTask,measures = mea)



# No tuning parameters in logistic Reg.
logistic.Learn <- NULL
getParamSet("classif.logreg")
cv.logistic <- crossval(learner = logistic.Learn,task = trainTask,iters = 10,
                        stratify = TRUE,measures = acc,show.info = TRUE,
                        keep.pred = TRUE)
fmodel <- train(logistic.Learn,trainTask)
getLearnerModel(fmodel)
fpmodel <- predict(fmodel, testTask)
confusionMatrix(fpmodel$data$truth, fpmodel$data$response, positive = "1",
                mode = "everything")
plotHyperParsEffect()
View(cv.logistic)
?makeLearner

#View(logistic.Model)
#View(logistic.Predict)
#######################################################################################
#Load NB -> Naive Bayes
#######################################################################################

nb.learner <- NULL
nb.learner=makeLearner("classif.naiveBayes",predict.type="prob")
nb.model=train(nb.learner,trainTask)
nb.predict=predict(nb.model,testTask)

confusionMatrix(nb.predict$data$truth,nb.predict$data$response, positive = "1",
                mode = "everything")
performance(nb.predict,measures = mlr::auc)


# NB - CV
nb.learner <- NULL
nb.learner=makeLearner("classif.naiveBayes",predict.type="prob")
getParamSet("classif.naiveBayes")
getLearnerParamSet(nb.learner)
nb.param <- makeParamSet(makeNumericParam("laplace",lower = 1, upper = 10))
rancontrol = makeTuneControlRandom(maxit = 5L)
set_cv = makeResampleDesc("CV",iters = 3L)
nb.tuned = tuneParams(learner = nb.learner, resampling = set_cv, task = trainTask, par.set = nb.param, control = rancontrol, measures = acc)
tuned.nbLearner = setHyperPars(nb.learner, par.vals = nb.tuned$x)
tuned.nbModel=train(tuned.nbLearner,trainTask)
tuned.nbPredict=predict(tuned.nbModel,testTask)
confusionMatrix(tuned.nbPredict$data$truth,tuned.nbPredict$data$response, positive = "1",
                mode = "everything")



#######################################################################################
# Try Linear Regression
#######################################################################################
lrtrain_df <- train_df
lrtest_df <- test_df
lrtrain_df$Churnstatus <- as.numeric(lrtrain_df$Churnstatus)
lrtest_df$Churnstatus <- as.numeric(lrtest_df$Churnstatus)

lrTrainTask= makeRegrTask(data=lrtrain_df, target = "Churnstatus")
lrTestTask=makeRegrTask(data=lrtest_df, target = "Churnstatus")

lrTrainTask = dropFeatures(task = lrTrainTask,
                         features = c("Customer.ID","Referred.a.Friend",
                                      "Avg.Monthly.Long.Distance.Charges",
                                      "Monthly.Charge",
                                      "Total.Refunds","Total.Extra.Data.Charges",
                                      "Total.Long.Distance.Charges","Total.Revenue",
                                      "Country","State","City","Zip.Code","Churn.Category",
                                      "Churn.Reason"))

lrTestTask = dropFeatures(task = lrTestTask,
                        features = c("Customer.ID","Referred.a.Friend",
                                     "Avg.Monthly.Long.Distance.Charges",
                                     "Monthly.Charge",
                                     "Total.Refunds","Total.Extra.Data.Charges",
                                     "Total.Long.Distance.Charges","Total.Revenue",
                                     "Country","State","City","Zip.Code","Churn.Category",
                                     "Churn.Reason"))

lr.learner= makeLearner("regr.lm")
lr.model=train(lr.learner, lrTrainTask)
lr.model=getLearnerModel(lr.model)
summary(lr.model)

## LASSO Regression
library(glmnet)
glmnet.learnerLASSO=makeLearner("regr.glmnet", alpha= 1, predict.type = "response")
glmnet.modelLasso= train(glmnet.learnerLASSO, lrTrainTask)
glmnetLassoModel= getLearnerModel(glmnet.modelLasso)

### Contract , Dependents, Premium tech support, Married, Offer are the top 5 in LASSo

#install.packages("plotmo")
library(plotmo) # for plot_glmnet
plot_glmnet(glmnetLassoModel)                             # default colors
plot_glmnet(glmnetLassoModel, label=5)

## RIDGE REGRESSION

glmnet.learnerRIDGE=makeLearner("regr.glmnet", alpha= 0, predict.type = "response")
glmnet.modelRIDGE= train(glmnet.learnerRIDGE, lrTrainTask)
glmnetRIDGEModel= getLearnerModel(glmnet.modelRIDGE)

plot_glmnet(glmnetRIDGEModel)                             # default colors
plot_glmnet(glmnetRIDGEModel, label=5)

# Contract and Dependendts are the top 5 in RIDGE

#######################################################################################
#QDA - Quadratic Discriminant Analysis => Error refer to
#@http://www.socr.umich.edu/people/dinov/2017/Spring/DSPA_HS650/notes/20_PredictionCrossValidation.html
# Do not run
#######################################################################################
qda.learner=makeLearner("classif.qda", predict.type = "response")
qda.model=train(qda.learner, trainTask)
qda.predict= predict(qda.model, testTask)
confusionMatrix(qda.predict$data$truth, qda.predict$data$response, positive = "1",
                mode = "everything")
performance(qda.predict,measures = mlr::auc)


#######################################################################################
#LDA - Linear Discriminant Analysis
#######################################################################################
lda.learner <- NULL
lda.learner=makeLearner("classif.lda", predict.type = "prob")
lda.model=train(lda.learner, trainTask)
lda.predict= predict(lda.model, testTask)
confusionMatrix(lda.predict$data$truth, lda.predict$data$response, positive = "1",
                mode = "everything")
performance(lda.predict,measures = mlr::auc)


#LDA - CV
lda.learner <- NULL
lda.learner=makeLearner("classif.lda",predict.type="prob")
getParamSet("classif.lda")
lda.param <- makeParamSet(makeNumericParam("nu",lower = 2, upper = 3),
                         makeNumericParam("tol",lower = 0, upper = 0)
                         )
lda.control = makeTuneControlRandom(maxit = 10L)
set_cv = makeResampleDesc("CV",iters = 3L)
lda.tuned = tuneParams(learner = lda.learner, resampling = set_cv, task = trainTask, par.set = lda.param, control = lda.control, measures = acc)
tuned.ldaLearner = setHyperPars(lda.learner, par.vals = lda.tuned$x)
tuned.ldaModel=train(tuned.ldaLearner,trainTask)
tuned.ldaPredict=predict(tuned.ldaModel,testTask)
confusionMatrix(tuned.ldaPredict$data$truth,tuned.ldaPredict$data$response, positive = "1",
                mode = "everything")


#######################################################################################
## CART
#######################################################################################
library(rpart.plot)
cart.learner <- NULL
cart.learner=makeLearner("classif.rpart", predict.type = "prob")
cart.model=train(cart.learner, trainTask)
cart.predict= predict(cart.model, testTask)
confusionMatrix(cart.predict$data$truth, cart.predict$data$response, positive = "1",
                mode = "everything")
performance(cart.predict,measures = mlr::auc)
rpart.plot(cart.model$learner.model,roundint=FALSE,type=4)

#CART/Decision Tree - CV
cart.learner <- NULL
cart.learner=makeLearner("classif.rpart",predict.type="prob")
getParamSet("classif.rpart")
cart.param <- makeParamSet(makeIntegerParam("minsplit",lower = 50, upper = 100),
                          makeIntegerParam("minbucket",lower = 20, upper = 50),
                          makeNumericParam("cp",lower = 0.001, upper = 0.1)
                          )
#minsplit - min number of observations in the node that could be split further 
# pre-pruning, value = 1 to 4% of number of records
#minbucket - min number of observations that are allowed in a terminal node
# pre-pruning , value = 1/3 of min split
#maxdepth - prevents the tree from growing past a certain depth / height
#Cp - Complexity parameter is used to mention the minimum improvement before proceeding
#further.It is the amount by which splitting a node improved the relative error.
#http://datamining.togaware.com/survivor/Complexity_cp.html
#https://statinfer.com/203-3-10-pruning-a-decision-tree-in-r/

cart.control = makeTuneControlRandom(maxit=200L)
set_cv = makeResampleDesc("CV",iters = 10L)
cart.tuned = tuneParams(learner = cart.learner, resampling = set_cv, task = trainTask,
                        par.set = cart.param, control = cart.control, measures = acc)
tuned.cartLearner = setHyperPars(cart.learner, par.vals = cart.tuned$x)
tuned.cartModel=train(tuned.cartLearner,trainTask)
tuned.cartPredict=predict(tuned.cartModel,testTask)
confusionMatrix(tuned.cartPredict$data$truth,tuned.cartPredict$data$response, positive = "1",
                mode = "everything")




#######################################################################################
## RandomForest
#######################################################################################
rf.learner <- NULL
rf.learner=makeLearner("classif.randomForest", predict.type = "prob")
rf.model=train(rf.learner, trainTask)
rf.predict= predict(rf.model, testTask)
confusionMatrix(rf.predict$data$truth, rf.predict$data$response, positive = "1",
                mode = "everything")
performance(rf.predict,measures = mlr::auc)
plot(rf.model$learner.model)


# CV - RF
rf.learner <- NULL
rf.learner=makeLearner("classif.randomForest",predict.type="prob")
getParamSet("classif.randomForest")
rf.param <- makeParamSet(makeIntegerParam("ntree",lower = 400, upper = 500),
                         makeIntegerParam("mtry",lower = 5, upper = 10),
                         makeNumericParam("nodesize",lower = 10, upper = 10)
                         )
#ntree - number of trees
#mtry - number of variables randomly sampled for splitting the node, sqrt(ncols(data))
#nodesize - min number of observations in each node
rf.control = makeTuneControlRandom(maxit = 10L)
# random search and sample from the sampling dist. of hyper parameters
set_cv = makeResampleDesc("CV",iters = 3L) # resampling
rf.tuned = tuneParams(learner = rf.learner, resampling = set_cv, task = trainTask,
                      par.set = rf.param, control = rf.control, measures = acc)
tuned.rfLearner = setHyperPars(rf.learner, par.vals = rf.tuned$x)
tuned.rfModel=train(tuned.rfLearner,trainTask)
tuned.rfPredict=predict(tuned.rfModel,testTask)
confusionMatrix(tuned.rfPredict$data$truth,tuned.rfPredict$data$response, positive = "1",
                mode = "everything")


#######################################################################################
## SVM
## https://www.hackerearth.com/blog/developers/simple-tutorial-svm-parameter-tuning-python-r/
#######################################################################################
library(kernlab)
svm.learner=makeLearner("classif.ksvm", predict.type = "prob")
svm.model=train(svm.learner, trainTask)
svm.predict= predict(svm.model, testTask)
confusionMatrix(svm.predict$data$truth, svm.predict$data$response, positive = "1",
                mode = "everything")
performance(svm.predict,measures = mlr::auc)

#plot(svm.model$learner.model, data = train_df[,-c(1:2,7,23,25:28,36:42)])

#x <- rbind(matrix(rnorm(120),,2),matrix(rnorm(120,mean=3),,2))
#y <- matrix(c(rep(1,60),rep(-1,60)))
#svp <- ksvm(x,y,type="C-svc")
#plot(svp,data=x)


# CV - SVM
ksvm.learner <- NULL
ksvm.learner=makeLearner("classif.ksvm",predict.type="prob")
getParamSet("classif.ksvm")
ksvm.param <- makeParamSet(makeDiscreteParam("C", values = 2^c(1:4)), 
                           #cost parameters
                           makeDiscreteParam("sigma", values = c(0,0.1,1)) 
                           # RBF Kernel Parameter                         
                            )
ksvm.control = makeTuneControlRandom(maxit = 5L) # random search
set_cv = makeResampleDesc("CV",iters = 3L)
ksvm.tuned = tuneParams(learner = ksvm.learner, resampling = set_cv, task = trainTask,
                      par.set = ksvm.param, control = ksvm.control, measures = acc)
tuned.ksvmLearner = setHyperPars(ksvm.learner, par.vals = ksvm.tuned$x)
tuned.ksvmModel=train(tuned.ksvmLearner,trainTask)
tuned.ksvmPredict=predict(tuned.ksvmModel,testTask)
confusionMatrix(tuned.ksvmPredict$data$truth,tuned.ksvmPredict$data$response, positive = "1",
                mode = "everything")


#######################################################################################
## GBM - Gradient Boosting Machine
## https://www.kaggle.com/camnugent/gradient-boosting-and-parameter-tuning-in-r
#######################################################################################
#install.packages("gbm")
gbm.learner=makeLearner("classif.gbm", predict.type = "prob")
gbm.model=train(gbm.learner, trainTask)
gbm.predict= predict(gbm.model, testTask)
confusionMatrix(gbm.predict$data$truth, gbm.predict$data$response, positive = "1",
                mode = "everything")
performance(gbm.predict,measures = mlr::auc)

# CV - GBM
gbm.learner <- NULL
gbm.learner=makeLearner("classif.gbm",predict.type="prob")
getParamSet("classif.gbm")
gbm.param <- makeParamSet(makeIntegerParam("n.trees",lower = 100, upper = 200),
                           makeIntegerParam("interaction.depth",lower = 8, upper = 10),
                           makeNumericParam("n.minobsinnode",lower =20, upper = 20),
                           makeDiscreteParam("distribution",values="bernoulli")
)
gbm.control = makeTuneControlRandom(maxit = 5L)
set_cv = makeResampleDesc("CV",iters = 3L)
gbm.tuned = tuneParams(learner = gbm.learner, resampling = set_cv, task = trainTask,
                        par.set = gbm.param, control = gbm.control, measures = acc)
tuned.gbmLearner = setHyperPars(gbm.learner, par.vals = gbm.tuned$x)
tuned.gbmModel=train(tuned.gbmLearner,trainTask)
tuned.gbmPredict=predict(tuned.gbmModel,testTask)
confusionMatrix(tuned.gbmPredict$data$truth,tuned.gbmPredict$data$response, positive = "1",
                mode = "everything")


#######################################################################################
# ADA - Adaptive Boosting
# https://www.machinelearningplus.com/machine-learning/caret-package/
# https://medium.com/diogo-menezes-borges/boosting-with-adaboost-and-
# gradient-boosting-9cbab2a1af81
#######################################################################################
ada.learner=makeLearner("classif.ada", predict.type = "prob")
ada.model=train(ada.learner, trainTask)
ada.predict= predict(ada.model, testTask)
confusionMatrix(ada.predict$data$truth, ada.predict$data$response, positive = "1",
                mode = "everything")

performance(ada.predict,measures = mlr::auc)

# CV - ADA
gbm.learner <- NULL
gbm.learner=makeLearner("classif.ada",predict.type="prob")
getParamSet("classif.ada")
ada.param <- makeParamSet(makeIntegerParam("minsplit",lower = 50, upper = 100),
                           makeIntegerParam("minbucket",lower = 20, upper = 50),
                           makeNumericParam("cp",lower = 0.001, upper = 0.002)
                          )
ada.control = makeTuneControlRandom(maxit = 10L)
set_cv = makeResampleDesc("CV",iters = 3L)
ada.tuned = tuneParams(learner = ada.learner, resampling = set_cv, task = trainTask,
                       par.set = ada.param, control = ada.control, measures = acc)
tuned.adaLearner = setHyperPars(ada.learner, par.vals = ada.tuned$x)
tuned.adaModel=train(tuned.adaLearner,trainTask)
tuned.adaPredict=predict(tuned.adaModel,testTask)
confusionMatrix(tuned.adaPredict$data$truth,tuned.adaPredict$data$response, positive = "1",
                mode = "everything")



#######################################################################################
#KNN does not support factors, use kknn instead
#######################################################################################
kknn.learner=makeLearner("classif.kknn", predict.type = "prob")
kknn.model=train(kknn.learner, trainTask)
kknn.predict= predict(kknn.model, testTask)
confusionMatrix(kknn.predict$data$truth, kknn.predict$data$response, positive = "1",
                mode = "everything")
performance(kknn.predict,measures = mlr::auc)

# CV - KKNN
kknn.learner <- NULL
kknn.learner=makeLearner("classif.kknn",predict.type="prob")
getParamSet("classif.kknn")
kknn.param <- makeParamSet(makeIntegerParam("k",lower = 10, upper = 10),
                          makeNumericParam("distance",lower = 5, upper = 10)
                          )
kknn.control = makeTuneControlRandom(maxit = 3L)
set_cv = makeResampleDesc("CV",iters = 3L)
kknn.tuned = tuneParams(learner = kknn.learner, resampling = set_cv, task = trainTask,
                       par.set = kknn.param, control = kknn.control, measures = acc)
tuned.kknnLearner = setHyperPars(kknn.learner, par.vals = kknn.tuned$x)
tuned.kknnModel=train(tuned.kknnLearner,trainTask)
tuned.kknnPredict=predict(tuned.kknnModel,testTask)
confusionMatrix(tuned.kknnPredict$data$truth,tuned.kknnPredict$data$response, positive = "1",
                mode = "everything")


#######################################################################################
#XGBoost ( No factors), cannot be used - Do  not run
#######################################################################################
xgb.learner=makeLearner("classif.xgboost", predict.type = "response")
xgb.model=train(gbm.learner, trainTask)
xgb.predict= predict(gbm.model, testTask)
confusionMatrix(xgb.predict$data$truth, xgb.predict$data$response, positive = "1",
                mode = "everything")

#######################################################################################
# NN 
#######################################################################################
library(nnet)
nn.learner<- NULL
nn.learner=makeLearner("classif.nnet", predict.type = "prob")
nn.model=train(nn.learner, trainTask)
nn.predict= predict(nn.model, testTask)
confusionMatrix(nn.predict$data$truth, nn.predict$data$response, positive = "1",
                mode = "everything")
performance(nn.predict,measures = mlr::auc)
#plot(nn.model$learner.model)
#library(devtools)
#source_url('https://gist.github.com/fawda123/7471137/raw/cd6e6a0b0bdb4e065c597e52165e5ac887f5fe95/nnet_plot_update.r')


# CV - NN
nn.learner <- NULL
nn.learner=makeLearner("classif.nnet",predict.type="prob")
getParamSet("classif.nnet")
nn.param <- makeParamSet(makeIntegerParam("size",lower = 10, upper = 15),
                         makeIntegerParam("maxit",lower = 100, upper = 200),
                         makeNumericParam("reltol",lower = 0.0001, upper = 0.0002),
                         makeNumericParam("decay",lower = 0.0001, upper = 0.0001)
                        )
nn.control = makeTuneControlRandom(maxit = 10L)
set_cv = makeResampleDesc("CV",iters = 3L)
nn.tuned = tuneParams(learner = nn.learner, resampling = set_cv, task = trainTask,
                       par.set = nn.param, control = nn.control, measures = acc)
tuned.nnLearner = setHyperPars(nn.learner, par.vals = nn.tuned$x)
tuned.nnModel=train(tuned.nnLearner,trainTask)
tuned.nnPredict=predict(tuned.nnModel,testTask)
confusionMatrix(tuned.nnPredict$data$truth,tuned.nnPredict$data$response, positive = "1",
                mode = "everything")



#######################################################################################
### Hierarchial Clustering
#######################################################################################
library(cluster)
cluster_df <- clean_df
#gower.dist <- daisy(x=train_df[,-c(1,36:42)], metric = c("gower")) 
#divisive.clust <- diana(as.matrix(gower.dist),diss = TRUE, keep.diss = TRUE)
#plot(divisive.clust, main = "Divisive")

#aggl.clust <- hclust(gower.dist, method = "complete")
#plot(aggl.clust, main = "Agglomerative, complete linkages")
cluster_df$Customer.ID <- as.numeric(cluster_df$Customer.ID)
numeric.cluster_df <- Filter(is.numeric,cluster_df)
factor.cluster_df <- Filter(is.factor,cluster_df)
names(numeric.cluster_df)

d.euc <- dist(numeric.cluster_df[-1],"euclidean")
clus1 <- hclust(d.euc, method = "average")
plot(clus1)

## scale function standardizes the values
scaled.df<- scale(numeric.cluster_df[-1])
head(scaled.df, 10)
d.scaled.euc <- dist(x=scaled.df, method = "euclidean") 
clus2 <- hclust(d.scaled.euc, method = "average")
plot(clus2)
rect.hclust(clus2, k=6, border="yellow")
clus2$height

## profiling the clusters
tmp_df <- NULL
tmp_df<-numeric.cluster_df
tmp_df$Clusters <- cutree(clus2, k=6)
names(tmp_df)
aggr = aggregate(tmp_df[-c(1,14)],list(tmp_df$Clusters),mean)
clus.profile1 <- data.frame( Cluster=aggr[,1],
                            Freq=as.vector(table(tmp_df$Clusters)),
                            aggr[,-1])

View(clus.profile1)


# K Means Clustering
## Identifying the optimal number of clusters from WSS
ktmp_df <- NULL
ktmp_df<-numeric.cluster_df
ktmp.scaled.df<- scale(ktmp_df[-1])

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(ktmp.scaled.df, nc=15)

## Identifying the optimal number of clusters
library(NbClust)
set.seed(1234)
nc <- NULL
nc <- NbClust(ktmp.scaled.df, min.nc=4, max.nc=10, method="kmeans")
table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen ")


?kmeans
kmeans.clus = kmeans(x=ktmp.scaled.df, centers = 4, nstart = 200)
library(fpc)
library(cluster)
plotcluster(ktmp.scaled.df, kmeans.clus$cluster)
?plotcluster

clusplot(ktmp.scaled.df, kmeans.clus$cluster, 
         color=TRUE, shade=TRUE, labels=5, lines=1,
         plotchar = FALSE, cex = 0, cex.txt=0)
?clusplot
ktmp_df$Clusters <- kmeans.clus$cluster
names(ktmp_df)
## profiling the clusters
kaggr = aggregate(ktmp_df[,-c(1,14)],list(ktmp_df$Clusters),mean)
clus.profile2 <- data.frame( Cluster=kaggr[,1],
                            Freq=as.vector(table(ktmp_df$Clusters)),
                            kaggr[,-1])

View(clus.profile2)
head(clus.profile2)
fclusterd_df <- cbind.data.frame(ktmp_df,factor.cluster_df)
names(fclusterd_df)

#######################################################################################
### End of Code
#######################################################################################





