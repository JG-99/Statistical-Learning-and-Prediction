library(kableExtra)
library(rpart)
library(rpart.plot)
library(glmnet)

project<-read.csv("Data2021_final.csv")
project_test<-read.csv("Data2021test_final_noY.csv")
head(project)
View(project)

summary(project)
pairs(project)
pairs(project[1:8])

lmdl<-lm(Y~X1^2,data=project)
summary(lmdl)
plot(Y~X1*X2,data=project)

get.MSPE = function(Y, Y.hat){
  return(mean((Y - Y.hat)^2))
}


## Random Forest
rf.project = randomForest(Y~.,data=project,importance=T)
plot(rf.project)
importance(rf.project)
varImpPlot(rf.project)
OOB=predict(rf.project)
get.MSPE(project$Y,OOB)


## Boosting
fit.gbm.1 = gbm(Y ~ ., data = project, distribution = "gaussian", n.trees = 5000,
                interaction.depth = 1, shrinkage = 0.001, bag.fraction = 0.8)
gbm.perf(fit.gbm.1, oobag.curve = T)
n.trees.gbm = gbm.perf(fit.gbm.1, plot.it=F)
n.trees.RoT = 2 * n.trees.gbm


K = 10 #Number of folds
set.seed(2928893)

### Container for CV MSPEs
all.models = c("Least Squares","LASSO-Min", "LASSO-1se","Random Forest","RF Less","Boosting","Boosting Less")
CV.MSPEs = array(0, dim = c(length(all.models), K))
rownames(CV.MSPEs) = all.models
colnames(CV.MSPEs) = 1:K
### Get CV fold labels
n = nrow(project)
folds = get.folds(n, K)

for (i in 1:K) {
  ### Get training and validation sets
  data.train = project[folds != i, ]
  data.valid = project[folds == i, ]
  Y.train = data.train$Y
  Y.valid = data.valid$Y
  ### We need the data matrix to have an intercept for ridge, and to not have an intercept for LASSO. Best to just construct both.
  mat.train.int = model.matrix(Y ~ ., data = data.train)
  mat.train = mat.train.int[,-1]
  mat.valid.int = model.matrix(Y ~ ., data = data.valid)
  mat.valid = mat.valid.int[,-1]
  
  
  ### Fit model
  fit.LS = randomForest(Y~X1+X2+X4+X5+X6+X8+X9+X10+X12+X13+X14+X15,data=data.train,importance=F,mtry=4,nodesize=8)
  fit.LASSO = gbm(Y ~ X1+X2+X4+X5+X6+X8+X9+X10+X12+X13+X14+X15, data = data.train, distribution = "gaussian", n.trees = 10000,
                  interaction.depth = 4, shrinkage = 0.001, bag.fraction = 0.8)
  fit.RF = randomForest(Y~.,data=data.train,importance=F,mtry=4,nodesize=8)
  fit.RF.less = randomForest(Y~X1+X2+X4+X10+X12+X13+X14+X15,data=data.train,importance=F,mtry=4,nodesize=8)
  fit.gbm = gbm(Y ~ ., data = data.train, distribution = "gaussian", n.trees = 10000,
                  interaction.depth = 4, shrinkage = 0.001, bag.fraction = 0.8)
  fit.gbm.less = gbm(Y ~ X1+X2+X4+X10+X12+X13+X14+X15, data = data.train, distribution = "gaussian", n.trees = 10000,
                interaction.depth = 4, shrinkage = 0.001, bag.fraction = 0.8)
  
  ### Get optimal lambda values
  lambda.min = fit.LASSO$lambda.min
  lambda.1se = fit.LASSO$lambda.1se
  
  ### RoT's rule for choosing number of trees for Boosting
  n.trees.gbm = gbm.perf(fit.gbm, plot.it=F)
  n.trees.RoT = 2 * n.trees.gbm
  
  
  ### Get predictions
  pred.ls = predict(fit.LS,data.valid)
  #pred.min = predict(fit.LASSO, mat.valid, lambda.min)
  pred.min = predict(fit.LASSO, data.valid)
 # pred.1se = predict(fit.LASSO, mat.valid, lambda.1se)
  pred.rf = predict(fit.RF,data.valid)
  pred.rf.less = predict(fit.RF.less,data.valid)
  pred.gbm = predict(fit.gbm, data.valid, n.trees.RoT)
  pred.gbm.less = predict(fit.gbm.less,data.valid,n.trees.RoT)
  
  ### Get and store MSPEs
  MSPE.ls = get.MSPE(Y.valid,pred.ls)
  MSPE.min = get.MSPE(Y.valid, pred.min)
  #MSPE.1se = get.MSPE(Y.valid, pred.1se)
  MSPE.rf = get.MSPE(Y.valid,pred.rf)
  MSPE.rf.less = get.MSPE(Y.valid,pred.rf.less)
  MSPE.gbm = get.MSPE(Y.valid, pred.gbm)
  MSPE.gbm.less = get.MSPE(Y.valid,pred.gbm.less)
  CV.MSPEs["Least Squares",i] = MSPE.ls
  CV.MSPEs["LASSO-Min", i] = MSPE.min
  CV.MSPEs["LASSO-1se", i] = MSPE.1se
  CV.MSPEs["Random Forest",i] = MSPE.rf
  CV.MSPEs["RF Less",i] = MSPE.rf.less
  CV.MSPEs["Boosting",i] = MSPE.gbm
  CV.MSPEs["Boosting Less",i] = MSPE.gbm.less
}
### Get full-data MSPEs
full.MSPEs = apply(CV.MSPEs, 1, mean)
plot.MSPEs = t(CV.MSPEs)
boxplot(plot.MSPEs,main='MSPE Boxplots')

plot.RMSPEs = apply(CV.MSPEs, 2, function(W){
  best = min(W)
  return(W/best)
})
plot.RMSPEs = t(plot.RMSPEs)
### RMSPE Boxplot
boxplot(plot.RMSPEs,main="RMSPE Boxplots")


fit.final = gbm(Y ~ X1+X2+X4+X5+X6+X8+X9+X10+X11+X12+X13+X14+X15, data = project, distribution = "gaussian", n.trees = 10000,
                   interaction.depth = 4, shrinkage = 0.001, bag.fraction = 0.8)

n.trees.gbm = gbm.perf(fit.final, plot.it=F)
n.trees.RoT = 2 * n.trees.gbm
pred.gbm.less = predict(fit.gbm.less,project_test,n.trees.RoT)
write.table(pred.gbm.less,file='predictions.csv' ,sep = ",", row.names = F, col.names = F)
