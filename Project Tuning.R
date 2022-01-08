prb.1_.001 <- gbm(data=AQ, Ozone~ Solar.r + Wind + Temp, distribution="gaussian", 
                  n.trees=10000, interaction.depth=1, shrinkage=0.001)
fit.gbm.1 = gbm(Ozone~Solar.R+Wind+Temp, data = AQ, distribution = "gaussian", n.trees = 100,
                interaction.depth = 1)

max.trees = 10000
all.shrink = c(0.001, 0.005, 0.025, 0.125,0.5)
all.depth = c(1, 2,3,4, 6)
all.pars = expand.grid(shrink = all.shrink, depth = all.depth)
n.pars = nrow(all.pars)

### Number of folds
K = 5

### Create folds
n=nrow(project)
### Create container for CV MSPEs
CV.MSPEsboost = array(0, dim = c(K, n.pars))
folds = get.folds(n, K)
  
for(i in 1:K){
    ### Print progress update
    print(paste0(i, " of ", K))
    
    ### Split data
    data.train = project[folds != i,]
    data.valid = project[folds == i,]
    Y.valid = data.valid$Y
    
    
    
    ### Fit boosting models for each parameter combination
    for(j in 1:n.pars){
      ### Get current parameter values
      this.shrink = all.pars[j,"shrink"]
      this.depth = all.pars[j,"depth"]
      
      ### Fit model using current parameter values.
      fit.gbm = gbm(Y~., data = data.train, distribution = "gaussian", 
                    n.trees = max.trees, interaction.depth = this.depth, shrinkage = this.shrink)
      
      ### Choose how many trees to keep using RoT's rule. This will print many
      ### warnings about not just using the number of trees recommended by
      ### gbm.perf(). We have already addressed this problem though, so we can
      ### just ignore the warnings.
      n.trees = gbm.perf(fit.gbm, plot.it = F) * 2
      
      ### Check to make sure that RoT's rule doesn't tell us to use more than 1000
      ### trees. If it does, add extra trees as necessary
      if(n.trees > max.trees){
        extra.trees = n.trees - max.trees
        fit.gbm = gbm.more(fit.gbm, extra.trees)
      }
      ### Get predictions and MSPE, then store MSPE
      pred.gbm = predict(fit.gbm, data.valid, n.trees)
      MSPE.gbm = get.MSPE(Y.valid, pred.gbm)
      
      
      CV.MSPEsboost[i, j] = MSPE.gbm # Be careful with indices for CV.MSPEs
      

      
    }
}


names.pars = paste0(all.pars$shrink,"-",
                    all.pars$depth)
colnames(CV.MSPEsboost) = names.pars


### Make boxplot
boxplot(CV.MSPEsboost, las = 2, main = "Root MSPE Boxplot")


### Get relative MSPEs and make boxplot
CV.RMSPsboost = apply(CV.MSPEsboost, 1, function(W) W/min(W))
CV.RMSPEsboost = t(CV.RMSPEs)
boxplot(CV.RMSPEsboost, las = 2, main = "RMSPE Boxplot")



### Tuning Random Forest:

all.mtry = 1:4
all.nodesize = c(2, 5, 8)
all.pars = expand.grid(mtry = all.mtry, nodesize = all.nodesize)
n.pars = nrow(all.pars)
### Number of times to replicate process. OOB errors are based on bootstrapping,
### so they are random and we should repeat multiple runs
M = 5

### Create container for OOB MSPEs
OOB.MSPEs = array(0, dim = c(M, n.pars))

for(i in 1:n.pars){
  ### Print progress update
  print(paste0(i, " of ", n.pars))
  
  ### Get current parameter values
  this.mtry = all.pars[i,"mtry"]
  this.nodesize = all.pars[i,"nodesize"]
  
  ### Fit random forest models for each parameter combination
  ### A second for loop will make our life easier here
  for(j in 1:M){
    ### Fit model using current parameter values. We don't need variable
    ### importance measures here and getting them takes time, so set
    ### importance to F
    fit.rf = randomForest(Y~ ., data = project, importance = F,
                          mtry = this.mtry, nodesize = this.nodesize)
    
    ### Get OOB predictions and MSPE, then store MSPE
    OOB.pred = predict(fit.rf)
    OOB.MSPE = get.MSPE(project$Y, OOB.pred)
    
    OOB.MSPEs[j, i] = OOB.MSPE # Be careful with indices for OOB.MSPEs
  }
}


### We can now make an MSPE boxplot. First, add column names to indicate
### which parameter combination was used. Format is mtry-nodesize
names.pars = paste0(all.pars$mtry,"-",
                    all.pars$nodesize)
colnames(OOB.MSPEs) = names.pars

### Make boxplot
boxplot(OOB.MSPEs, las = 2, main = "MSPE Boxplot")

### Get relative MSPEs and make boxplot
OOB.RMSPEs = apply(OOB.MSPEs, 1, function(W) W/min(W))
OOB.RMSPEs = t(OOB.RMSPEs)
boxplot(OOB.RMSPEs, las = 2, main = "RMSPE Boxplot")

### Zoom in on the competitive models
boxplot(OOB.RMSPEs, las = 2, main = "RMSPE Boxplot", ylim = c(1, 1.02))

fit.rf.2 = randomForest(Y ~ ., data = project, importance = T,
                        mtry = 4, nodesize = 8)

rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}

M = 20 # Number of times to re-fit each model

### Define parameter values and use expand.grid() to get all combinations
all.n.hidden = c(1, 5, 9)
all.shrink = c(0, 1, 5, 10)
all.pars = expand.grid(n.hidden = all.n.hidden,
                       shrink = all.shrink)
n.pars = nrow(all.pars) # Number of parameter combinations

K = 5 # Number of folds

### Create folds
folds = get.folds(n, K)

### Create container for MSPEs
CV.MSPEsNN = array(0, dim = c(K, n.pars))


for(i in 1:K){
  ### Print progress update
  print(paste0(i, " of ", K))
  
  ### Split data and rescale predictors
  data.train = project[folds != i,]
  X.train.raw = project[,-5]
  X.train = rescale(X.train.raw, X.train.raw)
  Y.train = data.train[,5]
  
  data.valid = project[folds == i,]
  X.valid.raw = data.valid[,-5]
  X.valid = rescale(X.valid.raw, X.train.raw)
  Y.valid = data.valid[,5]
  
  
  ### Fit neural net models for each parameter combination. A second 
  ### for loop will make our life easier here
  for(j in 1:n.pars){
    ### Get current parameter values
    this.n.hidden = all.pars[j,1]
    this.shrink = all.pars[j,2]
    
    ### We need to run nnet multiple times to avoid bad local minima. Create
    ### containers to store the models and their errors.
    all.nnets = list(1:M)
    all.SSEs = rep(0, times = M)
    
    ### We need to fit each model multiple times. This calls for another
    ### for loop.
    for(l in 1:M){
      ### Fit model
      fit.nnet = nnet(X.train, Y.train, linout = TRUE, size = this.n.hidden,
                      decay = this.shrink, maxit = 500, trace = FALSE)
      
      ### Get model SSE
      SSE.nnet = fit.nnet$value
      
      ### Store model and its SSE
      all.nnets[[l]] = fit.nnet
      all.SSEs[l] = SSE.nnet
    }
    
    ### Get best fit using current parameter values
    ind.best = which.min(all.SSEs)
    fit.nnet.best = all.nnets[[ind.best]]
    
    ### Get predictions and MSPE, then store MSPE
    pred.nnet = predict(fit.nnet.best, X.valid)
    MSPE.nnet = get.MSPE(Y.valid, pred.nnet)
    
    CV.MSPEsNN[i, j] = MSPE.nnet # Be careful with indices for CV.MSPEs
  }
}


### We can now make an MSPE boxplot. It would be nice to have more 
### informative names though. We can construct names from all.pars
### using the paste0() function.
names.pars = paste0(all.pars$n.hidden,",",
                    all.pars$shrink)
colnames(CV.MSPEs) = names.pars

### Make boxplot
boxplot(CV.MSPEs, las = 2, main = "MSPE Boxplot")


### Get relative MSPEs and make boxplot
CV.RMSPEs = apply(CV.MSPEs, 1, function(W) W/min(W))
CV.RMSPEs = t(CV.RMSPEs)
boxplot(CV.RMSPEs, las = 2, main = "RMSPE Boxplot")
