#########################This is the server FILE#######@#########################


#######create a Model#####
CreateModel <- function(depVar, depMeasureName, depvar_id, indepVarList, indepMeasureNameList, indepvar_id, dSLoc, modelType, modelTypeID, modelName){
  ##step 1 : read the training Dataset
  trainingDS <- read.csv(dSLoc, header = TRUE, sep = ',')
  
  depMeasureName <- gsub(" ", "_", depMeasureName)
  indepMeasureNameList <- gsub(" ", "_", indepMeasureNameList)
  
  depVar <- gsub(" ", "", depVar)
  indepVarList <- gsub(" ", "", indepVarList)
  
  ## Step 2: Assign the corresponding values to each measure
  depMeasure <- trainingDS[1:1000,which(colnames(trainingDS)==depMeasureName)]
  
  indepMeasurelist <- NULL
  indepMeasureList <- as.data.frame(sapply(1:length(indepMeasureNameList),function(i){
    indepMeasurelist <- data.frame(trainingDS[1:1000, which(colnames(trainingDS)%in%indepMeasureNameList[i])])
  }))
  
  NewData <- cbind(depMeasure,indepMeasureList)
  po <- c()
  for(i in 1:length(indepMeasureList)){
    assign(paste0("indepMeasure",i), indepMeasureList[[i]])
    po <- c(po, paste0("indepMeasure",i))
  }
  
  colnames(NewData)[-1] <- po
  
  ## step 5: Run the model 
  switch(modelType, 
         "Regression"= LinearModelP2(depVar, depvar_id, indepVarList, indepvar_id, modelName, modelTypeID, NewData, dSLoc, depMeasureName, indepMeasureNameList),
         "Neural Networks"= NNModels(depVar, depvar_id, indepVarList, indepvar_id, modelName, modelTypeID, NewData, dSLoc, depMeasureName, indepMeasureNameList),
         "Ridge Regression"= RidgeRegressionModel(depVar, depvar_id, indepVarList, indepvar_id, modelName, modelTypeID, NewData, dSLoc, depMeasureName, indepMeasureNameList),
         "Robust Regression Model"=RobustRegressionModel(depVar, depvar_id, indepVarList, indepvar_id, modelName, modelTypeID, NewData, dSLoc, depMeasureName, indepMeasureNameList),
         "Naive Bayes Model"=Naive_Bayes_Model(depVar, depvar_id, indepVarList, indepvar_id, modelName, modelTypeID, NewData, dSLoc, depMeasureName, indepMeasureNameList),
         "K Nearest Neighbour Model"=KNearestNeighborModel(depVar, depvar_id, indepVarList, indepvar_id, modelName, modelTypeID, NewData, dSLoc, depMeasureName, indepMeasureNameList),
         "Random Forest Model"=RandomForestModel(depVar, depvar_id, indepVarList, indepvar_id, modelName, modelTypeID, NewData, dSLoc, depMeasureName, indepMeasureNameList),
         "Lasso and Lars Regression Model"=LassoandLarsRegressionModel(depVar, depvar_id, indepVarList, indepvar_id, modelName, modelTypeID, NewData, dSLoc, depMeasureName, indepMeasureNameList),
         "Var Model" = RegressionPolynomial(depVar, depvar_id, indepVarList, indepvar_id, modelName, modelTypeID, NewData, dSLoc, depMeasureName, indepMeasureNameList)
         )
}


##################LINEAR MODEL#################################################################################################################
###############################################################################################################################################
LinearModelP2 <- function(depVar, depvar_id, indepVarList, indepvar_id, modelName, modelTypeID, NewData, dSLoc, depMeasureName, indepMeasureNameList) {
  
  mod=lm(depMeasure~. , data = NewData)
  
  label <- paste0(modelName,"LinearModel_",depVar,"_",indepVarList[1])
  filePath <- paste("/home/house_price_data/r_models/",label,".rds",sep="")
  
  mod <- list(mod, dSLoc, depMeasureName, indepMeasureNameList)
  
  saveRDS(mod, 
          file = filePath)
  Sys.chmod(filePath ,mode="777")
  
  model_id <- updateModelID()
  saveModelIntoOntology(model_id, modelTypeID, indepvar_id, depvar_id, label)
}

##################NEURAL NET MODEL#############################################################################################################
###############################################################################################################################################
NNModels <- function(depVar, depvar_id, indepVarList, indepvar_id, modelName, modelTypeID, NewData, dSLoc, depMeasureName, indepMeasureNameList) {

  Newtab=NewData[complete.cases(NewData), ]
  
  maxs <- apply(Newtab, 2, max)
  mins <- apply(Newtab, 2, min)
  scaled <- as.data.frame(scale(Newtab, center = mins, scale = maxs - mins))  #normalization
  
  pdf(file = NULL)
  nn <- neuralnet(depMeasure~. ,data=scaled,hidden=c(10,4),linear.output=T, if(names(dev.cur()) != "null device") dev.off()) #train NN with normalized data
  
  #pr.nn <- compute(nn,scaled) #predict the result using training data, still normalized
  #pr.nn_ <- pr.nn$net.result*(max(depMeasure)-min(depMeasure))+min(depMeasure) #result denormalization
  nn <- list(nn, dSLoc, depMeasureName, indepMeasureNameList)
  
  label <- paste0(modelName,"NNModels_",depVar,"_",indepVarList[1])
  filePath <- paste("/home/house_price_data/r_models/",label,".rds",sep="")
  pdf(NULL)
  saveRDS(nn, 
          file = filePath)
  
  Sys.chmod(filePath ,mode="777")
  
  model_id <- updateModelID()
  saveModelIntoOntology(model_id, modelTypeID, indepvar_id, depvar_id, label)
}

#########Polynomial REGRESSION MODEL################################################################################################################
####################################
RegressionPolynomial <- function(depVar, depvar_id, indepVarList, indepvar_id, modelName, modelTypeID, NewData, dSLoc, depMeasureName, indepMeasureNameList) {
  inputData=NewData[complete.cases(NewData), ]
  
  trainingIndex <- sample(1:nrow(inputData), 0.8*nrow(inputData)) # indices for 80% training data
  trainingData <- inputData[trainingIndex, ] # training data
  testData <- inputData[-trainingIndex, ] # test data
  
  if (ncol(trainingData) == 2){
    mReg<-lm(trainingData$depMeasure ~ poly(indepMeasure1, 3, raw=TRUE),trainingData)
  }else if(ncol(trainingData) == 3){
    mReg<-lm(depMeasure ~ poly(indepMeasure1, 3, raw=TRUE) + poly(indepMeasure2, 3, raw=TRUE),trainingData)
  }else if(ncol(trainingData) == 4){
    mReg<-lm(depMeasure ~ poly(indepMeasure1, 3, raw=TRUE) + poly(indepMeasure2, 3, raw=TRUE)+ poly(indepMeasure3, 3, raw=TRUE),trainingData)
  }else if(ncol(trainingData) == 5){
    mReg<-lm(depMeasure ~ poly(indepMeasure1, 3, raw=TRUE) + poly(indepMeasure2, 3, raw=TRUE)+ poly(indepMeasure3, 3, raw=TRUE)+ poly(indepMeasure4, 3, raw=TRUE),trainingData)
  }
  PolRegModel <- list(mReg, dSLoc, depMeasureName, indepMeasureNameList)
  
  label <- paste0(modelName,"RegressionPolynomial",depVar,"_",indepVarList[1],'.rds')
  filePath <- paste("/home/house_price_data/r_models/",label,".rds",sep="")
  
  saveRDS(PolRegModel, file =filePath)
  
  Sys.chmod(filePath ,mode="777")
  
  model_id <- updateModelID()
  saveModelIntoOntology(model_id, modelTypeID, indepvar_id, depvar_id, label)
}
#########RIDGE REGRESSION MODEL################################################################################################################
###############################################################################################################################################
RidgeRegressionModel <- function(depVar, depvar_id, indepVarList, indepvar_id, modelName, modelTypeID, NewData, dSLoc, depMeasureName, indepMeasureNameList) {
  library(lmridge)
  
  inputData=NewData[complete.cases(NewData), ]
  
  set.seed(100) # set seed to replicate results
  trainingIndex <- sample(1:nrow(inputData), 0.8*nrow(inputData)) # indices for 80% training data
  trainingData <- inputData[trainingIndex, ] # training data
  testData <- inputData[-trainingIndex, ] # test data

  linRidgeMod <- lmridge(depMeasure ~ ., data = trainingData)# the ridge regression model
  
  linRidgeMod <- list(linRidgeMod, dSLoc, depMeasureName, indepMeasureNameList)

  label <- paste0(modelName,"RidgeReg_",depVar,"_",indepVarList[1])
  filePath <- paste("/home/house_price_data/r_models/",label,".rds",sep="")
  saveRDS(linRidgeMod, file =filePath)
  
  Sys.chmod(filePath ,mode="777")
  
  model_id <- updateModelID()
  saveModelIntoOntology(model_id, modelTypeID, indepvar_id, depvar_id, label)
  
} 

########## Robust Regression ##################################################################################################################
###############################################################################################################################################
RobustRegressionModel <- function(depVar, depvar_id, indepVarList, indepvar_id, modelName, modelTypeID, NewData, dSLoc, depMeasureName, indepMeasureNameList) {
  library(MASS)
  
  fit <- rlm(depMeasure~ . , data = NewData)
  
  fit <- list(fit, dSLoc, depMeasureName, indepMeasureNameList)
  
  label <- paste0(modelName,"RobustRegression",depVar,"_",indepVarList[1])
  filePath <- paste("/home/house_price_data/r_models/",label,".rds",sep="")
  
  saveRDS(fit, file =filePath)
  
  Sys.chmod(filePath ,mode="777")
  
  model_id <- updateModelID()
  saveModelIntoOntology(model_id, modelTypeID, indepvar_id, depvar_id, label)
}  


Naive_Bayes_Model <- function(depVar, depvar_id, indepVarList, indepvar_id, modelName, NewData) {
  library(e1071)
  
  naivebayesmod <- naiveBayes(depMeasure~ . , data = NewData)
  naivebayesmod <- c(naivebayesmod, dSLoc, depMeasureName, indepMeasureNameList)
  
  label <- paste0(modelName,"Naive_Bayes_Model-",depVar,"_",indepVarList[1])
  filePath <- paste("/home/house_price_data/r_models/",label,".rds",sep="")
  
  saveRDS(naivebayesmod, file =filePath)
  
  Sys.chmod(filePath ,mode="777")
  
  model_id <- updateModelID()
  saveModelIntoOntology(model_id, modelTypeID, indepvar_id, depvar_id, label)
}  


KNearestNeighborModel <- function(depVar, depvar_id, indepVarList, indepvar_id, modelName, modelTypeID, NewData, dSLoc, depMeasureName, indepMeasureNameList) {
  library(class)
  library(DMwR)
  
  inputData=NewData[complete.cases(NewData), ]

  trainingIndex <- sample(1:nrow(inputData), 0.8*nrow(inputData)) # indices for 80% training data
  trainingData <- inputData[trainingIndex, ] # training data
  testData <- inputData[-trainingIndex, ] # test data
  
  knn <- NULL
  ##### Loop to run all the k-Nearest Neighbour Classification 
  knn <- sapply(3:10, function(i){##this function create a new df with all the indep Measures
    knn <- kNN(depMeasure~.,testData,trainingData,norm=TRUE,k=i) #######ASK QUENTIN FOR ORDER OF TRAININGDATA VS TESTDATA
    cbind(as.data.frame(knn, stringsAsFactors = F))
  })
  
  knn =as.data.frame(knn)
  knn =cbind(knn,trainingData$depMeasure)
  
  label <- paste0(modelName,"KNearestNeighborModel",depVar,"_",indepVarList[1])
  filePath <- paste("/home/house_price_data/r_models/",label,".rds",sep="")
  
  knn <- list(knn, dSLoc, depMeasureName, indepMeasureNameList)
  
  saveRDS(knn, file =filePath)
  
  Sys.chmod(filePath ,mode="777")
  
  model_id <- updateModelID()
  saveModelIntoOntology(model_id, modelTypeID, indepvar_id, depvar_id, label)
}


RandomForestModel <- function(depVar, depvar_id, indepVarList, indepvar_id, modelName, modelTypeID, NewData, dSLoc, depMeasureName, indepMeasureNameList) {
  library(randomForest)
  
  inputData=NewData[complete.cases(NewData), ]

  trainingIndex <- sample(1:nrow(inputData), 0.8*nrow(inputData)) # indices for 80% training data
  trainingData <- inputData[trainingIndex, ] # training data
  testData <- inputData[-trainingIndex, ] # test data
  
  oob.err=double(13)
  test.err=double(13)
  
  #mtry is no of Variables randomly chosen at each split
  for(mtry in 1:13) 
  {
    rf=randomForest(depMeasure~. , data = inputData , subset = trainingIndex,mtry=mtry,ntree=400) 
    oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
    
    pred<-predict(rf,inputData[-trainingIndex,]) #Predictions on Test Set for each Tree
    test.err[mtry]= with(inputData[-trainingIndex,], mean( (depMeasure - pred)^2)) #Mean Squared Test Error
    
    cat(mtry," ") #printing the output to the console
    
  }
  results=cbind(oob.err,test.err,mtry)
  results=as.data.frame(results)
  
  label <- paste0(modelName,"RandomForestModel",depVar,"_",indepVarList[1])
  filePath <- paste("/home/house_price_data/r_models/",label,".rds",sep="")
  
  results <- list(results, dSLoc, depMeasureName, indepMeasureNameList)
  
  saveRDS(results, file =filePath)
  
  Sys.chmod(filePath ,mode="777")
  
  model_id <- updateModelID()
  saveModelIntoOntology(model_id, modelTypeID, indepvar_id, depvar_id, label)
}  


LassoandLarsRegressionModel <- function(depVar, depvar_id, indepVarList, indepvar_id, modelName, NewData) {
  library(lars)
  # LAR on data data using lars package 
  # Splitting the data in half and modeling each half separately.
  # Splitting data in half using random uniform selection to make two "set"s.
  
  inputData=NewData[complete.cases(NewData), ]

  set.seed(nrow(inputData)) 
  par(mfrow=c(3,3))
  
  inputData$set <- ifelse(runif(n=nrow(inputData))>0.5, yes=2, no=1)

  # lars() requires x to be in matrix class, so saving out 
  #   the separate variables to be used as Y and X.
  
  y.1 <- inputData[which(inputData$set==1),1]
  x.1 <- as.matrix(inputData[which(inputData$set==1),c(2:length(inputData))])
  y.2 <- inputData[which(inputData$set==2),1]
  x.2 <- as.matrix(inputData[which(inputData$set==2),c(2:length(inputData))])
  
  ####### Lasso Model
  
  
  # First half of data 
  lasso.1 <- lars(y=y.1, x=x.1, type="lasso", trace= TRUE)
  summary(lasso.1)
  plot(lasso.1) # Plots coefficient path
  coef(lasso.1) # Lists out coefficients for each step in path
  
  ## Find the optimal position for the model
  # cv.lars() uses crossvalidation to estimate optimal position in path
  
  cv.lasso.1 <- cv.lars(y=y.1, x= x.1, type="lasso")
  cv.lasso.1
  # Use the "+1SE rule" to find best model: 
  #    Take the min CV and add its SE ("limit").  
  #    Find smallest model that has its own CV within this limit (at "s.cv.1")
  limit <- min(cv.lasso.1$cv)
  s.cv.1 <- cv.lasso.1$index[min(which(cv.lasso.1$cv == limit))]
  s.cv.1
  
  
  # Print out coefficients at optimal s.
  coef(lasso.1, s=s.cv.1, mode="fraction")
  
  # Predict both halves using first-half fit
  predict.1.1=predict.lars(lasso.1, newx=x.1,s=s.cv.1, mode="fraction")
  predict.1.2=predict.lars(lasso.1, newx=x.2,s=s.cv.1, mode="fraction")
  
  #Print the predict solution
  mat=cbind(y.1,predict.1.1$fit)
  matplot(mat,type='l',col=c("black","red"),main="In Sample Plot",ylab="HAM2")
  legend("topright",legend=c("Actual","Predicted"),fill=c("black","red"),bty='n')
  grid()
  
  mat=cbind(y.2,predict.1.2$fit)
  matplot(mat,type='l',col=c("black","red"),main="In Sample Plot",ylab="HAM2")
  legend("topright",legend=c("Actual","Predicted"),fill=c("black","red"),bty='n')
  grid()
  
  
  ####### LARS Model
  
  # First half of data 
  lar.1 <- lars(y=y.1, x= x.1, type="lar", trace=TRUE)
  summary(lar.1)
  plot(lar.1) # Plots coefficient path
  coef(lar.1) # Lists out coefficients for each step in path
  
  
  
  ## Find the optimal number of steps for the model
  a <- summary(lar.1)
  a
  # Print out coefficients at optimal s
  coef(lar.1, s=which.min(a$Cp), mode="step") 
  
  
  
  predict.2.1 <- predict(lar.1, newx=x.1, mode="fraction")
  mat=cbind(y.1,predict.1.1$fit)
  matplot(mat,type='l',col=c("black","red"),main="In Sample Plot",ylab="HAM2")
  legend("topright",legend=c("Actual","Predicted"),fill=c("black","red"),bty='n')
  grid()
  
  ##Clear all the variables from the current environment and close all the plots.
  ##rm(list = ls())
  ##graphics.off()
  #saveRDS(nn5, file = paste("Modelz/KNearestNeighborModel",depVar,"_",indepVarList[1],"_",indepVarList[2],".rds",sep="")  )
  
  
  
  
}


