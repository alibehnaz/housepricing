


depVar <- "randomDepVarName"
depMeasureName="Sold_Private_Treaty_AverageDiscount"
depvar_id <- "OOO"

indepVarList <- c("ExchangeRate", "Population")
indepMeasureNameList <- c("Sold_Private_Treaty_DaysOnMarketRecordCount","For_Sale_Both_Auction_Private_Treaty_DetailedPosition05Price")      #,indevarname3,indevarname4)
indepvar_id <- "000"

dSLoc <- 'randwick-2008-2017.csv'
trainingDS <- read.csv(dSLoc, header = TRUE, sep = ',')

depMeasureName <- gsub(" ", "_", depMeasureName)
indepMeasureNameList <- gsub(" ", "_", indepMeasureNameList)

depVar <- gsub(" ", "", depVar)
indepVarList <- gsub(" ", "", indepVarList)

## Step 2: Assign the corresponding values to each measure
depMeasure = trainingDS[1:100,which(colnames(trainingDS)==depMeasureName)]

indepMeasureList=data.frame(trainingDS[1:100,which(colnames(trainingDS)==indepMeasureNameList[1])])
for (i in 2:length(indepMeasureNameList)) {
  tmp= trainingDS[1:100,which(colnames(trainingDS)==indepMeasureNameList[i])]
  indepMeasureList=cbind(indepMeasureList,tmp)
}
indepMeasureList=as.data.frame(indepMeasureList)
modelName <- "Gilles"




Naive_Bayes_Model <- function(depVar,depMeasure,indepVarList,indepMeasureList) {
  
  library(e1071)
  
  Newtab <- sapply(1:length(indepMeasureList), function(i){##this function create a new df with all the indep Measures
    cbind(indepMeasureList[[i]])
  })
  
  po <- c()
  for(i in 1:length(indepMeasureList)){
    assign(paste0("indepMeasure",i), indepMeasureList[[i]])
    po <- c(po, paste0("indepMeasure",i))
  }
  
  colnames(Newtab) <- po
  
  inputData <- cbind(depMeasure,Newtab)
  
  
  inputData=inputData[complete.cases(inputData), ]
  inputData <- as.data.frame(inputData)
  
  trainingIndex <- sample(1:nrow(inputData), 0.8*nrow(inputData)) # indices for 80% training data
  trainingData <- inputData[trainingIndex, ] # training data
  testData <- inputData[-trainingIndex, ] # test data
  
  f <- as.formula(paste0("depMeasure~", paste(po, collapse="+")))
  
  
  naivebayesmod <- naiveBayes(f, data = trainingData)
  search_grid <- expand.grid(
    usekernel = c(TRUE, FALSE),
    fL = 0:5,
    adjust = seq(0, 5, by = 1)
  )
  
  nb.m2 <- train(
    x = trainingData[,2:3],
    y = trainingData$depMeasure,
    method = "nb",
    trControl = train_control,
    tuneGrid = search_grid,
    preProc = c("BoxCox", "center", "scale", "pca")
  )
  
  
  library("caret", lib.loc="~/R/win-library/3.5")
  ##saveRDS(naivebayesmod, file = paste("Naive_Bayes_Model",depVar,"_",indepVarList[[1]],"_",indepVarList[[2]],".rds",sep="")  )
  return(naivebayesmod)
}  
