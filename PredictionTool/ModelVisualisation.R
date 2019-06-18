library(curl)
library(MASS)
library("hydroGOF")#, lib.loc="~/R/win-library/3.5")
library(ggplot2)
library(jtools)
library(plotly)

#############load and Run prediction Model##########
LoadAndUseSavedModel2 <- function(UserindepVal, statModel){ ##better version (no hard coded)
  
  completeQuery <- paste0("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> PREFIX analytics: <http://www.adage.unsw.edu.au/analytics#> SELECT DISTINCT ?rCode WHERE{?model rdf:type analytics:StatisticalModel.?model rdfs:label '", statModel ,"'^^rdfs:Literal.?model analytics:rCode ?rCode.}")
  url <- URLencode(completeQuery, reserved = T)
  statModelCode <- GET(paste0("http://adage2.cse.unsw.edu.au:8005/v1/graphs/sparql?query=",url),
                       authenticate(user = "admin", password = "financeit", type = "basic"))
  
  if(length(content(statModelCode)$results$bindings) == 0){
    loadedModel <- "No Code available for this model yet"
    p <- "Nothing to show"
    forecast <- NULL
  }else{
    pathToModelCode <- content(statModelCode)$results$bindings[[1]]$rCode$value
    loadedModel <- readRDS(file = pathToModelCode )

    po <- c()
    for(i in 1:ncol(UserindepVal)){
      po <- c(po, paste0("indepMeasure",i))
    }
    po
    names(UserindepVal) <- po
    
    p <- predict(loadedModel[[1]], newdata = UserindepVal)
    if(length(loadedModel[[4]])>1){
      forecast <- NULL
    }else{
      forecast <- leForecast(UserindepVal, loadedModel)
    }
  }
  return(list(p, forecast))
}

ViewSummary <- function(statModel){
  completeQuery <- paste0("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> PREFIX analytics: <http://www.adage.unsw.edu.au/analytics#> SELECT DISTINCT ?rCode ?modelType WHERE{?model rdf:type analytics:StatisticalModel.?model rdfs:label '", statModel ,"'^^rdfs:Literal.?model analytics:rCode ?rCode.?model analytics:modelType ?modelTypeID.?modelTypeID rdfs:label ?modelType.}")
  url <- URLencode(completeQuery, reserved = T)
  statModelCode <- GET(paste0("http://adage2.cse.unsw.edu.au:8005/v1/graphs/sparql?query=",url),
                       authenticate(user = "admin", password = "financeit", type = "basic"))
  
  if(length(content(statModelCode)$results$bindings) == 0){
    return(NULL)
  }else{
    pathToModelCode <- content(statModelCode)$results$bindings[[1]]$rCode$value
    modelType <- content(statModelCode)$results$bindings[[1]]$modelType$value
    #pathToModelCode <- "/Applications/RStudio\ WorkSpace/PredictionToolLocal/Modelz/gilles3LinearModelexchangeRAte_MortgageRate.rds"
    #modelType <- "Regression"
    modelz <- readRDS(file = pathToModelCode)
    #modelz <- readRDS(ModelName)
    #paste0("/Applications/RStudio\ WorkSpace/PredictionToolLocal/",)
    trainingDS=read.csv(modelz[[2]][1], header = TRUE, sep = ",")
    
    depMeasure <- trainingDS[1:1000,which(colnames(trainingDS)==modelz[[3]][1])]
    
    indepMeasurelist <- NULL
    indepMeasureList <- as.data.frame(sapply(1:length(modelz[[4]]),function(i){
      indepMeasurelist <- data.frame(trainingDS[1:1000, which(colnames(trainingDS)%in%modelz[[4]][i])])
    }))
    
    NewData <- cbind(depMeasure,indepMeasureList)
    po <- c()
    for(i in 1:length(indepMeasureList)){
      assign(paste0("indepMeasure",i), indepMeasureList[[i]])
      po <- c(po, paste0("indepMeasure",i))
    }
    colnames(NewData)[-1] <- po
    #return("It's Okay")
    switch(modelType, 
           "Regression"= LinearModelSummary(NewData, modelz),
           "Var Model" = LinearModelSummary(NewData, modelz),
           "Neural Networks"= NNModelSummary(NewData, modelz),
           "Ridge Regression"= RidgeModelSummary(NewData, modelz),
           "K Nearest Neighbour Model"= KNearestNeighborModelSummary(NewData, modelz),
           "Random Forest Model" = RandomForestModelSummary(modelz)
    )
    
  }
}
LinearModelSummary <- function(NewData, modelz){
  if(length(modelz[[4]])>=2){
    
    inputData=NewData[complete.cases(NewData), ]
    trainingIndex <- sample(1:nrow(inputData), 0.8*nrow(inputData)) # indices for 80% training data
    trainingData <- inputData[trainingIndex, ] # training data
    testData <- inputData[-trainingIndex, ] # test data
    
    distPred <- predict(modelz[[1]], testData)
    actuals_preds <- data.frame(cbind(actuals=testData$depMeasure, predicteds=distPred))  # make actuals_predicteds dataframe.
    
    
    # Calculate prediction accuracy and error rates
    # A simple correlation between the actuals and predicted values can be used as a form of accuracy measure. 
    # A higher correlation accuracy implies that the actuals and predicted values have similar directional movement, 
    # i.e. when the actuals values increase the predicteds also increase and vice-versa.
    correlation_accuracy <- cor(actuals_preds)
    mseReg <- mse(distPred,testData$depMeasure)
    
    ### Mean squared error
    
    summary(modelz[[1]])$r.squared
    summary(modelz[[1]])$adj.r.squared
    
    numbcoef <- length(modelz[[1]]$coef)
    ### Get the equation and the coefficients
    
    coef <- summary(modelz[[1]])$coefficients
    depVar= colnames(NewData[1])
    equation=getFormula(coef, depVar, modelz[[4]], numbcoef, model_type)
    
    ## Plot ResultFinal
    if(numbcoef>2){
      regPlot <- getRegressionPlot(numbcoef, modelz[[1]])
    }else{
      regPlot <- NULL
    }
    ResidualsPlots <- ResidualRegressionPlots(modelz[[1]])
    
    return(list("Regression",equation,coef,mseReg,correlation_accuracy[2],summary(modelz[[1]])$r.squared,summary(modelz[[1]])$adj.r.squared,ResidualsPlots,regPlot))
    
    }else{
      a <- modelz[[1]]$coef[[2]]
      b <- modelz[[1]]$coef[[1]][1]
      c <- summary(modelz[[1]])$coefficients[1:2, ]
    
      equationez <- paste(a,"*",modelz[[3]],"+",b,"=",modelz[[4]])
      varianceAnalysis <- anova(modelz[[1]])
      varMatrix <- vcov(modelz[[1]])
      
      computed_value <- predict(modelz[[1]], NewData)
      historical_value <- NewData$depMeasure
      regression <- lm(historical_value ~ computed_value ) 
      predicted <- predict(regression, newdata = NewData, interval = "confidence", level = 0.95)
      
      upper_band <- cbind(computed_value, predicted[, 2])
      upper_band <- upper_band[order(upper_band[, 1]),]
      
      lower_band <- cbind(computed_value, predicted[, 3])
      lower_band <- lower_band[order(lower_band[, 1]),]
      
      valuescomparisongraph <- plot_ly(NewData, x = computed_value, y = historical_value, mode = "markers", showlegend = FALSE) %>%
        add_trace(y = predicted[, 1], x = computed_value, name = "Regression Line", mode = "lines", line = list(color = "#00BFC4"), showlegend = TRUE) %>%
        add_trace(y = upper_band[, 2], x = upper_band[, 1], mode = "lines", line = list(color = "grey"), showlegend = TRUE, name = "Confidence Interval 95%") %>%
        add_trace(y = lower_band[, 2], x = lower_band[, 1], mode = "lines", line = list(color = "grey"), showlegend = FALSE, name = "Confidence Interval 95%") %>%
        plotly::layout(title = "Time Series comparison : Historical vs Computed", yaxis = list(zeroline = FALSE, showline = FALSE), 
               xaxis = list(title = "Computed values", zeroline = FALSE, showline = FALSE), margin = list(l = 70, r = 40, t = 70, b = 50, pad = 4))
      
      residualstuff <- ResidualRegressionPlots(modelz[[1]])
      #################################################    
      #in Case we have a column Dates in the dataset#
      #################################################
      #date=NewData$Dates
      #timesseriesgraph <- plot_ly(data_set, x = date, y = historical_value, name = "Historical", mode = "lines", line = list(color = "#00BFC4")) %>%
      # add_trace(y = computed_value, x = date, name = "Computed", mode = "lines", line = list(color = "#F77D74")) %>%
      #  plotly::layout(title = "Time Series Graph", yaxis = list(zeroline = FALSE, showline = FALSE), xaxis = list(title = "Date"), 
      #         showlegend = TRUE, margin = list(l = 70, r = 40, t = 70, b = 50, pad = 4))
      return(list("Regression", equationez,c, varianceAnalysis, varMatrix, valuescomparisongraph, residualstuff))
    }
  }



getFormula <- function(coef, depVar, po, numbcoef, model_type){
  
  if (numbcoef == 2){
    equation <- paste(depVar, "=", po, "*", "(", round(coef[2, 1], 5), ")", "+", coef[1, 1])
  } else {
    equation <- paste(depVar, "=")
    for (i in 1 : length(po)){
      equation <- paste(equation, "(",  round(coef[i+1, 1], 5), ")", "*" , po[i], "+")
    }
    equation <- paste(equation, "(", round(coef[1, 1], 5), ")")
  }
  
  return (equation)
}

getRegressionPlot <- function(numbcoef, mReg) {
  if (numbcoef > 2){
    plt1=effect_plot(mReg, pred = indepMeasure1, interval = TRUE, plot.points = TRUE)
    plt2=effect_plot(mReg, pred = indepMeasure2, interval = TRUE, plot.points = TRUE)
    plt = list(plt1, plt2)
    if(numbcoef > 30) {
      plt3=effect_plot(mReg, pred = indepMeasure3, interval = TRUE, plot.points = TRUE)
      plt = list(plt1, plt2, plt3)
      if(numbcoef > 4) {
        plt4=effect_plot(mReg, pred = indepMeasure4, interval = TRUE, plot.points = TRUE)
        plt = list(plt1, plt2, plt3, plt4)
        if(numbcoef > 5) {
          plt5=effect_plot(mReg, pred = indepMeasure5, interval = TRUE, plot.points = TRUE)
          plt = list(plt1, plt2, plt3, plt4, plt5)
          if(numbcoef > 6) {
            plt6=effect_plot(mReg, pred = indepMeasure6, interval = TRUE, plot.points = TRUE)
            plt = list(plt1, plt2, plt3, plt4, plt5, plt6)
          }
        }
      }
    }
  }
  return(plt)
}

ResidualRegressionPlots <- function(fit){
  
  # Extract fitted values from lm() object
  Fitted.Values <-  fitted(fit)
  
  # Extract residuals from lm() object
  Residuals <-  resid(fit)
  
  # Extract standardized residuals from lm() object
  Standardized.Residuals <- stdres(fit)
  
  # Extract fitted values for lm() object
  Theoretical.Quantiles <- qqnorm(Residuals, plot.it = F)$x
  
  # Square root of abs(residuals)
  Root.Residuals <- sqrt(abs(Standardized.Residuals))
  
  # Calculate Leverage
  Leverage <- lm.influence(fit)$hat
  
  # Create data frame 
  # Will be used as input to plot_ly
  
  regMat <- data.frame(Fitted.Values, 
                       Residuals, 
                       Standardized.Residuals, 
                       Theoretical.Quantiles,
                       Root.Residuals,
                       Leverage)
  
  # Plot using Plotly
  
  # Fitted vs Residuals
  # For scatter plot smoother
  LOESS1 <- loess.smooth(Fitted.Values, Residuals)
  
  plt1 <- regMat %>% 
    plot_ly(x = Fitted.Values, y = Residuals, 
            type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
            marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
    
    add_trace(x = LOESS1$x, y = LOESS1$y, type = "scatter", mode = "line", name = "Smooth",
              line = list(width = 2)) %>% 
    
    plotly::layout(title = "Residuals vs Fitted Values", plot_bgcolor = "#e6e6e6", width = 1000)
  
  # QQ Pot
  plt2 <- regMat %>% 
    plot_ly(x = Theoretical.Quantiles, y = Standardized.Residuals, 
            type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
            marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
    
    add_trace(x = Theoretical.Quantiles, y = Theoretical.Quantiles, type = "scatter", mode = "line", name = "",
              line = list(width = 2)) %>% 
    
    plotly::layout(title = "Q-Q Plot", plot_bgcolor = "#e6e6e6")
  
  # Scale Location
  # For scatter plot smoother
  LOESS2 <- loess.smooth(Fitted.Values, Root.Residuals)
  
  plt3 <- regMat %>% 
    plot_ly(x = Fitted.Values, y = Root.Residuals, 
            type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
            marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
    
    add_trace(x = LOESS2$x, y = LOESS2$y, type = "scatter", mode = "line", name = "Smooth",
              line = list(width = 2)) %>% 
    
    plotly::layout(title = "Scale Location", plot_bgcolor = "#e6e6e6", width = 1000)
  
  # Residuals vs Leverage
  # For scatter plot smoother
  LOESS3 <- loess.smooth(Leverage, Residuals)
  
  plt4 <- regMat %>% 
    plot_ly(x = Leverage, y = Residuals, 
            type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
            marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
    
    add_trace(x = LOESS3$x, y = LOESS3$y, type = "scatter", mode = "line", name = "Smooth",
              line = list(width = 2)) %>% 
    
    plotly::layout(title = "Leverage vs Residuals", plot_bgcolor = "#e6e6e6")
  
  plt = list(plt1, plt2, plt3, plt4)
  return(plt)
}

leForecast <- function(textPrRien, loadedModel){
  x <- loadedModel
  
  Tabbat4=read.csv(file = x[[2]], header = TRUE, sep = ",")
  
  depMeasure <- Tabbat4[1:1000,which(colnames(Tabbat4)==x[[3]])]
  indepMeasure1 <- Tabbat4[1:1000,which(colnames(Tabbat4)==x[[4]])]
  
  data_set=data.frame(depMeasure,indepMeasure1)
  a <- x[[1]]$coef[[2]]
  b <- x[[1]]$coef[[1]][1]
  c <- summary(x[[1]])$coefficients[1:2, ]
  
  predictionGraph <-  plot_ly(data_set, x = indepMeasure1, y = depMeasure, mode = "markers", showlegend = FALSE, type = "scatter") %>%
    add_trace(y = a*indepMeasure1+b, x = indepMeasure1, name = "Computed", mode = "lines", line = list(color = "#F77D74")) %>%
    add_trace(x = as.numeric(textPrRien), y=a*as.numeric(textPrRien)+b, name = "Computed", mode = "markers",showlegend = FALSE, type = "scatter", color = "#F77D74", size=20 ) %>%
    plotly::layout(title = "Regression Graph", yaxis = list(zeroline = FALSE, showline = FALSE), 
           xaxis = list(title = "Values", zeroline = FALSE, showline = FALSE),
           margin = list(l = 70, r = 40, t = 70, b = 50, pad = 4))  
  
  calculation <- paste(a,"*",{ textPrRien },"+",b,"=",{ a*as.numeric({ textPrRien })+b })
  return(list(predictionGraph, calculation))
}

##########################bEGINNING new code qUENTIN #########################################

RidgeModelSummary <- function(NewData, modelz) {
  library(lmridge)
  inputData=NewData[complete.cases(NewData), ]
  
  ### Preprare the training Data & test Data
  trainingIndex <- sample(1:nrow(inputData), 0.8*nrow(inputData)) # indices for 80% training data
  trainingData <- inputData[trainingIndex, ] # training data
  #testData <- inputData[-trainingIndex, ] # test data
  
  
  
  linRidgeMod <- lmridge(depMeasure ~ ., data = trainingData)# the ridge regression model
  
  ## Ridge Model Pred
  #predictedRidge <-predict(linRidgeMod, newdata =testData) # predict on test data
  #compareRidge <- cbind (actual=testData$depMeasure, predictedRidge)  # combine actual and predicted
  
  ## Ridge on all the dataset
  resultfinal <- predict(linRidgeMod, inputData)
  
  compare <- cbind (actual=inputData, resultfinal)  # combine
  compare=compare[complete.cases(compare), ]
  
  
  
  ## Plot ResultFinal
  
  compareplot <- lapply(1:length(modelz[[4]]), function(i) {
    plot_ly(compare, x=compare[i+1],y=compare$actual.response, mode = "markers", showlegend = TRUE, type = "scatter", name = "Input Values") %>% 
      add_trace(compare, x=compare[i+1],y=compare$result, mode = "markers", showlegend = TRUE, type = "scatter", plot_bgcolor = "#e6e6e6", name = "Computed Values")%>%
      plotly::layout(yaxis = list(zeroline = FALSE, showline = FALSE), 
             showlegend = TRUE, margin = list(l = 70, r = 40, t = 70, b = 50, pad = 4))
  })
  
  data = list()
  ###############################this code works only for3 independent var#######
  data[1]=as.numeric(10) 
  data[2]=as.numeric(12) 
  data[3]=as.numeric(14) 
  
  
  coef = list()
  
  coef[1] <- as.numeric(summary(linRidgeMod)$summaries[[1]]$coefficients[1])
  coef[2] <- as.numeric(summary(linRidgeMod)$summaries[[1]]$coefficients[2])
  coef[3] <- as.numeric(summary(linRidgeMod)$summaries[[1]]$coefficients[3])
  #coef[4] <- as.numeric(summary(linRidgeMod)$summaries[[1]]$coefficients[4])
  
  numbcoef <- length(linRidgeMod$coef)
  
  ## Compute the forecast 
  y=getEquation(coef, data, numbcoef)
  
  
  coef <- summary(linRidgeMod)$summaries[[1]]$coefficients
  
  return(list("Ridge Regression", compareplot,coef,y))
}

#compute the values using the regression equation
getEquation <- function(coef, data, numbcoef){
  if (length(numbcoef) == 2){
    y <- as.numeric(coef[1]) + as.numeric(data[1]) * as.numeric(coef[2])
    #print("OK")
  } else {
    y <- coef[1]
    for (i in 1 : numbcoef){
      y <- as.numeric(y) + as.numeric(coef[i + 1]) * as.numeric(data[i])
    }
  }
  
  return (y)
}

NNModelSummary <- function(NewData, modelz) {
  
  Newtab=NewData[complete.cases(NewData), ]
  
  maxs <- apply(Newtab, 2, max)
  mins <- apply(Newtab, 2, min)
  scaled <- as.data.frame(scale(Newtab, center = mins, scale = maxs - mins))  #normalization
  
  indarr <- sample(1:nrow(scaled),round(0.8*nrow(scaled))) 
  train <- scaled[indarr,] 
  test <- scaled[-indarr,]
  
  library(neuralnet)
  nn <- modelz[[1]]
  
  trained_ <- nn$net.result[[1]]*(max(Newtab[,1])-min(Newtab[,1]))+min(Newtab[,1])
  train_gt <- train[,1]*(max(Newtab[,1])-min(Newtab[,1]))+min(Newtab[,1])
  #MSE.nn_train <- sum((train_gt - trained_)^2)/nrow(train)
  #calculate the MSE of training result
  
  predicted <- compute(nn,test) #predict the result using test data, still normalized
  predicted_t <- predicted$net.result*(max(Newtab[,1])-min(Newtab[,1]))+min(Newtab[,1]) 
  
  test_gt <- test[,1]*(max(Newtab[,1])-min(Newtab[,1]))+min(Newtab[,1])
  
  #MSE.nn_test <- sum((test_gt - predicted_t)^2)/nrow(test)
  
  #performance comparison between the results of training and validation(test) data.
  
  dataframeplot <- as.data.frame(cbind(train_gt,trained_))
  dataframeplot2 <- as.data.frame(cbind(test_gt,predicted_t))
  
  plotly1 <-  plot_ly(dataframeplot,x = dataframeplot$train_gt, y = dataframeplot$V2, mode = "markers", showlegend = FALSE, type = "scatter") %>%
    add_trace(x= dataframeplot$train_gt,y = dataframeplot$train_gt, name = 'Training', showlegend = TRUE, mode = 'lines') %>%
    plotly::layout(title = "Groundtruth vs Predicted", yaxis = list(zeroline = FALSE, showline = FALSE), 
           xaxis = list(title = "Values", zeroline = FALSE, showline = FALSE)
           ,margin = list(l = 70, r = 40, t = 70, b = 50, pad = 4)) 
  
  plotly2 <-  plot_ly(dataframeplot2,x = dataframeplot2$test_gt, y = dataframeplot2$V2, mode = "markers", showlegend = FALSE, type = "scatter") %>%
    add_trace(x= dataframeplot2$test_gt,y = dataframeplot2$test_gt, name = 'Validation', mode = 'lines') %>%
    plotly::layout(title = "Groundtruth vs Predicted", yaxis = list(zeroline = FALSE, showline = FALSE), 
           xaxis = list(title = "Values", zeroline = FALSE, showline = FALSE)) 
  
  return(list("Neural Networks", nn$result.matrix,plotly1, plotly2))
}

KNearestNeighborModelSummary <- function(NewData, modelz){
  ################# Need to be in the vizualisation 
  ##### Loop to run all the k-Nearest Neighbour Classification mean
  knn <- modelz[[1]]

  meantablecal <- sapply(3:11, function(i){##this function create a new df with all the indep Measures
    compare <- cbind (actual=knn$`trainingData$depMeasure`, knn[i-3]$knn) # for comparison
    100-mean (apply(compare, 1, min)/apply(compare, 1, max))*100 # calculate accuracy
  })
  
  meantable =as.data.frame(meantablecal)
  meantable$ID <- seq.int(nrow(meantable))
  colnames(meantable) <- c("k-nn result","k-Nearest Neighbour Classification")
  
  library(plotly)
  ##### Plot the mean for every k-Nearest Neighbour Classification 
  plotly1 <-  plot_ly(meantable,x = meantable$`k-Nearest Neighbour Classification`, y = meantable$`k-nn result`, mode = "markers+lines", showlegend = FALSE, type = "scatter") %>%
    plotly::layout(title = "k-Nearest Neighbour Classification mean", yaxis = list(zeroline = FALSE, showline = FALSE), 
           xaxis = list(title = "Values", zeroline = FALSE, showline = FALSE)
           ,margin = list(l = 70, r = 40, t = 70, b = 50, pad = 8)) 
  
  
  ##### Loop to find the best k-Nearest Neighbour Classification
  bestmean <- 0
  bestneig <- 0
  
  for (i in 1:9) {
    if(meantablecal[i]>bestmean) {
      bestmean= meantablecal[i]
      bestneig=i
    }
  }

  ##### Plot the best k-Nearest Neighbour Classification
  
  dataframeplot = cbind(knn$`trainingData$depMeasure`,knn[bestneig])
  dataframeplot
  colnames(dataframeplot) <- c("depMeasure","knn")
  dataframeplot$ID <- seq.int(nrow(dataframeplot))
  
  plotly2 <-  plot_ly(dataframeplot,x = dataframeplot$ID, y = dataframeplot$depMeasure, mode = "lines", showlegend = FALSE, type = "scatter") %>%
    add_trace(x= dataframeplot$ID,y = dataframeplot$knn, name = '4-nearest neighbours model', showlegend = TRUE, mode = 'lines') %>%
    plotly::layout(title = "TestValues vs Predicted", yaxis = list(zeroline = FALSE, showline = FALSE), 
           xaxis = list(title = "Values", zeroline = FALSE, showline = FALSE)
           ,margin = list(l = 70, r = 40, t = 70, b = 50, pad = 4)) 
  
  return(list("K Nearest Neighbour Model",meantable,plotly1, plotly2))
}


######Random Forest Visualisation#######
RandomForestModelSummary <- function(modelz){
  model <- modelz[[1]]
  
  results <- as.data.frame(model)
  results[,3] <- seq.int(nrow(results))
  
  plotly2 <-  plot_ly(results,x = results[,3], y = results[,1], mode = "lines+markers",name = 'Out of Bag Error', showlegend = TRUE, type = "scatter") %>%
    add_trace(x= results[,3],y = results[,2], name = 'Test Error', showlegend = TRUE, mode = 'lines+markers') %>%
    layout(title = "TestValues vs Predicted", yaxis = list(zeroline = FALSE, showline = FALSE), 
           xaxis = list(title = "Values", zeroline = FALSE, showline = FALSE)
           ,margin = list(l = 70, r = 40, t = 70, b = 50, pad = 4)) 
  
  return(list("Random Forest Model",results,plotly2))
}
