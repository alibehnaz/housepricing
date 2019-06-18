#################################################################################
#########################This is the Server FILE#######@#########################
library(curl)
library(httr)
library(neuralnet)
library(MASS)

options(shiny.sanitize.errors = FALSE)

################ GET the List of Concept By Quering MarkLogic ##############################
getConceptList <- function(){
  
  ConceptList <- list()
  
  url <- "http://adage2.cse.unsw.edu.au:8005/v1/graphs/sparql?query=PREFIX%20analytics%3A%20%3Chttp%3A%2F%2Fwww.adage.unsw.edu.au%2Fanalytics%23%3E%0APREFIX%20adage%3A%20%3Chttp%3A%2F%2Fadage.cse.unsw.edu.au%2Fresource%2Fanalytics%23%3E%0ASELECT%20DISTINCT%20%3Fconcept%0AWHERE%20%7B%20%0A%3Fx%20analytics%3Aoperationalize%20%3Fconcept.%20%7D"

  getConcept <- GET(url, authenticate(user = "admin", password = "financeit", type = "basic"))
  
  for(i in 1:length(content(getConcept)$results$bindings)){
    ConceptList <- c(ConceptList, content(getConcept)$results$bindings[[i]]$concept$value)
  }
  
  laListeDeConcept <- substring(ConceptList, 29)
  return(laListeDeConcept)
}

####################### Find the right Dep relative to the concepts chosen By Quering MarkLogic ########
#this function purpose is to find the Dep var that are only relative to the list of concept chosen.
getRelDepVar <- function(Onthology){

  subQuery <- paste(unlist(lapply(Onthology, function(Onthology){paste0("?var analytics:operationalize <http://dbpedia.org/resource/",Onthology,">.")})), collapse ='')
  completeQuery <- paste0("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>PREFIX analytics: <http://www.adage.unsw.edu.au/analytics#> SELECT DISTINCT ?variable ?var (COALESCE(?MeasureLabel, 'No Measures available for this Variable') as ?measureLabel) WHERE{?var rdf:type analytics:Variable.?var rdfs:label ?variable.?var analytics:operationalize ?concept.", subQuery,"Optional{?var analytics:measuredBy ?M.?M rdfs:label ?MeasureLabel.}}")
  
  url <- URLencode(completeQuery, reserved = T)
  
  depVarMeas <- GET(paste0("http://adage2.cse.unsw.edu.au:8005/v1/graphs/sparql?query=",url),
                authenticate(user = "admin", password = "financeit", type = "basic"))
  
  relativeDepVar <- NULL
  relativeDepMeasure <- NULL
  depVarID <- NULL
  for(i in 1:length(content(depVarMeas)$results$bindings)){
    if(length(content(depVarMeas)$results$bindings)==0){
      df <- NULL
    } else {
      depVarID <- c(depVarID, content(depVarMeas)$results$bindings[[i]]$var$value)
      relativeDepVar <- c(relativeDepVar, content(depVarMeas)$results$bindings[[i]]$variable$value)
      relativeDepMeasure <- c(relativeDepMeasure, content(depVarMeas)$results$bindings[[i]]$measureLabel$value)
      df <- data.frame(relativeDepVar, relativeDepMeasure, depVarID, stringsAsFactors = FALSE)}
  }
  
  return(df)
}

######################## Get the Causal Indep Var relative to the the DEP Var chosen by Quering ML ##########
getCausalIndepVar <- function(depVarChosen){
  
  subQuery <- paste0("?v2 rdfs:label '",depVarChosen,"'^^rdfs:Literal.")
  completeQuery <- paste0("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> PREFIX analytics: <http://www.adage.unsw.edu.au/analytics#> SELECT DISTINCT  ?v1Label ?v2Label WHERE { ?variable analytics:linkType analytics:Causal.  ?variable rdf:type analytics:LinkedVariables.  ?variable analytics:firstVariable ?v1.  ?v1 rdfs:label ?v1Label.  ?variable analytics:secondVariable ?v2. ",subQuery,"}")
  
  url <- URLencode(completeQuery, reserved = T)
  
  indepVar <- GET(paste0("http://adage2.cse.unsw.edu.au:8005/v1/graphs/sparql?query=",url),
                  authenticate(user = "admin", password = "financeit", type = "basic"))
  
  causalIndepVar <- c()
  
  for(i in 1:length(content(indepVar)$results$bindings)){
    if(length(content(indepVar)$results$bindings)==0){
      causalIndepVar <- NULL
    } else if(length(content(indepVar)$results$bindings)==1) {
      causalIndepVar <- content(indepVar)$results$bindings[[1]]$v1Label$value
    }else{
      causalIndepVar <- c(causalIndepVar, content(indepVar)$results$bindings[[i]]$v1Label$value)
    }
  }
  return(causalIndepVar)
}

########GET all the variables by Quering ML#######
getAllVariables <- function(){
  
  completeQuery <- "PREFIX analytics: <http://www.adage.unsw.edu.au/analytics#> PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> SELECT DISTINCT ?variable WHERE{ ?var rdf:type analytics:Variable. ?var rdfs:label ?variable.}"
  
  url <- URLencode(completeQuery, reserved = T)
  
  fullVarList <- GET(paste0("http://adage2.cse.unsw.edu.au:8005/v1/graphs/sparql?query=",url),
                     authenticate(user = "admin", password = "financeit", type = "basic"))
  
  FullVarList <- c()
  for(i in 1:length(content(fullVarList)$results$bindings)){
    FullVarList <- c(FullVarList, content(fullVarList)$results$bindings[[i]]$variable$value)
  }
  return(FullVarList)
}

########Get the measure related to the indep Variable chosen by quering ML#######
#variable <- c("Account Balance", "Exchange Rate", "Mortgage Rate")
getMeasuresRelToVar <- function(variable){
  subSelectQuery <- paste(unlist(lapply(1:length(variable), function(i){paste0("?Vlabel",i," ?variable",i," ?MeasureLabel",i)})), collapse = " ")
  subQuery <- paste(unlist(lapply(1:length(variable), function(i){paste0("?variable",i," rdf:type  analytics:Variable. ?variable",i," rdfs:label '",variable[i],"'^^rdfs:Literal. ?variable",i," rdfs:label ?Vlabel",i,". ?variable",i," analytics:measuredBy ?M",i,".?M",i," rdfs:label ?MeasureLabel",i,".")})), collapse ='')
  completeQuery <- paste0("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> PREFIX analytics: <http://www.adage.unsw.edu.au/analytics#> PREFIX dcat: <http://www.w3.org/ns/dcat#> SELECT DISTINCT ",subSelectQuery," WHERE {",subQuery,"}")
  url <- URLencode(completeQuery, reserved = T)
  measures <- GET(paste0("http://adage2.cse.unsw.edu.au:8005/v1/graphs/sparql?query=",url),
                  authenticate(user = "admin", password = "financeit", type = "basic"))
  
  content(measures)
  measureOfVar <- NULL
  indepVarID <- NULL
  nameOfVar <- NULL
  if(length(content(measures)$results$bindings) == 0){
    measureOfVar <- "No Measures available for this Variable"
    indepVarID <- ""
    df <- data.frame(measureOfVar, indepVarID, stringsAsFactors = FALSE)
    auFinale <- list(df, measureOfVar)
  }else{ 
    for (i in 1:length(variable)){
      for(j in 1:length(content(measures)$results$bindings)){
        measureOfVar <- c(measureOfVar, eval(parse(text = paste0("content(measures)$results$bindings[[",j,"]]$MeasureLabel",i,"$value"))))
        indepVarID <- c(indepVarID, eval(parse(text = paste0(" content(measures)$results$bindings[[",j,"]]$variable",i,"$value"))))
        nameOfVar <- c(nameOfVar, eval(parse(text = paste0("content(measures)$results$bindings[[",j,"]]$Vlabel",i,"$value"))))
      }
    }
    df <- unique(data.frame(nameOfVar, measureOfVar, indepVarID, stringsAsFactors = FALSE))
    measureList <- NULL
    measureList <- unique(lapply(1:nrow(df), function(i){##get them in order of lowest ID to highest ID
      measureList <- c(measureList, df$measureOfVar[df$indepVarID%in%df$indepVarID[order(df$indepVarID)][i]])
    }))
    auFinale <- list(df, measureList)
  }
  return(auFinale)
}
#c <- getMeasuresRelToVar(variable)

####Check if there is a link DS for Dep and Indep Measure########
checkForDataset <- function(setOfMeasures){
  subQuery <- paste(unlist(lapply(1:length(setOfMeasures), function(i){paste0("?variable",i," rdf:type  analytics:Variable. ?variable",i," analytics:measuredBy ?M",i,". ?M",i," rdfs:label '",setOfMeasures[i],"'^^rdfs:Literal. ?datastructure analytics:containMeasure ?M",i,".")})), collapse ='')
  completeQuery <- paste0("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> PREFIX analytics: <http://www.adage.unsw.edu.au/analytics#> PREFIX dcat: <http://www.w3.org/ns/dcat#> SELECT DISTINCT ?DataSetloc ?dssLabel WHERE {",subQuery,"?ds analytics:DatasetStructure ?datastructure. ?datastructure analytics:dataSource ?dsSource. ?dsSource rdfs:label ?dssLabel. ?ds dcat:accessURL ?DataSetloc.}")
  url <- URLencode(completeQuery, reserved = T)
  testX <- GET(paste0("http://adage2.cse.unsw.edu.au:8005/v1/graphs/sparql?query=",url),
               authenticate(user = "admin", password = "financeit", type = "basic"))
  
  
  DSloc <- NULL
  DsSource <- NULL
  if(length(content(testX)$results$bindings) == 0){
    DSloc <- "No Dataset available for this set of measures"
    DsSource <- ""
    }else{ for(i in 1:length(content(testX)$results$bindings)){
      DSloc <- c(DSloc, content(testX)$results$bindings[[i]]$DataSetloc$value)
      DsSource <- c(DsSource, content(testX)$results$bindings[[i]]$dssLabel$value)
    }}
  DSxDSS <- data.frame(DSloc, DsSource, stringsAsFactors = F)
  return(DSxDSS)
}
#################@####### Full list of ModelTypes by Quering ML ################@
############Full list of ModelType############################################
getModelTypes <- function(){
  
  completeQuery <- "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> PREFIX analytics: <http://www.adage.unsw.edu.au/analytics#> SELECT DISTINCT ?x ?var WHERE { ?var ?y analytics:ModelType.?var rdfs:label ?x.}"
  
  url <- URLencode(completeQuery, reserved = T)
  
  modelTypeNames <- GET(paste0("http://adage2.cse.unsw.edu.au:8005/v1/graphs/sparql?query=",url),
                        authenticate(user = "admin", password = "financeit", type = "basic"))
  
  ModelTypeNames <- NULL
  ModelTypeID <- NULL
  for(i in 1:length(content(modelTypeNames)$results$bindings)){
    ModelTypeNames <- c(ModelTypeNames, content(modelTypeNames)$results$bindings[[i]]$x$value)
    ModelTypeID <- c(ModelTypeID, content(modelTypeNames)$results$bindings[[i]]$var$value)
  }
  ModelType <- data.frame(ModelTypeNames, ModelTypeID, stringsAsFactors = F)
  return(ModelType)
}
getModelTypes()
############ Get Full List of Stat Model by Quering ML##################
getStatModel <- function(){
  
  completeQuery <- "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> PREFIX analytics: <http://www.adage.unsw.edu.au/analytics#> SELECT DISTINCT ?x ?rCode WHERE { ?var rdf:type analytics:StatisticalModel.?var rdfs:label ?x.OPTIONAL{ ?model analytics:rCode ?rCode}}"
  
  url <- URLencode(completeQuery, reserved = T)
  
  statModelNames <- GET(paste0("http://adage2.cse.unsw.edu.au:8005/v1/graphs/sparql?query=",url),
                        authenticate(user = "admin", password = "financeit", type = "basic"))
  
  StatModelNames <- NULL
  for(i in 1:length(content(statModelNames)$results$bindings)){
    if(length(content(statModelNames)$results$bindings)==0){
      StatModelNames <- c("No Model Available")
      }else{
        StatModelNames <- c(StatModelNames, content(statModelNames)$results$bindings[[i]]$x$value)
        }
  }
  return(StatModelNames)
}
getStatModelLocal <- function(){
  
  
}

#######get IndepVar associated to the existing model Chosen######
getIndepVarFromModel <- function(modelName){
  completeQuery <- paste0("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>PREFIX analytics: <http://www.adage.unsw.edu.au/analytics#> SELECT DISTINCT ?indepvar_label ?depvar_label ?ModTypeLabel WHERE{?mod analytics:dependentVariable ?depVar.?depVar rdfs:label ?depvar_label.?mod analytics:independentVariable ?indepVar.?indepVar rdfs:label ?indepvar_label.?mod analytics:modelType ?ModTypeID.?ModTypeID rdfs:label ?ModTypeLabel.?mod rdfs:label '",modelName,"'^^rdfs:Literal}")
  url <- URLencode(completeQuery, reserved = T)
  depIndepVarList <- GET(paste0("http://adage2.cse.unsw.edu.au:8005/v1/graphs/sparql?query=",url),
                         authenticate(user = "admin", password = "financeit", type = "basic"))

  depVar <- NULL
  indepVarList <- NULL
  modelType <- NULL
  if(length(content(depIndepVarList)$results$bindings) == 0){
    ##do the other method
    depVar <- 1
    indepVarList <- 1
    modelType <- 1
  }else{ for(i in 1:length(content(depIndepVarList)$results$bindings)){
    depVar <- content(depIndepVarList)$results$bindings[[1]]$depvar_label$value
    indepVarList <- c(indepVarList, content(depIndepVarList)$results$bindings[[i]]$indepvar_label$value)
    modelType <- content(depIndepVarList)$results$bindings[[1]]$ModTypeLabel$value
  }
  }
  return(list(depVar, indepVarList, modelType))
}

########get Data Source /Query ML###########
getDataSources <- function(){
  completeQuery <- "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> PREFIX analytics: <http://www.adage.unsw.edu.au/analytics#> SELECT DISTINCT ?ds ?label WHERE{?ds rdf:type analytics:DataSource.?ds rdfs:label ?label}"
  url <- URLencode(completeQuery, reserved = T)
  dataSources <- GET(paste0("http://adage2.cse.unsw.edu.au:8005/v1/graphs/sparql?query=",url),
                     authenticate(user = "admin", password = "financeit", type = "basic"))
  
  DataSourcesList <- NULL
  DataSourceRef <- NULL
  for(i in 1:length(content(dataSources)$results$bindings)){
    DataSourcesList <- c(DataSourcesList, content(dataSources)$results$bindings[[i]]$label$value)
    DataSourceRef <- c(DataSourceRef, sub(".*#", "", content(dataSources)$results$bindings[[i]]$ds$value))
    DataSources <- data.frame(DataSourcesList, DataSourceRef, stringsAsFactors = FALSE)
  }
  return(DataSources)
}

#####get DS Structures /Query ML#######
getDSStructures <- function(laDataSources, ladataSourceChosen ){
  DREF <- laDataSources$DataSourceRef[laDataSources$DataSourcesList%in%ladataSourceChosen]
  completeQuery <- paste0("PREFIX adage: <http://adage.cse.unsw.edu.au/resource/analytics#> PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> PREFIX analytics: <http://www.adage.unsw.edu.au/analytics#> SELECT DISTINCT ?DSSlabel ?dataSetStructure WHERE{?dataSetStructure rdf:type analytics:DatasetStructure.?dataSetStructure analytics:dataSource adage:",DREF,".?dataSetStructure rdfs:label ?DSSlabel .?dataSetStructure analytics:containMeasure ?measure.?measure rdfs:label ?label.}")
  url <- URLencode(completeQuery, reserved = T)
  dsStructures <- GET(paste0("http://adage2.cse.unsw.edu.au:8005/v1/graphs/sparql?query=",url),
                      authenticate(user = "admin", password = "financeit", type = "basic"))
  
  if(length(content(dsStructures)$results$bindings)==0){
    #DsStructuresNames <- NULL
    DsStructuresNames <- "No DataSet Structures available"
    DsStructuresREF <- NA
    DsStructures <- data.frame(DsStructuresNames, DsStructuresREF, stringsAsFactors = FALSE)
  }else{
    DsStructuresNames <- NULL
    DsStructuresREF <- NULL
    for(i in 1:length(content(dsStructures)$results$bindings)){
      DsStructuresNames <- c(DsStructuresNames, content(dsStructures)$results$bindings[[i]]$DSSlabel$value)
      DsStructuresREF <- c(DsStructuresREF, sub(".*#", "", content(dsStructures)$results$bindings[[i]]$dataSetStructure$value))
      DsStructures <- data.frame(DsStructuresNames, DsStructuresREF, stringsAsFactors = FALSE)
    }}
  return(DsStructures)
}

#####Get DataSet Location /Query ML#######
getDataSet <-  function(dsstructre, dssnames){
  SREF <- dsstructre$DsStructuresREF[dsstructre$DsStructuresNames%in%dssnames]
  completeQuery <- paste0("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> PREFIX analytics: <http://www.adage.unsw.edu.au/analytics#> PREFIX adage: <http://adage.cse.unsw.edu.au/resource/analytics#> PREFIX dcat: <http://www.w3.org/ns/dcat#>PREFIX dct: <http://purl.org/dc/terms/> SELECT DISTINCT ?location WHERE{?dataset rdf:type analytics:Dataset.?dataset analytics:DatasetStructure adage:",SREF,".?dataset dcat:accessURL ?location.?dataset dct:format ?format .?dataset rdfs:label ?label.?dataset rdfs:comment ?comment.}")
  url <- URLencode(completeQuery, reserved = T)
  datasetloc <- GET(paste0("http://adage2.cse.unsw.edu.au:8005/v1/graphs/sparql?query=",url),
                    authenticate(user = "admin", password = "financeit", type = "basic"))
  
  content(datasetloc)$results$bindings
  pathToDS <- NULL
  if(length(content(datasetloc)$results$bindings) == 0){
    pathToDS <- "No DS available for this Datastructure"
    loadedDs <- NA
  }else{ for(i in 1:length(content(datasetloc)$results$bindings)){
    
    pathToDS <- c(pathToDS, content(datasetloc)$results$bindings[[i]]$location$value)
    #loadedDs <<- list(loadedDs, read.csv(pathToDS, header =TRUE, sep = ","))
  }
  }
  return(pathToDS) 
}

# ########get dataset#######@
# getDS <- function(lespaths){##Not UDED
#   count <- length(lespaths)
#   Ds <- c()
#   lesDs <- NULL
#   lapply(1:count, function(i){
#     Ds[i] <- read.csv(lespaths[[i]], header = TRUE, sep = ",")
#     lesDs <- list(lesDs, Ds[i]) 
#   })
#   
#   return(lesDs)
# }
# #############get the summary ofo the model#########
# getModelSummary <- function(statModel){#                  ####Not USED
#   completeQuery <- paste0("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> PREFIX analytics: <http://www.adage.unsw.edu.au/analytics#> SELECT DISTINCT ?rCode WHERE{?model rdf:type analytics:StatisticalModel.?model rdfs:label '", statModel ,"'^^rdfs:Literal.?model analytics:rCode ?rCode.}")
#   url <- URLencode(completeQuery, reserved = T)
#   statModelCode <- GET(paste0("http://adage2.cse.unsw.edu.au:8005/v1/graphs/sparql?query=",url),
#                        authenticate(user = "admin", password = "financeit", type = "basic"))
#   
#    if(length(content(statModelCode)$results$bindings) == 0){
#      pathToModelCode <- NULL
#      loadedModel <- "No Code available for this model yet"
#      modelsummary <- loadedModel
#      pathToModelCode <- NULL
#      } else{
#        pathToModelCode <- content(statModelCode)$results$bindings[[1]]$rCode$value
#        loadedModel <- readRDS(file = pathToModelCode )
#        modelsummary <- summary(loadedModel)
#      }
#   return(list(pathToModelCode, modelsummary))
# }

##########Save the model in the onthology####@
saveModelIntoOntology <- function(model_id, modelTypeID, indepvar_id, depvar_id, label){
  subQuery <- paste(unlist(lapply(indepvar_id, function(indepvar_id){paste0("<http://adage.cse.unsw.edu.au/resource/analytics#",indepvar_id,">")})), collapse =',')
  postBody <- paste0("<http://adage.cse.unsw.edu.au/resource/analytics#",model_id,"> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#NamedIndividual> , <http://www.adage.unsw.edu.au/analytics#StatisticalModel>; <http://www.adage.unsw.edu.au/analytics#modelType> <",modelTypeID,"> ;<http://www.w3.org/2000/01/rdf-schema#label> '",label,"'^^<http://www.w3.org/2000/01/rdf-schema#Literal>;<http://www.adage.unsw.edu.au/analytics#rCode> '/home/house_price_data/r_models/",label,".rds'^^<http://www.w3.org/2000/01/rdf-schema#Literal>;<http://www.adage.unsw.edu.au/analytics#independentVariable> ",subQuery,";<http://www.adage.unsw.edu.au/analytics#dependentVariable> <http://adage.cse.unsw.edu.au/resource/analytics#",depvar_id,">.")
  urlPost <- "http://adage2.cse.unsw.edu.au:8005/v1/graphs"
  dothePost <- POST(urlPost, query = list(graph="default"), body = postBody, authenticate(user = "admin", password = "financeit", type = "basic"),content_type('text/turtle'))
}

#########Store and Update the ModelID#######
storeModelID <- function(){##To be executed once to set the value for the modelID@    ####LAST UPDATED 11/04/2019
  baseModelID <- "SMID0015"
  write.csv(baseModelID, file = "/home/house_price_data/currentModelID.csv", row.names = F)
}
updateModelID <- function(){
  oldModelID <- read.csv(file = "/home/house_price_data/currentModelID.csv", sep = ",", header = TRUE)
  
  matches <- regmatches(oldModelID[[1]], gregexpr("[[:digit:]]+\\.*[[:digit:]]*", oldModelID[[1]]))
  
  iDNumber <- as.numeric( matches) +1
  
  if(iDNumber<100){
    newModelID <- paste0("SMID00",iDNumber)
  }else if(iDNumber>=100 && iDNumber<1000){
    newModelID <- paste0("SMID0",iDNumber)
  }
  write.csv(newModelID, file = "/home/house_price_data/currentModelID.csv", row.names = F )
  return(newModelID)
}
# ###################Create DF with the info + save into csv###########
# leModel <- function(modelName, onthology, dependentVar, independentVar, modelType, dataset, indepchoice){
#   
#   ModelToSave <- data.frame(
#     ModelName=modelName, Concept=onthology, DependentVar=dependentVar,
#     IndependentVar=independentVar, ModelType=modelType, Dataset=dataset, ListChoice=indepchoice
#   )
# 
# 
#   write.csv(ModelToSave, file = paste0("SavedModels/",modelName,".csv"), row.names = FALSE)
#   return(ModelToSave)
# }

checkIndep <- function(indepChoice, independentVarA, independentVarC){
  if(indepChoice%in%"Causal Variables"){
    independentVar <- independentVarC
    return(independentVar)
    break()
  }else{independentVar <- independentVarA
  return(independentVar)
  break()}
}
checkDataset <- function(dsChoice, dsloc1, dsloc2){
  if(dsChoice%in%1){
    dsloc <- dsloc1
    return(dsloc)
    break()
  }else{dsloc <- dsloc2
  return(dsloc)
  break()}
  
}
