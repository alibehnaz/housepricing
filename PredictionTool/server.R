#########################This is the Server FILE#######@#########################
#options(shiny.sanitize.errors = FALSE)
library(shinyBS)
library(shiny)
library(XLConnect)
library(plotly)
#/srv/shiny-server/PredictionTool/
source("/srv/shiny-server/PredictionTool/functions.R")
source("/srv/shiny-server/PredictionTool/CreateModel.R")
source("/srv/shiny-server/PredictionTool/ModelVisualisation.R")

shinyServer(function(input, output, session){
  ##################################################################################################################
  ## STEP 1 ######################## CHOOSE CONCEPT + INDEP VAR ####################################################
  ##################################################################################################################
  ConceptList <- getConceptList()#Get all the concepts##########
  ConceptList <- ConceptList[]
  names(ConceptList) <- ConceptList[]
  
  output$theConceptList <- renderUI({
    selectInput("OnthFam", "Select a Concept", choices = ConceptList, multiple = TRUE)
    })
  addTooltip(session, id="theConceptList", "Try selecting many concepts to narrow your search results.", "right", trigger = "hover", options = list(container = "body"))
  
  withProgress(message = "Computing ...", {
    disabled = NULL
    icon = ""
    
    #########Inform the user that there is no variable available for the concepts chosen
    relDepVar <- reactive({
      DepVarInfo <- getRelDepVar(input$OnthFam)
      if(is.null(DepVarInfo)){
        output$NoDep <- renderPrint("No available Dependent Variable for the Concepts combination chosen")
        disabled = TRUE
        icon <- icon("ban")
      }else{
        disabled = FALSE
        output$NoDep <- NULL
      }
      updateButton(session, inputId = "apply", disabled = disabled, icon = icon)####Reactive 'apply' button
      
      DepVarList <- DepVarInfo$relativeDepVar
      names(DepVarList) <- DepVarList[]
      
      return(DepVarInfo)
    })
    
  #####################Select the Dependent Variable #########################################
    output$depVar <- renderUI({
      list(
        br(),
        br(),
        selectInput("depVar1", "Select the Dependent Variable:",choices =relDepVar()[[1]], selected = 1),
        br(),
        br(),
        br()
      )
    })
  })
  

 ########################Apply the Dependent Variable##############################
  observeEvent(input$apply,{ ## DEP Var is applied
    Onthology <- input$OnthFam
    dependentVar <- input$depVar1

    withProgress(message = "Computing ...", {
      Causal_Indep_var <<- getCausalIndepVar(dependentVar)#list of causal linked indep variables
      fullList_Var <<- getAllVariables()#list of all Variables
      ModelTypeList <<- getModelTypes()#list of modelTypes and their ID
      
  ##################################################################################################################
  ##STEP 2 ################# Choose DEP VAR ###########################################################
  ##############################################################################################################
      
      #2.a#######list of Indep Var with causal relation to DEP var######
      CausalIndepVarList <-Causal_Indep_var[]
      names(CausalIndepVarList) <- Causal_Indep_var[]
      
      #2.b#######full list of IndepVar######
      allVariables <- fullList_Var[]
      names(allVariables) <- fullList_Var[]
      
     
      
      #2.d######list of measures related to the indep Var chosen
      observeEvent(c(input$indepVar1, input$indepVar2,input$choicesIndep),{
        indepchoice <- input$choicesIndep
        independentVarC <- input$indepVar1
        independentVarA <- input$indepVar2
        rightIndepVar <- checkIndep(indepchoice, independentVarA, independentVarC)
        measuresAndID <<- getMeasuresRelToVar(rightIndepVar)
        measures <- measuresAndID[[2]]
        names(measures) <- measures
        
        ###this for loop will present the measures in the order of their ID with the smaller ID first
        indecis <- c()
        for(i in 1:length(measuresAndID[[2]])){
          indecis <- c(indecis, measuresAndID[[1]]$nameOfVar[measuresAndID[[1]]$measureOfVar%in%measuresAndID[[2]][[i]][1]])
        }
        
        output$indepMeasureField <- renderUI(
          lapply(1:length(measuresAndID[[2]]), function(i){
            selectInput(inputId = paste0("indepMeasureChosen",i), paste0("Select a measure for ",indecis[[i]]), choices = measures[[i]], multiple = FALSE, selected = 0)
            })
          )
        
        output$depMeasureField <- renderUI(
          selectInput("depMeasureChosen", paste0("Select a measure for ",input$depVar1), choices = relDepVar()[[2]][relDepVar()[[1]]%in%dependentVar])
        )
        
        ####the apply function will display the appropriate amount measures relative to the number of variable chosen
        indepMeasureChoice <- NULL
        lapply(1:length(measuresAndID[[2]]),function(i){
          observeEvent(input[[paste0("indepMeasureChosen", i)]],{
            setOfMeasures <- sapply(1:length(measuresAndID[[2]]), function(i){
              inpid <- paste0("indepMeasureChosen", i)
              indepMeasureChoice <- c(indepMeasureChoice, input[[inpid]])
              })
            setOfMeasures <- c(setOfMeasures, input$depMeasureChosen)
            availableDSxDsS <<- checkForDataset(setOfMeasures) ###the <<- will save the object in the parent environment
            availableDS <- availableDSxDsS$DSloc
            ##########DSOption==1#######
            names(availableDS[]) <- availableDS
            
            output$dsField1 <- NULL
            output$dsField1 <- renderUI({
              list(
                selectInput("dsloc1", "Select a dataset", choices = availableDS, multiple = FALSE),
                uiOutput("datasourceInfo")
                )
              })
            
              if(availableDS%in%"No Dataset available for this set of measures"){
                output$DsChoiceField2 <- NULL
                output$datasourceInfo <- NULL
              }else{
                output$datasourceInfo <- renderPrint(paste0("This Dataset is from the ",availableDSxDsS$DsSource[availableDSxDsS$DSloc%in%input$dsloc1]," DataSource."))
                output$DsChoiceField2 <- renderUI({
                  withProgress(message = "Fetching Datasets ...", {
                    lapply(1:length(availableDS), function(i){
                      
                      tablez <- read.csv(availableDS[i], header = TRUE, sep = ',')
                      
                      bsCollapse(id='bs2', bsCollapsePanel(title = sub(".*house_price_data/", "" ,availableDS[i]),
                                                           tags$h5("The Dataset"),
                                                           renderTable(tablez[1:15, which(colnames(tablez)%in%setOfMeasures)]),
                                                           tags$h5("Summary"),
                                                           renderPrint(summary(tablez[1:15, which(colnames(tablez)%in%setOfMeasures)]))
                      ))
                    })
                  })
                })
                }
             
             
            })
          })
        output$indepVariable <- renderPrint(cat("Independent Variable : ",rightIndepVar, sep = paste0("\n ",1:15, "  ")))
        })
      
      ##output Select IndepVAR##
      output$BmodelSideB <-  renderUI({
        list(
            bsCollapse(id="IndepOpCollapse",  open = 1, bsCollapsePanel(strong("Select the Independent Variable(s):"), value = 1,
                                                                        tipify(radioButtons("choicesIndep", "Select one", choices = c('Causal Variables','All Variables')),title = "If you can not find the independent Variable in the Causal Variables Tab, try All Variables")
                                                           )),
            conditionalPanel(condition = "input.choicesIndep=='Causal Variables'",
                             selectInput("indepVar1", NULL, choices = CausalIndepVarList, multiple = TRUE, selected = 1)),
            conditionalPanel(condition = "input.choicesIndep=='All Variables'",
                             selectInput("indepVar2", NULL, choices = allVariables, multiple = TRUE))
            
            ###############In case a new independent variable choice is added (ex: 'notCausal', 'related', etc), its entry must be implemented here.#########

        )
        })

      ## STEP 3 ################# Select DATASET #####################################################################
      ################################################################################################################
      DataSourcesNames <<- getDataSources()$DataSourcesList
      DataSources <<- getDataSources()
      
      output$DSSideB <- renderUI({
        list(
          tipify(uiOutput("leRadioChoice"),"If a dataset containing the measures you chose exist, it will appear in the According to Measures tab.", trigger = "hover"),
          hr(),
          conditionalPanel(condition = "input.DSOption=='2'",
                           output$DsChoiceField <- NULL,
                           output$DsChoiceField2 <- NULL,
                           selectInput("dataSource", "Select a Data Source", choices = c(DataSourcesNames), multiple = FALSE),
                           uiOutput("dataStrucuresfield"),
                           uiOutput("dSField2")),
          uiOutput("depMeasureField"),
          uiOutput("indepMeasureField"),
          hr(),
          conditionalPanel(condition = "input.DSOption=='1'",
                           output$DsChoiceField <- NULL,
                           output$DsChoiceField2 <- NULL,
                           uiOutput("dsField1"))
          )
        })
      output$leRadioChoice <- renderUI(radioButtons("DSOption", "Select an option", choices = c("According to Variables"=1, "According to Data Source"=2))
)
      
     
     
      
      ##########DSOption==2#######
      observeEvent(input$dataSource, {
        dataSourceChosen <- input$dataSource
        
        withProgress(message = "Computing ...", {
          DSStructuresNames <<- getDSStructures(DataSources, dataSourceChosen)$DsStructuresNames
          DSStrc <<- getDSStructures(DataSources, dataSourceChosen)
          output$dataStrucuresfield <- renderUI({
            selectInput("dataStrucures", "Select a Dataset Structure", choices = DSStructuresNames)
          })
        })
      })
      
      observeEvent(input$dataStrucures, {
        dsStrcName <- input$dataStrucures
        withProgress(message = "Computing ...", {
          DSLoc <<- getDataSet(DSStrc, dsStrcName)
          count <- length(DSLoc)
          
          if(DSLoc[1]=="No DS available for this Datastructure"){
            output$DsChoiceField <- NULL
            output$dSField2 <- renderUI({
              selectInput("dsloc2", "Select a dataset", choices = DSLoc, multiple = FALSE)})
            }else{
              output$dSField2 <- NULL
              output$dSField2 <- renderUI({list(
                selectInput("dsloc2", "Select a dataset", choices =  DSLoc, multiple = FALSE),
                br(),
                downloadButton("downloadDS", "Download the dataset"),
                hr()
                )})
              
              output$DsChoiceField <- renderUI({
                lapply(1:count, function(i){
                  laDS <- read.csv(DSLoc[i] , header =TRUE, sep = ",")
                  bsCollapse(id='bs',  bsCollapsePanel(sub(".*house_price_data/", "" ,DSLoc[i]),
                                                       list(helpText("Only the first 5 columns of the dataset are shown here"), 
                                                            tags$h5("First elements of dataset"), 
                                                            renderTable(laDS[1:20,]),
                                                            tags$h5("Structure of dataset"),
                                                            renderPrint(str(laDS[,5:10])),
                                                            tags$h5("Summary of dataset"),
                                                            renderPrint(summary(read.csv(DSLoc[i] , header =TRUE, sep = ",")[,5:10]))
                                                            )
                                                       ))
                  })
              })
              output$downloadDS <- downloadHandler(filename = function(){sub(".*house_price_data/", "", input$dsloc2)},
                                                   content = function(file){
                                                     file.copy(input$dsloc2, file)
                                                   })
              }
          })
        })
      
      
      ## STEP 4 ################# Select ModelType TAB is selected ###########################################################
      ###############################################################################################################
      #2.c#######full list of ModelType####
      allModelTypes <<- ModelTypeList[]$ModelTypeNames
      names(allModelTypes) <<- ModelTypeList[]$ModelTypeNames
      
      output$modelType <- renderUI(list(
        selectInput("modelType1", "Select the Model Type:",choices = allModelTypes, multiple = FALSE),
        uiOutput("modelDocu"),
        br(),
        textInput("ModelNewName","Enter a name for your model",placeholder = "MyNewModel"),
        br(),
        br(),
        tipify(actionButton("buildIt", "Build"),"Make sure you filled in all the fields before building your model.", "right", trigger = "hover", options = list(container = "body"))
        ))
      output$modelDocu <- renderUI({tags$a(href=paste0(gsub(" ", "_",input$modelType1),"_Model.html"), target="blank", paste0(input$modelType1, " Documentation"))})
      ######link to the documentation file abt the models. the file are in the www folder
      
      
      ####### Take the dataset selected by the user#####
      observeEvent(c(input$dsloc1,input$dsloc2, input$DSOption),{
        rightDS <<- checkDataset(input$DSOption, input$dsloc1, input$dsloc2)
        output$DSChosen <- renderPrint(cat("You selected the ",sub(".*house_price_data/", "" ,rightDS), " Training Dataset."))
        })
      
      
      output$preview <- renderUI(list(
        renderPrint(cat("Dependent Variable : ",input$depVar1)),
        verbatimTextOutput("indepVariable"), 
        verbatimTextOutput("DSChosen")
      ))
      
      #############################SAVE and Create Model file#############
      observeEvent(input$buildIt,{
        
        modelName <- input$ModelNewName
        
        onthology <- input$OnthFam
        
        depMeasure <- relDepVar()[[2]][relDepVar()[[1]]%in%dependentVar]
        lapply(1:length(measuresAndID[[2]]), function(i){
          ok <- c()
          indepMeasure <<- sapply(1:length(measuresAndID[[2]]), function(i) {
            inpid <- paste0("indepMeasureChosen",i)
            ok <- c(ok, input[[inpid]])
            })
          })
        
        modelType <- input$modelType1
        modelTypeID <- ModelTypeList$ModelTypeID[ModelTypeList$ModelTypeNames%in%modelType]
        
        dependentVar <- input$depVar1
        indepchoice <- input$choicesIndep
        independentVarC <- input$indepVar1
        independentVarA <- input$indepVar2
        depvar_id <- sub(".*#","", relDepVar()[[3]][relDepVar()[[1]]%in%input$depVar1])
        indepvar_id <- sub(".*#","", measuresAndID[[1]]$indepVarID[measuresAndID[[1]]$measureOfVar%in%indepMeasure])
        
        dsChoice <- input$DSOption
        dsloc1 <- input$dsloc1
        dsloc2 <- input$dsloc2
        
        withProgress(message = "Computing ...", {
          
          rightIndep <- checkIndep(indepchoice, independentVarA, independentVarC)
          independentVar <- rightIndep
          rightDS <- checkDataset(dsChoice, dsloc1, dsloc2)
          dataset <- rightDS
          
          #if(modelType%in%c("Regression", "Neural Networks", "Ridge Regression")){
            CreateModel(dependentVar, depMeasure, depvar_id, independentVar, indepMeasure, indepvar_id, dataset, modelType, modelTypeID, modelName)
            output$modelCreate <- renderPrint("Model created")
         # }
          
          #savedModel <<- leModel(modelName, onthology, dependentVar, independentVar, modelType, dataset, indepchoice)
          ####create a csv file of the characteristics of the model###
          
          output$ShowModelContainer <- renderUI({
            list(
              textOutput("modelCreate")
              )
            })
          })
        })
      #############################Show the dataframe file#############
      #observeEvent(input$showModel,{
      #  output$leShow <- renderPrint(
      #    read.csv(paste0( "SavedModels/",input$ModelNewName,".csv"), header = TRUE, sep = ",")
      #  )
       # })
      })
    })##This is the closing bracket for the APPLY BUTTON

  ## STEP 5 ########################USE MODEL TAB IS SELECTED###############################################
  ###############################################################################################################
  StatisticalModelList <<- getStatModel()
  StatModelList <- StatisticalModelList[]
  names(StatModelList) <- StatisticalModelList[]
  
  output$UseModelSideB <- renderUI({
    list(
      selectInput("statModel", "Select a Model: ", choices = StatModelList, multiple = FALSE, selected = F),
      uiOutput("textbox_xValue"),
      actionButton("runIt", "Run Prediction")
    )
  })
  ####Ask for the right Input###
  observeEvent(input$statModel,{
    ModelName <- input$statModel
    
    indepListOfModel <<- getIndepVarFromModel(input$statModel)
    
    if(indepListOfModel[[2]][1]==1){
      output$textbox_xValue <- renderUI(
        lapply(1:length(strsplit(sub(".*_*_", "", input$statModel),"-")[[1]]), function(i){
          textInput(inputId = paste0("xValue",i), label = paste0('Input value for ',strsplit(sub(".*_*_", "", input$statModel),"-")[[1]][i]))
          })
        )
      }else{
        output$textbox_xValue <- renderUI({
          list(
            renderUI(paste0("ModelType : ", indepListOfModel[[3]])),
            br(),
            renderUI(paste0("Predicting : ", indepListOfModel[[1]])),
            br(),
            lapply(1:length(indepListOfModel[[2]]), function(i){
              textInput(inputId = paste0("xValue",i), label = paste0('Input a value for ',indepListOfModel[[2]][i]))
              })
            )
        })
        }
    
    SummaryVisual <-  ViewSummary(ModelName)
    modelTypo <- SummaryVisual[[1]][1]

    output$type <- reactive({
      leSum <- ViewSummary(ModelName)
      return(leSum[[1]])
      })
    outputOptions(output, "type", suspendWhenHidden = FALSE)
    
    output$khara <- reactive({
      if(length(indepListOfModel[[2]])>1){
        acceptation <- "nop"
      }else{acceptation <- "yep"}
    })
    outputOptions(output, "khara", suspendWhenHidden = FALSE)
    


    switch(EXPR =modelTypo[1],
           'Regression' = {
             if(length(indepListOfModel[[2]])>=2){
               ####################### Regression Equation ##############################
               output$equation2 <- renderText(SummaryVisual[[2]])
               
               ####################### Regression Coefficients ##############################
               output$coefsreg2 <- renderTable(SummaryVisual[[3]])
             
               ####################### Displays the MSE ##############################
               output$mseReg <- renderText(SummaryVisual[[4]])
              
               ####################### Displays the correlation_accuracy ##############################
               output$correlation_accuracy <- renderText(SummaryVisual[[5]])
              
               ####################### Displays the R Square ##############################
               output$r.squared <- renderText(SummaryVisual[[6]])
               output$adj.r.squared <- renderText(SummaryVisual[[7]])
              
               ####################### Displays the Residuals Results ##############################
               output$residualStuff2 <- renderUI(SummaryVisual[[8]])
             
               ####################### Displays the Plots Results ##############################
               #lapply(1:length(SummaryVisual[[9]]), function(i){
                # idez <- paste0("plotmultireg",i)
                 #output[[idez]] <- renderPlot(SummaryVisual[[9]][i])
                  #})
               output$plotmultireg <- renderPlot(SummaryVisual[[9]])
               
               }else{
                 ####################### Regression Equation ##############################
                 output$equation <- renderText(SummaryVisual[[2]])
                 
                 ####################### Regression Coefficients ##############################
                 output$coefsreg <- renderTable(digits = 4, display = c("s", "g", "g", "g", "g"), SummaryVisual[[3]])

                 ####################### Analysis of Variance ##############################
                 output$varianceanalysis <- renderTable(digits = 4, display = c("s", "g", "g", "g", "g", "g"), SummaryVisual[[4]])
    
                 ####################### Variance/Covariance Matrix ##############################
                 output$varmatrix <- renderTable(SummaryVisual[[5]])
    
                 ####################### Values Comparison : historical_value/computed_value#############################
                 output$valuescomparisongraph <- renderPlotly(SummaryVisual[[6]])
    
                 ####################### Times Series ##############################
                 #output$timesseriesgraph <- renderPlotly(modelSummary[[6]])
    
                 ####################### Displays the Residuals Results ##############################
                 output$residualStuff <- renderUI(SummaryVisual[[7]])
                 }
             },
           
           "Neural Networks" = {
             output$plot2 <- renderPlotly(SummaryVisual[[3]])
             output$plot3 <- renderPlotly(SummaryVisual[[4]])
             },

           "Ridge Regression" = {
             output$compareplot1 <- renderPlotly(SummaryVisual[[2]][[1]])
             output$compareplot2 <- renderPlotly(SummaryVisual[[2]][[2]])

             output$text <- renderTable(SummaryVisual[[3]])
             
             output$ridgeequation <- renderText(SummaryVisual[[4]])
             },
    
           "K Nearest Neighbour Model" ={
             output$tableKNN <- renderTable(SummaryVisual[[2]])
    
             output$KNNplot1 <- renderPlotly(SummaryVisual[[3]])
             output$KNNplot2 <- renderPlotly(SummaryVisual[[4]])
             },
           
           "Random Forest Model" = {
             output$results <- renderTable(SummaryVisual[[2]])
             output$plotly2 <- renderPlotly(SummaryVisual[[3]])
             }
           )
    
    output$SummaryContent <- renderUI({
      list(
        conditionalPanel(condition = "output.type=='Regression' && output.khara == 'nop'",
                         tabsetPanel( id ="tabselected8",
                                      tabPanel("Informations",
                                               uiOutput("texti"),
                                               h3("Regression Statistics"),
                                               hr(),
                                               h4("Regression Equation"),
                                               uiOutput("equation2"),
                                               h4("Regression Coefficients"),
                                               uiOutput("coefsreg2"),
                                               h4("MSE"),
                                               uiOutput("mseReg"),
                                               h4("Correlation Accuracy"),
                                               uiOutput("correlation_accuracy"),
                                               h4("R Squared and Adjusted R Squared"),
                                               uiOutput("r.squared"),
                                               uiOutput("adj.r.squared")
                                               ),

                                      tabPanel("Graph Results",
                                               h3("Model results gr1aph"),
                                               hr(),
                                               #lapply(1:length(SummaryVisual[[9]]), function(i){
                                              #  plotOutput(outputId = paste0("plotmultireg",i))
                                              # })
                                               plotOutput("plotmultireg")
                                      ),

                                      tabPanel("Residuals Analysis",
                                               h3("Residuals Analysis"),
                                               hr(),
                                               uiOutput("residualStuff2")
                                       )
                          )
         ),
        conditionalPanel(condition = "output.type=='Regression'&& output.khara == 'yep'",
                         tabsetPanel( id ="tabselected2",
                                      tabPanel("Model Information",
                                               h3("Regression Statistics"),
                                               hr(),
                                               h4("Regression Equation"),
                                               uiOutput("equation"),
                                               h4("Regression Coefficients"),
                                               uiOutput("coefsreg"),
                                               h4("Analysis of Variance"),
                                               uiOutput("varianceanalysis"),
                                               h4("Variance/Covariance Matrix"),
                                               uiOutput("varmatrix")
                                               ),
                                      
                                      tabPanel("Graph",
                                               h3("Model results graph"),
                                               h4("Values Comparison : historical_value/computed_value"),
                                               plotlyOutput("valuescomparisongraph")
                                               ),
                                      
                                      tabPanel("Residuals",
                                               h3("Residuals Analysis"),
                                               hr(),
                                               uiOutput("residualStuff")
                                               ),
                                      
                                      tabPanel("Forecast",
                                               h3("Models Result"),
                                               hr(),
                                               h4("Result"),
                                               uiOutput("calcul"),
                                               h4("Prediction Graph"),
                                               plotlyOutput("predictiongraph")
                                               ) 
                                      )
                         ),
        conditionalPanel(condition = "output.type=='Neural Networks'", 
                         tabsetPanel( id ="tabselected3",
                                      tabPanel("NN Model Graph",
                                               h3("NeurolNet Graph"),
                                               hr(),
                                               plotlyOutput("plot2"),
                                               plotlyOutput("plot3"),
                                               hr()
                                               )
                                      )
                         ),
        conditionalPanel(condition = "output.type=='Ridge Regression'",
                         tabsetPanel( id ="tabselected1",
                                      tabPanel("General Information",
                                               h3("Ridge Model Statistics"),
                                               hr(),
                                               h4("Ridge Equation"),
                                               uiOutput("text"),
                                               uiOutput("ridgeequation")
                                               ),
                                     
                                      tabPanel("Ridge Graph",
                                               h3("Ridge Model results graph"),
                                               hr(),
                                               h4("Values Comparison : input_value/computed_value"),
                                              
                                               plotlyOutput("compareplot1"),
                                               plotlyOutput("compareplot2")
                                               #plotlyOutput("compareplot3")
                                               )
                                      )
                         ),
        conditionalPanel(condition = "output.type=='K Nearest Neighbour Model'",
                         tabsetPanel( id ="tabselected4",
                                      tabPanel("KNN Model Information",
                                               h3("KNN Model Statistics"),
                                               hr(),
                                               uiOutput("tableKNN")
                                               ),
                                      tabPanel("KNN Model Graph",
                                               h3("KNN Graph"),
                                               hr(),
                                               plotlyOutput("KNNplot1"),
                                               plotlyOutput("KNNplot2"),
                                               hr()
                                               )
                                      )
                         ),
        conditionalPanel(condition = "output.type=='Random Forest Model'",
                         tabsetPanel( id ="tabselected3",
                                      tabPanel("Random Forest Model ",
                                               h3("Random Forest Graph"),
                                               hr(),
                                               plotlyOutput("plotly2"),
                                               h3("Random Forest Results"),
                                               uiOutput("results"),
                                               hr()
                                               )
                                      )
                         )
        )
      })
    })
  
  
  observeEvent(input$runIt, {
    
    ########Retrieve the iputted value in a dataframe##########
    fullIndepValstring <- NULL
    # if(indepListOfModel[[2]]==1){########this exception is not used anymore
    #   fullIndepValstring <- lapply(1:length(strsplit(sub(".*_*_", "", input$statModel),"-")[[1]]), function(i) {
    #     inpid <- paste0("xValue",i)
    #     fullIndepValstring <- c(fullIndepValstring, input[[inpid]])
    #   })
    # }else{
      fullIndepValstring <- lapply(1:length(indepListOfModel[[2]]), function(i) {
        inpid <- paste0("xValue",i)
        fullIndepValstring <- c(fullIndepValstring, input[[inpid]])
      })
    # }
    
    matches <- regmatches(fullIndepValstring, gregexpr("[[:digit:]]+\\.*[[:digit:]]*", fullIndepValstring ))####get only the digit from the textInput
    newIndepVal <- as.data.frame( as.numeric(unlist(matches[[1]])))
    if(length(matches)>1){
      for( i in 2:length(matches)){
        newIndepVal[[i]] <- as.numeric(unlist(matches[[i]]))
      }}
    
    StatModelChosen <- input$statModel
    
    ModelResult <<- LoadAndUseSavedModel2(newIndepVal, StatModelChosen)
    
    modelResults <- t.data.frame(data.frame(newIndepVal, ModelResult[1]))
    colnames(modelResults) <- NULL
    rownames(modelResults)[nrow(modelResults)] <- indepListOfModel[[1]][1]
    rownames(modelResults)[1:nrow(modelResults)-1] <- indepListOfModel[[2]][]
    
    output$UseModz <- renderUI(
      list(
        br(), 
        tags$h4("Results"),
        renderPrint(modelResults)
        )
      )
    ####################### Regression Graph##########################
    output$predictiongraph <- renderPlotly(ModelResult[[2]][[1]])
    output$calcul <- renderText(ModelResult[[2]][[2]])
    })
  
  
  
  #############################################################################@
  ################   INPUT MODEL    #############################
  modelTypes <- getModelTypes()$ModelTypeNames
  names(modelTypes) <- modelTypes[]
  
  #####################STEP 1: CHOOSE MODELTYPE#####################
  output$selectModelTypeContainer <- renderUI({
    selectInput("modelTypezer", "Model Type", choices = modelTypes, multiple = FALSE)
  })
  
  
  #####################STEP 2: INPUT MODEL#####################
  observeEvent(input$fileRDS,{
    
    output$filedf <- renderTable({
      if (is.null(input$fileRDS)) {
        return()
      }
      input$fileRDS
    })
    
    if (substr(input$fileRDS$name, nchar(input$fileRDS$name)-4+1, nchar(input$fileRDS$name))==".rds") {
      output$filedf2 <- renderText("succeed")
      ModelName <- input$fileRDS$datapath
    }
    else{
      output$filedf2 <- renderText("this is not a RDS file, please use a file.rds")
      ModelName <- NULL
    }
    
    x <<- readRDS(ModelName)
    switch(input$modelTypezer,
           "Regression"= {coefNames <<- names(x$coef)[-1]},######retrieve the coefficient of the model depending on its type
           "Neural Networks"= {coefNames <<- colnames(x$covariate)},
           "Ridge Regression"= {coefNames <<- rownames(x$coef)}
           # "MultiLinearModel"={},
           # "RobustRegressionModel"={},
           # "Naive_Bayes_Model"={},
           # "KNearestNeighborModel"={},
           # "RandomForestModel"={},
           # "LassoandLarsRegressionModel"={}
           )
    
    output$coefName <- renderUI({
      list(
        renderPrint(coefNames)
        )
      })
    
    
    output$indepMeasureNames <- renderUI({
      lapply(1:length(coefNames), function(i){##########display the name of the measures of the model
        list(
          tags$h4(paste0("Independent ",i)),
          textInput(inputId = paste0("name",i), label = paste0("Enter the name of the independent measure ", i), value = coefNames[i]),
          textInput(inputId = paste0("predInput",i), label = paste0("Input a value for the independent measure",i)),
          hr()
          )
        })
      })
    
    output$buttonPred <- renderUI({
      list(
        div(column(1,offset=1), style="display:inline-block", actionButton("prediction", "Predict")),
        div(column(1,offset=4),style="display:inline-block", actionButton("visualize", "Visualize"))
      )
    })
  })
  
  observeEvent(input$prediction,{
    fullIndepValstring <- NULL
    fullIndepValstring <- lapply(1:length(coefNames), function(i){
      inpid <- paste0("predInput",i)
      fullIndepValstring <- c(fullIndepValstring, input[[inpid]])
    })
    matches <- regmatches(fullIndepValstring, gregexpr("[[:digit:]]+\\.*[[:digit:]]*", fullIndepValstring ))
    newIndepVal <- as.data.frame(as.numeric(unlist(matches[[1]])))
    if(length(matches)>1){
      for( i in 2:length(matches)){
        newIndepVal[[i]] <- as.numeric(unlist(matches[[i]]))
      }}
    fullname <- NULL
    fullname <- sapply(1:length(coefNames), function(i){
      inpide <- paste0("name",i)
      fullname <- c(fullname, input[[inpide]])
    })
    names(newIndepVal) <- fullname
    
    p <- predict(x, newdata = newIndepVal)
    modelResults <- t.data.frame(data.frame(newIndepVal, p))
    colnames(modelResults) <- NULL
    rownames(modelResults)[nrow(modelResults)] <- "Result(s)" 
    rownames(modelResults)[1:nrow(modelResults)-1] <- fullname
    
    
    output$preview <- renderPrint(modelResults)
    output$predResults <- renderUI({
      list(
        renderPrint(modelResults)
      )
    })
  })
  
  
  ###########################Jump to Main panel when Go Back is clicked##########################################
  ###############################################################################################################
  observeEvent({c(input$backtoDep2,input$backtoDep3,input$backtoDep1)},{
    updateTabsetPanel(session, "tabselected",selected = "1")
    })
    
  ###########################Jump to Dataset panel when view DS is clicked#######################################
  ###############################################################################################################
  observeEvent(input$viewDS,{
    updateTabsetPanel(session, "tabselected",
                      selected = "3")
  })
 
  ###########################NEXT & PREVIOUS NAVIGATION BUTTON###################################################
  ###############################################################################################################
  lapply(1:5, function(i){
    observeEvent(input[[paste0("prevBtn",i)]],{
      updateTabsetPanel(session, "tabselected", selected = paste0(i-1))
      })
    observeEvent(input[[paste0("nextBtn",i)]],{
      updateTabsetPanel(session, "tabselected", selected = paste0(i+1))
      
      })
    })
})

#################RESET MODELID########@
#storeModelID()


###############################################################################################################
##RECYCLE##
###############################################################################################################
