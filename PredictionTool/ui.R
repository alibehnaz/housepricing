#########################This is the server FILE#######@#########################
options(shiny.sanitize.errors = FALSE)
library(shinyBS)
library(shinythemes)
library(shiny)
library(plotly)


fluidPage(theme = shinytheme("united"),  ##adjust to screen 
          navbarPage(title='Generic Prediction Tool',
            tabPanel(title = "Create a Model",
                     sidebarLayout(
                       sidebarPanel(
                         ##############CHOOSE CONCEPT SIDEBAR##############
                         conditionalPanel(condition="input.tabselected==1", 
                                          titlePanel("Select Concept and a Dependent Variable"),
                                          br(),
                                          uiOutput("theConceptList"),
                                          uiOutput("depVar"),
                                          bsButton("apply", label = " Apply ", type = "action", disabled = T),
                                          conditionalPanel(condition = "input.apply!=0",
                                                           br(),
                                                           br(),
                                                           div(style="display:inline-block", actionButton("nextBtn1", "Next"))
                                                           )
                                          ),
          
                         ##############Select Indep Variable(s) SIDEBAR##############
                         conditionalPanel(condition="input.tabselected==2",
                                          titlePanel("Select Independent Variable(s)"),
                                          br(),
                                          uiOutput("BmodelSideB"),
                                          #uiOutput('test'),
                                          conditionalPanel(condition = "input.apply!=0",
                                                           br(),
                                                           br(),
                                                           div(column(1,offset=1), style="display:inline-block", actionButton("prevBtn2", "Previous")),
                                                           div(column(1,offset=4),style="display:inline-block", actionButton("nextBtn2", "Next"))
                                                           )
                                          ),
                    
                    
                        ##############CHOOSE DATASET SIDEBAR##############
                        conditionalPanel(condition="input.tabselected==3",
                                         titlePanel("Select Dataset"),
                                         br(),
                                         uiOutput("DSSideB"),
                                         uiOutput("oki"),
                                         conditionalPanel(condition = "input.apply!=0",
                                                          br(),
                                                          br(),
                                                          div(column(1,offset=1),style="display:inline-block", actionButton("prevBtn3", "Previous")),
                                                          div(column(1,offset=4),style="display:inline-block", actionButton("nextBtn3", "Next"))
                                                          )
                                         ),
                    
                        ##############Select Model Type and Build Model SIDEBAR######
                        conditionalPanel(condition="input.tabselected==4",
                                         titlePanel("Select Model Type and Create Model"),
                                         br(),
                                         uiOutput("modelType"),
                                         conditionalPanel(condition = "input.apply!=0",
                                                          br(),
                                                          br(),
                                                          div(column(1,offset=1),style="display:inline-block", actionButton("prevBtn4", "Previous")),
                                                          div(column(1,offset=4),style="display:inline-block", actionButton("nextBtn4", "Next"))
                                                          )
                                         )
                        ),
                       mainPanel(
                         tabsetPanel( id ="tabselected",
                                      tabPanel(HTML(paste("Select Concept and", "Dependent Variable", sep = "<br/>")), value = 1, 
                                               verbatimTextOutput("NoDep")                                               ),
                               
                                      tabPanel(HTML(paste("Select Indepedent Variable(s)", sep = "<br/>")), value = 2,
                                               conditionalPanel(condition="input.apply==0", 
                                                                helpText("Please select a dependent variable first"),
                                                                actionButton("backtoDep1","Select a Dependent Var")), 
                                               uiOutput("ShowModelContainer")
                                              ),
                                     
                                      tabPanel(HTML(paste("Select Training Dataset", sep = "<br/>")), value = 3,
                                               conditionalPanel(condition="input.apply==0", 
                                                                helpText("Please select a dependent variable first"),
                                                                actionButton("backtoDep2","Select a Dependent Variable")), 
                                               conditionalPanel(condition="input.apply!=0", uiOutput("DsChoiceField"),uiOutput('DsChoiceField2'))
                                               ), 
                                     
                                      tabPanel(HTML(paste("Select Model Type","and Create Model", sep = "<br/>")), value = 4,
                                               conditionalPanel(condition="input.apply==0", 
                                                                helpText("Please select a dependent variable first"),
                                                                actionButton("backtoDep3","Select a Dependent Var")), 
                                               uiOutput("preview") 
                                               )
                                      )
                         )
                       )),
            tabPanel(title ="Use Model",
                     sidebarLayout(
                       sidebarPanel(titlePanel("Use a model"),
                                br(),
                                uiOutput("UseModelSideB"),
                                br(),
                        
                                #hr(),
                                br(),
                                #fileInput("fileRDS", "Input your model", accept = c(".rds")),
                                conditionalPanel(condition = "input.apply!=0",
                                                 br(),
                                                 br(),
                                                 div(style="display:inline-block", actionButton("prevBtn5", "Previous"))
                                                 )
                                ),
                   mainPanel(
                     uiOutput("UseModz"),
                     #uiOutput("khara"),
                     uiOutput("SummaryContent")
                     )
                   )
                 ),
        tabPanel(title="Input Your Model",
                 sidebarLayout(
                   sidebarPanel(title='Steps to do',
                                uiOutput("selectModelTypeContainer"),
                                br(),
                                tipify(fileInput("fileRDS", "Input your model"),"The file extention must be .rds and contain only your model",
                                       placement = 'right', trigger = 'hover',options = list(container = "body")),
                                br(),
                                tipify(uiOutput("indepMeasureNames"), 
                                       "The name have to match with the one used in the training dataset", 
                                       placement = 'right', trigger = 'hover',options = list(container = "body")),
                                br(),
                                
                                uiOutput("buttonPred")
                                
                                
                   ),
                   mainPanel(
                     uiOutput("filedf"),
                     uiOutput("filedf2"),
                     uiOutput("coefName"),
                     uiOutput("predResults")
                   )
                 )
        )
))