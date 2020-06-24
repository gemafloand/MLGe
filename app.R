##LearnShiny application

####If you have problems with any of the libraries, run the following code to install them. 

#libraries <- c("shiny", "ggplot2", "caret","tidyverse", "NeuralNetTools","dplyr","wakefield", "kernlab", "MLeval")
#check.libraries <- is.element(libraries, installed.packages()[, 1])==FALSE
#libraries.to.install <- libraries[check.libraries]
#if (length(libraries.to.install!=0)) {
#    install.packages(libraries.to.install)
#}
#success <- sapply(libraries,require, quietly = FALSE,  character.only = TRUE)
#if(length(success) != length(libraries)) {stop("A package failed to return a success in require() function.")}

####If you want to use your own dataset save it in data folder and substitute the name and reading format of your dataset in "datos"


#Load libraries
library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(caret)
library(NeuralNetTools)
library(wakefield)
library(kernlab)
library(MLeval)

#Read dataset
datos<- read.csv("./data/Noccaea3.csv",header=TRUE,  colClasses=c('factor'))
set.seed(12345)
#Define the size of training dataset
inTrain <- createDataPartition(y=datos$X, p=0.66666666666666667, list=FALSE)
#Create training and test data
datos_train<-datos[inTrain,]
datos_test<-datos[-inTrain,]
#Label
class_train<-datos[inTrain,1]
class_test<-datos[-inTrain,1]


#Application
ui <- fluidPage( theme= "bootstrap.css",
                 titlePanel(h1(strong("Algorithm design and visualization tool"), style = "font-family: 'times'; font-size: 60px")
                 ),
             
    
    sidebarLayout(
        
        sidebarPanel(
            h3("Design your algorithm"),
            radioButtons("algorithm",
                         "Select one algorithm to visualize its output",
                         choices= c("Random Forest" = "rf",
                                    "Artificial Neural Network" = "mlp",
                                    "Support Vector Machine" = "svmRadial") ),
            
            radioButtons("resamp", 
                         "Select a resampling method", 
                         choices = c("Cross validation" = "cv", 
                                     "Bootstrap" = "boot") ),
            
            numericInput("num", 
                         "Select a number of resampling events", 
                         value = 5),
            
            checkboxGroupInput("preproc", 
                               "Select one or multiple preprocessing methods", 
                               choices = c("Center" = "center", 
                                           "Scale" = "scale", 
                                           "Box Cox" = "BoxCox")
            )
        ),
        
        
        mainPanel(
            tabsetPanel(type="tabs",
                        tabPanel("Prediction", verbatimTextOutput("model")),
                        tabPanel("Plot", plotOutput("plot")),
                        tabPanel("Model Performance", verbatimTextOutput("matrix"))
                        )
        )
    )
)

server <- function(input, output, session) {
    #Define the model with a reactive function
    modelo <- reactive({
                        train(X~., 
                            data= datos_train, 
                            na.action = na.pass,
                            method=input$algorithm,
                            trControl= trainControl(method=input$resamp, number=input$num, classProbs=TRUE, savePredictions = TRUE),
                            preProc = c(input$preproc))
                        })

    #Define the evaluation of the model with a reactive function
    eval <- reactive({
                      evalm(train(X~., 
                                  data= datos_train, 
                                  na.action = na.pass,
                                  method=input$algorithm,
                                  trControl= trainControl(method=input$resamp, number=input$num, classProbs=TRUE, savePredictions = TRUE),
                                  preProc = c(input$preproc))
                            )
                    })    
        

    #Define the output(s)
    output$model <- renderPrint(modelo())
    
    output$plot <- renderPlot({ eval()$roc })
        
    output$matrix <- renderPrint({ confusionMatrix(predict(modelo(), datos_test), class_test)
                                })
    
    
    }

# Run the application 
shinyApp(ui, server)

