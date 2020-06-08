library(shiny)
library(dygraphs)
shinyUI(fluidPage(
    titlePanel("Stock Prediction - using Fb.Prophet package"),
        sidebarLayout(
        sidebarPanel(textInput("symb", "Enter Valid Stock Symbol:", value = "", width = NULL, placeholder = NULL),
                     helpText("You can find symbols on 'Yahoo Finance' or use 'AMZN', 'AAPL', 'TTM' etc."),
                     radioButtons(inputId = "ptype", label= h4("Select from the Options below"),
                                 choices = list("Historical Data till Dec 2017" = 1, 
                                                "Run Fb.Prophet Prediction Algo: aprox 15 sec compute" = 2,
                                                "Compare prediction with Actual: aprox 15 sec compute" = 3),
                                 selected = 1),
                     submitButton("Submit"),
                     verbatimTextOutput("value")
        ),
        mainPanel(textOutput("text1"),textOutput("text2"),dygraphOutput("dygraph")))
)
)
