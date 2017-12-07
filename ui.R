library(shiny)
library(shinyjs)

#1. Problem z dekodowaniem znakow - znaki polskiego
renderSingleTrainingAnalyzer<-function()
{
  fluidRow(
    column(4,
           #login input
           textInput("loginValueInput", "Podaj swoj login"),
           actionButton("sendLoginValue", "Zatwierdz"),
           hr(),
           #file input
           fileInput("addedFile", label = "Dodaj trening do analizy", multiple = FALSE, accept = NULL, width = NULL,
                     buttonLabel = "Browse...", placeholder = "No file selected"),
           hr(),
           #data actualization
           HTML("<label>Zaktualizuj dane automatycznie</label>"),
           actionButton("updateDataBtn", "Aktualizuj"),
           hr(),
           #input trainings to analyze
           uiOutput("trainingSelect"),
           br(),
           h6("lista dodanych treningow do analizy ..."),
           sliderInput("speedLowessScale", "Lowess value", 0.01, 1, 0.01, step = 0.01)
    ),
    column(8,
           h4("Example 1"),
           plotOutput("examplePlot", inline = TRUE),
           h4("Example 2"),
           plotOutput("examplePlot2", inline = TRUE)
    )
  )
}

renderStatistics<-function(){
  fluidRow(
    selectInput("statisticType", "Statistic type:",
                c("Monthly" = "%Y-%m",
                  "Yearly" = "%Y"),
                selected = "%Y-%m"),
    selectInput("analyzedValue", "Variable to anayze:",
                c("Time lasting" = "timeLasting",
                  "Burn calories" = "burnCalories",
                  "Distance" = "distance"),
                selected = "timeLasting"),
    selectInput("barPositioning", "Bars positioning:",
                c("Stack" = "stack",
                  "Dodge" = "dodge"),
                selected = "stack"),
    plotOutput(outputId = "statisticPlot")
  )
}

renderUserData<-function(){
  fluidRow(
    column(4,
           textInput("userDataEmail", "Podaj e-mail"),
           textInput("userDataPassword", "Podaj haslo"),
           textInput("userDataLogin", "Podaj login"),
           textInput("userDataAge", "Podaj wiek"),
           actionButton("userDataApproveButton", "Zatwierdz"),
           hr()
    )
  )
}

renderComparisionSection<-function(){
  fluidRow(
    selectInput("percentileDateType", "Date scale:",
                c("Monthly" = "%Y-%m",
                  "Yearly" = "%Y"),
                selected = "%Y-%m"),
    selectInput("percentileAnalyzedValue", "Variable to anayze:",
                c("Time lasting" = "timeLasting",
                  "Burn calories" = "burnCalories",
                  "Distance" = "distance"),
                selected = "burnCalories"),
    selectInput("percentileValue", "Percentile value:",
                c("0" = "0",
                  "0.25" = "0.25",
                  "0.5" = "0.5",
                  "0.75" = "0.75",
                  "1" = "1"),
                selected = "0.5"),
    plotOutput(outputId = "percentilePlot")
  )
}

fluidPage(
  useShinyjs(),
  style = "background-color: #B0E0E6;",
  navbarPage("Training analyzer",
                     tabPanel("User data",
                              renderUserData()),
                     tabPanel("Analyze single training",
                              renderSingleTrainingAnalyzer()
                              ),
                     tabPanel("Statistics",
                              renderStatistics()
                     ),
                     tabPanel("Compare yourself to others",
                              renderComparisionSection())
  )
)



# Call the function with argument `n`

