library(shiny)
library(shinyjs)

#1. Problem z dekodowaniem znakow - znaki polskiego
renderSingleTrainingAnalyzer<-function()
{
  fluidRow(
      column(12,
         HTML("<center><strong>ANALYZE SINGLE TRAINING</strong></center>"),
         fluidRow(
            column(2, 
                   #file input
                   fileInput("addedFile", label = "Dodaj trening do analizy", multiple = FALSE, accept = NULL, width = NULL,
                             buttonLabel = "Browse...", placeholder = "No file selected"),
                   hr(),
                   #data actualization
                   HTML("<label>Zaktualizuj dane automatycznie</label>"),
                   actionButton("updateDataBtn", "Aktualizuj"),
                   br(),br(),
                   hr(),
                   #input single training to analyze
                   uiOutput("selectSingleTraining"),
                   br(),
                   sliderInput("speedLowessScale", "Lowess value", 0.01, 1, 0.01, step = 0.01)
            ),
            column(5,
                   h4("Route on the map"),
                   plotOutput("examplePlot", inline = TRUE)
                   
            ),
            column(5,
                   h4("Speed"),
                   plotOutput("examplePlot2", inline = TRUE)
            )
      ),
      HTML("<center><strong>ANALYZE TWO TRAININGS</strong></center>"),
      fluidRow(
        HTML("<label>Compare two trainings</label>"),
        uiOutput("trainingSelect"),
        #get this into one row next to other
        selectInput("AxisY", "Axis Y:",
                    c("Time percent" = "timePercent",
                      "Distance percent" = "distancePercent"),
                    selected = "timePercent"),
        selectInput("AxisX", "Axis X:",
                    c("Speed" = "speedKmPerH",
                      "Rate" = "rate"),
                    selected = "speedKmPerH"),
        plotOutput(outputId = "percentPlot")
      )
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
           HTML("<label>LOG IN</label>"),
           hr(),
           textInput("userDataLoginLog", "Podaj login"),
           textInput("userDataPasswordLog", "Podaj haslo"),
           actionButton("userDataApproveButtonLog", "Zatwierdz"),
           br(),br(),
           htmlOutput("loginProcessMessage"),
           hr()
    ),
    column(4
    ),
    column(4,
           HTML("<label>REGISTRATION</label>"),
           hr(),
           textInput("userDataEmailReg", "Podaj e-mail"),
           textInput("userDataPasswordReg", "Podaj haslo"),
           textInput("userDataLoginReg", "Podaj login"),
           textInput("userDataAgeReg", "Podaj wiek"),
           actionButton("userDataApproveButtonReg", "Zatwierdz"),
           br(),br(),
           htmlOutput("registrationProcessMessage"),
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

