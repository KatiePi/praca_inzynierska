library(shiny)
library(shinyjs)
Sys.setlocale("LC_ALL", "Polish")

#1. Problem z dekodowaniem znakow - znaki polskiego
renderSingleTrainingAnalyzer<-function()
{
  fluidRow(
      column(12,
         tags$div(HTML("<center><strong>ANALYZE SINGLE TRAINING</strong></center>"), style = "background-color: rgba(0,0,0,0.2); font-family: Verdana"),
         fluidRow(
            column(2, 
                   #file input
                   fileInput("addedFile", label = "Add training to analyse", multiple = FALSE, accept = NULL, width = NULL,
                             buttonLabel = "Browse...", placeholder = "No file selected"),
                   hr(),
                   #data actualization
                   HTML("<label>Update data automatically</label>"),
                   br(),
                   actionButton("updateDataBtn", "Update"),
                   br(),br(),
                   hr(),
                   #input single training to analyze
                   uiOutput("selectSingleTraining"),
                   br(),
                   sliderInput("zoomScale", "Zooming", 3, 21, 13, step = 1),
                   br(),
                   sliderInput("speedLowessScale", "Lowess value", 0.01, 1, 0.01, step = 0.01)
            ),
            column(5, align="center",
                   HTML("<center><strong>Route on the map</strong></center>"),
                   plotOutput("examplePlot", inline = TRUE)
                   
            ),
            column(5, align="center",
                   HTML("<center><strong>Speed</strong></center>"),
                   plotOutput("examplePlot2", inline = TRUE)
            )
      ),
      tags$div(HTML("<center><strong>COMPARE TWO TRAININGS</strong></center>"), style = "background-color: rgba(0,0,0,0.2); font-family: Verdana"),
      fluidRow(
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
    tags$div(HTML("<center><strong>YOUR STATISTICS</strong></center>"), style = "background-color: rgba(0,0,0,0.2); font-family: Verdana"),
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
    plotOutput(outputId = "statisticPlot"),
    tags$div(HTML("<center><strong>COMPARE YOUR SPEED/RATE</strong></center>"), style = "background-color: rgba(0,0,0,0.2); font-family: Verdana"),
    selectInput("activityType", "Activity type:",
                c("Running" = "RUNNING",
                  "Riding" = "BIKE_RIDING",
                  "Swimming" = "SWIMMING"),
                selected = "RUNNING"),
    selectInput("dataType", "Data type:",
                c("Month" = "MONTH",
                  "Year" = "YEAR"),
                selected = "MONTH"),
    uiOutput("selectedDates"),
    selectInput("AxisYStatistics", "Axis Y:",
                c("Time percent" = "timePercent",
                  "Distance percent" = "distancePercent"),
                selected = "timePercent"),
    selectInput("AxisXStatistics", "Axis X:",
                c("Speed" = "speedKmPerH",
                  "Rate" = "rate"),
                selected = "speedKmPerH"),
    plotOutput(outputId = "statisticSpeedRate")
  )
}

renderUserData<-function(){
  fluidRow(
    column(3,align="center",
           tags$div(HTML("<label>LOG IN</label>"), style = "background-color: rgba(0,0,0,0.2); font-family: Verdana"),
           hr(),
           textInput("userDataLoginLog", "Login"),
           passwordInput("userDataPasswordLog", "Password"),
           actionButton("userDataApproveButtonLog", "Send"),
           br(),br(),
           htmlOutput("loginProcessMessage"),
           hr()
    ),
    column(3, align="center",
           tags$div(HTML("<label>REGISTRATION</label>"), style = "background-color: rgba(0,0,0,0.2); font-family: Verdana"),
           hr(),
           textInput("userDataEmailReg", "E-mail"),
           passwordInput("userDataPasswordReg", "Password"),
           textInput("userDataLoginReg", "Login"),
           textInput("userDataAgeReg", "Age"),
           actionButton("userDataApproveButtonReg", "Send"),
           br(),br(),
           htmlOutput("registrationProcessMessage"),
           hr(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
           br(),br(),br(),br(),br(),br(), br(),br(),br(),br(),br(),br()
    )
  )
}

renderComparisionSection<-function(){
  fluidRow(
    tags$div(HTML("<center><strong>PERCENTILE CHART</strong></center>"), style = "background-color: rgba(0,0,0,0.2); font-family: Verdana"),
    selectInput("percentileActivityType", "Activity type:",
                c("Running" = "RUNNING",
                  "Bike riding" = "BIKE_RIDING",
                  "Swimming" = "SWIMMING"),
                selected = "RUNNING"),
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
    plotOutput(outputId = "percentilePlot"),
    
    tags$div(HTML("<center><strong>COMPARE YOUR SPEED/RATE WITH OTHER WITHIN MONTH/YEAR</strong></center>"), style = "background-color: rgba(0,0,0,0.2); font-family: Verdana"),
    selectInput("activityTypePercentile", "Activity type:",
                c("Running" = "RUNNING",
                  "Riding" = "BIKE_RIDING",
                  "Swimming" = "SWIMMING"),
                selected = "RUNNING"),
    selectInput("dataTypePercentile", "Data type:",
                c("Month" = "MONTH",
                  "Year" = "YEAR"),
                selected = "MONTH"),
    uiOutput("selectedSingleDate"),
    selectInput("AxisYPercentile", "Axis Y:",
                c("Time percent" = "timePercent",
                  "Distance percent" = "distancePercent"),
                selected = "timePercent"),
    selectInput("AxisXPercentile", "Axis X:",
                c("Speed" = "speedKmPerH",
                  "Rate" = "rate"),
                selected = "speedKmPerH"),
    plotOutput(outputId = "percentileSpeedRate")
  )
}

fluidPage(
  useShinyjs(),
  
  # 
  # style = "background-color: #B0E0E6;",
  style = "background: url(http://www.ahealthy.us/wp-content/uploads/2014/01/exercising-and-hypertension-ahealthyus.jpg);",
  
  navbarPage(inverse=TRUE, "Training analyzer",
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

