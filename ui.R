library(shiny)
library(shinyjs)

#1. Problem z dekodowaniem znakow - znaki polskiego
renderCharts<-function()
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
           h6("lista dodanych treningow do analizy ...")
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
                c("Monthly" = "%Y-%b",
                  "Yearly" = "%Y"),
                selected = "Monthly"),
    selectInput("analyzedValue", "Variable to anayze:",
                c("Time lasting" = "timeLasting",
                  "Burn calories" = "burnCalories",
                  "Distance" = "distance"),
                selected = "Time lasting"),
    selectInput("barPositioning", "Bars positioning:",
                c("Stack" = "stack",
                  "Dodge" = "dodge"),
                selected = "Stack"),
    plotOutput(outputId = "main_plot")
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

renderNews<-function(){
  
}

fluidPage(
  useShinyjs(),
  style = "background-color: #B0E0E6;",
  navbarPage("Training analyzer",
                     tabPanel("User data",
                              renderUserData()),
                     tabPanel("Charts",
                              renderCharts()
                              ),
                     tabPanel("Statistics",
                              renderStatistics()
                     ),
                     tabPanel("News",
                              renderNews())
  )
)



# Call the function with argument `n`

