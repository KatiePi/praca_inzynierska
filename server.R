library(shiny)
library(plotKML)
library(shinyjs)
library(RMySQL)
library(dbConnect)
library(R.oo)
library(ggplot2)
library(plyr)
source("~/Analyzer.R")
source("./DbWorker.R")

setGlobalEnv <- function(){
  Sys.setlocale("LC_TIME", "C")
}

renderCharts<-function(input, output, session, dbConnection){
  #create class to store list of .gpx files from database
  dataFrameObj <- setRefClass("dataFrameObj", fields = list(dataFrameValues="data.frame"))
  dataFrameRef <- dataFrameObj$new()
  observeEvent(input$addedFile, {
    # browser()
    addedFile = input$addedFile
    
    if (is.null(addedFile)) {
      return(NULL)
    }
    #inserting data schema
    login <- input$loginValueInput
    filePath <- paste("D:\\\\TemporaryServer\\\\",login,format(Sys.time(), "_%Y%m%d_%H%M%S"),".gpx",sep="")
    #TODO input real training name
    name <- "trainingTest"
    #insert data into table
    getInsertQ <- paste("insert into data(login, filePath, name) values(","'",login,"',","'",filePath,"',","'",name,"')",sep="")
    dbGetQuery(dbConnection, getInsertQ)
    analyzerObj <- analyzer$new(addedFile = addedFile)
    geodf <- analyzerObj$caltulateGPSData()
    
    #chart example 1
    output$examplePlot <- renderPlot({plot(geodf$lon, geodf$lat, ylab = "latitude", xlab = "longitude")}, width = 600, height = 400)
    
    #chart example 2
    output$examplePlot2 <- renderPlot({plot(geodf$speed.km.per.h, type = "l", bty = "n", xaxt = "n", ylab = "Speed (km/h)", xlab = "")
      lines(geodf$lowess.speed, col = "green", lwd = 3)
      legend(x="bottom", legend = c("GPS speed", "Lowess speed"),
             col = c("black", "green"), lwd = c(1,3), bty = "n")}, width = 600, height = 400)
  })
  
  observeEvent(input$sendLoginValue, {
    loginValue = input$loginValueInput
    disable("loginValueInput")
    disable("sendLoginValue")
    getDataFrameQ <- paste("select * from data where login like '",loginValue,"'",sep="")
    dataFrameRef$dataFrameValues <- dbGetQuery(dbConnection, getDataFrameQ)
    
    output$trainingSelect <- renderUI({
      selectInput("dataset", "Wybierz trening do analizy", as.list(dataFrameRef$dataFrameValues$na))
    })
  })
  
  renderUpdateDataBtn(input, output, session, dbConnection)
  
}

renderUpdateDataBtn<-function(input, output, session, dbConnection) {

  
  observeEvent(input$updateDataBtn, {
    cat("User login is: ")
    cat(input$userDataLogin)
    
    #put into another .R file
    browser()
    tmpGarminServer<-"C:/Users/Kasia/Documents/inzynierka/prototypeWebSite"
    newFolder <- paste("C:/Users/Kasia/Documents/inzynierka/temporaryServer/", input$userDataLogin, sep="")
    setwd(tmpGarminServer)
    files <- list.files() 
    sapply(files,FUN=function(eachPath){ 
      file.copy(eachPath, newFolder)
      #tak foreach - wchodzi tyle razy ile plikow
      cat("foreach ")
    }) 
  })
}

renderUserData<-function(input, output, session, dbConnection){
  #TODO validatory poprawnosci przesylanych danych
  
  observeEvent(input$userDataApproveButton, {
    disable("userDataApproveButton")
    disable("userDataEmail")
    disable("userDataPassword")
    disable("userDataLogin")
    disable("userDataAge")
    
    email = input$userDataEmail
    password = input$userDataPassword
    login = input$userDataLogin
    age = input$userDataAge

    setCurrentWorkingDctr<-function(login) {
      mainDir <- "C:/Users/Kasia/Documents/inzynierka/temporaryServer"
      subDir <- login
      browser()
      if(dir.exists(file.path(mainDir, subDir))){
        cat("file almost exists")
      }
      else{
        dir.create(file.path(mainDir, subDir))
      }
      #setwd(file.path(mainDir, subDir))
    }
    
    setCurrentWorkingDctr(login)

    #insert data into table
    checkIfUserJustExist<-function(email) {
      ifExistQ <- paste("select * from user where email like '",email,"'",sep="")
      return (nrow(dbGetQuery(dbConnection, ifExistQ)) > 0)
    }
    
    if(checkIfUserJustExist(email) == FALSE) {
      getInsertQ <- paste("insert into user(email, password, login, age) values(","'",email,"',","'",password,"',","'",login,"',","'",age,"')",sep="")
      dbGetQuery(dbConnection, getInsertQ)
    }
  })
}

renderNews<-function(input, output, session, dbConnection){
  
}

renderStatistisc<-function(input, output, session, dbConnection) {
  
  #TODO - pobierz dane tylko zalogowanego uzytkownika - where id = zalogowanyUzytkownik
  getActivitiesDataQuery <- "select 
                             activity.date,
                             activity.timeLasting,
                             activity.burnCalories,
                             activitytype.name as activityType,
                             activity.userId,
                             activity.distance
                             from activity join activitytype 
                             on activity.activityType = activitytype.idactivityType;"
  
  activitiesData <- dbGetQuery(dbConnection, getActivitiesDataQuery)
  #sort data by activity date
  activitiesData <- with(activitiesData,  activitiesData[order(date) , ])
  #change date format from char to Date
  activitiesData$date <- as.Date(activitiesData$date, "%Y-%m-%d")
  
  output$main_plot <- renderPlot({
    #setting interactive plot properties
    statisticType <- input$statisticType
    analyzedColumn <- activitiesData[,c(input$analyzedValue)]
    barPositioning <- input$barPositioning
    selectLabel <- ({
      switch(input$analyzedValue,
             "timeLasting" = "Time lasting",
             "burnCalories" = "Burn calories",
             "distance" = "Distance")
    })
    
    interactivePlot <- ggplot(activitiesData, aes(x=format(date, statisticType), y=analyzedColumn,
                                                  fill=factor(activityType))) +
      labs(fill = "Activity Type", x="Date", y=selectLabel) +
      geom_bar(stat="identity", position = barPositioning) + scale_fill_manual(breaks = c("RUNNING", "BIKE_RIDING","SWIMMING"),
                                                    labels = c("Running", "Bike riding", "Swimming"),
                                                    values = c("#D55E00", "#E69F00", "#56B4E9"))
    print(interactivePlot)
  })
}

renderPercentiles<-function(input, output, session, dbConnection) {
  
  getActivitiesDataQuery <- "select activity.idActivity,
  activity.pathToFile,
  activity.date,
  activity.name,
  activity.timeLasting,
  activity.burnCalories,
  activity.sharedName,
  activitytype.name as activityType,
  activity.userId,
  activity.distance
  from activity join activitytype 
  on activity.activityType = activitytype.idactivityType;"
  
  getDataFrameQ <- paste(getActivitiesDataQuery,sep="")
  activitiesData <- dbGetQuery(dbConnection, getDataFrameQ)
  
  #sort data by activity date
  activitiesData <- with(activitiesData,  activitiesData[order(date) , ])
  #change date format from char to Date
  activitiesData$date <- as.Date(activitiesData$date, "%Y-%m-%d")
  
  #dorobic przycinanie po roku i dopisac
  activitiesData$date <- cut(activitiesData$date, "months")
  
  newAggregate <- aggregate(activitiesData$burnCalories, 
                            by=list(date=activitiesData$date, userId=activitiesData$userId), 
                            FUN=sum)
  
  #cut data to months
  
  dat_quantiles <- ddply(activitiesData, .(userId, date), summarise,
                         P00=quantile(burnCalories, 0.00), 
                         P25=quantile(burnCalories, 0.25), 
                         P50=quantile(burnCalories, 0.5), 
                         P75=quantile(burnCalories, 0.75),
                         P1=quantile(burnCalories, 1))
  
  total <- merge(activitiesData, dat_quantiles, by='date')
  #need to change to Date type again
  total$date <- as.Date(total$date, "%Y-%m-%d")
  
  dat_quantiles$date <- as.Date(dat_quantiles$date, "%Y-%m-%d")
  
  
  #ggplot(year, aes(x=format(date, "%Y"), y=burnCalories)) + geom_bar(stat="identity", width = 0.1)
  
  #work
  #p <- ggplot(total, aes(x=format(date, "%Y-%b"), y=P50)) + geom_line(stat="identity")
  
  
  #p <- ggplot() + geom_rug(data=total, mapping=aes(y=P50), color="blue")
  #print(p)
  #p <- p + geom_point(data=total, aes(x=format(date, "%Y-%b"), y=P50), colour="blue")
  #print(p)
  
  p <- ggplot() + geom_bar(data=dat_quantiles, aes(x=format(date, "%Y-%b"), y=P50), colour="blue")
  print(p)
  p <- p + abline(total, h=P50, color="blue")
  print(p)
  
  #p <- ggplot(activitiesData, aes(x=format(date, "%Y"), y=burnCalories)) + geom_bar(stat="identity")
  #p <- p + geom_hline(aes(yintercept=quantile(activitiesData$burnCalories, prob = c(0.5))), colour='blue') 
  #p <- p + geom_text(aes(1.1,qTest,label = "Percentile 50", vjust = -1))
  #print(p)
  
  output$main_plot <- renderPlot({
    #setting plot properties
    statisticType <- input$statisticType
    analyzedColumn <- activitiesData[,c(input$analyzedValue)]
    barPositioning <- input$barPositioning
    selectLabel <- ({
      switch(input$analyzedValue,
             "timeLasting" = "Time lasting",
             "burnCalories" = "Burn calories",
             "distance" = "Distance")
    })
    
    interactivePlot <- ggplot(activitiesData, aes(x=factor(date, ordered = T), y=analyzedColumn,
                                                  fill=factor(activityType))) + 
      labs(fill = "Activity Type", x="Date", y=selectLabel) +
      geom_bar(stat="identity", position = barPositioning) + scale_fill_manual(breaks = c("RUNNING", "BIKE_RIDING","SWIMMING"),
                                                                               labels = c("Running", "Bike riding", "Swimming"),
                                                                               values = c("#D55E00", "#E69F00", "#56B4E9"))
    print(interactivePlot)
    
  })
}

function(input, output, session) {
  #create connection to mySQL db
  dbConnection = dbConnect(MySQL(), user='admin', password='admin', dbname='analyzer_db', host='localhost')
  dbConnection2 = dbConnect(MySQL(), user='admin', password='admin', dbname='analyzer_db_2', host='localhost')
  
  setGlobalEnv()
  renderCharts(input, output, session, dbConnection2)
  renderStatistisc(input, output, session, dbConnection2)
  #renderPercentiles(input, output, session, dbConnection2)
  renderUserData(input, output, session, dbConnection2)
  renderNews(input, output, session, dbConnection2)
  
  session$onSessionEnded(function(){
    cat("close connections")
    dbDisconnect(dbConnection)
    dbDisconnect(dbConnection2)
  })
}


