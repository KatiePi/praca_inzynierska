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

renderSingleTrainingCharts<-function(input, output, session, dbConnection){
  
  dbConnection <- dbConnection 
  
  observeEvent(input$addedFile, { 
    addedFile = input$addedFile
     if (is.null(addedFile)) {
      return(NULL)
     }
    # Object from Analyzer.R
    analyzerObj <- analyzer$new(addedFile = addedFile)
    gpxAsDataFrame <- analyzerObj$caltulateGPSData()
    
    if(!(is.data.frame(gpxAsDataFrame) && nrow(gpxAsDataFrame)==0)) {
      #chart example 1
      output$examplePlot <- renderPlot({plot(gpxAsDataFrame$longitude, gpxAsDataFrame$latitude,
                                             ylab = "latitude", xlab = "longitude")}, width = 600, height = 400)
      
      observeEvent(input$speedLowessScale, { 
        print("i am here speedLowessScale")
        #chart example 2
        speedLowessScale = input$speedLowessScale
        gpxAsDataFrame$lowess.speed <- lowess(gpxAsDataFrame$speed.km.per.h, f = speedLowessScale)$y
        # mozna dorobic z wysokoscia nad p morza
        #gpxAsDataFrame$lowess.elevation <- lowess(gpxAsDataFrame$ele, f = 0.2)$y
        output$examplePlot2 <- renderPlot({plot(gpxAsDataFrame$speed.km.per.h, type = "l", bty = "n", xaxt = "n", ylab = "Speed (km/h)", xlab = "")
        lines(gpxAsDataFrame$lowess.speed, col = "green", lwd = 3)
        legend(x="bottom", legend = c("GPS speed", "Lowess speed"),
             col = c("black", "green"), lwd = c(1,3), bty = "n")}, width = 600, height = 400)
      })
    }
  })
}


# renderSingleTrainingCharts<-function(input, output, session, dbConnection){
#   #create class to store list of .gpx files from database
#   dataFrameObj <- setRefClass("dataFrameObj", fields = list(dataFrameValues="data.frame"))
#   gpxAsDataFrameClass <- setRefClass("gpxAsDataFrame", fields = list(values="data.frame"))
#   
#   dataFrameRef <- dataFrameObj$new()
#   gpxAsDataFrame <- gpxAsDataFrameClass$new()
#   
#   observeEvent(input$addedFile, {
#     
#     addedFile = input$addedFile
#     if (is.null(addedFile)) {
#       return(NULL)
#     }
#     #inserting data schema
#     #login <- input$loginValueInput
#     #filePath <- paste("D:\\\\TemporaryServer\\\\",login,format(Sys.time(), "_%Y%m%d_%H%M%S"),".gpx",sep="")
#     #TODO input real training name
#     #name <- "trainingTest"
#     #insert data into table
#     ##getInsertQ <- paste("insert into data(login, filePath, name) values(","'",login,"',","'",filePath,"',","'",name,"')",sep="")
#     ##dbGetQuery(dbConnection, getInsertQ)
#     analyzerObj <- analyzer$new(addedFile = addedFile)
#     gpxAsDataFrame$values <- analyzerObj$caltulateGPSData
#     
#     gpxValues <- gpxAsDataFrame$values
#     
#     #chart example 1
#     output$examplePlot <- renderPlot({plot(gpxValues$longitude, gpxValues$latitude, 
#                                            ylab = "latitude", xlab = "longitude")}, width = 600, height = 400)
#     
#     ## f = 0.04 need to be changed dinamicly
#     lowessScale = input$speedLowessScale
#     gpxValues$lowess.speed <- lowess(gpxValues$speed.km.per.h, f = lowessScale)$y
#     
#     #chart example 2
#     #output$examplePlot2 <- renderPlot({plot(gpxValues$speed.km.per.h, type = "l", bty = "n", xaxt = "n", ylab = "Speed (km/h)", xlab = "")
#      # lines(gpxValues$lowess.speed, col = "green", lwd = 3)
#       #legend(x="bottom", legend = c("GPS speed", "Lowess speed"),
#        #      col = c("black", "green"), lwd = c(1,3), bty = "n")}, width = 600, height = 400)
#   })
#   
#   observeEvent(input$speedLowessScale, {
#     ## f = 0.04 need to be changed dinamicly
#     lowessScale = input$speedLowessScale
#     if(!is.null(gpxAsDataFrame$values)) {
#       gpxValues <- gpxAsDataFrame$values
#       
#       gpxValues$lowess.speed <- lowess(gpxValues$speed.km.per.h, f = lowessScale)$y
#       
#       #chart example 2
#       output$examplePlot2 <- renderPlot({plot(gpxValues$speed.km.per.h, type = "l", bty = "n", xaxt = "n", ylab = "Speed (km/h)", xlab = "")
#         lines(gpxValues$lowess.speed, col = "green", lwd = 3)
#         legend(x="bottom", legend = c("GPS speed", "Lowess speed"),
#                col = c("black", "green"), lwd = c(1,3), bty = "n")}, width = 600, height = 400)
#     }
# 
#   })
  
#   observeEvent(input$sendLoginValue, {
#     loginValue = input$loginValueInput
#     disable("loginValueInput")
#     disable("sendLoginValue")
#     getDataFrameQ <- paste("select * from data where login like '",loginValue,"'",sep="")
#     dataFrameRef$dataFrameValues <- dbGetQuery(dbConnection, getDataFrameQ)
#     
#     output$trainingSelect <- renderUI({
#       selectInput("dataset", "Wybierz trening do analizy", as.list(dataFrameRef$dataFrameValues$na))
#     })
#   })
#   
#   renderUpdateDataBtn(input, output, session, dbConnection)
#   
# }

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
  
  output$statisticPlot <- renderPlot({
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
  
  #TODO - pobierz dane tylko zalogowanego uzytkownika - where id = zalogowanyUzytkownik
  #TODO- dodaC do analizy wiek uZytkownika - jako percentyl np
  getActivitiesDataQuery <- "select
                            activity.date,
                            activity.timeLasting,
                            activity.burnCalories,
                            activity.userId,
                            activity.distance
                            from activity ;"

  activitiesData <- dbGetQuery(dbConnection, getActivitiesDataQuery)
  #sort data by activity date
  activitiesData <- with(activitiesData,  activitiesData[order(date) , ])
  #change date format from char to Date
  activitiesData$date <- as.Date(activitiesData$date, "%Y-%m-%d")
  
  getJoinedData <- function(dateType, percentileValue) {
    activitiesData$date <- cut(activitiesData$date, dateType)
    dataToCaltulatePercentile <- setNames(aggregate(list(activitiesData$burnCalories, activitiesData$timeLasting, activitiesData$distance),
                                                    by=list(activitiesData$date, activitiesData$userId), 
                                                    FUN=sum), c("date", "userId", "burnCalories", "timeLasting", "distance"))
    
    #change userId to input$userId
    currentUserData <- subset(dataToCaltulatePercentile, userId == 10)

    switch(percentileValue,
           "0" = {    
             burnCaloriesPercentile <- ddply(dataToCaltulatePercentile, .(date), summarise,
                                             burnCalories=quantile(burnCalories, 0))
             timeLastingPercentile <- ddply(dataToCaltulatePercentile, .(date), summarise,
                                            timeLasting=quantile(timeLasting, 0))
             distancePercentile <- ddply(dataToCaltulatePercentile, .(date), summarise,
                                         distance=quantile(distance, 0))},
           "0.25" = {    
             burnCaloriesPercentile <- ddply(dataToCaltulatePercentile, .(date), summarise,
                                             burnCalories=quantile(burnCalories, 0.25))
             timeLastingPercentile <- ddply(dataToCaltulatePercentile, .(date), summarise,
                                            timeLasting=quantile(timeLasting, 0.25))
             distancePercentile <- ddply(dataToCaltulatePercentile, .(date), summarise,
                                         distance=quantile(distance, 0.25))},
           "0.5" = {    
                burnCaloriesPercentile <- ddply(dataToCaltulatePercentile, .(date), summarise,
                                                        burnCalories=quantile(burnCalories, 0.5))
                timeLastingPercentile <- ddply(dataToCaltulatePercentile, .(date), summarise,
                                            timeLasting=quantile(timeLasting, 0.5))
                distancePercentile <- ddply(dataToCaltulatePercentile, .(date), summarise,
                                         distance=quantile(distance, 0.5))},
           "0.75" = {    
             burnCaloriesPercentile <- ddply(dataToCaltulatePercentile, .(date), summarise,
                                             burnCalories=quantile(burnCalories, 0.75))
             timeLastingPercentile <- ddply(dataToCaltulatePercentile, .(date), summarise,
                                            timeLasting=quantile(timeLasting, 0.75))
             distancePercentile <- ddply(dataToCaltulatePercentile, .(date), summarise,
                                         distance=quantile(distance, 0.75))},
           "1" = {    
             burnCaloriesPercentile <- ddply(dataToCaltulatePercentile, .(date), summarise,
                                             burnCalories=quantile(burnCalories, 1))
             timeLastingPercentile <- ddply(dataToCaltulatePercentile, .(date), summarise,
                                            timeLasting=quantile(timeLasting, 1))
             distancePercentile <- ddply(dataToCaltulatePercentile, .(date), summarise,
                                         distance=quantile(distance, 1))}
    )
    percentilesGroup <- join_all(list(burnCaloriesPercentile,timeLastingPercentile,distancePercentile), by="date")
    
    #get percentiles connected just with current user
    percentilesGroup <- percentilesGroup[(percentilesGroup$date %in% currentUserData$date),]
    percentilesGroup$date <- as.Date(percentilesGroup$date, "%Y-%m-%d")
    currentUserData$date <- as.Date(currentUserData$date, "%Y-%m-%d")
    
    #join user and percentile data into one data frame
    currentUserData$name <- "currentUserData"
    percentilesGroup$name <- "percentilesGroup"
    common_cols <- intersect(colnames(currentUserData), colnames(percentilesGroup))
    joinedData <- rbind(
      currentUserData[common_cols], 
      percentilesGroup[common_cols]
    )
    return (joinedData)
  }

  output$percentilePlot <- renderPlot({
    #setting plot properties
    dateType <- input$percentileDateType
    percentileType <- ({
      switch(input$percentileDateType,
             "%Y-%m" = "months",
             "%Y" = "years")
    })
    percentileValue <- input$percentileValue
    joinedData <- getJoinedData(percentileType, percentileValue)
    analyzedColumn <- joinedData[,c(input$percentileAnalyzedValue)]
    selectLabel <- ({
      switch(input$percentileAnalyzedValue,
            "timeLasting" = "Time lasting",
            "burnCalories" = "Burn calories",
            "distance" = "Distance")
    })
    
    p <- ggplot(joinedData, aes(format(date, dateType), analyzedColumn, fill = name)) + 
      geom_bar(position = "dodge", stat="identity") +
      labs(fill = "Data Type", x="Date", y=selectLabel) +
      scale_fill_manual(breaks = c("currentUserData", "percentilesGroup"),
                          labels = c("Yours data", paste("Percentile ", percentileValue ,sep="")),
                          values = c("#D55E00", "#E69F00"))
      print(p)
  })
}

function(input, output, session) {
  #create connection to mySQL db
  dbConnection = dbConnect(MySQL(), user='admin', password='admin', dbname='analyzer_db', host='localhost')
  dbConnection2 = dbConnect(MySQL(), user='admin', password='admin', dbname='analyzer_db_2', host='localhost')
  
  setGlobalEnv()
  renderSingleTrainingCharts(input, output, session, dbConnection2)
  renderStatistisc(input, output, session, dbConnection2)
  renderPercentiles(input, output, session, dbConnection2)
  renderUserData(input, output, session, dbConnection2)
  renderNews(input, output, session, dbConnection2)
  
  session$onSessionEnded(function(){
    cat("close connections")
    dbDisconnect(dbConnection)
    dbDisconnect(dbConnection2)
  })
}


