library(shiny)
library(plotKML)
library(shinyjs)
library(RMySQL)
library(dbConnect)
library(R.oo)
library(ggplot2)
library(plyr)
library(data.table)
source("~/Analyzer.R")
source("./DbWorker.R")

setGlobalEnv <- function(){
  Sys.setlocale("LC_TIME", "C")
}

USER_LOGIN.env <- new.env()
USER_LOGIN.env$var <- ""

makeLoginRegistrationFieldEnable <- function () {
  enable("userDataApproveButtonReg")
  enable("userDataEmailReg")
  enable("userDataPasswordReg")
  enable("userDataLoginReg")
  enable("userDataAgeReg")
  enable("userDataApproveButtonLog")
  enable("userDataLoginLog")
  enable("userDataPasswordLog")
}

makeLoginRegistrationFieldDisable <- function () {
  disable("userDataApproveButtonReg")
  disable("userDataEmailReg")
  disable("userDataPasswordReg")
  disable("userDataLoginReg")
  disable("userDataAgeReg")
  disable("userDataApproveButtonLog")
  disable("userDataLoginLog")
  disable("userDataPasswordLog")
}

renderSingleTrainingCharts<-function(input, output, session, dbConnection){
  
  userLogin <- USER_LOGIN.env$var
  
  renderCharts <- function(gpxAsDataFrame) {
    observeEvent(input$speedLowessScale, { 
      speedLowessScale = input$speedLowessScale
      gpxAsDataFrame$lowess.speed <- lowess(gpxAsDataFrame$speedKmPerH, f = speedLowessScale)$y
      
      gpxAsDataFrame$mean <- with(gpxAsDataFrame, mean(lowess.speed))
      #caltulate if speed is above or below mean
      gpxAsDataFrame$speedComparison <- 0
      gpxAsDataFrame <- transform(gpxAsDataFrame, speedComparison = ifelse(lowess.speed > mean, "ABOVE", "BELOW"))
      # mozna dorobic z wysokoscia nad p morza
      #gpxAsDataFrame$lowess.elevation <- lowess(gpxAsDataFrame$ele, f = 0.2)$y
      output$examplePlot <- renderPlot({plot(gpxAsDataFrame$longitude, gpxAsDataFrame$latitude,
                                             col = c("green","red")[factor(gpxAsDataFrame$speedComparison)],
                                             ylab = "latitude", xlab = "longitude")},
                                       width = 600, height = 400)
      
      output$examplePlot2 <- renderPlot({plot(gpxAsDataFrame$speedKmPerH, type = "l", bty = "n", xaxt = "n", ylab = "Speed (km/h)", xlab = "")
        lines(gpxAsDataFrame$lowess.speed, col = "green", lwd = 3)
        legend(x="bottom", legend = c("GPS speed", "Lowess speed"),
               col = c("black", "green"), lwd = c(1,3), bty = "n")}, width = 600, height = 400)
    })
  }

  observeEvent(input$addedFile, { 
    addedFile = input$addedFile
     if (is.null(addedFile)) {
      return(NULL)
     }
    renderChooseTrainingToAnalyse()
    renderStatistisc(input, output, session, dbConnection)
    renderPercentiles(input, output, session, dbConnection)
    
    if(!(is.data.frame(gpxAsDataFrame) && nrow(gpxAsDataFrame)==0)) {
      renderCharts(gpxAsDataFrame)
    }
  })

  renderChooseTrainingToAnalyse <- function () {
    getTrainingsDataQuery <- paste("select distinct
                                   CONCAT(activity.name, ', ', activity.date) as fullName,
                                   activity.name
                                   from activity
                                   join user on activity.userId = user.iduser
                                   where user.login like '", userLogin, "'", sep ="")
    
    trainings <- dbGetQuery(dbConnection, getTrainingsDataQuery)
    
    if(is.data.frame(trainings) && nrow(trainings)!=0) {
      
      output$trainingSelect <- renderUI({
        selectInput("choosenTraining", "Choose trening to analyse", as.list(trainings$fullName), 
                    selected = NULL, multiple = TRUE)
      })
      
      output$selectSingleTraining <- renderUI({
        selectInput("selectedSingleTraining", "Choose single training to analyse", as.list(trainings$fullName), 
                    selected = NULL, multiple = FALSE)
      })
    }
  }
  renderChooseTrainingToAnalyse()
  
  observeEvent(input$updateDataBtn, {
    userDataLogin <- USER_LOGIN.env$var
    #put into another .R file
    pathToFolderWithUserActivities <- paste("C:/Users/Kasia/Documents/inzynierka/temporaryServer/", userDataLogin, sep="")
    #take me to place where my folder is
    setwd(pathToFolderWithUserActivities)
    files <- list.files()
    n <- 1
    sapply(files,FUN=function(singleFile){ 
      singleFileName <- singleFile[n]
      singleFilePath <- paste(pathToFolderWithUserActivities, "/", singleFileName, sep="")
      singleFileDF <- data.frame(name = singleFileName, datapath = singleFilePath)
      analyzerObj <- analyzer$new(addedFile = singleFileDF, userLogin = userDataLogin)
      gpxAsDataFrame <- analyzerObj$caltulateGPSData()
      cat("foreach ")
      n <- n + 1
    })
    do.call(file.remove, list(files))
    renderStatistisc(input, output, session, dbConnection)
    renderPercentiles(input, output, session, dbConnection)
    renderChooseTrainingToAnalyse()
  })
  
  observeEvent(input$selectedSingleTraining, {
    if(!is.null(input$selectedSingleTraining)) {
      query <- paste("select gpxdata.latitude,
                   gpxdata.longitude,
                     gpxdata.distToNextP,
                     gpxdata.timeToNextP,
                     gpxdata.speedKmPerH,
                     activity.name
                     from gpxdata join activity on gpxdata.idActivity = activity.idActivity
                     join user on user.iduser = activity.userId
                     where user.login like '", USER_LOGIN.env$var, "'", "and CONCAT(activity.name, ', ', activity.date) like '",
                     input$selectedSingleTraining[1], "'" , sep="")
      
      gpxAsDataFrame <- dbGetQuery(dbConnection, query)
      gpxAsDataFrame$mean <- with(gpxAsDataFrame, mean(speedKmPerH))
      #caltulate if speed is above or below mean
      gpxAsDataFrame$speedComparison <- 0
      gpxAsDataFrame <- transform(gpxAsDataFrame, speedComparison = ifelse(speedKmPerH > mean, "ABOVE", "BELOW"))
      renderCharts(gpxAsDataFrame)
    }
  })
  
  observeEvent(input$choosenTraining, {
    if(!is.null(input$choosenTraining))
    {
      if(length(input$choosenTraining) == 2)
      {
        query <- paste("select gpxdata.latitude,
                        gpxdata.longitude,
                        gpxdata.distToNextP,
                        gpxdata.timeToNextP,
                        gpxdata.speedKmPerH,
                        CONCAT(activity.name, ', ', activity.date) as name,
                        gpxdata.rate
                        from gpxdata join activity on gpxdata.idActivity = activity.idActivity
                        join user on user.iduser = activity.userId
                       where user.login like '", USER_LOGIN.env$var, "'", "and CONCAT(activity.name, ', ', activity.date) in ('", 
                        input$choosenTraining[1], "', '", input$choosenTraining[2] , "')", sep="")

        gpxAsDataFrame <- dbGetQuery(dbConnection, query)
        
        gpxAsDataFrame$speedKmPerH <- round(gpxAsDataFrame$speedKmPerH)
        gpxAsDataFrame$rate <- round(gpxAsDataFrame$rate, digits = 1)
        
  #calculate SPEED
        totalDistance <- aggregate(distToNextP~speedKmPerH + name,gpxAsDataFrame,sum)
        totalTime <- aggregate(timeToNextP~speedKmPerH + name,gpxAsDataFrame,sum)
        #convert seconds to minuters and round
        totalTime$timeToNextP <- ifelse(totalTime$timeToNextP ==0, 
               0, 0.6 * (totalTime$timeToNextP/60 - floor(totalTime$timeToNextP/60)) + floor(totalTime$timeToNextP/60))
        
        timeSum <- sum(totalTime$timeToNextP)
        totalTime$timePercent <- ((totalTime$timeToNextP)/timeSum) * 100 
        
        distanceSum <- sum(totalDistance$distToNextP)
        totalDistance$distancePercent <- ((totalDistance$distToNextP)/distanceSum) * 100 
        
        testDistance <- sum(totalDistance$distancePercent)
        testTime <- sum(totalTime$timePercent)
        
        totalSpeed <- merge(totalDistance,totalTime,by=c("speedKmPerH", "name"))
        # END calculate SPEED
        
  #calculate RATE
        totalDistance <- aggregate(distToNextP~rate + name,gpxAsDataFrame,sum)
        totalTime <- aggregate(timeToNextP~rate + name,gpxAsDataFrame,sum)
        #convert seconds to minuters and round
        totalTime$timeToNextP <- ifelse(totalTime$timeToNextP ==0, 
                                        0, 0.6 * (totalTime$timeToNextP/60 - floor(totalTime$timeToNextP/60)) + floor(totalTime$timeToNextP/60))
        
        timeSum <- sum(totalTime$timeToNextP)
        totalTime$timePercent <- ((totalTime$timeToNextP)/timeSum) * 100 
        
        distanceSum <- sum(totalDistance$distToNextP)
        totalDistance$distancePercent <- ((totalDistance$distToNextP)/distanceSum) * 100 
        
        testDistance <- sum(totalDistance$distancePercent)
        testTime <- sum(totalTime$timePercent)
        
        totalRate <- merge(totalDistance,totalTime,by=c("rate", "name"))
        #END calculate RATE
        
        if(!(is.data.frame(gpxAsDataFrame) && nrow(gpxAsDataFrame)==0)) {
          output$percentPlot <- renderPlot({
            if(input$AxisX == "speedKmPerH") {
                selectedColumnTest <- totalSpeed[,c(input$AxisY)]
                selectedColumn <- totalTime$timePercent
  
                interactivePlot <- ggplot(totalSpeed, aes(x=format(speedKmPerH), y=selectedColumnTest, fill=factor(name)
                )) +
                  geom_bar(stat="identity",colour="black", position = 'dodge') + labs(fill = "Training", x=input$AxisX, y=input$AxisY)
  
                print(interactivePlot)
            }
            else {
                selectedColumnTest <- totalRate[,c(input$AxisY)]
                selectedColumn <- totalRate$timePercent
                
                interactivePlot <- ggplot(totalRate, aes(x=format(rate), y=selectedColumnTest, fill=factor(name)
                )) +
                  geom_bar(stat="identity", position = 'dodge') + labs(fill = "Training", x=input$AxisX, y=input$AxisY)
                
                print(interactivePlot)
            }
          })
        }
      }
    }
  })
}

renderUserDataRegistration<-function(input, output, session, dbConnection){
  #TODO validator tylko sprawdza czy taki login jest juz w bazie
  observeEvent(input$userDataApproveButtonReg, {
    makeLoginRegistrationFieldDisable()
    
    email = input$userDataEmailReg
    password = input$userDataPasswordReg
    login = input$userDataLoginReg
    age = input$userDataAgeReg
    
    createUserFolderIfDontExist<-function(login) {
      mainDir <- "C:/Users/Kasia/Documents/inzynierka/temporaryServer"
      subDir <- login
      if(dir.exists(file.path(mainDir, subDir))){
        cat("file almost exists")
      }
      else{
        dir.create(file.path(mainDir, subDir))
      }
    }

    checkIfUserJustExist<-function(login) {
      ifExistQ <- paste("select login from user where login like '",login,"'",sep="")
      return (nrow(dbGetQuery(dbConnection, ifExistQ)) > 0)
    }
    
    if(checkIfUserJustExist(login) == FALSE) {
      createUserFolderIfDontExist(login)
      getInsertQ <- paste("insert into user(email, password, login, age) values(","'",email,"',","'",password,"',","'",login,"',","'",age,"')",sep="")
      dbGetQuery(dbConnection, getInsertQ)
      USER_LOGIN.env$var <- login
      output$registrationProcessMessage <- renderText({ 
        "<font size='3' color='green'>Registration was succesfull!</font>"
      })
      renderSingleTrainingCharts(input, output, session, dbConnection)
      renderStatistisc(input, output, session, dbConnection)
      renderPercentiles(input, output, session, dbConnection)
    }
    else {
      makeLoginRegistrationFieldEnable()
      output$registrationProcessMessage <- renderText({ 
        "<font size='3' color='red'>Wrong registration data!</font>"
      })
    }
  })
}

renderUserDataLogIn<-function(input, output, session, dbConnection){
  
  #TODO validatory poprawnosci przesylanych danych
  observeEvent(input$userDataApproveButtonLog, {
    
    makeLoginRegistrationFieldDisable()
    
    login = input$userDataLoginLog
    password = input$userDataPasswordLog
    
    validateUser<-function(login, password) {
      userValidation <- paste("select * from user where login like '",login,"'", 
                        "and password like '", password, "'",sep="")
      return (nrow(dbGetQuery(dbConnection, userValidation)) > 0)
    }
    
    if(validateUser(login, password) == FALSE) {
      output$loginProcessMessage <- renderText({ 
        "<font size='3' color='red'>Wrong login data!</font>"
      })
      makeLoginRegistrationFieldEnable()
    }
    else {
      USER_LOGIN.env$var <- login
      output$loginProcessMessage <- renderText({ 
        "<font size='3' color='greeb'>Login was succesfull!</font>"
      })
      renderSingleTrainingCharts(input, output, session, dbConnection)
      renderStatistisc(input, output, session, dbConnection)
      renderPercentiles(input, output, session, dbConnection)
    }
  })
}

renderStatistisc<-function(input, output, session, dbConnection) {
  
  #####################################FIRST CHART####################################
  
  userLogin <- USER_LOGIN.env$var
  getActivitiesDataQuery <- paste("select 
                             activity.date,
                             activity.timeLasting,
                             activity.burnCalories,
                             activitytype.name as activityType,
                             activity.userId,
                             activity.distance
                             from activity join activitytype 
                             on activity.activityType = activitytype.idactivityType
                             join user on activity.userId = user.iduser
                             where user.login like '", userLogin, "'", sep ="")
  
  activitiesData <- dbGetQuery(dbConnection, getActivitiesDataQuery)
  #sort data by activity date
  activitiesData <- with(activitiesData,  activitiesData[order(date) , ])
  #change date format from char to Date
  activitiesData$date <- as.Date(activitiesData$date, "%Y-%m-%d")
  
  output$statisticPlot <- renderPlot({
    #setting interactive plot properties
    # activityType <- activitiesData
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
  
  ###################################SECOOND CHART #################################################
  # browser()
  query <- paste("select
                 activity.date
                 from activity
                 join user on user.iduser = activity.userId
                 where user.login like '", USER_LOGIN.env$var, "'", sep="")
  
  datesLimits <- dbGetQuery(dbConnection, query)
  datesLimits$date <- format(as.Date(datesLimits$date, format="%Y-%m-%d"),"%Y")
  uniqueYears <- unique(datesLimits$date)
  dataFilteredByUserLogin <- data.table()
  allData <- data.frame()
  
  for(i in 1:length(uniqueYears)) {
    print("foreach")
    query <- paste("select
                   gpxdata.distToNextP,
                   gpxdata.timeToNextP,
                   gpxdata.speedKmPerH,
                   gpxdata.rate,
                   activity.date,
                   activitytype.name as activityType,
                   user.login
                   from gpxdata join activity on gpxdata.idActivity = activity.idActivity
                   join user on user.iduser = activity.userId
                   join activityType on activityType.idActivityType = activity.activityType
                   where activity.date like '%", uniqueYears[i], "%'", sep="")
    gpxAsDataFrame <- dbGetQuery(dbConnection, query)
    allData <- rbind(allData, gpxAsDataFrame)
  }
  allData$date <- as.Date(allData$date, format="%Y-%m-%d")
  
  observeEvent(c(input$activityType, input$dataType), {
    #setting interactive plot properties
    activityType <- input$activityType
    dataFilteredByActivityType <- allData[allData$activityType == activityType, ]
    dataFilteredByUserLogin <- dataFilteredByActivityType[dataFilteredByActivityType$login == USER_LOGIN.env$var, ]
    
    dataType <- input$dataType
    switch(dataType, 
      "MONTH" = {
        # dataFilteredByUserLogin <- dataFilteredByActivityType[dataFilteredByActivityType$login == USER_LOGIN.env$var, ]
        dataFilteredByUserLogin$date <- format(as.Date(dataFilteredByUserLogin$date, format="%Y-%m-%d"),"%Y-%m")
        
        output$selectedDates <- renderUI({
          selectInput("selectedDates", "Choose months to analyze", as.list(dataFilteredByUserLogin$date), 
                      selected = NULL, multiple = TRUE)
        })
      },
      "YEAR" = {
        # dataFilteredByUserLogin <- dataFilteredByActivityType[dataFilteredByActivityType$login == USER_LOGIN.env$var, ]
        dataFilteredByUserLogin$date <- format(as.Date(dataFilteredByUserLogin$date, format="%Y-%m-%d"),"%Y")
        
        output$selectedDates <- renderUI({
          selectInput("selectedDates", "Choose year to analyze", as.list(dataFilteredByUserLogin$date), 
                      selected = NULL, multiple = TRUE)
        })
      }
    )
    
    observeEvent(input$selectedDates, {
      if(!is.null(input$selectedDates))
      {
        if(length(input$selectedDates) == 2)
        {
          temp <- c(input$selectedDates[1], input$selectedDates[2])
          dataFilteredByUserLogin <- dataFilteredByUserLogin[dataFilteredByUserLogin$date %in% temp, ]
          dataFilteredByUserLogin$speedKmPerH <- round(dataFilteredByUserLogin$speedKmPerH)
          dataFilteredByUserLogin$rate <- round(dataFilteredByUserLogin$rate, digits = 1)
          
          #calculate SPEED
          totalDistance <- aggregate(distToNextP~speedKmPerH + date,dataFilteredByUserLogin,sum)
          totalTime <- aggregate(timeToNextP~speedKmPerH + date,dataFilteredByUserLogin,sum)
          #convert seconds to minuters and round
          totalTime$timeToNextP <- ifelse(totalTime$timeToNextP ==0, 
                                          0, 0.6 * (totalTime$timeToNextP/60 - floor(totalTime$timeToNextP/60)) + floor(totalTime$timeToNextP/60))
          
          timeSum <- sum(totalTime$timeToNextP)
          totalTime$timePercent <- ((totalTime$timeToNextP)/timeSum) * 100 
          
          distanceSum <- sum(totalDistance$distToNextP)
          totalDistance$distancePercent <- ((totalDistance$distToNextP)/distanceSum) * 100 
          
          testDistance <- sum(totalDistance$distancePercent)
          testTime <- sum(totalTime$timePercent)
          
          totalSpeed <- merge(totalDistance,totalTime,by=c("speedKmPerH", "date"))
          # END calculate SPEED
          
          #calculate RATE
          totalDistance <- aggregate(distToNextP~rate + date,dataFilteredByUserLogin,sum)
          totalTime <- aggregate(timeToNextP~rate + date,dataFilteredByUserLogin,sum)
          #convert seconds to minuters and round
          totalTime$timeToNextP <- ifelse(totalTime$timeToNextP ==0, 
                                          0, 0.6 * (totalTime$timeToNextP/60 - floor(totalTime$timeToNextP/60)) + floor(totalTime$timeToNextP/60))
          
          timeSum <- sum(totalTime$timeToNextP)
          totalTime$timePercent <- ((totalTime$timeToNextP)/timeSum) * 100 
          
          distanceSum <- sum(totalDistance$distToNextP)
          totalDistance$distancePercent <- ((totalDistance$distToNextP)/distanceSum) * 100 
          
          testDistance <- sum(totalDistance$distancePercent)
          testTime <- sum(totalTime$timePercent)
          
          totalRate <- merge(totalDistance,totalTime,by=c("rate", "date"))
          #END calculate RATE
          print("selected two")
          print(input$selectedDates)
          
          if(!(is.data.frame(dataFilteredByUserLogin) && nrow(dataFilteredByUserLogin)==0)) {
            output$statisticSpeedRate <- renderPlot({
              print("cpos")
              print(input$AxisXMeOther)
              if(input$AxisXMeOther == "speedKmPerH") {
                selectedColumnTest <- totalSpeed[,c(input$AxisYMeOther)]
                selectedColumn <- totalTime$timePercent
                
                interactivePlot <- ggplot(totalSpeed, aes(x=format(speedKmPerH), y=selectedColumnTest, fill=factor(date)
                )) +
                  geom_bar(stat="identity", position = 'dodge') + labs(fill = "Date", x=input$AxisXMeOther, y=input$AxisYMeOther)
                
                print(interactivePlot)
              }
              else {
                selectedColumnTest <- totalRate[,c(input$AxisYMeOther)]
                selectedColumn <- totalRate$timePercent
                
                interactivePlot <- ggplot(totalRate, aes(x=format(rate), y=selectedColumnTest, fill=factor(date)
                )) +
                  geom_bar(stat="identity", position = 'dodge') + labs(fill = "Date", x=input$AxisXMeOther, y=input$AxisYMeOther)
                
                print(interactivePlot)
              }
            })
          }
        }
      }
    })
    
  }
  )
    
  # observeEvent(input$selectedDates, {
  #     if(!is.null(input$selectedDates))
  #     {
  #       if(length(input$selectedDates) == 2)
  #       {
  #         browser()
  #         temp <- c(input$selectedDates[1], input$selectedDates[2])
  #         dataFilteredByUserLogin <- dataFilteredByUserLogin[dataFilteredByUserLogin$date %in% temp, ]
  #         dataFilteredByUserLogin$speedKmPerH <- round(dataFilteredByUserLogin$speedKmPerH)
  #         dataFilteredByUserLogin$rate <- round(dataFilteredByUserLogin$rate, digits = 1)
  #         
  #         #calculate SPEED
  #         totalDistance <- aggregate(distToNextP~speedKmPerH + date,dataFilteredByUserLogin,sum)
  #         totalTime <- aggregate(timeToNextP~speedKmPerH + date,dataFilteredByUserLogin,sum)
  #         #convert seconds to minuters and round
  #         totalTime$timeToNextP <- ifelse(totalTime$timeToNextP ==0, 
  #                                         0, 0.6 * (totalTime$timeToNextP/60 - floor(totalTime$timeToNextP/60)) + floor(totalTime$timeToNextP/60))
  #         
  #         timeSum <- sum(totalTime$timeToNextP)
  #         totalTime$timePercent <- ((totalTime$timeToNextP)/timeSum) * 100 
  #         
  #         distanceSum <- sum(totalDistance$distToNextP)
  #         totalDistance$distancePercent <- ((totalDistance$distToNextP)/distanceSum) * 100 
  #         
  #         testDistance <- sum(totalDistance$distancePercent)
  #         testTime <- sum(totalTime$timePercent)
  #         
  #         totalSpeed <- merge(totalDistance,totalTime,by=c("speedKmPerH", "date"))
  #         # END calculate SPEED
  #         
  #         #calculate RATE
  #         totalDistance <- aggregate(distToNextP~rate + date,dataFilteredByUserLogin,sum)
  #         totalTime <- aggregate(timeToNextP~rate + date,dataFilteredByUserLogin,sum)
  #         #convert seconds to minuters and round
  #         totalTime$timeToNextP <- ifelse(totalTime$timeToNextP ==0, 
  #                                         0, 0.6 * (totalTime$timeToNextP/60 - floor(totalTime$timeToNextP/60)) + floor(totalTime$timeToNextP/60))
  #         
  #         timeSum <- sum(totalTime$timeToNextP)
  #         totalTime$timePercent <- ((totalTime$timeToNextP)/timeSum) * 100 
  #         
  #         distanceSum <- sum(totalDistance$distToNextP)
  #         totalDistance$distancePercent <- ((totalDistance$distToNextP)/distanceSum) * 100 
  #         
  #         testDistance <- sum(totalDistance$distancePercent)
  #         testTime <- sum(totalTime$timePercent)
  #         
  #         totalRate <- merge(totalDistance,totalTime,by=c("rate", "date"))
  #         #END calculate RATE
  #         print("selected two")
  #         print(input$selectedDates)
  #         
  #         if(!(is.data.frame(dataFilteredByUserLogin) && nrow(dataFilteredByUserLogin)==0)) {
  #           output$statisticSpeedRate <- renderPlot({
  #             print("cpos")
  #             print(input$AxisXMeOther)
  #             if(input$AxisXMeOther == "speedKmPerH") {
  #               selectedColumnTest <- totalSpeed[,c(input$AxisYMeOther)]
  #               selectedColumn <- totalTime$timePercent
  #               
  #               interactivePlot <- ggplot(totalSpeed, aes(x=format(speedKmPerH), y=selectedColumnTest, fill=factor(date)
  #               )) +
  #                 geom_bar(stat="identity", position = 'dodge') + labs(fill = "Date", x=input$AxisXMeOther, y=input$AxisYMeOther)
  #               
  #               print(interactivePlot)
  #             }
  #             else {
  #               selectedColumnTest <- totalRate[,c(input$AxisYMeOther)]
  #               selectedColumn <- totalRate$timePercent
  #               
  #               interactivePlot <- ggplot(totalRate, aes(x=format(rate), y=selectedColumnTest, fill=factor(date)
  #               )) +
  #                 geom_bar(stat="identity", position = 'dodge') + labs(fill = "Date", x=input$AxisXMeOther, y=input$AxisYMeOther)
  #               
  #               print(interactivePlot)
  #             }
  #           })
  #         }
  #       }
  #     }
  #   })
}

renderPercentiles<-function(input, output, session, dbConnection) {
  browser()
  #TODO- dodaC do analizy wiek uZytkownika - jako percentyl np
  getActivitiesDataQuery <- "select
                            activity.date,
                            activity.timeLasting,
                            activity.burnCalories,
                            activity.userId,
                            activity.distance,
                            activitytype.name as activityType
                            from activity 
                            join activitytype on activity.activityType = activitytype.idActivityType;"

  activitiesData <- dbGetQuery(dbConnection, getActivitiesDataQuery)
  #sort data by activity date
  activitiesData <- with(activitiesData,  activitiesData[order(date) , ])
  #change date format from char to Date
  activitiesData$date <- as.Date(activitiesData$date, "%Y-%m-%d")
  
  getJoinedData <- function(dateType, percentileValue, activityType) {
    activityTypeInput <- activityType
    activitiesData <- subset(activitiesData, activityType == activityTypeInput)
    activitiesData$date <- cut(activitiesData$date, dateType)
    dataToCaltulatePercentile <- setNames(aggregate(list(activitiesData$burnCalories, activitiesData$timeLasting, activitiesData$distance),
                                                    by=list(activitiesData$date, activitiesData$userId), 
                                                    FUN=sum), c("date", "userId", "burnCalories", "timeLasting", "distance"))
    
    #change userId to input$userId
    getIdUser <- paste("select iduser from user where login like '", USER_LOGIN.env$var, "'", sep="")
    userIdValue <- as.numeric(dbGetQuery(dbConnection, getIdUser))
    currentUserData <- subset(dataToCaltulatePercentile, userId == userIdValue, select=date:distance)

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
    activityType <- input$percentileActivityType
    dateType <- input$percentileDateType
    percentileType <- ({
      switch(input$percentileDateType,
             "%Y-%m" = "months",
             "%Y" = "years")
    })
    percentileValue <- input$percentileValue
    joinedData <- getJoinedData(percentileType, percentileValue, activityType)
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
  renderUserDataRegistration(input, output, session, dbConnection2)
  renderUserDataLogIn(input, output, session, dbConnection2)
  
  session$onSessionEnded(function(){
    cat("close connections")
    dbDisconnect(dbConnection)
    dbDisconnect(dbConnection2)
  })
}


