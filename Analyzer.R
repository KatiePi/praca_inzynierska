library(XML)
library(lubridate)
library(dbConnect)
library(raster)

analyzer <- setRefClass("analyzer", fields = list(addedFile="data.frame", userLogin = "character"))
dbConnection = dbConnect(MySQL(), user='admin', password='admin', dbname='analyzer_db_2', host='localhost')

analyzer$methods(
  
  setNextPointValue = function(vector, shift) {
    #Function to set next point values
    if(length(vector) <= abs(shift)) {
      rep(NA ,length(vector))
    }
    else {
      if (shift >= 0) {
        c(rep(NA, shift), vector[1:(length(vector)-shift)]) 
      }
      else {
        c(vector[(abs(shift)+1):length(vector)], rep(NA, abs(shift))) 
      } 
    }
  },
  
  caltulateGPSData = function() {
    # Function to load data from gpx file into dataframe
    fileDataPath <- as.character(addedFile$datapath)
    fileName <- as.character(addedFile$name)
    
    parsedGPX <- htmlTreeParse(fileDataPath, useInternalNodes = T)
    # Get values from parsed GPX files variables
    gpsValues <- xpathSApply(parsedGPX, path = "//trkpt", xmlAttrs)
    lat <- as.numeric(gpsValues["lat",])
    lon <- as.numeric(gpsValues["lon",])
    time <- xpathSApply(parsedGPX, path = "//trkpt/time", xmlValue)
    ele <- as.numeric(xpathSApply(parsedGPX, path = "//trkpt/ele", xmlValue))
    
    # Assign variables into data frame
    gpxAsDataFrame <- data.frame(latitude = lat, longitude = lon, elevation = ele, time = time)
    # Formate time
    gpxAsDataFrame$time <- strptime(gpxAsDataFrame$time, format = "%Y-%m-%dT%H:%M:%OS")
    
    # Remove temporary variables
    ##rm(list=c("ele", "lat", "lon", "parsedGPX", "time", "gpsValues"))
    rm(list=c("ele", "lat", "lon", "time", "gpsValues"))
    
    # Set next point position values
    gpxAsDataFrame$latitudeNextP <- setNextPointValue(gpxAsDataFrame$latitude, -1)
    gpxAsDataFrame$longitudeNextP <- setNextPointValue(gpxAsDataFrame$longitude, -1)
    # Calculate distance between two points
    gpxAsDataFrame$distToNextP <- apply(gpxAsDataFrame, 1, FUN = function (row) {
      pointDistance(c(as.numeric(row["latitudeNextP"]),as.numeric(row["longitudeNextP"])),
                    c(as.numeric(row["latitude"]), as.numeric(row["longitude"])),
                    lonlat = T)
    })
    browser()
    # Set next point time
    gpxAsDataFrame$timeNextP <- setNextPointValue(gpxAsDataFrame$time, -1)
    # Calculate time difference between two times
    gpxAsDataFrame$timeToNextP <- as.numeric(difftime(gpxAsDataFrame$timeNextP, gpxAsDataFrame$time))
    # Calculate speed
    gpxAsDataFrame$speedKmPerH <- ((gpxAsDataFrame$distToNextP / gpxAsDataFrame$timeToNextP) * 3.6)
    gpxAsDataFrame$speedKmPerH <- ifelse(is.na(gpxAsDataFrame$speedKmPerH), 0, gpxAsDataFrame$speedKmPerH)
    #rate - km na min
    gpxAsDataFrame$rate <- ifelse(gpxAsDataFrame$speedKmPerH ==0, 
                                  0, 0.6 * (60/gpxAsDataFrame$speedKmPerH - floor(60/gpxAsDataFrame$speedKmPerH)) + floor(60/gpxAsDataFrame$speedKmPerH))
    
    #Prepare data to activity table
    getIdUser <- paste("select iduser from user where login like '", userLogin, "'", sep="")
    activityUserId <- as.numeric(dbGetQuery(dbConnection, getIdUser))
    activityDate <- xpathSApply(parsedGPX, path = "//metadata/time", xmlValue)
    activityDate <- strptime(activityDate, format = "%Y-%m-%d")
    activityName <- xpathSApply(parsedGPX, path = "//trk/name", xmlValue)
    #change seconds to minutes
    activityTimeLasting <-  round(as.numeric(difftime(tail(gpxAsDataFrame$time, n=1), head(gpxAsDataFrame$time,1))))
    activityType <- as.character(xpathSApply(parsedGPX, path = "//trk/type", xmlValue))
    
    if(!identical(activityType, character(0))) {
      sapply(activityType, switch,
             "running" = {
               activityType <- 1
               activityBurnCalories <- 590 * activityTimeLasting
             },
             "riding" = {
               activityType <- 3
               activityBurnCalories <- 620 * activityTimeLasting
             },
             "swimming" = {
               activityType <- 4
               activityBurnCalories <- 413 * activityTimeLasting
             }
      )
    }
    else {
      if(grepl('run', fileName, ignore.case = TRUE)) {
        activityType = 1;
        activityBurnCalories <- 590 * activityTimeLasting
      }
      if(grepl('ride', fileName, ignore.case = TRUE)) {
        activityType = 3;
        activityBurnCalories <- 620 * activityTimeLasting
      }
      if(grepl('swim', fileName, ignore.case = TRUE)) {
        activityType = 4;
        activityBurnCalories <- 413 * activityTimeLasting
      }
    }
    activityBurnCalories <- round(activityBurnCalories/60)
    #change meters to km and round
    activityDistance <- round((sum(gpxAsDataFrame$distToNextP, na.rm = TRUE) / 1000), digits = 1)
    
    activitydataTable <- data.frame(date = activityDate, name = activityName,
                                    timeLasting = activityTimeLasting, burnCalories = activityBurnCalories, 
                                    activityType = activityType, userId = activityUserId, distance = activityDistance)
    #save activity data in DB
    dbWriteTable(dbConnection, "activity", activitydataTable, append=TRUE, row.names = FALSE)

    getMaxIdActivity <- "select 
                         max(activity.idActivity)
                         from activity"
    
    idActivity <- as.numeric(dbGetQuery(dbConnection, getMaxIdActivity))
    gpxAsDataFrame$idActivity <- idActivity
    #save gpx data in DB
    dbWriteTable(dbConnection, "gpxdata", gpxAsDataFrame, append=TRUE, row.names = FALSE)
    #remove temporary variables
    rm(list=c("parsedGPX", "activityUserId", "activityDate", "activityName", 
              "activityTimeLasting", "activityType", "activityBurnCalories",
              "activityDistance", "activitydataTable", "idActivity", "getMaxIdActivity",
              "fileDataPath", "fileName", "getIdUser"))
    return(gpxAsDataFrame)
  }
)