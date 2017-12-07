library(XML)
library(lubridate)
library(dbConnect)
library(raster)

analyzer <- setRefClass("analyzer", fields = list(addedFile="data.frame"))
dbConnection = dbConnect(MySQL(), user='admin', password='admin', dbname='analyzer_db_2', host='localhost')

## need to pass user id here - folder should be name as user Id
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
    
    browser()
    
    activityTypesDF <- c("RUNNING", "BIKE RIDING", "SWIMMING")
    activityNumbersDF <- c(1, 3, 4)
    
    activityTypesTable <- data.frame(activityNumbersDF, activityTypesDF)
    
    # Function to load data from gpx file into dataframe
    parsedGPX <- htmlTreeParse(addedFile$datapath, useInternalNodes = T)
    # Get values from parsed GPX files variables
    gpsValues <- xpathSApply(parsedGPX, path = "//trkpt", xmlAttrs)
    lat <- as.numeric(gpsValues["lat",])
    lon <- as.numeric(gpsValues["lon",])
    time <- xpathSApply(parsedGPX, path = "//trkpt/time", xmlValue)
    ele <- as.numeric(xpathSApply(parsedGPX, path = "//trkpt/ele", xmlValue))
    
    # Assign variables into data frame
    gpxAsDataFrame <- data.frame(latitude = lat, longitude = lon, elevation = ele, time = time)
    print("I am here")
    # Formate time
    gpxAsDataFrame$time <- strptime(gpxAsDataFrame$time, format = "%Y-%m-%dT%H:%M:%OS")
    
    # Remove temporary variables
    ##rm(list=c("ele", "lat", "lon", "parsedGPX", "time", "gpsValues"))
    rm(list=c("ele", "lat", "lon", "time", "gpsValues"))
    
    # Set next point position values
    gpxAsDataFrame$latitude.nextP <- setNextPointValue(gpxAsDataFrame$latitude, -1)
    gpxAsDataFrame$longitude.nextP <- setNextPointValue(gpxAsDataFrame$longitude, -1)
    # Calculate distance between two points
    gpxAsDataFrame$dist.to.nextP <- apply(gpxAsDataFrame, 1, FUN = function (row) {
      pointDistance(c(as.numeric(row["latitude.nextP"]),as.numeric(row["longitude.nextP"])),
                    c(as.numeric(row["latitude"]), as.numeric(row["longitude"])),
                    lonlat = T)
    })
    
    # Set next point time
    gpxAsDataFrame$time.nextP <- setNextPointValue(gpxAsDataFrame$time, -1)
    # Calculate time difference between two times
    gpxAsDataFrame$time.to.nextP <- as.numeric(difftime(gpxAsDataFrame$time.nextP, gpxAsDataFrame$time))
    # Calculate speeds
    gpxAsDataFrame$speed.km.per.h <- ((gpxAsDataFrame$dist.to.nextP / gpxAsDataFrame$time.to.nextP) * 3.6)
    #gpxAsDataFrame$speed.km.per.h <- gpxAsDataFrame$speed.m.per.sec 
    gpxAsDataFrame$speed.km.per.h <- ifelse(is.na(gpxAsDataFrame$speed.km.per.h), 0, gpxAsDataFrame$speed.km.per.h)
    # Calculate lowess speed(smoothing)
    ## f = 0.04 need to be changed dinamicly
    #gpxAsDataFrame$lowess.speed <- lowess(gpxAsDataFrame$speed.km.per.h, f = 0.04)$y
    #gpxAsDataFrame$lowess.elevation <- lowess(gpxAsDataFrame$ele, f = 0.2)$y
    
    #Prepare data to activity table
    activityUserId <- 8
    activityDate <- xpathSApply(parsedGPX, path = "//metadata/time", xmlValue)
    activityDate <- strptime(activityDate, format = "%Y-%m-%d")
    activityName <- xpathSApply(parsedGPX, path = "//trk/name", xmlValue)
    #change seconds to minutes
    activityTimeLasting <-  round(as.numeric(difftime(tail(gpxAsDataFrame$time, n=1), head(gpxAsDataFrame$time,1))))
    activityType <- as.character(xpathSApply(parsedGPX, path = "//trk/type", xmlValue))
    
    testPath <- addedFile$name
    
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
      if(grepl('run', addedFile$name, ignore.case = TRUE)) {
        activityType = 1;
        activityBurnCalories <- 590 * activityTimeLasting
      }
      if(grepl('ride', addedFile$name, ignore.case = TRUE)) {
        activityType = 3;
        activityBurnCalories <- 620 * activityTimeLasting
      }
      if(grepl('swim', addedFile$name, ignore.case = TRUE)) {
        activityType = 4;
        activityBurnCalories <- 413 * activityTimeLasting
      }
    }
    activityBurnCalories <- round(activityBurnCalories/60)
    #change meters to km and round
    activityDistance <- round((sum(gpxAsDataFrame$dist.to.nextP, na.rm = TRUE) / 1000), digits = 1)
    
    activitydataTable <- data.frame(date = activityDate, name = activityName,
                                    timeLasting = activityTimeLasting, burnCalories = activityBurnCalories, 
                                    activityType = activityType, userId = activityUserId, distance = activityDistance)
    dbWriteTable(dbConnection, "activity", activitydataTable, append=TRUE, row.names = FALSE)
    
    
    getMaxIdActivity <- "select 
                              max(activity.idActivity)
                              from activity"
    
    idActivity <- as.numeric(dbGetQuery(dbConnection, getMaxIdActivity))
    gpxAsDataFrame$idActivity <- idActivity
    
    dbWriteTable(dbConnection, "gpxdata", gpxAsDataFrame, append=TRUE, row.names = FALSE)
    return(gpxAsDataFrame)
  }
)