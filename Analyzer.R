library(XML)
library(lubridate)
library(raster)

analyzer <- setRefClass("analyzer", fields = list(test="character", addedFile="data.frame"))

analyzer$methods(
  #function that shifts vectors conviniently
  shift.vec = function(vec, shift) {
    browser()
    #abs- wartosc bezwzgledna
    #NA - missing value
    if(length(vec) <= abs(shift)) {
      rep(NA ,length(vec))
    }else{
      if (shift >= 0) {
        c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
      else {
        c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } }

  },
  
  caltulateGPSData = function() {
    #TODO
    #pokombinowac jak wycignac tym sposobem nazwe treningu i rodzaj
    # Parse the GPX file
    pfile <- htmlTreeParse(addedFile$datapath, useInternalNodes = T)
    # Get all elevations, times and coordinates via the respective xpath
    elevations <- as.numeric(xpathSApply(pfile, path = "//trkpt/ele", xmlValue))
    times <- xpathSApply(pfile, path = "//trkpt/time", xmlValue)
    coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)
    # Extract latitude and longitude from the coordinates
    lats <- as.numeric(coords["lat",])
    lons <- as.numeric(coords["lon",])
    # Put everything in a dataframe and get rid of old variables
    geodf <- data.frame(lat = lats, lon = lons, ele = elevations, time = times)
    rm(list=c("elevations", "lats", "lons", "pfile", "times", "coords"))
    
    ##the strangers part
    # Shift vectors for lat and lon so that each row also contains the next position.
    geodf$lat.p1 <- shift.vec(geodf$lat, -1)
    geodf$lon.p1 <- shift.vec(geodf$lon, -1)
    # Calculate distances (in metres) using the function pointDistance from the raster package.
    # Parameter lonlat has to be TRUE!
    geodf$dist.to.prev <- apply(geodf, 1, FUN = function (row) {
      pointDistance(c(as.numeric(row["lat.p1"]),
                      as.numeric(row["lon.p1"])),
                    c(as.numeric(row["lat"]), as.numeric(row["lon"])),
                    lonlat = T)
    })
    # Transform the column time so that R knows how to interpret it.
    geodf$time <- strptime(geodf$time, format = "%Y-%m-%dT%H:%M:%OS")
    # Shift the time vector, too.
    geodf$time.p1 <- shift.vec(geodf$time, -1)
    # Calculate the number of seconds between two positions.
    geodf$time.diff.to.prev <- as.numeric(difftime(geodf$time.p1, geodf$time))
    # Calculate metres per seconds, kilometres per hour and two LOWESS smoothers to get rid of some noise.
    geodf$speed.m.per.sec <- geodf$dist.to.prev / geodf$time.diff.to.prev
    geodf$speed.km.per.h <- geodf$speed.m.per.sec * 3.6
    geodf$speed.km.per.h <- ifelse(is.na(geodf$speed.km.per.h), 0, geodf$speed.km.per.h)
    geodf$lowess.speed <- lowess(geodf$speed.km.per.h, f = 0.04)$y
    geodf$lowess.ele <- lowess(geodf$ele, f = 0.2)$y
    return(geodf)
  }
)