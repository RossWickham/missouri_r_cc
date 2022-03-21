#'
#' Exploratory Data Analysis of USGS gages from HUC shapefile
#'
#' Determine USGS gages within shapefile, determine the period of record 
#'   of all USGS gages within each individual polygon, compiled dataset and
#'   plot
#'   
#' @author Ross Wickham, NWW
#' @date   Feb 2022
#'

# Load DSS package - machine specific folder location.  Ask Ross 
#   for necessary dependencies (HEC-DSSVue/Java, dssrip R package)
try({
  # setwd("D:\\R_Scripting_Tutorial")
  source("configure_dssrip.R")
})




library(dataRetrieval)
library(raster)
library(rgdal)
library(tidyverse)
library(openxlsx)


### Config --------------------------------------------------------------------

# Point this file to the HUC basin shapefile
basinFileName <- paste0("N:\\EC-H\\HYDROLOGY SECTION\\Wickham\\projects\\",
                        "2022-02_Missouri_R_CC\\data\\vector\\",
                        "WBDHU4_MissouriR_Project.shp")

# Which states to look at USGS gages
# Montana, N Dakota, S Dakota, Wyoming, Minnesota, Nebraska,
#   Colorado, Kansas, Iowa, Missouri, Idaho
states <- c("MT", "ND", "SD", "WY", "MN", "NE", "CO", "KS", "IA", "MO", "ID")

# PROJ4 string for WGS84 conversion
proj4StringWGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Save location of metadata file 
# May want to switch to RData once convert to RProj
metaSaveFileName <- "D:/temp/missouri_usgs.xlsx"

### Functions -----------------------------------------------------------------

# Extract data.frame of x-y coordinates from polygon
getSinglePolyCoords <- function(poly){
  poly@polygons[[1]]@Polygons[[1]]@coords %>%
    as.data.frame() %>%
    rename(x=1, y=2)
}

# Get index of points that are in a polygon
whichPointsInPolygon <- function(poly, pointsX, pointsY){
  polyCoods <- getSinglePolyCoords(poly)
  point.in.polygon(point.x = pointsX,
                   point.y = pointsY,
                   pol.x = polyCoods$x,
                   pol.y = polyCoods$y) %>%
    as.logical() %>%
    which()
}


#' usgsToDss
#' Pulls USGS daily flow datasets from USGS webservice and saves to DSS
#' 
#'
#' @param site_no character, USGS site number
#' @param dssFile string or Java object, absolute file path to DSS file or
#'                  DSS Java object 
#'
#' @return NULL
#'
#' @examples
#' usgsDailyFlowToDss(site_no = "14015000", dssFile = "D:/temp/ww_gage.dss")
usgsDailyFlowToDss <- function(site_no, dssFile){
  # Pull daily flows from webservice
  raw <- readNWISdv(siteNumbers = site_no, parameterCd = "00060")
  
}

#' Unlist a list of vectors to a data.frame, preserving the names of the
#'   data.frames as a column name
#'
#' @param l            List of vectors
#' @param valueColname Name of output data.frame's column associated with 
#'                       vector values
#' @param listColName  Name of output data.frame's column associated with
#'                       list element names to preserve
unlistWithNames <- function(l, valueColname = "value", listColName = "name"){
  l %>%
    unlist(use.names = F) %>%
    {
      data.frame(site_no = .,
                 basin = rep(names(l), lengths(l)))
    } %>%
    setNames(c(valueColname, listColName))
}


#' Convert USGS daily data to TimeSeriesMath
#'
#' @param usgsDvDf Data.frame from USGS webservice 'readNWISdv' function call
#' @param aPart    String, A part of DSS path
#' @param bPart    String, B part of DSS path
#' @param cPart    String, C part of DSS path
#' @param fPart    String, F part of DSS path
#' @return Java object, TimeSeriesMath
usgsDvToHecMath <- function(usgsDvDf,
                        aPart = "",
                        bPart = "TEST",
                        cPart = "FLOW",
                        fPart = "EXAMPLE"){
  tryCatch({
    
    usgsDvDf <- na.omit(usgsDvDf)
    
    # Fill missing with undefined
    if(length(unique(diff(usgsDvDf$Date))) > 1){
      constantUndef <- -3.4028234663852888e+38
      mDf <- data.frame(
        Date = seq.Date(from = min(usgsDvDf$Date),
                        to = max(usgsDvDf$Date),
                        by = "d"))
      usgsDvDf <- merge(mDf, usgsDvDf, by = "Date", all.x = T)
      usgsDvDf$X_00060_00003[is.na(usgsDvDf$X_00060_00003)] <-
        constantUndef
    }
    
    tsc = .jnew("hec/io/TimeSeriesContainer")
    
    # Extract times and values, set times as minutes since 1899-12-31 00:00
    baseDate <- as.Date("1899-12-30") %>% as.integer() # -1day for 2400 offset
    times <- (as.integer(usgsDvDf$Date) - baseDate)*60*24
    values <- as.numeric(usgsDvDf$X_00060_00003)
    
    # Assing times and values
    tsc$times <- .jarray(as.integer(times), contents.class="java/lang/Integer")
    tsc$values <- .jarray(values, contents.class="java/lang/Double")
    
    # Assign metadata
    tsc$endTime <- as.integer(max(times))
    tsc$startTime <- as.integer(min(times))
    tsc$numberValues <- length(values)
    tsc$interval <- as.integer(1440) # daily data
    tsc$type <- .jstrVal("PER-AVER")
    tsc$units <- .jstrVal("CFS")
    
    # Assign path names
    tsc$watershed <- aPart %>% .jstrVal()
    tsc$location  <- substring(bPart, 1, 64) %>% .jstrVal()# 64 char limit
    tsc$parameter <- cPart %>% .jstrVal()
    tsc$version   <- fPart %>% .jstrVal()
    tsc$fullName <- sprintf("/%s/%s/%s//1DAY/%s/",
                            aPart,
                            bPart,
                            cPart,
                            fPart) %>% .jstrVal()
    
    # For testing:
    # dssFile <- opendss("D:/temp/test.dss")
    # dssFile$put(tsc)
    # head(usgsDvDf)
    # tail(usgsDvDf)

    # Convert to TimeSeriesMath
    hm <- .jnew("hec/hecmath/TimeSeriesMath")
    hm <- hm$createInstance(tsc)
    
    return(hm)
  }, error = function(e){
    message(e)
    return(NULL)
  })
}

# Load basin shapefile, convert to WGS84
loadBasinShp <- function(basinFileName){
  basinFileName %>%
    readOGR() %>%
    spTransform(CRSobj = proj4StringWGS84)
}

# Load metadata for all USGS river gages in each defined state that
#   have daily discharge data, bind to single dataframe
getUSGSstateMeta <- function(states){
  usgsMeta <-
    states %>%
    Map(whatNWISdata,
        stateCd = .,
        parameterCd = list("00060"),
        service = "dv") %>%
    bind_rows()
}

getGageMetaData <- function(basinFileName, states, metaSaveFileName){
  basinShp <- loadBasinShp(basinFileName)
  usgsMeta <- getUSGSstateMeta(states)
  
  if(file.exists(metaSaveFileName)){
    load(metaSaveFileName)
    return(gageMeta)
  }
  
  # For each basin polygon, get the index of corresponding 
  #   USGS gages in the stateMeta data.frame via whichPointsInPolygon
  basinShp %>%
    split(1:nrow(.)) %>%
    Map(f = whichPointsInPolygon, 
        pointsX = list(usgsMeta$dec_long_va),
        pointsY = list(usgsMeta$dec_lat_va)) %>%
    # Convert gage index to gage ID
    map(.f = function(x) usgsMeta$site_no[x]) %>%
    # Rename list to names of basins
    set_names(basinShp$NAME) %>%
    # Convert to a dataframe with USGS gage ID (site_no) and basin name (basin)
    unlistWithNames(valueColname = "site_no", listColName = "basin") %>%
    # Merge with original USGS gage metadata
    merge(y = usgsMeta, by = "site_no", all.x = T)
}

### Load Metadata -------------------------------------------------------------


# Load USGS gage metadata for all basins
gageMeta <- getGageMetaData(basinFileName, states, metaSaveFileName)


# Write out metadata to file
try({
  write.xlsx(x = gageMeta, file = metaSaveFileName)
})



# Assign geospatial coordinates and projection
coordinates(gageMeta) <- ~dec_long_va+dec_lat_va
crs(gageMeta) <- proj4StringWGS84

plot(basinShp)
box(); axis(1); axis(2)
points(gageMeta, pch=3, cex=0.1)
text(-100,50,sprintf("# Sites = %g", length(unique(gageMeta$site_no))))

### Download and Save ---------------------------------------------------------

dssFileName <- "D:/temp/missouri_usgs_raw.dss"
dssFile <- opendss(dssFileName)

# Iterate through each basin, save all USGS gage data to DSS
for(basin in unique(gageMeta$basin)){
  
  cat(sprintf("\nProcessing basin:\t%s", basin))
  
  # Create data.frame to map to 'readNWISdv' function
  subMeta <- 
    gageMeta[gageMeta$basin == basin, ] %>%
    as.data.frame() %>%
    select(c("site_no", "station_nm")) %>%
    rename(siteNumbers=1) %>%
    mutate(
      parameterCd = "00060"
    )
  
  # Download USGS data to list of data.frames
  dfList <- pmap(.l = subMeta[, c("siteNumbers", "parameterCd")],
                 .f = readNWISdv)
  
  # Convert data.frames to Java HecMath objects and save
  Map(f = usgsDvToHecMath,
      usgsDvDf = dfList,
      aPart = list(basin),
      bPart = str_replace_all(subMeta$station_nm, "/","-"),
      cPart = list("FLOW"),
      fPart = sprintf("USGS %s", subMeta$siteNumbers)) %>%
    compact() %>%
    map(.f = dssFile$write)

}
dssFile$close()



