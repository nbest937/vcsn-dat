library( ncdf4)
library( raster)
library( stringr)
library( doMC)

registerDoMC( cores=3)

vcsnZipFiles <- list.files( path= "Daily_VCSN_data", full.names= TRUE)

vcsn <- {
  dats <- as.character( unzip( vcsnZipFiles[1], list= TRUE)$Name)
  datFile <- unzip( vcsnZipFiles[1], files= dats[1])
  vcsn <- read.csv( datFile, header= TRUE)
  file.remove( datFile)
  colnames( vcsn)[ which( colnames(vcsn) == "TMax")]  <- "Tmax"
  within( vcsn, {
    Date <- as.character( as.Date( Date, format="%d/%m/%Y"))
    #cell <- as.character( cellFromXY( world, cbind( Longt, Lat))})
  })
}

nzRes <- 3/60  # 3'
nz <- raster( xmn= min( vcsn$Longt) -nzRes /2, 
              xmx= max( vcsn$Longt) +nzRes /2,
              ymn= min( vcsn$Lat)   -nzRes /2,
              ymx= max( vcsn$Lat)   +nzRes /2)
res(nz) <- 3/60

## rain <- rasterize( vcsn[, c("Longt", "Lat")], nz, vcsn[, "Rain"])

vcsnVars <- function( year, days, ...) {
  dimVars <- {
    x <- ncdim_def( "longitude", "degrees_east", 
      vals= seq( 
        from= min( vcsn$Longt), 
        to= max( vcsn$Longt), 
        by= nzRes))
    y <- ncdim_def( "latitude", "degrees_north", 
      vals= seq( 
        from= max( vcsn$Lat), 
        to= min( vcsn$Lat), 
        by= -nzRes))
    t <- ncdim_def( "time", sprintf( "days since %s-12-31 0:0:0", year-1),
      vals= 1:days)
    list( x, y, t)
  }
  list(
    ## crs= ncvar_def( "crs", list(), list(), prec= "integer"),
    MSLP=  ncvar_def( "MSLP", "hPa", dimVars, 1.e30, 
      longname= "Pressure reduced to Mean Sea Level in hPa at 9am local day",
      ...),
    PET=   ncvar_def( "PET",   "mm", dimVars, 1.e30,
      longname= "24 hour Penman Potential Evapotranspiration total in mm from 9am local day",
      ...),
    Rain=  ncvar_def( "Rain",  "mm", dimVars, 1.e30, 
      longname= "Total amount of rain in mm from 9am local day",
      ...),
    RH=    ncvar_def( "RH",    "%",  dimVars, 1.e30,
      longname= "Relative humdity in percent at 9am local day",
      ...),
    SoilM= ncvar_def( "SoilM", "mm", dimVars, 1.e30, 
      longname= "Soil moisture in mm from 9am local day calculated from rainfall and evapotranspiration. The base value is -150mm (\"permanent wilting point\") based on \"soil store capacity\". A value of \"0\" indicates the soil is at \"field capacity\" (amount of water held in the soil after the excess has drained away). A value greater than \"0\" indicates runof[sic]",
      ...),
    ETmp= ncvar_def( "ETmp", "degC", dimVars, 1.e30,
      longname= "The 10cm earth temperature in degC at 9am local day",
      ...),
    Rad=  ncvar_def( "Rad",  "MJ/m2", dimVars, 1.e30, 
      longname= "Amount of accumulated global solar radiation in MJ/m2 (Mega Joules per square metre) from midnight local day",
      ...),
    Tmax= ncvar_def( "Tmax", "degC", dimVars, 1.e30,
      longname= "Maximum tempereature in degC from 9am local day",
      ...),
    Tmin= ncvar_def( "Tmin", "degC", dimVars, 1.e30,
      longname= "Minimum tempereature in degC to 9am local day. Note: take care when comparing this with the daily combined \"Form 301\" minimum temperature - this will be one day earlier",
      ...),
    VP=   ncvar_def( "VP",   "hPa", dimVars, 1.e30,
      longname= "Vapour pressure in hPa at 9am local day",
      ...),
    Wind= ncvar_def( "Wind", "m/s", dimVars, 1.e30,
      longname= "Mean wind speed in m/s at 10m above ground level over 24 hours from midnight local day",
      ...))
}

## nc <- nc_create( "vcsn_1972.nc", vcsnVars( 1972, length(dats)), verbose= TRUE)
## ncvar_put( nc, vcsnVars[[ "Rain"]], rain[], start= c(1,1,1), verbose= TRUE)

getVcsnDf <- function( vcsnDatFile) {
  vcsnDf <- read.csv( vcsnDatFile, header= TRUE)
  colnames( vcsnDf)[ which( colnames(vcsnDf) == "TMax")]  <- "Tmax"
  within( vcsnDf, {
    Date <- as.character( as.Date( Date, format="%d/%m/%Y"))
    #cell <- as.character( cellFromXY( world, cbind( Longt, Lat))})
  })
}

## for( vcsnZipFile in list.files( path= "Daily_VCSN_data", full.names= TRUE)[1:2]) {
##   vcsnDatFiles <- sort( as.character( unzip( vcsnZipFile, list= TRUE)$Name))
##   nc <- nc_create( 
##     str_replace( basename( vcsnZipFile), "zip", "nc"), 
##     vcsnVars( 
##       as.integer( str_extract( basename( vcsnZipFile), "[0-9]{4}")),
##       length( vcsnDatFiles), 
##       compression= 9))
##   ncatt_put( nc, 0, "title", "New Zealand Virtual Climate Station Network (VCSN) weather data")
##   ncatt_put( nc, 0, "institution", "Computation Institute, University of Chicago")
##   ncatt_put( nc, 0, "source", "New Zealand National Institute of Water and Atmospheric Research")
##   for( day in 1:length( vcsnDatFiles) ) {
##     vcsnDatFile <- unzip( vcsnZipFile, files= vcsnDatFiles[ day])
##     vcsnDf <- getVcsnDf( vcsnDatFile)
##     file.remove( vcsnDatFile)
##     for( vcsnVar in names( nc$var)) {  
##       r <- try( rasterize( vcsnDf[, c("Longt", "Lat")], nz, vcsnDf[, vcsnVar]), 
##                 silent= TRUE)
##       ncvar_put( nc, 
##         var, 
##         if( class( r) =="RasterLayer")  r[] else rep( NA, ncell( nz)), 
##         start= c(  1,  1, day),
##         count= c( -1, -1, 1))
##       nc_sync( nc)
##     }
##   }
##   nc_close( nc)
## }

vcsnCreateNc <- function( vcsnZipFile, compression= 5) {
  vcsnDatFiles <- sort( as.character( unzip( vcsnZipFile, list= TRUE)$Name))
  vcsnZipFileYear <- as.integer( str_extract( basename( vcsnZipFile), "[0-9]{4}"))
  vcsnNcFile <- str_replace( basename( vcsnZipFile), "zip", "nc")
  if( file.exists( vcsnNcFile)) file.remove( vcsnNcFile)
  nc <- nc_create( 
    vcsnNcFile, 
    vcsnVars( 
      vcsnZipFileYear,
      length( vcsnDatFiles), 
      compression= compression))
  ncatt_put( nc, 0, "title", 
    sprintf( "New Zealand Virtual Climate Station Network (VCSN) weather data, %s",
      vcsnZipFileYear))
  ncatt_put( nc, 0, "institution", "Computation Institute, University of Chicago")
  ncatt_put( nc, 0, "source", "New Zealand National Institute of Water and Atmospheric Research")
  ## ncatt_put( nc, "crs", "grid_mapping_name", "latitude_longitude")
  ## ncatt_put( nc, "crs", "semi_major_axis", 6378388)
  ## ncatt_put( nc, "crs", "inverse_flattening", 297)
  for( day in 1:length( vcsnDatFiles) ) {
    vcsnDatFile <- unzip( vcsnZipFile, files= vcsnDatFiles[ day])
    vcsnDf <- getVcsnDf( vcsnDatFile)
    file.remove( vcsnDatFile)
    for( vcsnVar in names( nc$var)) {  
      r <- try( rasterize( vcsnDf[, c("Longt", "Lat")], nz, vcsnDf[, vcsnVar]), 
                silent= TRUE)
      ncvar_put( nc, 
        vcsnVar, 
        if( class( r) =="RasterLayer")  r[] else rep( NA, ncell( nz)), 
        start= c(  1,  1, day),
        count= c( -1, -1, 1))
      nc_sync( nc)
    }
  }
  nc_close( nc)
}


vcsnCreateNc( vcsnZipFiles[1])

fe <- foreach( vcsnZipFile= vcsnZipFiles)

fe %dopar% vcsnCreateNc( vcsnZipFile)
