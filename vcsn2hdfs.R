library(RJSONIO)
library(raster)

world <- raster()
res(world) <- 3/60

vcsn <- read.csv("19720101_vcsn.dat", header= TRUE)

vcsn <- within( vcsn, {
  Date <- as.characater( as.Date( Date, format="%d/%m/%Y"))
  cell <- as.character( cellFromXY( world, cbind( Longt, Lat))}))


toKeyValJSON <- function( x, 
    keys= names( x)[1], 
    values= names( x)[ -which( names( x) %in% keys)]) {
  k <- toJSON( x[ keys], collapse= "")
  v <- toJSON( x[ values], collapse= "")
  paste( k, v, sep= "\t")
}

writeLines( apply( vcsn, 1, toKeyValJSON, keys= c("cell","Date")),
            con= "19720101_vcsn.json")