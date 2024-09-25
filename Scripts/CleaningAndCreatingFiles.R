
library(sf)
library(terra)


##=================================================================================================================
##                
##                Creates buffers around streams and lakes  
##                The union crashed in R so I exported to qgis
##
##==================================================================================================================

streams <- "Data/spatialdat/Plot_selection_Beetles/SEKI lakes rivers fire/riversstreamsall.shp"
file.exists(streams)
streams_seki <- st_read(streams)
crs(streams_seki)

lakes <- "Data/spatialdat/Plot_selection_Beetles/SEKI lakes rivers fire/lakes.shp"
file.exists(streams)

lakes_seki <- st_read(lakes)

buff <- st_buffer(lakes_seki, 100)
buff_streams <- st_buffer(streams_seki, 50)

## writing buffers
# st_write(buff, 
#          "Data/spatialdat/Plot_selection_Beetles/SEKI lakes rivers fire/buffer_lakes_100m.shp")
# st_write(buff_streams, 
#          "Data/spatialdat/Plot_selection_Beetles/SEKI lakes rivers fire/buffer_streams_50m.shp")

## this crashes R so I exported to qgis
newdat <- st_union(buff, buff_streams)



##=================================================================================================================
##                      
##                Calculates slope and aspect rasters
##                Then creates files for clipping later in plot selection
##    
##==================================================================================================================

## reading in aspect tif
aspectpath <- "Data/spatialdat/Plot_selection_Beetles/DEM data/aspect_seki.tif"
file.exists(aspectpath)
aspect1 <- terra::rast(aspectpath)
aspect <- terra::project(aspect1, pico.shp)
plot(aspect)


## creating an aspect raster with south-facing aspects: 101-259
aspect_mod <- app(aspect, fun=function(x) { 
  x[x > 260] <- NA; return(x)} )
plot(aspect_mod)

aspect_mod2 <- app(aspect_mod, fun=function(x) { 
  x[x < 100] <- NA; return(x)} )

plot(aspect_mod2)
help(app)

## checking results
m.aspect <- as.matrix(aspect_mod2)
max(m.aspect, na.rm=T)
hist(m.aspect)

#terra::writeRaster(aspect_mod2, "Data/spatialdat/Plot_selection_Beetles/DEM data/aspect_seki_SOUTH.tif", overwrite=T)


## creating a slope raster with slopes <25

dempath <- "Data/spatialdat/Plot_selection_Beetles/DEM data/seki_dem.tif"
file.exists(dempath)
dem1 <- terra::rast(dempath)
dem <- terra::project(dem1, pico.shp)
plot(dem1)
dem

## extracting slopes
area_slope <- terra::terrain(dem,'slope')
plot(area_slope)

## clipping by 25°
slope_mod <- app(area_slope, fun=function(x) { 
  x[x > 25] <- NA; return(x)} )
plot(slope_mod)

## checking results
m.slope <- as.matrix(slope_mod)
max(m.slope, na.rm=T)
hist(m.slope) ## it worked!!


#terra::writeRaster(slope_mod, "Data/spatialdat/Plot_selection_Beetles/DEM data/slope_seki_less25.tif", overwrite=T)


##=================================================================================================================
##                      Reading in Jenny's plots to make sure we add them to strata
##==================================================================================================================
getwd()

northfacing <- "Data/spatialdat/Plot_selection_Beetles/Polygons_roadsEnd.shp"
southfacing <- "Data/spatialdat/Plot_selection_Beetles/CopperCreekTransect.shp"
file.exists(southfacing)

north <- sf::st_read(northfacing)
plot(north, max.plot=1)

south <- sf::st_read(southfacing)
plot(south,max.plot=1)

jnorth <- c(2,4,5,6,9,11)
jsouth <- c(1,2)

jnorth2 <- north[north$id %in%c(2,6,12,15,20),]
plot(jnorth2, max.plot=1)

centroid_north <- st_centroid(jnorth2)

plot(jnorth2, max.plot=1, col="grey")
plot(centroid_north, add=T, max.plot=1, col="blue")

#write_sf(centroid_north, "Data/spatialdat/Plot_selection_Beetles/randomplots.shp")


##=================================================================================================================
##            Reading in waypoints from Jenny's sampled plots and converting to spatial points         
##==================================================================================================================
jplots <- read_csv("Data/spatialdat/Plot_selection_Beetles/CSV files plotselection/PlotCoordinates.csv")


