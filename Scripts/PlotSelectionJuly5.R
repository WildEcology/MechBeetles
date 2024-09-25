
## ---------------------------
##
## Script name: Plot selection for SEKI beetle project
##
## Author: Dr. Joan Dudney
##
## Date Created: 2022-06-06
##
## Copyright (c) Joan Dudney, 2022
## Email: dudney@ucsb.edu
##
## ---------------------------
##
## Notes: This code reads in shapefiles and tifs, calculates 
##  the ppt strata for each species, and creates raster files
##  for each species to be read into the sgsR package
##
## ---------------------------

## to do: add in seki pial plots and jenny's plots
## make sure slope isn't impossible and plots are located on trees



# Packages
library(tidyverse)
library(terra)
library(sgsR)
library(tmap)
library(tmaptools)
library(sf)


# set working directory
wdir <- 'Data/spatialdat'


##=================================================================================================================
##                      
##    ## Reading in spatial data, checking their accuracy, and reprojecting them 
##    ## so they all have the same CRS
##    
##==================================================================================================================

##PILA
pila_polys <- "Data/white pine polygons/PILA/Simplified_dissolved_PILA.shp"
pila.shp <- vect(pila_polys)
plot(pila.shp)

## PIBA
piba_polys <- "Data/white pine polygons/Eastern Sierra/Dissolv_PIBA_east.shp"
piba.shp <- terra::vect(piba_polys)
plot(piba.shp)

##PIMO
pimo_polys <- "Data/white pine polygons/PIMO/Simplified_dissolved_PIMO.shp"
pimo.shp <- vect(pimo_polys)

##PIAL
pial_polys <- "Data/white pine polygons/Eastern Sierra/Dissolv_PIAL_east.shp"
pial.shp <- vect(pial_polys)
plot(pial.shp)

##PICO
pico_polys <- "Data/white pine polygons/Eastern Sierra/Dissolv_PICO_east.shp"
pico.shp <- vect(pico_polys)
plot(pico.shp)


## reading in ppt prism data 30 year normals

rasnam <- "Data/spatialdat/Prism/prism_ppt/PRISM_ppt_30yr_normal_800mM3_annual_bil.bil"
file.exists(rasnam)

ppt_prism <- terra::rast(rasnam)

## projecting to pico.shp crs
ppt_proj <- terra::project(ppt_prism, pico.shp)


##=================================================================================================================
##                      Creating climate maps for each species
##==================================================================================================================

## reading in buffer data and reprojecting

buffer_all <- terra::vect("Data/spatialdat/Plot_selection_Beetles/SEKI lakes rivers fire/BufferLakes100mstreams50m.shp")
buffer <- terra::project(buffer_all, pico.shp)
#plot(buffer)

slope_ras <- terra::rast("Data/spatialdat/Plot_selection_Beetles/DEM data/slope_seki_less25.tif")
slope <- terra::project(slope_ras, pico.shp)

aspect_ras <- terra::rast("Data/spatialdat/Plot_selection_Beetles/DEM data/aspect_seki_SOUTH.tif")
aspect <- terra::project(aspect_ras, pico.shp)

tmap_mode("view")

## visualizing rasters
aspectfig <- tm_shape(aspect)+
  tm_raster(style= "pretty",
            title="Apect")+
  tm_layout(legend.outside = T)

slopefig <- tm_shape(slope)+
  tm_raster(style= "cont",title="Slope", palette=get_brewer_pal("-Greys", plot=FALSE))+
  tm_layout(legend.outside = TRUE)

aspectfig + slopefig


## Combining slope and aspect rasters into one

slope1_0 <- slope >= 0
#plot(slope1_0)

slope_fig <- tm_shape(slope1_0)+
  tm_raster()

aspect1_0 <- aspect >= 0
#plot(aspect1_0)

combdem <- aspect1_0 * slope1_0

com_fig <- tm_shape(combdem)+
  tm_raster(style= "cont",title="Slope", palette=get_brewer_pal("-Greys", plot=FALSE))

slope_fig+com_fig

#combdem <- slope1_0


## checking the dem file 
# global(combdem,  'max')
# freq(combdem)
# values(combdem)


##=================================================================================================================
##          ## first cropping the prism and dem data 
##          ## then creating a combined raster with dem + precip cells
##==================================================================================================================

## crop ppt raster and project it to shapefile crs
## get crop dimensions

cropped_ppt <- ppt_proj %>% 
  terra::crop(combdem)

#terra::writeRaster(cropped_ppt, "seki_ppt_prism.tif")

## rasters have different extents and resolution
plot(cropped_ppt)
plot(combdem, add=T)

## creating a raster with the same dims as ppt data
newdem = combdem %>% 
  terra::resample(cropped_ppt)

## check if it worked
plot(newdem)

## combine with climate data
dem_ppt <- cropped_ppt * newdem
plot(dem_ppt)

##=================================================================================================================
##       Create a function that extracts climate data by species ranges
##       and buffered by streams and lakes
##==================================================================================================================


## function
clim_range <- function(spdat){
  sp_range = spdat %>% 
    terra::rasterize(dem_ppt, touches=TRUE)## make sure it's extracting all climate data that covers the species' ranges
  rast_map <- dem_ppt %>% ## now clip the ppt prism data to the species' ranges
    terra::mask(sp_range) %>% 
    terra::mask(buffer) 
  return(rast_map)
}


## create a tibble of shapefiles and species names
plotlist <- tibble(species = c("PIBA", "PIMO", "PICO", "PILA", "PIAL"), 
                   sp_shapefile = c(list(piba.shp), list(pimo.shp),
                                    list(pico.shp), list(pila.shp), list(pial.shp)))

## function that runs clim_range for each species
write_clim_range <- function(sp_code) {
  sp_shp <- plotlist %>%
    filter(species == sp_code) %>%
    pull(sp_shapefile)
  rastclim <- sp_shp[[1]] %>%
    clim_range()
  out_name <- paste0("Data/white pine polygons/Prism Ranges/", sp_code, "all_buff.tif")
  terra::writeRaster(rastclim, filename=out_name, 
                     filetype = "GTiff", overwrite=T)
}

## for loop that writes tifs for each species
species_list <- plotlist %>% 
  pull(species)

for (sp in species_list) {
  write_clim_range(sp)
}


##=================================================================================================================
##              Calculating strata        
##==================================================================================================================

## list of species distributions and climate data
path='Data/white pine polygons/Prism Ranges/'
files <- sort(list.files('Data/white pine polygons/Ranges July 5/', pattern = "(all)+(_buff)+(.tif)"))
#files <- sort(list.files('Data/white pine polygons/Prism Ranges/', pattern = "(buffered)+(.tif)"))
#files <- sort(list.files('Data/white pine polygons/Prism Ranges/', pattern = "(buff)+(.tif)"))
filenames <- paste0(path, files)

## reading in files
myfiles = lapply(filenames, terra::rast)

species = sort(c("PIBA", "PIMO", "PICO", "PILA", "PIAL"))

newfiles=list()

for (file in myfiles) {
  newlist = list(file)
  newfiles = rbind(newfiles, newlist)
}


sp_data <- tibble(rasters=myfiles, species=species)

xy_clim <- function(sp_map) {
  new_map <- as.data.frame(sp_map, xy = TRUE) %>% 
    drop_na()
  return(new_map)
}

sp_data_all <- sp_data %>%
  mutate(clim_vals = map(rasters, xy_clim)) %>%
  unnest(cols = clim_vals) %>% 
  dplyr::select(-rasters)


colnames(sp_data_all)[c(4)]=c("ppt")

# standard_devs <- sp_data_all %>% 
#   dplyr::select(-c(x,y)) %>% 
#   group_by(species) %>% 
#   summarise(mean = mean(ppt, na.rm=T),mean1sd=mean(ppt)+sd(ppt), mean1sdbelow=mean(ppt)-sd(ppt),
#             mean1.5sd=mean(ppt)+(1.5*sd(ppt)), mean1.5sdbelow=mean(ppt)-(1.5*sd(ppt)),
#             mean2sd=mean(ppt)+(2*sd(ppt)), mean2sdbelow=mean(ppt)-(2*sd(ppt))) %>% 
#   pivot_longer(-species) %>% 
#   group_by(species) %>% 
#   dplyr::select(-name)

quantnew = sp_data_all %>% 
  group_by(species) %>% 
  summarize(quant = quantile(ppt, probs = c(.05, .10, .20, .50, .80, .90, .95))) %>% 
  ungroup()

quantextreme = sp_data_all %>% 
  group_by(species) %>% 
  summarize(quant = quantile(ppt, probs = c(.03, .97))) %>% 
  ungroup()

# quantsextremes = sp_data_all %>% 
#   group_by(species) %>% 
#   summarize(quant = quantile(ppt, probs = c(.01, .99))) %>% 
#   ungroup()

#write_csv(quantnew, "Data/white pine polygons/Ranges July 5/strata_quants_ALLBuffers_newJuly7.csv")
#write_csv(quantsextremes, paste0(path, "strata_quants_ALLBuffers_extremes.csv"))
#write_csv(quantextreme, paste0(path, "strata_quants_ALLBuffers_extremes_july8.csv"))

# strata2=strata %>% dplyr::select(-probs)
# 
# comb_dat_quants <- sp_data_all %>% 
#   left_join(strata2) %>% 
#   left_join(quantsuneven)
# 
# pila = ggplot(filter(comb_dat_quants, species=="PILA"), aes(x=ppt))+
#   geom_vline(xintercept = quant)+
#   geom_histogram()
#   
# 
# theme_set(
#   theme_bw(base_size = 15)+
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           plot.title = element_text(hjust = 0.5))
# )
# 
# 
# ## show figures for each strata
# for (sp_name in species){
#   
#   sp=comb_dat_quants %>% 
#     filter(species==sp_name) %>% 
#     pull(quant)
# 
#   spdata=comb_dat_quants %>% 
#     filter(species==sp_name)
#   
#   sp_plot <- ggplot(data=spdata)+
#     geom_histogram(aes(x=ppt, fill=species))+
#     scale_fill_manual(values="darkgreen")+
#     geom_vline(xintercept = sp)+
#     xlab("Precipitation (30 year normals)")+
#     ylab("Count")+
#     ggtitle(paste0(sp_name," ", "Strata ~10-20th quantiles"))
#   
#   print(sp_plot)
#             
# }


#write_csv(standard_devs, paste0(path, "strata_standard_devs_SlopeOnlyandBuffers.csv"))


##=================================================================================================================
##                      pulling data
##==================================================================================================================


#strata=read_csv("Data/white pine polygons/Ranges July 5/strata_quants_ALLBuffers_newJuly7.csv")
strata=read_csv(paste0(path, "strata_quants_ALLBuffers_extremes_july8.csv"))
finalfiles <- sort(list.files('Data/white pine polygons/Ranges July 5/', pattern = "(all)+(_buff)+(.tif)"))
finalfilenames <- paste0(path, finalfiles)

## reading in files
myfiles_final = lapply(finalfilenames, terra::rast)

species = sort(c("PIBA", "PIMO", "PICO", "PILA", "PIAL"))



buff_data <- tibble(rasters=myfiles_final, species=species)


## a function to pull the calculated strata
strata_pull <- function(sp_pull){
  
  ## pull strata and sort them
  stratdat <- strata %>% 
    filter(species == sp_pull) %>% 
    mutate(value1 = sort(quant)) %>% 
    pull(value1)
}

## a function to create the stratified raster
strat_rast <- function(sp_code){
  
  ## pull raster for each species
  rast_file <- buff_data %>%
    filter(species==sp_code) %>% 
    pull(rasters)
  
  rast_file = rast_file[[1]]
  
  ## pull breaks for each species
  str <- strata_pull(sp_code)
  
  ## create sraster
  sp_strat_r <- strat_breaks(
    mraster = rast_file,
    breaks =  str)
  
  return(sp_strat_r)
}


## for loop that creates strata tifs for each species
species_dat <- buff_data %>% 
  pull(species)

list_rs <- list()


## for loop for 5-95 quantiles
# for (sp in species_dat) {
#   
#   ## sample plots
#   r_file <- strat_rast(sp)
#   list_rs <- c(list_rs, r_file)
# }


## for loop for extreme values
for (sp in species_dat) {
  
  ## sample plots
  r_file <- strat_rast(sp)
  r_file[r_file==2] <- NA
  list_rs <- c(list_rs, r_file)
}


plot(list_rs[[1]])


## randomly sample 3 plots in each strata
## then export to kml

## reading in SEKI trails for buffering

# trails <- "Data/spatialdat/Plot_selection_Beetles/Trails and Roads/NationalParksTrails.shp"
# trails_seki <- st_read(trails)

trails <- "Data/spatialdat/Plot_selection_Beetles/Trails and Roads/ClippedMostTrailsRoadsNPSFS.shp"
trails_seki1 <- st_read(trails)
prj2<-toString(crs(pico.shp))
trails_seki <- st_transform(trails_seki1, prj2)


trails_fig <- tm_shape(trails_seki)+
  tm_lines(palette=get_brewer_pal("-Greys", plot=FALSE))+
  tm_layout(legend.outside = T)

trails_fig

kmlfile <- st_read(paste0("Data/white pine polygons/KML files/", "PICOJuly72020_80samples.kml"))
plot(kmlfile)

points_fig <- tm_shape(kmlfile)+
  tm_dots(col="darkblue", size=.1, title = "Plots")+
  tm_layout(legend.outside = F)



trails_fig + points_fig + aspectfig + slopefig

help(tm_dots)
#st_trails <- sf::st_as_sf(trails_seki)
#st_write(st_trails, paste0("Data/white pine polygons/KML files/",  "SEKI_trails.kml"), driver = "kml")


## the for loop:
plot(list_rs[[2]])
species_dat
#for (i in length(list_rs)){

## sample 3 plots in each strata
plots_samp <-sample_strat(sraster = list_rs[[3]], # input sraster
                          allocation = "equal",
                          nSamp = 3, 
                          access = trails_seki, # define trail network
                          mindist = 100, # minimum distance samples must be apart from one another
                          buff_inner = 10, # inner buffer - no samples within this distance from trails
                          buff_outer = 1000) # outer buffer - no samples further than this distance from trails

##converting to sf and writing each file into kml
stdat <- sf::st_as_sf(plots_samp)
st_write(stdat, paste0("Data/white pine polygons/KML files/", "PICO", "July6_extremes.kml"), driver = "kml")

#}


plot(sraster)



plot(piba_map.tiff)










# plots_piba <- sample_srs(raster = list_rs[[1]], # input mraster
#                          nSamp = 100, # number of desired samples
#                          access = trails_seki, # define access road network
#                          mindist = 400, # minimum distance samples must be apart from one another
#                          buff_inner = 50, # inner buffer - no samples within this distance from road
#                          buff_outer = 1000, # outer buffer - no samples further than this distance from road
#                          plot = TRUE) # plot
# 
# 
# test1 <- sample_strat(sraster = list_rs[[1]], 
#                       nSamp = 200, 
#                       mindist = 500,
#                       plot = TRUE)
# 
# ## export to kml files
#     
#     
#     stdat <- s <- sf::st_as_sf(test1)
#     
# 
# out_name <- paste0("Data/white pine polygons/KML files/", sp_code, "samplepoints.kml")
# 
# st_write(test1, paste0("Data/white pine polygons/KML files/", "sp_code", "samplepoints.kml"), driver = "kml")
# 
# plots_piba <- sample_srs(raster = piba_strata, # input mraster
#                          nSamp = 100, # number of desired samples
#                          access = trails_seki, # define access road network
#                          mindist = 400, # minimum distance samples must be apart from one another
#                          buff_inner = 50, # inner buffer - no samples within this distance from road
#                          buff_outer = 1000, # outer buffer - no samples further than this distance from road
#                          plot = TRUE) # plot
# 
# 
# 
# 
# 
# 
# ## now pull vectors of the quantiles for each species
# 
# piba_max <- strata %>% 
#   filter(species == "PIBA") %>% 
#   mutate(value1 = sort(value)) %>% 
#   pull(value1)
# 
# piba_maxbuff <- strata_buff %>% 
#   filter(species == "PIBA") %>% 
#   mutate(value1 = sort(value)) %>% 
#   pull(value1)
# 
# pila_max <- standard_devs %>% 
#   filter(species == "PILA") %>% 
#   mutate(value1 = sort(value)) %>% 
#   pull(value1)
# 
# pimo_max <- standard_devs %>% 
#   filter(species == "PIMO") %>% 
#   mutate(value1 = sort(value)) %>% 
#   pull(value1)
# 
# pico_max <- standard_devs %>% 
#   filter(species == "PICO") %>% 
#   mutate(value1 = sort(value)) %>% 
#   pull(value1)
# 
# pial_max <- standard_devs %>% 
#   filter(species == "PIAL") %>% 
#   mutate(value1 = sort(value)) %>% 
#   pull(value1) 
#  
# 
# pial_strata <- strat_breaks(
#   mraster = pial_map,
#   breaks =  pial_max
# )
# 
# piba_strata <- strat_breaks(
#   mraster = piba_map,
#   breaks =  piba_max
# )
# 
# pila_strata <- strat_breaks(
#   mraster = pila_map,
#   breaks =  pila_max
# )
# 
# piba_strata
# plot_piba <- extract_metrics(mraster = piba_map)
#                         existing = existing)
# 
# plot(plot_piba)
# 
# test1 <- sample_strat(sraster = combstrat, # use mraster as input for sampling
#                           nSamp = 40, force = T, # request 200 samples be taken
#                           mindist = 500,
#                           plot = TRUE)
# 
# plots_piba <- sample_srs(raster = piba_strata, # input mraster
#                          nSamp = 100, # number of desired samples
#                          access = trails_seki, # define access road network
#                          mindist = 400, # minimum distance samples must be apart from one another
#                          buff_inner = 50, # inner buffer - no samples within this distance from road
#                          buff_outer = 1000, # outer buffer - no samples further than this distance from road
#                          plot = TRUE) # plot
# 
# plots_pila <- sample_srs(raster = pila_strata, # input mraster
#                          nSamp = 100, # number of desired samples
#                          access = trails_seki, # define access road network
#                          mindist = 400, # minimum distance samples must be apart from one another
#                          buff_inner = 50, # inner buffer - no samples within this distance from road
#                          buff_outer = 1000, # outer buffer - no samples further than this distance from road
#                          plot = TRUE)
# 
# 
# existing2 <- sample_strat(sraster = pila_strata, # use mraster as input for sampling
#                          nSamp = 40, force = T, # request 200 samples be taken
#                          mindist = 500,
#                          plot = TRUE)
# 
# new_exist <- st_as_sf(existing2)
# 
# library(maptools)
# 
# st_write(plots_piba, "piba.kml", driver = "kml")
# 
# existing <- sample_strat(sraster = piba_strata, # use mraster as input for sampling
#                          nSamp = 100, # request 200 samples be taken
#                          mindist = 1000,
#                          plot = TRUE)
# 
# plot(existing, max.plot=1)
# 
# 
# plots_pial <- sample_srs(raster = piba_strata, # input mraster
#            nSamp = 100, # number of desired samples
#            access = trails, # define access road network
#            mindist = 400, # minimum distance samples must be apart from one another
#            buff_inner = 50, # inner buffer - no samples within this distance from road
#            buff_outer = 1000, # outer buffer - no samples further than this distance from road
#            plot = TRUE) # plot
# 
# plot(pial_strata, max.plot=1)
# plot(trails_seki, max.plot=1, add=T)
# plot(plots_pial, add=T, col="black", size=10)
# 
# library(tmap)
# tmap_mode("view")
# tm_shape(pial_strata) +
#   tm_raster()+
# tm_shape(trails_seki)+
#   tm_lines()+
# tm_shape(plots_pial)+
#   tm_dots()
# 
# 
# 
# ##=================================================================================================================
# ##                 OLD CODE     
# ##==================================================================================================================
# # colnames(sp_data_all)
# # 
# # sp_data <- tibble(species = c("PIBA", "PIMO", "PICO", "PILA", "PIAL"), 
# #                   shapefile = c(list(piba_map), list(pimo_map),
# #                                 list(pico_map), list(pila_map), list(pial_map)))
# # 
# # ## calculating quantiles for each map
# # 
# # ## function to extract ppt values from raster data
# # xy_clim <- function(sp_map) {
# #   new_map <- as.data.frame(sp_map,xy = TRUE) %>% 
# #       drop_na()
# #   
# #   return(new_map)
# # }
# # 
# # ## mapping on the xy_clim function to the list of raster files
# # sp_data <- sp_data %>%
# #   mutate(clim_vals = map(shapefile, xy_clim))
# # 
# # ## unnesting and cleaning
# # sp_data_all <- sp_data %>% 
# #   unnest(cols = clim_vals) %>% 
# #   select(-shapefile) 
# # 
# # colnames(sp_data_all)[c(4)]=c("ppt")
# # 
# # 
# # standard_devs <- sp_data_all %>% 
# #   select(-c(x,y)) %>% 
# #   group_by(species) %>% 
# #   summarise(mean = mean(ppt, na.rm=T),mean1sd=mean(ppt)+sd(ppt), mean1sdbelow=mean(ppt)-sd(ppt),
# #             mean1.5sd=mean(ppt)+(1.5*sd(ppt)), mean1.5sdbelow=mean(ppt)-(1.5*sd(ppt)),
# #             mean2sd=mean(ppt)+(2*sd(ppt)), mean2sdbelow=mean(ppt)-(2*sd(ppt))) %>% 
# #   pivot_longer(-species) %>% 
# #   group_by(species) %>% 
# #   select(-name)
# # 
# # #write_csv(strata_10, "Data/strata_10_start0.5_withbuffer.csv")
# 
# 
# # ##=================================================================================================================
# # ##            Calculates the climate range buffered by lakes and streams NOT slope and aspect      
# # ##==================================================================================================================
# # 
# # # terra::writeRaster(pico_map, "Data/white pine polygons/Prism Ranges/pico_map_buffered.tiff",
# # # filetype = "GTiff")
# # 
# # pimo_map <- pimo.shp %>% 
# #   clim_range()
# # plot(pimo_map)
# # 
# # # terra::writeRaster(pimo_map, "Data/white pine polygons/Prism Ranges/pimo_map_buffered.tiff",
# # #       filetype = "GTiff", overwrite=T)
# # 
# # pila_map <- pila.shp %>% 
# #   clim_range()
# # plot(pila_map)
# # 
# # # terra::writeRaster(pila_map, "Data/white pine polygons/Prism Ranges/pila_map_buffered.tiff",
# # #                    filetype = "GTiff", overwrite=T)
# # 
# # 
# # pial_map <- pial.shp %>% 
# #   clim_range()
# # plot(pial_map)
# # 
# # # terra::writeRaster(pial_map, "Data/white pine polygons/Prism Ranges/pial_map_buffered.tiff",
# # #                    filetype = "GTiff", overwrite=T)
# # 
# # piba_map <- piba.shp %>% 
# #   clim_range()
# # plot(piba_map)
# # 
# # # terra::writeRaster(piba_map, "Data/white pine polygons/Prism Ranges/piba_map_buffered.tiff",
# # #                     filetype = "GTiff", overwrite=T)
# # 
# # 
# 
# 
# 
# 
# ##=================================================================================================================
# ##            READING IN BUFFER DATA          
# ##==================================================================================================================
# 
# library(sf)
# 
# trails <- "Data/spatialdat/Plot_selection_Beetles/NationalParksTrails.shp"
# trails_seki <- st_read(trails)
# plot(trails_seki, max.plot=1)
# 
# trails_crop = trails_seki %>% 
#   st_crop(pial.shp2)
# 
# 
# 
# plot(trails_crop, max.plot=1)
# 
# 
# pial_polys <- "Data/white pine polygons/PIAL/Simplified_dissolved_PIAL.shp"
# pial.shp2 <- st_read(pial_polys)
# 
# 
# strat_quantiles(
#   mraster = mr$zq90,
#   nStrata = 4,
#   plot = TRUE
# )
# 
# plot(seki_only)
# 
# extract_strata()
# e.sr <- extract_strata(sraster = pico_map)
# 
# sample_srs(raster = pico_map, # input mraster
#            nSamp = 100, # number of desired samples
#            access = trails_seki, # define access road network
#            mindist = 200, # minimum distance samples must be apart from one another
#            buff_inner = 10, # inner buffer - no samples within this distance from road
#            buff_outer = 1000, # outer buffer - no samples further than this distance from road
#            plot = TRUE)
# 
# 
# sample_srs(raster = pico_map, # input mraster
#            nSamp = 200, # number of desired samples
#            access = trails_seki, # define access road network
#            mindist = 200, # minimum distance samples must be apart from one another
#            buff_inner = 50, # inner buffer - no samples within this distance from road
#            buff_outer = 200, # outer buffer - no samples further than this distance from road
#            plot = TRUE)
# 
# ## looking at the map
# # library(sf)
# # buffer_trails2 <- st_read(buffer)
# # plot(buffer_trails2[1])
# 
# 
# 
# library(tmap)
# tmap_mode("view")
# 
# tm_shape(buffer_trails2) +
#   tm_polygons(col = "TrailName")
# 
# tmap_mode("view")
# 
# tm_shape(pimo_map) +
#   tm_raster()
# 
# 
# 
# sample_srs(raster = piba_map, # input sraster
#            nSamp = 200, # number of desired samples
#            plot = TRUE)
# 
# sample_systematic(raster = piba_map, # input sraster
#                   cellsize = 10, # grid distance
#                   plot = TRUE) 
# 
# sample_balanced(mraster = piba_map, # input
#                 nSamp = 100, # desired sample number
#                 plot = TRUE)
# 
# sample_ahels(mraster = pico_map, 
#              existing = existing, # existing samples
#              nQuant = 20, # define 20 quantiles
#              nSamp = 300, # total samples desired
#              filename = tempfile(fileext = ".shp")) # write samples to disc
# 
# 
# crop_tmean <- crop(tmean_proj, extent(pico.shp))
# plot(crop_ppt)
# plot(crop_tmean)
# 
# 
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# # Summarize species niches in SEKI  --------------------------------------
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# library(stars)
# library(raster)
# 
# ## call the shapefile of species range; then create a raster from that shapefile that matches ppt_prism 
# ## dimensions and datatype; getCover = TRUE extracts the fraction of each grid cell that is covered by the polygons;
# ## this allows for more accurate extraction of climate data; especially since prism dimensions and polygon dimensions
# ## are very different
# 
# sp_range_pimo = pico.shp %>% 
#   terrra: rasterize(crop_ppt, getCover=TRUE) 
# 
# ## creates a raster with 0/1
# sp_range_pimo[sp_range_pimo==0] <- NA ## make 0 = no data
# 
# plot(sp_range_pimo)
# 
# ppt_vals <- crop_ppt %>% ## mask ppt with no data/1 raster
#   mask(sp_range_pimo) %>% 
#   as.data.frame(xy = TRUE) %>% 
#   drop_na()
# 
# 
# 
# ## piba
# sp_range_piba = piba.shp %>% 
#   rasterize(crop_ppt, getCover=TRUE) 
# 
# ## creates a raster with 0/1
# sp_range_pimo[sp_range_pimo==0] <- NA ## make 0 = no data
# 
# plot(sp_range_pimo)
# 
# ppt_vals <- crop_ppt %>% ## mask ppt with no data/1 raster
#   mask(sp_range_pimo) %>% 
#   as.data.frame(xy = TRUE) %>% 
#   drop_na()
# 
# 
# 
# library(tmap)
# tmap_mode("view")
# tm_shape(sp_range_pimo) +
#   tm_raster()
# 
# temp_vals <- tmax_prism %>% 
#   mask(sp_range) %>% 
#   as.data.frame(xy = TRUE) %>% 
#   drop_na()
# 
# # Combine into tibble
# clim_vals <- ppt_vals %>% 
#   left_join(temp_vals, by = c("x", "y"))
# 
# 
# 
# sp_range_pimo = pico.shp %>% 
#   rasterize(crop_ppt, getCover=TRUE) 
# 
# ## creates a raster with 0/1
# sp_range_pimo[sp_range_pimo==0] <- NA ## make 0 = no data
# 
# plot(sp_range_pimo)
# 
# ppt_vals <- crop_ppt %>% ## mask ppt with no data/1 raster
#   mask(sp_range_pimo) %>% 
#   as.data.frame(xy = TRUE) %>% 
#   drop_na()
# 
# 
# spfile = pico.shp
# 
# sp_data <- tibble(species = c("PIBA", "PIMO", "PICO", "PILA", "PIAL"), 
#                   shapefile = c(list(piba.shp), list(pimo.shp),
#                                 list(pico.shp), list(pila.shp), list(pial.shp)))
# 
# # Pull and organize climate into dataframe for each species
# 
# pull_clim <- function(spfile){
#   # Pull relevant range map
#   sp_range <- spfile %>%
#     rasterize(crop_ppt, getCover=TRUE) ## creates a raster with 0/1
#   sp_range[sp_range==0] <- NA ## make 0 = no data
#   
#   # Pull ppt and temp values
#   ppt_vals <- crop_ppt %>% ## mask cwd historic with no data/1 raster
#     mask(sp_range) %>% 
#     as.data.frame(xy = TRUE) %>% 
#     drop_na()
#   
#   temp_vals <- crop_tmean %>% 
#     mask(sp_range) %>% 
#     as.data.frame(xy = TRUE) %>% 
#     drop_na()
#   
#   # Combine into tibble
#   clim_vals <- ppt_vals %>% 
#     left_join(temp_vals, by = c("x", "y"))
#   
#   return(clim_vals)
# }
# 
# sp_data <- sp_data %>%
#   mutate(clim_vals = map(shapefile, pull_clim))
# 
# sp_data_all <- sp_data %>% 
#   unnest(cols = clim_vals) %>% 
#   select(-shapefile) 
# 
# colnames(sp_data_all)[c(4,5)]=c("ppt", "tmean")
# 
# 
# ## now creating a shapefile for each species
# 
# 
# 
# shape_clim <- function(spfile){
#   # Pull relevant range map
#   sp_range <- spfile %>%
#     rasterize(crop_ppt, getCover=TRUE) ## creates a raster with 0/1
#   sp_range[sp_range==0] <- NA ## make 0 = no data
#   
#   # Pull ppt and temp values
#   ppt_shape <- crop_ppt %>% ## mask cwd historic with no data/1 raster
#     mask(sp_range)
#   
#   #temp_shape <- crop_tmean %>% 
#   # mask(sp_range) 
#   
#   # Combine into list
#   clim_shapefiles <- list(ppt_shape)
#   
#   return(clim_shapefiles)
# }
# 
# rasters_pico <- piba.shp %>% 
#   shape_clim() %>% 
#   unlist()
# 
# plot(rasters_pico[[1]])
# 
# 
# ## plot selection
# 
# library(sgsR)
# 
# 
# help(terra)
# #--- Load mraster files ---#
# r <- system.file("extdata", "mraster.tif", package = "sgsR")
# 
# #--- load the mraster using the terra package ---#
# mraster <- terra::rast(rasters_pico)
# 
# #--- apply quantiles algorithm to mraster ---#
# sraster <- strat_quantiles(mraster = mraster$zq90, # use mraster as input for stratification
#                            nStrata = 4) # produce 4 strata
# 
# #--- apply stratified sampling ---#
# existing <- sample_strat(sraster = sraster, # use sraster as input for sampling
#                          nSamp = 200, # request 200 samples
#                          mindist = 100, # samples must be 100 m apart
#                          plot = TRUE) # plot output
# 
# 
# 
# 
# 
# 
# 
# 
# # Pull and organize climate distribution for species
# pull_clim <- function(spp_code){
#   print(spp_code)
#   # Pull relevant range map
#   sp_range <- range_sf %>%
#     filter(sp_code == spp_code) %>% 
#     rasterize(ppt_prism, getCover=TRUE) ## creates a raster with 0/1
#   sp_range[sp_range==0] <- NA ## make 0 = no data
#   
#   # Pull cwd and aet values
#   ppt_vals <- ppt_prism %>% ## mask cwd historic with no data/1 raster
#     mask(sp_range) %>% 
#     as.data.frame(xy = TRUE) %>% 
#     drop_na()
#   
#   temp_vals <- tmax_prism %>% 
#     mask(sp_range) %>% 
#     as.data.frame(xy = TRUE) %>% 
#     drop_na()
#   
#   # Combine into tibble
#   clim_vals <- ppt_vals %>% 
#     left_join(temp_vals, by = c("x", "y"))
#   
#   return(clim_vals)
# }
# 
# 
# library(fasterize)
# mammal_shapes <- st_read("Data/spatialdat/Mammals_Terrestrial")
# 
# mammal_raster <- raster(mammal_shapes, res = 1/6)
# mammal_raster <- fasterize(mammal_shapes, mammal_raster, fun="sum")
# 
# plot(mammal_raster)
# 
# mammal_raster <- raster(piba.shp, res = 1/6)
# mammal_raster <- fasterize(piba.shp, mammal_raster, fun="sum")
# 
# sp_range = piba.shp %>% 
#   rasterize(ppt_prism, getCover=TRUE) ## creates a raster with 0/1
# 
# sp_range[sp_range==0] <- NA ## make 0 = no data
# 
# 
# piba = st_read(pila_polys)
# 
# 
# 
# 
# # ## crop pico.shp so it overlaps aspect and slope rasters
# # ## change extent so it's overlapping
# # e <- ext(328000.1, 412361.7, 3873320, 4207890)
# # 
# # shape_pico <- pico.shp %>%
# #   crop(e)
# # 
# # ## check
# # plot(shape_pico, col="blue")
# # plot(slope, add=T)
# 
# 
# ## reading in SEKI veg map
# 
# # polyname <- "Data/spatialdat/SEKI veg map/sekigeodata.gdb"
# # file.exists(polyname)
# # 
# # fc_list <- ogrListLayers(polyname) ## see different files in the gdb
# # print(fc_list)
# # 
# # ranges <- st_read(polyname, layer = "SEKI_VegPolys")
# # species <- st_read(polyname, layer = "SEKI_tSpecies")
# # parkbound <- st_read(polyname, layer = "SEKI_Park_Boundary")
# # 
# # plot(ranges)
# # species$Spp_Code
# # 
# # "PIAL10" 
# # "PIBA11"
# # "PICO10"
# # "PIJE10"
# # "PILA10"   
# # "PIMO10"
# # "PIMO20"
# # "PINUS"
# # "PIPO10"
# # "PIPO11"
# # 
# 


