## Data Science for Economic History
## U Bayreuth, summer term 2025
## Lecture 1

## This file replicates: 
## R as GIS for Economists
## By Taro Mieno
## Ruggedness: Nunn and Puga 2012 
## Chapter 1.7
## Note: The script sometimes deviates slightly from the book
## All data downloaded from Mieno.

## Here: calculate Terrain Ruggedness Index (TRI) for African countries 
## from the world elevation data

## Set working directory to where you stored the data for lecture 1
setwd("")

#--- Load packages ---#
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  sf, # vector data operations
  tidyverse, # data wrangling
  stars,
  tmap, # package for creating maps
  raster,
  rnaturalearth,
  rnaturalearthdata,
  skimr
)

#--- Read world elevation data using terra::rast() ---#
(
  dem <- terra::rast("nunn_2012/GDEM-10km-BW.tif")
)

#--- Elevation of ocean floor is measured zero => replace by NA ---#
dem <- terra::subst(dem, 0, NA)

tm_shape(dem, raster.downsample = FALSE) +
  tm_raster()

#--- Crop the world raster data to its African portion ---#
africa_sf <-
  #--- get an sf of all the countries in the world ---#
  rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  #--- filter our non-African countries ---#
  filter(continent == "Africa")

africa_dem <- terra::crop(dem, africa_sf)

#--- Figure 1 in lecture slides ---#
tm_shape(africa_dem) +
  tm_raster(title="Elevation", palette = terrain.colors(6))+
  tm_layout(frame=FALSE,
            legend.position=c("left", "bottom"),
            legend.title.size = 1.2,
            legend.text.size = 0.6)

#--- Calculate TRI ---#

#--- Define TRI function ---#
calc_tri <- function(matr) {
  # matr is a length 9 matrix
  center <- matr[5]
  sum_squares <- sum((matr - center)^2, na.rm = TRUE)
  return(sqrt(sum_squares))
}

#--- Apply a function to every raster cell ---#
tri_africa <-
  terra::focal(
    africa_dem,
    w = 3,
    fun = calc_tri
  )

#--- Figure 2 in lecture slides ---#
tm_shape(tri_africa) +
  tm_raster(title="Ruggedness")+
  tm_layout(frame=FALSE,
            legend.position=c("left", "bottom"),
            legend.title.size = 1.2,
            legend.text.size = 0.6)

### End of file