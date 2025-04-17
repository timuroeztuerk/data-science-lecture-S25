## Data Science for Economic History
## U Bayreuth, summer term 2025
## Lecture 1

## This file replicates: 
## R as GIS for Economists
## By Taro Mieno
## African Economy and Slaves: Nunn 2008 
## Chapter 1.6
## Note: The script sometimes deviates slightly from the book
## All data downloaded from Mieno.

## Set working directory to where you stored the data for lecture 1
setwd("")

#--- Load packages ---#
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  sf, # vector data operations
  tidyverse, # data wrangling
  units, #
  rmapshaper, # topologically-aware simplification algorithm
  lwgeom # for st_endpoint(), identifies last point of a line
)


#--- Read all the GIS data and re-project to epsg:3857 ---#

#--- African countries ---#
countries <-
  sf::st_read("nunn_2008/gadm36_africa/gadm36_africa.shp") %>%
  st_transform(3857)

#--- coast line ---#
coast <-
  sf::st_read("nunn_2008/10m-coastline/10m_coastline.shp") %>%
  st_transform(3857)

#--- ethnic regions ---#
ethnic_regions <-
  sf::st_read("nunn_2008/Murdock_shapefile/borders_tribes.shp") %>%
  st_transform(3857)

#--- latitude/longitude for slave trade centers ---#
trade_centers <- read_csv("nunn_2008/nunn2008.csv")

#--- Simplify and map geometries (so that the code runs faster) ---#
countries_simp <- rmapshaper::ms_simplify(countries)

(
  g_countries <-
    ggplot(data = countries_simp) +
    geom_sf() +
    theme_void()
)

#--- Find centroid of each country ---#
countries_centroid <- st_centroid(countries)

#--- Figure 1 in lecture slides ---#
(
  g_countries <-
    ggplot() +
    geom_sf(data = countries_simp) +
    geom_sf(data = countries_centroid, color = "red") +
    theme_void()
)

#--- Union coast line to have a single, combined geometry ---#
(
  coast_union <- st_union(coast)
)

#--- Find closest point on the coast line for the centroid of each country ---#
minum_dist_to_coast <- st_nearest_points(countries_centroid, coast_union)

#--- Returns a line between the centroid and the coast ---#

(
  g_min_dist_line <-
    ggplot() +
    geom_sf(data = countries_simp) +
    geom_sf(data = minum_dist_to_coast, color = "red") +
    theme_void()
)

#--- Extract end points of the lines ---#
closest_pt_on_coast <- lwgeom::st_endpoint(minum_dist_to_coast)

#--- Figure 2 in lecture slides ---#
(
  g_min_dist_line <-
    ggplot() +
    geom_sf(data = countries_simp) +
    geom_sf(data = minum_dist_to_coast, color = "red") +
    geom_sf(data = closest_pt_on_coast, color = "blue") +
    geom_sf(data = countries_centroid, color = "red") +
    theme_void()
)

#--- Assign closest point on coast to new column in countries_simp ---#
countries_simp$nearest_pt <- closest_pt_on_coast

#--- calculate distance b/w the closest point on the coast to the nearest slave trade center ---#
#--- convert to sf object, re-project to epsg:3857 ---#
(
  trade_centers_sf <-
    trade_centers %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(crs = 3857)
)

#--- all distances to trade centers ---#
trade_dist <- st_distance(countries_simp$nearest_pt, trade_centers_sf)
head(trade_dist)

#--- choose minimum distance ---#
(
  min_trade_dist <- apply(trade_dist, 1, min)
)

#--- assign minimum distance to new column ---#
countries_simp$distance_to_trade_center <- min_trade_dist / 1000

#--- Figure 3 in lecture slides ---#
ggplot() +
  geom_sf(data = trade_centers_sf, color = "red") +
  geom_sf(
    data = countries_simp,
    aes(geometry = geometry, fill = distance_to_trade_center)
  ) +
  scale_fill_viridis_c(name = "Distance to trade center") +
  theme_void() +
  theme(legend.position = "bottom")

### End of file
