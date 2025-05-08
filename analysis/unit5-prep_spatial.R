# R-SPATIAL ---------------------------------------------------------------------------------------

# for a good introduction to R-spatial, see: https://cengel.github.io/R-spatial/
# for a collection of spatial Data for Austria go to: https://www.data.gv.at/ 

# 1. PREP -----------------------------------------------------------------------------------------
# clean working directory
rm(list = ls()); gc()

# attach packages
# if you haven't done so, install them first!
packages <- c("sf", "leaflet", "tidyverse")
sapply(packages, library, character.only = T)

# the sf package is used for reading and writing spatial data. it is the successor of the older sp package
# leaflet is for creating interactive maps
# tidyverse is the standard package collection for data handling

# 2. DATA ----------------------------------------------------------------------------------------

# there are many ways to store spatial data, the most important ones are:
### geojson: the cleaner and more modern way
### shp: shapefiles might seem more comfortable if you are used to them but they store metadata extrinsically

