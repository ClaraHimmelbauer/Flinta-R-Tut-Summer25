# R-SPATIAL ---------------------------------------------------------------------------------------

# for a good introduction to R-spatial, see: https://cengel.github.io/R-spatial/
# for a collection of spatial Data for Austria go to: https://www.data.gv.at/ 
# for graphs in R: https://r-graph-gallery.com/ 
# for a tutorial on leaflet in R: https://rstudio.github.io/leaflet/ 

# 1. PREP -----------------------------------------------------------------------------------------
# clean working directory
rm(list = ls()); gc()

# attach packages
# if you haven't done so, install them first!
packages <- c("sf", "leaflet", "tidyverse", "htmltools")
sapply(packages, library, character.only = T)

# the sf package is used for reading and writing spatial data. it is the successor of the older sp package
# leaflet is for creating interactive maps, in combination with htmltools
# tidyverse is the standard package collection for data handling

# 2. DATA ----------------------------------------------------------------------------------------

# there are many ways to store spatial data, the most important ones are:
### geojson: the cleaner and more modern way
### shp: shapefiles might seem more comfortable if you are used to them but they store metadata extrinsically
##### .dbf stores attributes (like data)
##### .prj stores the projection format
##### .shp stores the geometry
##### .shx stores the geometry index - like a positional index to better access features

# find the data for today's session on my github repository:
# https://github.com/ClaraHimmelbauer/Flinta-R-Tut-Summer25/
# download it or copy raw link and read in directly

# read in geojson
bez <- st_read("data/bez_schnitzel-inc.geojson")

# look at data
class(bez)
glimpse(bez)
bez

# plotting it
plot(bez)
plot(st_geometry(bez))

# 3. PROJECTIONS ----------------------------------------------------------------------------------
# The earth is a sphere, but maps (and our geomentry) is flat
# How do we reduce 3D earth to 2D computer screens? We have to reduce some dimensions
# there is some tradeoff, we cannot keep all of the following
### area, shape/angles, distance, directions

# common projections are 
### "WGS 84" (EPSG: 4326) (EPSG = European Petroleum Survey Group): longitudes and latitudes like you are used to
### Mercator (EPSG: 3857) used for google maps/navigation (preserves directions)
### MGI/Austria Lambert (EPSG:31287) for data you get for Austria

# why are there different ones?
### more distortions far away from reference point
### different purpose (e.g. merkator for navigation)

# check the projection
st_crs(bez)

# if you work with different datasets, you have to use the same projection!
# use st_transform() to transform the projection
# e.g. to transform to austria lambert
bez_lambert <- st_transform(bez, 31287)
plot(st_geometry(bez_lambert))

# also notice how the coordinates change
bez$geometry
bez_lambert$geometry

# now assume you have two spatial data sources and then you transform one to the crs of the other one
# sometimes you will still encounter problems with some minor differences with overlap
# e.g., if you download data from https://www.data.gv.at/ and then transform it to WGS84
# I usually fix this by taking the difference in the centroid coordinates
### st_centroid(st_union(data)); st_centroid(st_union(transformed-data))
# and then shift the coordinates of the second dataset
### raster::shift(transformed-data, dx, dy)

# 4. DATA WRANGLING -------------------------------------------------------------------------------
# data wrangling works just like with usual dataframes

# make a new variable for the number of schnitzel someone can buy in a year from their income
bez$schni_inc <- bez$incpp / bez$schnitzel

# you can also use tidyverse pipes for data wrangling AND for spatial operations
# let's say we want to make a new dataframe for our federal states
bl <- bez %>% 
  mutate(bl = floor(bkz / 100)) %>% 
  group_by(bl) %>% 
  summarise(geometry = st_union(geometry))

bl
plot(st_geometry(bl))
# why is Tirol a multipolygon and everything else a polygon?

bl$name <- c("Burgenland", "Kärnten", "Niederösterreich",
             "Oberösterreich", "Salzburg", "Steiermark",
             "Tirol", "Vorarlberg", "Wien")

# to export this data, use st_write
st_write(bl, "data/bl.geojson", append = F)

# 5. GGPLOT ---------------------------------------------------------------------------------------
# go to https://r-graph-gallery.com/ for examples!
# as always with plots: start simple and tehn graudally improve it


# some schnitzelbrown colors
col1 <- "#ffc40c"
col2 <- "#eeaa0d"
col3 <- "#de900f"
col4 <- "#cd7710"
col5 <- "#bd5d12"
col6 <- "#ac4313"
col7 <- "#702b0b"

ggplot(bez) +
  geom_sf(aes(fill = schnitzel), color = "black", linewidth = .5) +
  
  theme_void() +
  
  scale_fill_gradient(low = col1, high = col7,
                      name = "Price",
                      labels = scales::label_number(prefix = "€", big.mark = ".", decimal.mark = ",")) +
  
  labs(title = "Price of Schnitzel in Austrian Districts in €",
       subtitle = "In Spring 2021")

# 6. EXERCISE -------------------------------------------------------------------------------------
# Make some ggplot for the number of Schnitzel people can buy from their average salary per year
# also experiment a bit with the formatting

# 7. LEAFLET --------------------------------------------------------------------------------------
# for a full tutorial go to: https://rstudio.github.io/leaflet/
pal <- colorNumeric(palette = c(col1, col2, col3, col4, col5, col6, col7), domain = bez$schnitzel)

leaflet(data = bez) %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  
  addPolygons(
    fillColor = ~pal(schnitzel),
    color = "black",
    weight = 1,
    opacity = 1,
    fillOpacity = 0.7,
    label = ~paste0(name, ": ", schnitzel, "€")
  ) %>% 
  addPolylines(data = bl, color = "black", weight = 3) %>% 
  
  addLegend(
    "topleft",
    pal = pal,            
    values = bez$schnitzel, 
    title = "Price of Schnitzel in €",
    labFormat = labelFormat(suffix = "€"),
    opacity = 1
  )

