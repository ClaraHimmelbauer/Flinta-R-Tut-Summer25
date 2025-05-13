# PREP ----------------------------------------------------

rm(list = ls()); gc()

packages <- c("tidyverse", "sf", "leaflet", "htmltools", "spdep")
sapply(packages, install.packages, character.only = T)
sapply(packages, library, character.only = T)

# install.packages("sf")
# install.packages("leaflet")
# library(sf)


# tidyverse for data wrangling

# sf = spatial features = for spatial operations.
# it's the successor of the older sp package

# leaflet and htmltools for interactive maps

# DATA ----------------------------------------------------

# general structure: st_read("file_path")
st_read("C:/Users/chimmelb/OneDrive - WU Wien/Dokumente/Flinta-R-Tut-Summer25/data/bez_schnitzel-inc.geojson")
bez <- st_read("data/bez_schnitzel-inc.geojson")

# look at the data
class(bez)
glimpse(bez)

# plot it
plot(bez)
plot(st_geometry(bez))

head(bez)

# PROJECTIONS ---------------------------------------------
# CRS
st_crs(bez)

# EPS: 4326 - WGS84
# Mercator Projection: EPSG 3857
# MGI/Austria Lambert EPSG 31287

bez_lambert <- sf::st_transform(bez, 31287)

plot(st_geometry(bez))
plot(st_geometry(bez_lambert))

bez$geometry
bez_lambert$geometry

# DATA WRANGLING ------------------------------------------

# number of schnitzel someone can consume in a year
bez$schni_inc <- bez$incpp / bez$schnitzel

# min and max values
bez$name[bez$schni_inc == min(bez$schni_inc)]

# claculating the federal state
# and then making a new geometry of these federal states

bl <- bez %>% 
  mutate(bl = floor(bkz/100),
         bl2 = substr(bkz, 1, 1)) %>% 
  group_by(bl) %>% 
  summarise(geometry = st_union(geometry))
plot(st_geometry(bl))

bl$name <- c("Burgenland", "Carinthya", "Lower Austria",
             "Upper Austria", "Salzburg", "Styria",
             "Tyrol", "Vorarlberg", "Vienna")
bl

# export data
st_write(bl, "data/bl.geojson", append = F)

# GGPLOT --------------------------------------------------

ggplot(bez) +
  geom_sf(aes(fill = schnitzel), color = "black") +
  geom_sf(data = bl,
          color = "black", linewidth = 1, alpha = 0) +
  theme_void() +
  
  scale_fill_gradient(low = "#ffc40c", high = "#702b0b",
                      name = "Price",
                      labels = scales::label_number(
                        prefix = "€"
                      )) +
  labs(title = "Average Price of Schnitzel in Austrian districts",
       subtitle = "In Spring 2022")

# EXERCISE ------------------------------------------------
# try plotting the amoutn of schnitzel someone can buy
# from their average salary
# maybe add stronger borders for the federal state borders

# SPATIAL AUTOCORRELATION ---------------------------------
# creating neighborhood object
nb <- poly2nb(bez)
lw <- nb2listw(nb, style = "W")

# W <- spdep::listw2mat(lw)
# colnames(W) <- rownames(W) <- bez$name
# View(W)

# global autocorrelation
moran.test(bez$schnitzel, lw)
moran.plot(bez$schnitzel, lw)

# local autocorrelation
loc <- localmoran(bez$schnitzel, lw)
loc <- as.data.frame(loc)

# extract local moran's I
bez$localmoran <- loc$Ii
# extract p-values for significance
bez$pmoran <- loc$`Pr(z != E(Ii))`

# histogram
hist(bez$localmoran)
# identify clusters where local autocorrelation is siginificant

bez$cluster <- ifelse(bez$localmoran > 0 & bez$pmoran < 0.05, "positive",
                      ifelse(bez$localmoran < 0 & bez$pmoran < 0.05, "negative",
                      "no"))
table(bez$cluster)

ggplot(bez) +
  geom_sf(aes(fill = cluster), color = "black") +
  theme_void() +
  
  scale_fill_manual(
    values = c("slateblue", "grey", "firebrick"))
