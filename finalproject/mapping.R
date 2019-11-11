load(url("http://github.com/mgimond/Spatial/raw/master/Data/Sample1.RData"))
library(ggplot2)
library(maps)
library(sf)
library(tmap)

#load the county data from maps

cnty = st_as_sf(map("county", plot = FALSE, fill = TRUE))
plot(cnty)

tm_shape(cnty) + tm_polygons()




tm_shape(s.sf) + tm_polygons(col="grey", border.col="white")
