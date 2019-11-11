# Luke Miller
# 2 September 2019
# following map drawing tutorial 
# from https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
# Modifying to visualize project data

#load required packages using a bulk check/install function from professor
packages <- c("cowplot", "googleway", "ggplot2", "ggrepel",  "ggspatial", "lwgeom", "sf", "rnaturalearth", "rnaturalearthdata","maps",'dplyr')

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

theme_set(theme_bw())

#################
# Plot world map
#################

world <- ne_countries(scale='medium',returnclass='sf')
#class(world)

ggplot(data=world) + 
  geom_sf() +
  coord_sf()

##################
# Plot state map
##################

#turn map state into a shapefile
states <- st_as_sf(map("state",plot=FALSE,fill=TRUE))
#add coordinates of 'centroid' so we can later plot names
states <- cbind(states, st_coordinates(st_centroid(states)))
#label state names as uppercase
states$name <- toupper(states$ID)

#add us so we can use the limits
us <- map_data("county")[,c('long','lat')]
xlimit <- c(max(us$long)+2,min(us$long)-2)
ylimit <- c(max(us$lat)+2,min(us$lat)-2)


ggplot(data=world) + 
  geom_sf() +
  geom_sf(data=states,fill=NA) + 
  geom_label(data=states,aes(X,Y, label=name), size=3) + 
  coord_sf(xlim=xlimit,ylim=ylimit, expand=FALSE)

#############################
# Zoom in on chosen location
#############################

zoomAmount <- 5

#knoxville x=-83.99,y=35.82 Pick your favorite city
centerx <- -83.99 
centery <- 35.82 

ylimit <- c(centery-zoomAmount, centery+zoomAmount)
xlimit <- c(centerx-zoomAmount*2, centerx+zoomAmount*2) 
 
ggplot() + 
  geom_sf(data=states) + 
  geom_label(data=states,aes(X,Y, label=name), size=3) + 
  coord_sf(xlim=xlimit,ylim=ylimit, expand=FALSE)

#################
# Counties
#################

counties <- st_as_sf(map("county", plot=FALSE, fill=TRUE))

ggplot(data = world) + 
  geom_sf() + 
  geom_sf(data=states,size=0.8) + 
  geom_sf(data=counties,fill=NA,color=gray(0.5)) + 
  coord_sf(xlim=xlimit,ylim=ylimit, expand=FALSE)

############################
# Counties with smoking data
############################

#must have loaded chsi already
smokers <- combined[,c('CHSI_State_Name','CHSI_County_Name','Smoker','Poverty')] 
smokers$Smoker <- as.numeric(smokers$Smoker)/100
smokers$state <- tolower(smokers$CHSI_State_Name)
smokers$county<- tolower(smokers$CHSI_County_Name)
smokers$ID <- paste(smokers$state,smokers$county,sep=',')

counties <- st_as_sf(map("county", plot=FALSE, fill=TRUE))
 
#create new object that combines the counties sf with the smoker dataset
s.map <- left_join(counties,smokers)

zoomAmount <- 3 
#knoxville x=-83.99,y=35.82 Pick your favorite city
centerx <- -83.99 
centery <- 35.82 

ylimit <- c(centery-zoomAmount, centery+zoomAmount)
xlimit <- c(centerx-zoomAmount*2, centerx+zoomAmount*2) 

#inherits xlimit and ylimit from above; change as desired 
ggplot(data = world) + 
  geom_sf() + 
  geom_sf(data=s.map,aes(fill=Smoker),color=gray(0.1)) + 
  geom_sf(data=states,fill=NA,size=0.8,color=gray(0.1)) + 
  coord_sf(xlim=xlimit,ylim=ylimit, expand=FALSE)

##################################
# ALE
##################################

ALE <- combined[,c('CHSI_State_Name','CHSI_County_Name','ALE')] 
ALE$state <- tolower(ALE$CHSI_State_Name)
ALE$county<- tolower(ALE$CHSI_County_Name)
ALE$ID <- paste(ALE$state,ALE$county,sep=',')

counties <- st_as_sf(map("county", plot=FALSE, fill=TRUE))
 
#create new object that combines the counties sf with the smoker dataset
s.map <- left_join(counties,ALE)

#zoomAmount <- 3 
##knoxville x=-83.99,y=35.82 Pick your favorite city
#centerx <- -83.99 
#centery <- 35.82 
#
#ylimit <- c(centery-zoomAmount, centery+zoomAmount)
#xlimit <- c(centerx-zoomAmount*2, centerx+zoomAmount*2) 

#inherits xlimit and ylimit from above; change as desired 
s.map.1 =ggplot(data = world) + 
  geom_sf() + 
  geom_sf(data=s.map,aes(fill=ALE),color=gray(0.1)) + 
  geom_sf(data=states,fill=NA,size=0.8,color=gray(0.1)) + 
  coord_sf(xlim=xlimit,ylim=ylimit, expand=FALSE) + labs(title='US life expectancy by county')


s.map1 = tm_shape(s.map) + tm_fill("ALE", legend.show = TRUE, id = "NAME", palette = "Greens" ) +  tm_polygons("ALE", "orange", style = "cont",  n = 2, alpha = 1 , stretch.palette = TRUE, popup.vars = NA, convert2density = FALSE, midpoint = FALSE)
tmap_mode("view")
s.map1
