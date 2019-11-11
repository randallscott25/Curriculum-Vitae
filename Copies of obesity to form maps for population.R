#Randall Taylor 

library(ggmap)
library(maps)
#a copy of the function
lower.df = function(v) 
{
  if(is.character(v)) return(tolower(v)) 
  else return(v)
}
# subset data to only include some columns
demographics <- subset(demographics, select = 
                             c(CHSI_County_Name:CHSI_State_Abbr, Population_Size, 
                               Population_Density, Poverty, Age_19_Under, Age_19_64, 
                               Age_65_84, Age_85_and_Over, White, Black, Native_American, Asian, Hispanic))
# clean names of the subset
nms_demo_dat <- c("county.name","state.name","state.abbr","pop.size","pop.density",
                  "poverty","age.19_under","age.19_64","age.65_84","age.85_over",
                  "white","black","nat.amer","asian","hispanic")
# change col names
names(demographics)<-nms_demo_dat
head(demographics)

demographics$state.name <- lower.df(demographics$state.name)
demographics$county.name <- lower.df(demographics$county.name)
demographics$state.abbr <- lower.df(demographics$state.abbr)
# get state map
us_state <- map_data("state")
# change us_state_map names
names(us_state)<- c("long","lat","group","order","state.name","subregion")


# merge us_state_map data frame with demographics_dat by state.name
# only include matching records
Po_data<-merge(us_state, demographics, by ='state.name')

# preserve order
Po_data<-Po_data[order(Po_data$order),]
# remove subregion column
Po_data$subregion<-NULL

# split %'s into 6 cuts 
Po_data$poverty <- cut_interval(Po_data$poverty, 6)

# state data
state_df <- map_data("state")

# create dataframe with county information from maps 
# Longitude and Latitude information here
county_df<-map_data("county")

# change names of county_df
names(county_df)<- c("long","lat","group","order","state.name","county.name")

# check out state.abb and state.name Datasets 
# will add state.abbr to county_df based on match
head(state.abb)
head(state.name)

# add a column with state abbrevitions based on matching between 
# county_df$state_name and lowercase state.name dataset
county_df$state.abbr<- state.abb[match(x = county_df$state.name, tolower(state.name))]
# remove state_name column since have abbreviations
county_df$state.name<-NULL

# make all names lowercase to match
county_df <- data.frame(lapply(county_df, lower.df))

#--------------------------------------------------------------
# will use this to zoom in on % obesity at County level
#--------------------------------------------------------------

# merge county_df and demographics_dat by county.name and state.abbr
Pop_map <- merge(x = county_df, y = demographics, by=c("county.name","state.abbr"))
# retain order
Pop_map <- Pop_map[order(Pop_map$order), ]

# add breaks for ranges in new map risk_map
Pop_map$poverty <- cut_interval(Pop_map$poverty ,6)


#-----------------------------------------------------------------------------------------------
# plot Poverty accross US states - not at county level
#-----------------------------------------------------------------------------------------------
# create dataframe to add state map abbreviations on map
# state.center - x=long, y=lat. state.abb is a list containing state abbreviations
state.info <- data.frame(state.center, state.abb)
# lower names
state.info$state.abb <- tolower(state.info$state.abb)
# add group info
state.info$group <- Po_data$group[match(x = state.info$state.abb, Po_data$state.abbr)]
# remove ak and hi (no group)
state.info <- state.info[!is.na(state.info$group),]



# map of poverty at state level - org palette
# doesnt include state names
Pop_map_all_org <- ggplot(Po_data, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = poverty)) +
  geom_polygon(data = state_df, colour = "black", fill = NA, size =0.2) + 
  geom_polygon(data = county_df, colour = "snow", fill = NA, size =0.1) + 
  geom_text(data = state.info, aes(x=x, y=y, label = state.abb, group = group), colour ='black') +
  theme_classic() +  
  theme(legend.position="right") + 
  xlab(label = "") +
  ylab(label = "") +
  scale_x_continuous(expand = c(0,0)) + # expand size of map along x axis 
  scale_y_continuous(expand = c(0,0)) + # expand size of map along y axis
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank()) + 
  ggtitle(label = "US Map showing Poverty Intervals at the State Level") +
  scale_fill_brewer(type='div', palette = 'RdBu', name = "% Poverty")
Pop_map_all_org

#Poverty Calculated at the county level

poverty_Cmap_purd <- ggplot(Pop_map, aes(long, lat, group = group)) +
  theme(panel.background = element_rect(fill = "snow")) +
  geom_polygon(aes(fill = poverty), colour = alpha("snow", 1/2), size = 0.2) + 
  geom_polygon(data = state_df, colour = "black", fill = NA,size =0.2) + 
  geom_polygon(data = county_df, colour = "grey", fill = NA, size =0.1) + 
  theme_classic() +  
  theme(legend.position="bottom") + 
  xlab(label = "") +
  ylab(label = "") +
  scale_x_continuous(expand = c(0,0)) + # expand size of map along x axis 
  scale_y_continuous(expand = c(0,0)) + # expand size of map along y axis
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank()) + 
  ggtitle(label = "US Map showing poverty \nIncludes County information") +
  scale_fill_brewer(type='seq',palette = 'PuRd', name ="% Poverty") 
poverty_Cmap_purd 
