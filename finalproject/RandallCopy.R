#FInal Project in R 


####################### Initial Import Data (Below)  ######################## ####################################################################

#packages <- c("readr","sqldf","cowplot", "googleway", "ggplot2", "ggrepel",  "ggspatial", "lwgeom", "sf", "rnaturalearth", "rnaturalearthdata","maps","dplyr","stringr","spData","leaflet","sp","tidyr","devtools","tmap","tmaptools","corrplot","rgeos","rgdal","maptools","scales","shiny","Rcmdr")

#package.check <- lapply(packages, FUN = function(x) {
#  if (!require(x, character.only = TRUE)) {
 #   install.packages(x, dependencies = TRUE)
  #  library(x, character.only = TRUE)
#  }
#})

#additional libraries I require (Randall 4 SEPT)
#ONLY INSTALL VIA CONSOLE ONLY IF YOU NEED TO INSTALL 

devtools::install_github("tidyverse/ggplot2", force = TRUE)
library(ggplot2)
install.packages("rgdal")
library(rgdal)
install.packages("mapstools")
library(maptools)
install.packages("scales")
library(scales)
install.packages("cowplot")
library(cowplot)
install.packages("rgeos")
library(rgeos)
install.packages("googleway")
library(googleway)
install.packages("ggrepel")
library(ggrepel)
install.packages("ggspatial")
library(ggspatial)
install.packages("lwgeom")
library(lwgeom)
install.packages("rnaturalearthdata")
library(rnaturalearthdata)
install.packages("maps")
library(maps)
install.packages("rnaturalearth")
library(rnaturalearth)
devtools::install_github("r-spatial/sf")
library(sf)
install.packages("tidyverse")
library(tidyverse)
install.packages("raster")
library(raster)
install.packages("dplyr")
library(dplyr)
install.packages("stringr")
library(stringr)
install.packages("spData")
library(spData)
install.packages("leaflet")
library(leaflet)
install.packages("sp")
library(sp)
install.packages("tidyr")
library(tidyr)
install.packages("tmap")
library(tmap)
install.packages("tmaptools")
library(tmaptools)
devtools::install_github("Nowosad/spDataLarge")
library(spDataLarge)
devtools::install_github("geocompr/geocompkg")
library(geocompkg)
devtools::install_github("mtennekes/tmaptools")

devtools::install_github("mtennekes/tmap")
install.packages("corrplot")
library(corrplot)
install.packages("shiny")
library(shiny)
install.packages("devtools")
library(devtools)
install.packages("readr")
library(readr)
install.packages("sqldf")
library(sqldf)


theme_set(theme_bw())

#This script imports data from the chsi dataset and combines several sheets to
#make it easier to work with. You need to run it from within the folder that
#contains all the individual csv files



#!!change to your own directory where the CSV files are located!!
setwd("C://Users//randa//Desktop//workingdirectory//finalproject")
#dir_local <- DirSource("chsi_dataset")
#this is the one that explains what columns mean
key = read_csv("DATAELEMENTDESCRIPTION.csv")

#Healthypeople is a reference sheet; we shouldn't join it. Instead, we'll use it
#later for comparison to standards
healthypeople = read_csv("HEALTHYPEOPLE2010.csv")

#These 5 will be combined
demographics = read_csv("DEMOGRAPHICS.csv")
vulnpopsandenvhealth = read_csv("VUNERABLEPOPSANDENVHEALTH.csv")
riskfactors = read_csv("RISKFACTORSANDACCESSTOCARE.csv")
leadingcausesofdeath = read_csv("LEADINGCAUSESOFDEATH.csv")
summarymeasures = read_csv("SUMMARYMEASURESOFHEALTH.csv")


combined <- sqldf('
select 
  d.county_fips_code  ,d.state_fips_code  ,d.chsi_county_name  ,d.chsi_state_name  ,d.chsi_state_abbr  ,d.strata_id_number  ,d.strata_determining_factors  ,d.number_counties  ,d.population_size  ,d.population_density  ,d.poverty  ,d.Age_19_Under  ,d.Age_19_64 ,d.Age_65_84, d.Age_85_and_Over, d.White, d.Black, d.Native_American, d.Asian, d.Hispanic, v.No_HS_Diploma, v.Unemployed, v.Sev_Work_Disabled, v.Major_Depression, v.Recent_Drug_Use, v.Ecol_Rpt, v.Ecol_Rpt_Ind, v.Ecol_Exp, v.Salm_Rpt, v.Salm_Rpt_Ind , v.Salm_Exp, v.Shig_Rpt, v.Shig_Rpt_Ind, v.Shig_Exp, v.Toxic_Chem, v.Carbon_Monoxide_Ind, v.Nitrogen_Dioxide_Ind, v.Sulfur_Dioxide_Ind, v.Ozone_Ind, v.Particulate_Matter_Ind,  v.Lead_Ind, v.EH_Time_Span, r.No_Exercise, r.Few_Fruit_Veg, r.Obesity, r.High_Blood_Pres, r.Smoker, r.Diabetes, r.Uninsured, r.Elderly_Medicare, r.Disabled_Medicare, r.Prim_Care_Phys_Rate, r.Dentist_Rate, r.Community_Health_Center_Ind, r.HPSA_Ind, l.A_Wh_Comp, l.A_Bl_Comp, l.A_Ot_Comp, l.A_Hi_Comp, l.A_Wh_BirthDef, l.A_Bl_BirthDef, l.A_Ot_BirthDef, l.A_Hi_BirthDef, l.B_Wh_Injury, l.B_Bl_Injury, l.B_Ot_Injury, l.B_Hi_Injury, l.B_Wh_Cancer, l.B_Bl_Cancer, l.B_Ot_Cancer, l.B_Hi_Cancer, l.B_Wh_Homicide, l.B_Bl_Homicide, l.B_Ot_Homicide, l.B_Hi_Homicide, l.C_Wh_Injury, l.C_Bl_Injury, l.C_Ot_Injury, l.C_Hi_Injury, l.C_Wh_Homicide, l.C_Bl_Homicide, l.C_Ot_homicide, l.C_Hi_Homicide, l.C_Wh_Suicide, l.C_Bl_Suicide, l.C_Ot_Suicide, l.C_Hi_Suicide, l.C_Wh_Cancer, l.C_Bl_Cancer, l.C_Ot_Cancer, l.C_Hi_Cancer, l.D_Wh_Injury, l.D_Bl_Injury, l.D_Ot_Injury, l.D_Hi_Injury, l.D_Wh_Cancer, l.D_Bl_Cancer, l.D_Ot_Cancer, l.D_Hi_Cancer, l.D_Wh_HeartDis, l.D_Bl_HeartDis, l.D_Ot_HeartDis, l.D_Hi_HeartDis, l.D_Wh_Suicide, l.D_Bl_Suicide, l.D_Ot_Suicide, l.D_Hi_Suicide, l.D_Wh_HIV, l.D_Bl_HIV, l.D_Ot_HIV, l.D_Hi_HIV, l.D_Wh_Homicide, l.D_Bl_Homicide, l.D_Ot_Homicide, l.D_Hi_Homicide, l.E_Wh_Cancer, l.E_Bl_Cancer, l.E_Ot_Cancer, l.E_Hi_Cancer, l.E_Wh_HeartDis, l.E_Bl_HeartDis, l.E_Ot_HeartDis, l.E_Hi_HeartDis, l.F_Wh_HeartDis, l.F_Bl_HeartDis, l.F_Ot_HeartDis, l.F_Hi_HeartDis, l.F_Wh_Cancer, l.F_Bl_Cancer, l.F_Ot_Cancer, l.F_Hi_Cancer, l.LCD_Time_Span, s.ALE, s.US_ALE, s.All_Death  ,s.US_All_Death, s.Health_Status, s.US_Health_Status, s.Unhealthy_Days
,s.US_Unhealthy_Days 
from demographics d 
left join vulnpopsandenvhealth v on v.county_fips_code=d.county_fips_code and v.state_fips_code=d.state_fips_code
left join riskfactors r on r.county_fips_code=d.county_fips_code and r.state_fips_code=d.state_fips_code
left join leadingcausesofdeath l on l.county_fips_code=d.county_fips_code and l.state_fips_code=d.state_fips_code
left join summarymeasures s on s.county_fips_code=d.county_fips_code and s.state_fips_code=d.state_fips_code
' )

# We know that negative numbers are really NAs; let's replace them
combined[,-1:-7] <-  data.frame(lapply(combined[,-1:-7], function(x){
  as.numeric(gsub("-1*|-2*|-9*",NA,x))
}))

str(combined)
write_csv(combined,'combined.csv')

#replace NAs with mean of column auto
na_mean_swap <- function(x) {
  replace(x, is.na(x),mean(as.numeric(x),na.rm=TRUE))
}

mean_clean <- cbind(combined[,1:7],replace(combined[,-1:-7],TRUE, lapply(combined[,-1:-7], na_mean_swap)))

write_csv(mean_clean,'mc.csv')
str(mean_clean)


#####Bar Graphs for Demographics######
#Transform demographics for valid columns
# subset data to only include some columns
demographics <- subset(combined, select = 
                         c(CHSI_County_Name:CHSI_State_Abbr, Population_Size, 
                           Population_Density, Poverty, Age_19_Under, Age_19_64, 
                           Age_65_84, Age_85_and_Over, White, Black, Native_American, Asian, Hispanic))
# clean names of the subset
nms_demo_dat <- c("county.name","state.name","state.abbr","pop.size","pop.density",
                  "poverty","age.19_under","age.19_64","age.65_84","age.85_over",
                  "white","black","nat.amer","asian","hispanic")
# change col names
names(demographics)<-nms_demo_dat
#This data is a representation at the county level
#we have to do some work to find it at state level
mean(demographics$pop.size)
max(demographics$pop.size)
min(demographics$pop.size)
sd(demographics$pop.size)
#made with help from R Studio Community
library(tidyverse) #load library
# build vectors  
county <- c(demographics$county.name)
state <- c(demographics$state.name)
pop <- c(demographics$pop.size)
# now we assemble into data frame from vectors
df <- data.frame(county, state, pop)
# assembled pipe 
stateandmeanpop <- df %>% group_by(state) %>% summarize(mean_pop = mean(pop))
#Now we have a dataframe we can make different charts from
#with each states mean population
#This is the true mean of the population in our dataset
mean(stateandmeanpop$mean_pop)
#This is the range of the population in our dataset
max(stateandmeanpop$mean_pop)
min(stateandmeanpop$mean_pop)
sd(stateandmeanpop$mean_pop)
#B.	Show mean age, and ranges (a distribution would be a good visual for this as well)
#We can do the same with age. 
demographics$age.19_underraw = (demographics$pop.size*demographics$age.19_under/100)
demographics$age.19_64raw = (demographics$pop.size*demographics$age.19_64/100)
demographics$age.65_84raw = (demographics$pop.size*demographics$age.65_84/100)
demographics$age.85_overraw = (demographics$pop.size*demographics$age.85_over/100)
xviiii_U <- mean(demographics$age.19_under)
max(demographics$age.19_under)
min(demographics$age.19_under)

xvii_lxiv <- mean(demographics$age.19_64)
max(demographics$age.19_64)
min(demographics$age.19_64)

lxv_lxxxiv <- mean(demographics$age.65_84)
max(demographics$age.65_84)
min(demographics$age.65_84)

lxxxv_O <- mean(demographics$age.85_over)
max(demographics$age.85_over)
min(demographics$age.85_over)
meanofage <- c(xviiii_U, xvii_lxiv, lxv_lxxxiv, lxxxv_O)
barplot (meanofage,
         main = "Figure 1 - National Age %",
         xlab = "Mean % Age",
         ylab = "Population %",
         names.arg = c("Under19", "19-64", "65-84", "Over85"),
         col = "blue",
         horiz = FALSE)
#C.	Show ethnicity nationally (e.g. 87% white, etc.) - bar chart for visual
#How to create raw population numbers for age and ethnicity
#First we have to add new columns in the data.frame with the raw data
demographics$whiteraw = (demographics$pop.size*demographics$white/100)
demographics$blackraw = (demographics$pop.size*demographics$black/100)
demographics$hispanicraw = (demographics$pop.size*demographics$hispanic/100)
demographics$asianraw = (demographics$pop.size*demographics$asian/100)
demographics$nat.amerraw = (demographics$pop.size*demographics$nat.amer/100)

#Create mean vectors for each ethnicity
w <- mean(demographics$whiteraw)
b <- mean(demographics$blackraw)
h <- mean(demographics$hispanicraw)
a <- mean(demographics$asianraw)
n.a <- mean(demographics$nat.amerraw)
#Create dataframe
meanofrace <- c(w,b,h,a,n.a)
#Create bar chart 
barplot (meanofrace,
         main = "Figure 2 - National Mean Ethnicity",
         xlab = "Mean Ethnicity",
         ylab = "Population",
         names.arg = c("White", "Black", "Hispanic", "Asian", "NativeAmerican"),
         col = "darkred",
         horiz = FALSE)


library(tidyverse)
vuln <- sqldf('
          	select
            	county_fips_code
            	,state_fips_code
            	,chsi_county_name
            	,chsi_state_name
            	,population_size
            	,poverty
            	,No_HS_Diploma
            	,Unemployed
            	,Sev_Work_Disabled
            	,Major_Depression
            	,Recent_Drug_Use
            	,No_Exercise
            	,Few_Fruit_Veg
            	,Obesity
            	,High_Blood_Pres
            	,Smoker
            	,Diabetes
            	,Uninsured
            	,Elderly_Medicare
            	,Disabled_Medicare
            	,Prim_Care_Phys_Rate
            	,Dentist_Rate
            	,ALE
            	,All_Death
            	,US_All_Death
          	from combined')

str(vuln)
#Convert raw count data into percentages so we can compare it across counties with different populations
vuln$No_HS_Diploma <- vuln$No_HS_Diploma/vuln$Population_Size*100
vuln$Unemployed <- vuln$Unemployed/vuln$Population_Size*100
vuln$Sev_Work_Disabled <-vuln$Sev_Work_Disabled/vuln$Population_Size*100
vuln$Major_Depression <- vuln$Major_Depression/vuln$Population_Size*100
vuln$Recent_Drug_Use <- vuln$Recent_Drug_Use/vuln$Population_Size*100
vuln$Uninsured <- vuln$Uninsured/vuln$Population_Size*100
vuln$Elderly_Medicare <- vuln$Elderly_Medicare/vuln$Population_Size*100
vuln$Disabled_Medicare <- vuln$Disabled_Medicare/vuln$Population_Size*100
vuln$DeathRate <- vuln$All_Death/vuln$Population_Size*100

vuln$CHSI_State_Name
DF_Bar <- sqldf( 'select Poverty,Unemployed,Sev_Work_Disabled,Recent_Drug_Use,No_Exercise,Obesity,High_Blood_Pres,Smoker,Diabetes,Uninsured,Elderly_Medicare, Disabled_Medicare,CHSI_County_Name, CHSI_State_Name from vuln')
PLot_Bar <- replace(DF_Bar, TRUE, lapply(DF_Bar, na_mean_swap))
#calculate mean of each column
PLot_Bar <- colMeans(PLot_Bar)
#assistance from Quick-R by DataCamp
#used melt from library(reshape)
#this makes each row a unique id-variable combination
PLot_Bar <- melt(PLot_Bar)
PLot_Bar$Variables <- rownames(PLot_Bar)
#used Cookbook for R, Colors(ggplot2) for assistance
#toned the colors down to be a bit darker and match the colors of our other graphs
BarPlotFin<- ggplot(data=PLot_Bar) + geom_bar(aes(x = Variables,y=value,fill=Variables),stat="identity") + coord_flip() +scale_fill_hue(l=45) + ggtitle('Figure 3 - Population Health Statistics')
BarPlotFin

############### Plot Average Life Expectancy by State ####################
#assistance from https://www.datanovia.com/en/blog/ggplot-themes-gallery/
Life_Exp_States <- ggplot(data = mean_clean, aes(x=CHSI_State_Abbr, y = ALE, color = ALE, group=CHSI_State_Abbr)) + 
  geom_point() +
  scale_color_continuous(name = "Average Age") +
  theme_classic() +
  xlab(label = "States") +
  ylab(label = "Average Life Expectancy") + 
  ggtitle(label = "Figure 4 - State Average Life Expectancy") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
Life_Exp_States
################### plot ALE Under 1st Quartile #######################
#################################################################
Life_Exp_75 <- ggplot(data = mean_clean[mean_clean$ALE<75,], aes(x=CHSI_State_Abbr, y = ALE, color = ALE, group=CHSI_State_Abbr)) + 
  geom_point() +
  scale_color_continuous(name = "Average Age") +
  theme_classic() +
  xlab(label = "States") +
  ylab(label = "Average Life Expectancy") + 
  scale_y_reverse() +
  ggtitle(label = "State ALE Below Nation's 1st Quartile") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
Life_Exp_75


###############################################################
######################Death rate- Comparing Suicide, Homicide to the total death in population##############
#Suicide
mean_clean$totalSuicide <- mean_clean$C_Bl_Suicide+mean_clean$C_Hi_Suicide+mean_clean$C_Ot_Suicide+mean_clean$C_Wh_Suicide+mean_clean$D_Bl_Suicide+mean_clean$D_Wh_Suicide+mean_clean$D_Hi_Suicide+mean_clean$D_Ot_Suicide
mean_clean$percentSuicide <- (mean_clean$totalSuicide/mean_clean$Population_Size)*100
#Homiside
mean_clean$totalHomicide <- mean_clean$C_Bl_Homicide+mean_clean$C_Hi_Homicide+mean_clean$C_Wh_Homicide+mean_clean$D_Bl_Homicide+mean_clean$D_Wh_Homicide+mean_clean$D_Hi_Homicide
mean_clean$percentHomicide <- (mean_clean$totalHomicide/mean_clean$Population_Size)*100
mean_clean$percentAllDeath <- (mean_clean$All_Death/mean_clean$Population_Size)*100
deathDf <- sqldf('select percentSuicide, percentHomicide, percentAllDeath from mean_clean')
deathDfMeans <- colMeans(deathDf)
deathDfMeans <- melt(deathDfMeans)
deathDfMeans$variables <- rownames(deathDfMeans)
BarPlotDeath<- ggplot(data=deathDfMeans) + geom_bar(aes(x = variables,y=value,fill=variables),stat="identity") + coord_flip() +scale_fill_hue(l=45) + ggtitle('Figure 5 - Suicide/Homicide Statistics')
BarPlotDeath

##### Suicide by State####
Suicide_by_States <- ggplot(data = mean_clean, aes(x=CHSI_State_Abbr, y = percentSuicide, color = percentSuicide, group=CHSI_State_Abbr)) + 
  geom_point() +
  scale_color_continuous(name = "Percent Suicide") +
  theme_classic() +
  xlab(label = "States") +
  ylab(label = "% of Suicides") + 
  ggtitle(label = "Figure 5 - Suicides by State") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
Suicide_by_States

##### Homicide by State####
Homicide_by_States <- ggplot(data = mean_clean, aes(x=CHSI_State_Abbr, y = percentHomicide, color = percentHomicide, group=CHSI_State_Abbr)) + 
  geom_point() +
  scale_color_continuous(name = "Percent Homicide") +
  theme_classic() +
  xlab(label = "States") +
  ylab(label = "% of Homicide") + 
  ggtitle(label = "Figure 6 - Homicides by State") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
Homicide_by_States
################ Plot of Smokers relating to ALE and Exercise ################ #####################################################################
# Shows a plot of no exercise to ALE. Less smoking in green, heavy smoking in blue. 
#determine the range of smokers 
summary(combined$Smoker)
#plot 
#assistance from https://www.datanovia.com/en/blog/ggplot-themes-gallery/
gtt <- ggplot(data = combined, aes(x=No_Exercise, y = ALE,   color = Smoker)) + 
  theme_classic() +  
  geom_point() +  
  xlab(label = "Percent not Exercising") +
  #scale_x_reverse() + #x_axis reversed to show positive correlation
  ylab(label = "Average Life Expectancy") + 
  scale_colour_gradient(limits=c(3.6,46.2), low="green", high="red") + #Smoker Range
  ggtitle(label = "Negative Affects of Smoking and Not Working Out on ALE")
gtt

###############################################################################
##VISUALIZATIONS BANK
######################################################################


devtools::install_github("tidyverse/ggplot2", force = TRUE)
#install.packages('testthat')

#################
# STEP 1 PLOT A WORLD MAP 
#################

map(world)





us = ne_countries(country = 'united states of america',scale='large',returnclass='sf')

plot(us)

class(us)
class(sf_world)
ggplot(data= us) + 
  geom_sf() +
  coord_sf()
##################
# STEP 2 CREATE AND PLOT ST_AS_SF MAP STATES COUNTIES 
##################
?st_as_sf
#turn map state into a shapefile
states = sf::st_as_sf(map('county', plot = FALSE, fill = TRUE))
states <- sf::st_as_sf(map("state",plot=FALSE,fill=TRUE))
plot(states)
class(states)
#add coordinates of 'centroid' so we can later plot names
states <- cbind(states, st_coordinates(st_centroid(states)))
#label state names as uppercase
states$name <- toupper(states$ID)
plot(states)
#add us so we can use the limits
us <- map_data("county")[,c('long','lat')]
xlimit <- c(max(us$long))
ylimit <- c(max(us$lat))
plot(us)
ggplot() +
  geom_sf(data = states) +
  coord_sf(crs = st_crs(102003))


ggplot(data=world) + 
  geom_sf() +
  geom_sf(data=states,fill=NA) + 
  geom_label(data=states,aes(X,Y, label=name), size=3) +
  coord_sf(crs = st_crs(102003))
counties <- sf::st_as_sf(map("county", plot=FALSE, fill=TRUE))
plot(counties)
class(counties)

countiesG = ggplot(data = states) + 
  geom_sf() + 
  geom_sf(data=states,size=0.8) + 
  geom_sf(data=counties,fill=NA,color=gray(0.5)) + 
  coord_sf(crs = st_crs(102003),expand=FALSE)
plot(countiesG)

class(countiesG)
class(counties)

###############Demographics Maps ####################

class(demographics)
counties$ID
demographicsmap = demographics[,c('county.name','state.name','state.abbr',"pop.size","pop.density",
                                  "poverty","age.19_under","age.19_64","age.65_84","age.85_over",
                                  "white","black","nat.amer","asian","hispanic")]

demographicsmap$state = tolower(demographicsmap$state.name)
demographicsmap$county = tolower(demographicsmap$county.name)
demographicsmap$ID = paste(demographicsmap$state, demographicsmap$county, sep = ',')

d.map = left_join(counties, demographicsmap)
plot(d.map)

class(d.map)


#####interactive map coding#####################
#Population size###


d.intermap = tm_shape(d.map) + tm_fill("pop.size", legend.show = TRUE, legend.is.portrait = FALSE, legend.hist = FALSE, legend.hist.title = "Population Size United States of America", id = "county", palette = "Greens") + tm_polygons("county", "orange", style = 'class', n = 2, alpha = 1, stretch.palette = FALSE, popup.vars = NA, convert2density = FALSE, midpoint = FALSE)
tmap_mode("plot")
d.intermap
d.map$pop.size
#Population Density Plot and Interactive 
d.intermap.popden = tm_shape(d.map) + tm_fill("pop.density",legend.show = TRUE, legend.is.portrait = FALSE, legend.hist = FALSE, legend.hist.title = "Population Density, Per County United States of America", id = "county", palette = terrain.colors(7)) + tm_polygons(c("pop.density"), "Red", style = "Kmeans", n = 4, alpha = 2, stretch.palette = FALSE, popup.vars = TRUE, popup.vars = "pop.size", convert2density = TRUE, midpoint = TRUE)
tmap_mode("plot")
d.intermap.popden
d.map$age.19_64

###Age demographics under 19
d.intermap.age19 = tm_shape(d.map) + tm_fill("age.19_under", id = "county", palette = "Blues", legend.hist = TRUE, legend.hist.title = "Under The Age of 19, Per County United States of America",) + tm_polygons(c("age.19_under","age.19_64"), "White", style = "pretty", n = 4, alpha = 2, stretch.palette = TRUE, popup.vars = TRUE, popup.vars = "pop.size", convert2density = TRUE, midpoint = TRUE )
tmap_mode("view")

d.intermap.age19

#Age Demographics Above 19 

d.intermap.abv19 = tm_shape(d.map) + tm_fill("age.19_64", id = "county", palette = "RdYlGn", legend.show = TRUE, legend.is.portrait = TRUE, legend.hist = TRUE,legend.format = list(),title = "Above The Age of 19 by County", legend.hist.title = "Above The Age of 19, Per County United States of America",) + tm_polygons("RdYlGn", "White", style = "pretty", n = 4, alpha = 2, stretch.palette = TRUE, popup.format = list(),interval.closure = "left", zindex = NA, group = d.map, popup.vars = d.map$pop.size, convert2density = FALSE, midpoint = NA )
tmap_mode("plot")
d.intermap.abv19


d.map$
#####retirement age 

d.intermap.retire = tm_shape(d.map) + tm_fill("age.65_84", id = "county", palette = "Purples", legend.show = TRUE, legend.is.portrait = TRUE, legend.hist = TRUE,legend.format = list(),title = "Retirement Age, Per County", legend.hist.title = "Retirement Age, Per County",) + tm_polygons("RdYlGn", "White", style = "pretty", n = 4, alpha = 2, stretch.palette = TRUE, popup.format = list(),interval.closure = "left", zindex = NA, group = d.map, popup.vars = d.map$pop.size, convert2density = FALSE, midpoint = NA )
tmap_mode("plot")

d.intermap.retire


#######White Population

d.intermap.w = tm_shape(d.map) + tm_fill("white", id = "county", palette = "Spectral", legend.show = TRUE, legend.is.portrait = TRUE, legend.hist = TRUE,legend.format = list(),title = "Demographic: WHITE, Per County", legend.hist.title = "Demographic: WHITE, Per County",) + tm_polygons("RdYlGn", "White", style = "pretty", n = 4, alpha = 2, stretch.palette = TRUE, popup.format = list(),interval.closure = "left", zindex = NA, group = d.map, popup.vars = d.map$pop.size, convert2density = FALSE, midpoint = NA )
tmap_mode("plot")
d.intermap.w


############ Black Population 
d.map$black

d.intermap.b = tm_shape(d.map) + tm_fill("black", id = "county", palette = "PRGn", legend.show = TRUE, legend.is.portrait = TRUE, legend.hist = TRUE,legend.format = list(),title = "Demographic: BLACK, Per County", legend.hist.title = "Demographic: BLACK, Per County",) + tm_polygons("PRGn", "White", style = "pretty", n = 4, alpha = 2, stretch.palette = TRUE, popup.format = list(),interval.closure = "right", zindex = NA, group = d.map, popup.vars = "d.map$pop.size", convert2density = FALSE, midpoint = NA )

tmap_mode("plot")
d.intermap.b

#####Native Americans 
d.map$nat.amer

d.intermap.na = tm_shape(d.map) + tm_fill("nat.amer", id = "county", palette = "BrBG", legend.show = TRUE, legend.is.portrait = TRUE, legend.hist = TRUE,legend.format = list(),title = "Demographic: Native America, Per County", legend.hist.title = "Demographic: Native American, Per County",) + tm_polygons("BrBG", "White", style = "pretty", n = 4, alpha = 2, stretch.palette = TRUE, popup.format = list(),interval.closure = "right", zindex = NA, group = d.map, popup.vars = "d.map$pop.size", convert2density = FALSE, midpoint = NA )

tmap_mode("plot")
d.intermap.na

########Asian Americans



d.intermap.as = tm_shape(d.map) + tm_fill("asian", id = "county", palette = "Reds", legend.show = TRUE, legend.is.portrait = FALSE, legend.hist = FALSE, title = "Demographic: Asians, Per County", legend.hist.title = "Demographic: Asains, Per County",) + tm_polygons("Reds", "White", style = "pretty", n = 4, alpha = 2, stretch.palette = TRUE, popup.format = list(),interval.closure = "left", zindex = NA, group = d.map, popup.vars = "d.map$pop.size", convert2density = TRUE, midpoint = NA )

tmap_mode("plot")
d.intermap.as


####Hispanic Americans
d.map$hispanic
d.intermap.hs = tm_shape(d.map) + tm_fill("hispanic", id = "county", palette = "PuBuGn", legend.show = TRUE, legend.is.portrait = FALSE, legend.hist = FALSE, title = "Demographic: Hispanics, Per County", legend.hist.title = "Demographic: HIspanics, Per County",) + tm_polygons("PuBuGn", "White", style = "pretty", n = 4, alpha = 2, stretch.palette = TRUE, popup.format = list(),interval.closure = "left", zindex = NA, group = d.map, popup.vars = "d.map$pop.size", convert2density = TRUE, midpoint = NA )

tmap_mode("view")
d.intermap.hs

##POVERTY 
d.map$poverty

d.intermap.pov = tm_shape(d.map) + tm_fill("poverty", id = "county", palette = "Blues", legend.show = TRUE, legend.is.portrait = FALSE, legend.hist = FALSE, title = "Demographic: Poverty Rates", legend.hist.title = "Demographic: Poverty, Per County",) + tm_polygons("Blues", "White", style = "pretty", n = 4, alpha = 2, stretch.palette = TRUE, popup.format = list(),interval.closure = "left", zindex = NA, group = d.map, popup.vars = "d.map$pop.size", convert2density = TRUE, midpoint = NA )
tmap_mode("plot")

d.intermap.pov

############DF_BAR AT RISK VISUALIZATIONS

counties
class(DF_Bar)
DF.bar = DF_Bar[,c('CHSI_State_Name','CHSI_County_Name','Unemployed',"Sev_Work_Disabled","Recent_Drug_Use","No_Exercise","Obesity","High_Blood_Pres","Smoker","Diabetes",
                                  "Uninsured","Elderly_Medicare","Disabled_Medicare")]

DF.bar$state = tolower(DF.bar$CHSI_State_Name)
DF.bar$county = tolower(DF.bar$CHSI_County_Name)
DF.bar$ID = paste(DF.bar$state, DF.bar$county, sep = ',')

vuln.map = left_join(counties, DF.bar)  
  
###Vulnerable Population Rates USofA UnEmployed
vuln.map$Unemployed
v.intermap.ue = tm_shape(vuln.map) + tm_fill("Unemployed", id = "county", palette = "GnBu", legend.show = TRUE, legend.is.portrait = FALSE, legend.hist = FALSE, title = "Vulnerable Populations:Unemployment Rates", legend.hist.title = "Unemployment Rates",) + tm_polygons("GnBu", "White", style = "pretty", n = 4, alpha = 2, stretch.palette = TRUE, popup.format = list(),interval.closure = "left", zindex = NA, group = d.map, popup.vars = "d.map$pop.size", convert2density = TRUE, midpoint = NA )
tmap_mode("plot")

v.intermap.ue
######### Vulnerable Population Rates of Sev Work Disabled
vuln.map$Sev_Work_Disabled

v.intermap.swd = tm_shape(vuln.map) + tm_fill("Sev_Work_Disabled", id = "county", palette = "BuGn", legend.show = TRUE, legend.is.portrait = FALSE, legend.hist = FALSE, title = "Vulnerable Populations:Severely Disabled Rates", legend.hist.title = "Severely Disabled Rates",) + tm_polygons("PiYG", "White", style = "pretty", n = 4, alpha = 2, stretch.palette = TRUE, popup.format = list(),interval.closure = "left", zindex = NA, group = d.map, popup.vars = "d.map$pop.size", convert2density = TRUE, midpoint = NA )
tmap_mode("plot")

v.intermap.swd
####





#=================================================

ale.mMap = tm_shape(ale.Map) + tm_fill("ALE", legend.show = TRUE,legend.is.portrait = FALSE, legend.hist	= FALSE,legend.hist.title = "ALE UNITED STATES",
                                       id = "county", palette = "Greens")  +  tm_polygons("county", "orange", style = "cont",  n = 2, alpha = 1 , stretch.palette = TRUE, popup.vars = NA, convert2density = FALSE, midpoint = FALSE)
tmap_mode("view")
ale.mMap

check_shape(counties, d.map)




check_shape <- function(shp, name) {
  if (inherits(shp, "Spatial")) {
    shp <- as(shp, "sf")
  } else if (!inherits(shp, c("sf", "sfc"))) {
    stop("Object ", name, " is neither from class sf, Spatial, nor Raster.", call. = FALSE)
  }		
  # drop z/m
  shp <- sf::st_zm(shp)
  
  # check if shp is valid (if not, fix it with a warning)
  if (!all(st_is_valid(shp))) {
    warning("The shape ", name, " is invalid. See sf::st_is_valid", call. = FALSE)
    shp <- lwgeom::st_make_valid(shp)
  }
  
  # remove empty units
  empty_units <- st_is_empty(shp)
  if (all(empty_units)) {
    stop("The shape ", name, " only contains empty units.", call. = FALSE)
  } else if (any(empty_units)) {
    warning("The shape ", name, " contains empty units.", call. = FALSE)
    shp <- if (inherits(shp, "sf")) shp[!empty_units, ] else shp[!empty_units]
  }
  shp
}





















##################################
# ALE
##################################
View(Ale.Contribs.df)
class(BarPlotDeath)
ale.1 <- Ale.Contribs.df[,c('CHSI_State_Name','CHSI_County_Name','County_FIPS_Code','No_HS_Diploma', 'Population_Size','ALE')] 
ale.1$state <- tolower(ale.1$CHSI_State_Name)
ale.1$county<- tolower(ale.1$CHSI_County_Name)
ale.1$ID <- paste(ale.1$state,ale.1$county,sep=',')

counties <- st_as_sf(map("county", plot=FALSE, fill=TRUE))

#create new object that combines the counties sf with the ALE dataset
ale.Map  <- left_join(counties,ale.1)
plot(ale.Map)


ale.mMap = tm_shape(ale.Map) + tm_fill("ALE", legend.show = TRUE,legend.is.portrait = FALSE, legend.hist	= TRUE,legend.hist.title = "ALE UNITED STATES",
                                       id = "county", palette = "Greens")  +  tm_polygons("county", "orange", style = "cont",  n = 2, alpha = 1 , stretch.palette = TRUE, popup.vars = NA, convert2density = FALSE, midpoint = FALSE)
tmap_mode("view")
ale.mMap

