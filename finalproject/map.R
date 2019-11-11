############# Barchart Summary of Variables Most Affecting Death #############
#####################################################################
#this is from the section which takes population and percent of population into #consideration. It's a true average of the population as values are originally presented #as percentage of county.
#This calculates the count then takes percent of the US

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

DF_Bar <- sqldf( 'select Poverty,Unemployed,Sev_Work_Disabled,Recent_Drug_Use,No_Exercise,Obesity,High_Blood_Pres,Smoker,Diabetes,Uninsured,Elderly_Medicare, Disabled_Medicare 
                 from vuln')
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

library(tidyverse)
library(sf)
library(maps)

#################
# STEP 1 PLOT A WORLD MAP 
#################

world = ne_countries(scale='medium',returnclass='sf')
class(world)
ggplot(data=world) + 
  geom_sf() +
  coord_sf()
+##################
# STEP 2 CREATE AND PLOT ST_AS_SF MAP STATES COUNTIES 
##################

#turn map state into a shapefile
states <- sf::st_as_sf(map("state",plot=FALSE,fill=TRUE))
#add coordinates of 'centroid' so we can later plot names
states <- cbind(states, st_coordinates(st_centroid(states)))
#label state names as uppercase
states$name <- toupper(states$ID)

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
  coord_sf(crs = st_crs(102003),expand=FALSE)

counties <- st_as_sf(map("county", plot=FALSE, fill=TRUE))

countiesG = ggplot(data = states) + 
  geom_sf() + 
  geom_sf(data=states,size=0.8) + 
  geom_sf(data=counties,fill=NA,color=gray(0.5)) + 
  coord_sf(crs = st_crs(102003),expand=FALSE)
plot(countiesG)




##################################
# ALE
##################################
View(Ale.Contribs.df)
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
