#Luke Miller's script - Randall Taylor working copy 2.2
#7 Aug 2019 created; Taylor edits, 2.2 begin 4 SEPT
#IST687 group
#This script imports data from the chsi dataset and combines several sheets to
#make it easier to work with. You need to run it from within the folder that
#contains all the individual csv files

# use sqldf for joins
#library(sqldf)
#library(readr)
#additional libraries I require (Randall 4 SEPT)
#ONLY INSTALL VIA CONSOLE ONLY IF YOU NEED TO INSTALL 
#install.packages("sf")
#install.packages("raster")
#install.packages(“dplyr”)
#install.packages(“stringr”)
#install.packages("spData")
#install.packages("leaflet")
#install.packages("sp")
#install.packages("tidyr")
#install.packages("tmap")
#install.packages("tmaptools")
#devtools::install_github("Nowosad/spDataLarge")
#devtools::install_github("geocompr/geocompkg")
#install_github("mtennekes/tmaptools")
#install_github("mtennekes/tmap")
#install.packages("corrplot")
#install.packages("shiny")

#####devtools::install_github("geocompr/geocompkg")###########
#library(sf)
#library(raster)
#library(dplyr)
#library(stringr)
#library(spData)
#library(leaflet)
#library(sp)
#library(tidyr)
#library(devtools)
#library(ggplot2)
#library(tmap)
#library(tmaptools)
#library(corrplot)
#library(rgeos)
#library(rgdal)
#library(maptools)
#library(scales)
#library(shiny)
#detach("package:raster", unload=TRUE)

#load required packages using a bulk check/install function from professor
packages <- c("readr","sqldf","cowplot", "googleway", "ggplot2", "ggrepel",  "ggspatial", "lwgeom", "sf", "rnaturalearth", "rnaturalearthdata","maps","dplyr","stringr","spData","leaflet","sp","tidyr","devtools","tmap","tmaptools","corrplot","rgeos","rgdal","maptools","scales","shiny","Rcmdr")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})



theme_set(theme_bw())









#!!change to your own directory where the CSV files are located!!
setwd("C://Users//randa//Desktop//Applied Data Science//Final Project//chsi_dataset")

#this is the one that explains what columns mean
key = read_csv("DATAELEMENTDESCRIPTION.csv")

#Healthypeople is a reference sheet; we shouldn't join it. Instead, we'll use it
#later for comparison to standards
healthypeople = read_csv("HEALTHYPEOPLE2010.csv")

#These 6 will be combined
demographics = read_csv("DEMOGRAPHICS.csv")
vulnpopsandenvhealth = read_csv("VUNERABLEPOPSANDENVHEALTH.csv")
riskfactors = read_csv("RISKFACTORSANDACCESSTOCARE.csv")
leadingcausesofdeath = read_csv("LEADINGCAUSESOFDEATH.csv")
summarymeasures = read_csv("SUMMARYMEASURESOFHEALTH.csv")
birthandeath = read_csv("MEASURESOFBIRTHANDDEATH.csv")
#View(riskfactors)
combined = sqldf('
select 
  d.county_fips_code
  ,d.state_fips_code
  ,d.chsi_county_name
  ,d.chsi_state_name
  ,d.chsi_state_abbr
  ,d.strata_id_number
  ,d.strata_determining_factors
  ,d.number_counties
  ,d.population_size
  ,d.population_density
  ,d.poverty
  ,d.Age_19_Under
  ,d.Age_19_64                 
  ,d.Age_65_84                 
  ,d.Age_85_and_Over
  ,d.White
  ,d.Black
  ,d.Native_American
  ,d.Asian
  ,d.Hispanic
  ,v.No_HS_Diploma
  ,v.Unemployed
  ,v.Sev_Work_Disabled
  ,v.Major_Depression
  ,v.Recent_Drug_Use
  ,v.Ecol_Rpt 
  ,v.Ecol_Rpt_Ind
  ,v.Ecol_Exp 
  ,v.Salm_Rpt 
  ,v.Salm_Rpt_Ind              
  ,v.Salm_Exp 
  ,v.Shig_Rpt 
  ,v.Shig_Rpt_Ind 
  ,v.Shig_Exp 
  ,v.Toxic_Chem 
  ,v.Carbon_Monoxide_Ind 
  ,v.Nitrogen_Dioxide_Ind 
  ,v.Sulfur_Dioxide_Ind 
  ,v.Ozone_Ind 
  ,v.Particulate_Matter_Ind 
  ,v.Lead_Ind 
  ,v.EH_Time_Span  
  ,r.No_Exercise
  ,r.Few_Fruit_Veg
  ,r.Obesity
  ,r.High_Blood_Pres
  ,r.Smoker
  ,r.Diabetes
  ,r.Uninsured
  ,r.Elderly_Medicare
  ,r.Disabled_Medicare
  ,r.Prim_Care_Phys_Rate
  ,r.Dentist_Rate
  ,r.Community_Health_Center_Ind
  ,r.HPSA_Ind
  ,l.A_Wh_Comp
  ,l.A_Bl_Comp
  ,l.A_Ot_Comp
  ,l.A_Hi_Comp
  ,l.A_Wh_BirthDef
  ,l.A_Bl_BirthDef
  ,l.A_Ot_BirthDef
  ,l.A_Hi_BirthDef
  ,l.B_Wh_Injury
  ,l.B_Bl_Injury
  ,l.B_Ot_Injury
  ,l.B_Hi_Injury
  ,l.B_Wh_Cancer
  ,l.B_Bl_Cancer
  ,l.B_Ot_Cancer
  ,l.B_Hi_Cancer
  ,l.B_Wh_Homicide
  ,l.B_Bl_Homicide
  ,l.B_Ot_Homicide
  ,l.B_Hi_Homicide
  ,l.C_Wh_Injury
  ,l.C_Bl_Injury
  ,l.C_Ot_Injury
  ,l.C_Hi_Injury
  ,l.C_Wh_Homicide
  ,l.C_Bl_Homicide
  ,l.C_Ot_homicide
  ,l.C_Hi_Homicide
  ,l.C_Wh_Suicide
  ,l.C_Bl_Suicide
  ,l.C_Ot_Suicide
  ,l.C_Hi_Suicide
  ,l.C_Wh_Cancer
  ,l.C_Bl_Cancer
  ,l.C_Ot_Cancer
  ,l.C_Hi_Cancer
  ,l.D_Wh_Injury
  ,l.D_Bl_Injury
  ,l.D_Ot_Injury
  ,l.D_Hi_Injury
  ,l.D_Wh_Cancer
  ,l.D_Bl_Cancer
  ,l.D_Ot_Cancer
  ,l.D_Hi_Cancer
  ,l.D_Wh_HeartDis
  ,l.D_Bl_HeartDis
  ,l.D_Ot_HeartDis
  ,l.D_Hi_HeartDis
  ,l.D_Wh_Suicide
  ,l.D_Bl_Suicide
  ,l.D_Ot_Suicide
  ,l.D_Hi_Suicide
  ,l.D_Wh_HIV
  ,l.D_Bl_HIV
  ,l.D_Ot_HIV
  ,l.D_Hi_HIV
  ,l.D_Wh_Homicide
  ,l.D_Bl_Homicide
  ,l.D_Ot_Homicide
  ,l.D_Hi_Homicide
  ,l.E_Wh_Cancer
  ,l.E_Bl_Cancer
  ,l.E_Ot_Cancer
  ,l.E_Hi_Cancer
  ,l.E_Wh_HeartDis
  ,l.E_Bl_HeartDis
  ,l.E_Ot_HeartDis
  ,l.E_Hi_HeartDis
  ,l.F_Wh_HeartDis
  ,l.F_Bl_HeartDis
  ,l.F_Ot_HeartDis
  ,l.F_Hi_HeartDis
  ,l.F_Wh_Cancer
  ,l.F_Bl_Cancer
  ,l.F_Ot_Cancer
  ,l.F_Hi_Cancer
  ,l.LCD_Time_Span
  ,s.ALE 
  ,s.US_ALE
  ,s.All_Death 
  ,s.US_All_Death 
  ,s.Health_Status 
  ,s.US_Health_Status 
  ,s.Unhealthy_Days 
  ,s.US_Unhealthy_Days
  ,b.Suicide
  ,b.LBW
  ,b.VLBW
  ,b.Premature
  ,b.Under_18
  ,b.Over_40
  ,b.Unmarried
  ,b.Late_Care
  ,b.Infant_Mortality
  ,b.IM_Hisp
  ,b.IM_Neonatal
  ,b.IM_Postneonatal
  ,b.Brst_Cancer
  ,b.Col_Cancer
  ,b.CHD
  ,b.Homicide
  ,b.Lung_Cancer
  ,b.MVA
  ,b.Stroke
  ,b.Suicide
  ,b.Injury
  ,b.Total_Births
  ,b.Total_Deaths



from demographics d 
left join vulnpopsandenvhealth v on v.county_fips_code=d.county_fips_code and v.state_fips_code=d.state_fips_code
left join riskfactors r on r.county_fips_code=d.county_fips_code and r.state_fips_code=d.state_fips_code
left join leadingcausesofdeath l on l.county_fips_code=d.county_fips_code and l.state_fips_code=d.state_fips_code
left join summarymeasures s on s.county_fips_code=d.county_fips_code and s.state_fips_code=d.state_fips_code
left join birthandeath b on b.county_fips_code=d.county_fips_code and b.state_fips_code=d.state_fips_code
' )

# We know that negative numbers are really NAs; let's replace them
combined[,-1:-7] <-  data.frame(lapply(combined[,-1:-7], function(x){
  as.numeric(gsub("-1*|-2*|-9*",NA,x))
}))

#str(combined)
write_csv(combined,'combined.csv')
#View(combined)

#replace NAs with mean of column auto
na_mean_swap <- function(x) {
  replace(x, is.na(x),mean(as.numeric(x),na.rm=TRUE))
}

mean_clean <- cbind(combined[,1:7],replace(combined[,-1:-7],TRUE, lapply(combined[,-1:-7], na_mean_swap)))
#str(mean_clean)

#data set = mean_clean
dim(mean_clean) #two dimmensional rows 3141 columns 140 
nrow(mean_clean)#how many rows 
ncol(mean_clean)#how many columns
class(mean_clean)#what is this objects classification 
#make Randall dataset based upon dplyr, tidyr: for first correlation runs: suicide
#View(mean_clean)
#rstDataset = mean_clean %>% 
# select(State_FIPS_Code, CHSI_County_Name,  Population_Size, Poverty, Age_19_Under, Age_19_64, Age_65_84, Age_85_and_Over, No_HS_Diploma, Unemployed,Sev_Work_Disabled, Major_Depression, Recent_Drug_Use,No_Exercise, Few_Fruit_Veg, Obesity, Smoker, Diabetes, Uninsured, Elderly_Medicare, Disabled_Medicare,C_Wh_Suicide,)


#suicide study

sDemo.DF =  mean_clean %>%
  select(County_FIPS_Code, State_FIPS_Code, CHSI_County_Name, CHSI_State_Name, C_Wh_Suicide, C_Bl_Suicide, C_Ot_Suicide, C_Hi_Suicide, Population_Size )
#View(sDemo.DF)



sContribs.df = mean_clean %>% 
  select(State_FIPS_Code, CHSI_County_Name, County_FIPS_Code, Population_Size, Poverty, Age_19_Under, Age_19_64, Age_65_84, Age_85_and_Over, No_HS_Diploma, Unemployed,Sev_Work_Disabled, Major_Depression, Recent_Drug_Use,No_Exercise, Few_Fruit_Veg, Obesity, Smoker, Diabetes, Uninsured, Elderly_Medicare, Disabled_Medicare, High_Blood_Pres, Toxic_Chem, Carbon_Monoxide_Ind,Nitrogen_Dioxide_Ind,Sulfur_Dioxide_Ind, Ozone_Ind, Particulate_Matter_Ind, Lead_Ind, Disabled_Medicare, Prim_Care_Phys_Rate, Dentist_Rate, Community_Health_Center_Ind, Suicide)

Ale.Contribs.df = mean_clean %>% 
  select(CHSI_State_Name,CHSI_County_Name, County_FIPS_Code,No_HS_Diploma, ALE,Population_Size)
#Ale.Contribs2.df = mean_clean %>%
 # select(CHSI_County_Name, County_FIPS_Code,ALE)


#, Unemployed,Sev_Work_Disabled, Major_Depression, Recent_Drug_Use,No_Exercise, Few_Fruit_Veg, Obesity, Smoker, Diabetes, Uninsured, Elderly_Medicare, Disabled_Medicare, High_Blood_Pres, Toxic_Chem, Carbon_Monoxide_Ind,Nitrogen_Dioxide_Ind,Sulfur_Dioxide_Ind, Ozone_Ind, Particulate_Matter_Ind, Lead_Ind, Disabled_Medicare, Prim_Care_Phys_Rate, Dentist_Rate, Community_Health_Center_Ind, Suicide)





ALEdimin.df = mean_clean %>%
  select(CHSI_State_Name, CHSI_County_Name, Population_Size, Poverty, Age_19_Under, Age_19_64, Age_65_84, Age_85_and_Over, No_HS_Diploma, Unemployed,Sev_Work_Disabled, Major_Depression, Recent_Drug_Use,No_Exercise, Few_Fruit_Veg, Obesity, Smoker, Diabetes, Uninsured, Elderly_Medicare, Disabled_Medicare, High_Blood_Pres, Toxic_Chem, Carbon_Monoxide_Ind,Nitrogen_Dioxide_Ind,Sulfur_Dioxide_Ind, Ozone_Ind, Particulate_Matter_Ind, Lead_Ind, Disabled_Medicare, Prim_Care_Phys_Rate, Dentist_Rate, Community_Health_Center_Ind,ALE, US_ALE,US_Unhealthy_Days,Suicide, Premature, Under_18, Over_40, Unmarried, Late_Care, Infant_Mortality, IM_Hisp, IM_Neonatal, IM_Postneonatal, Brst_Cancer, Col_Cancer, CHD, Homicide, Lung_Cancer, MVA, Stroke, Injury ,  Total_Deaths, US_All_Death, All_Death, US_Health_Status, Unhealthy_Days)
#### This data is apart of the data sub collection that will be utilized in the interactive map mode. Trouble having multiple variables in one df, splitting them up 


###########################################################################################


#THIS IS A MUST LOAD FOR THE MATHS TO WORK; CORRELATIONS, 3D SCATTERPLOTS, SCATTERPLOTS
#library(Rcmdr)
####################DEMOGRAPHIC INFORMATION###################
#County Data Ages 15 - 24 suicide, black americans

s.black = with(sDemo.DF, tapply(C_Bl_Suicide, list(CHSI_State_Name), mean, na.rm=TRUE))
#View(s.black)

#County Data Ages 15 - 24 suicide, hispanic americans
hist(with(sDemo.DF, tapply(C_Hi_Suicide, list(CHSI_State_Name), mean, na.rm=TRUE)))
# County Data Ages 15 - 24, listed as OTHER (than black, hispanic, white)
hist(with(sDemo.DF, tapply(C_Ot_Suicide, list(CHSI_State_Name), mean, na.rm=TRUE)))
#County Data Ages 15 - 24 suicide, white americans
hist(with(sDemo.DF, tapply(C_Wh_Suicide, list(CHSI_State_Name), mean, na.rm=TRUE)))
# Percentages of suicide, to the total, per state

stateLVL.s.DF = local({
  .Table <- with(sDemo.DF, table(CHSI_State_Name))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
#plot the percentages 
View(stateLVL.s.DF)                     





##########Suicide to Uninsured iterations 

#scatterplot  Suicide to Uninsured 

scatterplot(Suicide~Uninsured, regLine=TRUE, smooth=FALSE, boxplots=FALSE, data=sContribs.df)
#correlation Suicide to Uninsured 
with(sContribs.df, cor.test(Suicide, Uninsured, 
                            alternative="two.sided", method = "pearson"))
cor(sContribs.df[,c("Suicide","Uninsured")], use="complete")
############Suicide Uninsured
#Suicide    1.000000 -0.122214
#Uninsured -0.122214  1.000000

# Table for Suicide:
county.suicide.list = with(sContribs.df, tapply(Suicide, list(CHSI_County_Name), mean, na.rm=TRUE))
# Table for Uninsured:
county.uninsured.list = with(sContribs.df, tapply(Uninsured, list(CHSI_County_Name), 
                          mean, na.rm=TRUE))
##3d Scatter plot Suicide Unemployed Uninsured 
scatter3d(Suicide~Unemployed+Uninsured, data=sContribs.df, fit="linear", 
          residuals=TRUE, bg="black", axis.scales=TRUE, grid=TRUE, ellipsoid=FALSE)




######################Suicide to Toxic Chemicals###########
scatterplot(Suicide~Toxic_Chem, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="Toxic Chemicals", ylab="Suicide", 
            data=sContribs.df)
cor(sContribs.df[,c("Suicide","Toxic_Chem")], use="complete")

#Suicide Toxic_Chem
#Suicide    1.0000000  0.2004474
#Toxic_Chem 0.2004474  1.0000000
##################################

rcorr.adjust(sContribs.df[,c("Suicide","Toxic_Chem")], 
                 type="pearson", use="complete")



#Pairwise two-sided p-values:
 # Suicide Toxic_Chem
#Suicide            <.0001    
#Toxic_Chem <.0001            


################Suicide to Smoker#######################
scatterplot(Suicide~Poverty, 
            regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, 
            xlab="Poverty", 
            ylab="Suicide", data=sContribs.df)

###correlation smoker ~suicide
rcorr.adjust(sContribs.df[,
                          c("Smoker","Suicide")], 
             type="pearson", use="complete")



######establishing the dataframe as a numeric only for correlation exploration###


na_mean_swap_suicidecontributors <- function(x) {
  replace(x, is.na(x),mean(as.numeric(x),na.rm=TRUE))
  }

na_mean_swap_suicidecontributors <- cbind(sContribs.df[,1:7],replace(sContribs.df[,-1:-7],TRUE, lapply(sContribs.df[,-1:-7], na_mean_swap_suicidecontributors)))

str(na_mean_swap_suicidecontributors)


sContribs.corr = cor(sContribs.df[,c("Suicide","Toxic_Chem",  "Population_Size", "Poverty", "No_HS_Diploma", "Unemployed","Sev_Work_Disabled", "Major_Depression", "Recent_Drug_Use","No_Exercise", "Few_Fruit_Veg", "Obesity",  "Diabetes", "Uninsured", "Elderly_Medicare", "Disabled_Medicare", "High_Blood_Pres", "Toxic_Chem", "Carbon_Monoxide_Ind", "Ozone_Ind", "Particulate_Matter_Ind", "Lead_Ind", "Disabled_Medicare", "Prim_Care_Phys_Rate", "Dentist_Rate", "Community_Health_Center_Ind")], use="complete")

sContribs.corrPlt = corrplot::corrplot(sContribs.corr, method = ('pie'), type = ('upper'))
View(sContribs.corrPlt)
#So per the correlation plot, the following should be ran in the

sContribs.corr2 = cor(sContribs.df[,c("Suicide","Toxic_Chem",  "Population_Size", "Poverty", "No_HS_Diploma", "Unemployed", "Major_Depression", "Recent_Drug_Use","No_Exercise", "Elderly_Medicare", "Disabled_Medicare", "High_Blood_Pres","Ozone_Ind", "Prim_Care_Phys_Rate", "Dentist_Rate")], use="complete")
  
sContribs.corrPlt2 = corrplot::corrplot(sContribs.corr2, method = ('pie'), type = ('upper'))
#Boxplot all counties, suicide
suicideBXplT = Boxplot(Suicide~CHSI_County_Name,
        data=sContribs.df, 
        id=list(method="y"))

sContribs.corr = cor(sContribs.df[,c("Suicide","Toxic_Chem",  "Population_Size", "Poverty", "No_HS_Diploma", "Unemployed", "Major_Depression", "Recent_Drug_Use","No_Exercise", "Elderly_Medicare", "Disabled_Medicare", "High_Blood_Pres","Ozone_Ind", "Prim_Care_Phys_Rate", "Dentist_Rate")], use="complete")





#####First run removed environmental factors that had no correlative value 
#### Second run; removed categorical descriptions: US ALE, US UNHEALTHY DAYS 

sContribs.corr3 = cor(ALEdimin.df[,c( "No_HS_Diploma", "Unemployed","Sev_Work_Disabled", "Major_Depression", "Recent_Drug_Use","No_Exercise", "Few_Fruit_Veg", "Obesity", "Smoker", "Diabetes", "Uninsured", "Elderly_Medicare", "Disabled_Medicare", "High_Blood_Pres","Toxic_Chem", "Carbon_Monoxide_Ind", "Ozone_Ind", "Particulate_Matter_Ind", "Lead_Ind", "Disabled_Medicare", "Prim_Care_Phys_Rate", "Dentist_Rate", "Community_Health_Center_Ind","ALE","Suicide", "Premature", "Under_18", "Over_40", "Unmarried", "Late_Care", "Infant_Mortality", "IM_Hisp", "IM_Neonatal", "IM_Postneonatal", "Brst_Cancer", "Col_Cancer", "CHD", "Homicide", "Lung_Cancer", "MVA", "Stroke", "Injury" ,  "Total_Deaths", "All_Death", "Unhealthy_Days")], use="complete")


sContribs.corr3 = cor(ALEdimin.df[,c( "No_HS_Diploma", "Unemployed","Sev_Work_Disabled", "Major_Depression", "Recent_Drug_Use","No_Exercise", "Few_Fruit_Veg", "Obesity", "Smoker", "Diabetes", "Uninsured", "Elderly_Medicare", "Disabled_Medicare", "High_Blood_Pres","Toxic_Chem", "Carbon_Monoxide_Ind", "Ozone_Ind", "Particulate_Matter_Ind", "Lead_Ind", "Disabled_Medicare", "Prim_Care_Phys_Rate", "Dentist_Rate", "Community_Health_Center_Ind","ALE","Suicide", "Premature", "Under_18", "Over_40", "Unmarried", "Late_Care", "Infant_Mortality", "IM_Hisp", "IM_Neonatal", "IM_Postneonatal", "Brst_Cancer", "Col_Cancer", "CHD", "Homicide", "Lung_Cancer", "MVA", "Stroke", "Injury" ,  "Total_Deaths", "All_Death", "Unhealthy_Days")], use="complete")







sContribs.corr3.1 = cor(ALEdimin.df[c("ALE", "Suicide")], use='complete')
View(sContribs.corr3.1)


sContribs.corrPlt3 = corrplot::corrplot(sContribs.corr3, method = ('pie'), type = ('upper'), add = FALSE, col = NULL, bg = "white", title = "ALE correlating risk factors", is.corr=TRUE, diag = TRUE, outline = FALSE, addgrid.col = TRUE,addCoef.col = NULL,addCoefasPercent = TRUE, sig.level = 0.05)
plot(sContribs.corrPlt3)

sContribs.corrPlt3.1 = corrplot::corrplot(sContribs.corr3.1, method = ('pie'), type = ('upper'), add = FALSE, col = NULL, bg = "white", title = "ALE correlating risk factors", is.corr=TRUE, diag = TRUE, outline = FALSE, addgrid.col = TRUE,addCoef.col = NULL,addCoefasPercent = TRUE, sig.level = 0.05)

###############################################################################
##VISUALIZATIONS BANK




#################
# STEP 1 PLOT A WORLD MAP 
#################

world <- ne_countries(scale='medium',returnclass='sf')
class(world)

ggplot(data=world) + 
  geom_sf() +
  coord_sf()

##################
# STEP 2 CREATE AND PLOT ST_AS_SF MAP STATES COUNTIES 
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

#zoomAmount <- 5

#knoxville x=-83.99,y=35.82 Pick your favorite city
#centerx <- 43.0481 
#centery <- -76.1474 

#ylimit <- c(centery-zoomAmount, centery+zoomAmount)
#xlimit <- c(centerx-zoomAmount*2, centerx+zoomAmount*2) 

ggplot() + 
  geom_sf(data=states) + 
  geom_label(data=states,aes(X,Y, label=name), size=3)+
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
# Counties No Diploma 1st QUESTION 
############################

#must have loaded chsi already
noHSDiploma <- Ale.Contribs.df[,c('CHSI_State_Name','CHSI_County_Name','County_FIPS_Code','No_HS_Diploma', 'Population_Size')] 
noHSDiploma$No_HS_Diploma <- as.numeric(noHSDiploma$No_HS_Diploma) / noHSDiploma$Population_Size
noHSDiploma$state <- tolower(noHSDiploma$CHSI_State_Name)
noHSDiploma$county<- tolower(noHSDiploma$CHSI_County_Name)
noHSDiploma$ID <- paste(noHSDiploma$state,noHSDiploma$county,sep=',')

counties <- st_as_sf(map("county", plot=FALSE, fill=TRUE))

######ALEdimin.df- MAP ########################
ALEdiminishers = ALEdimin.df[,c("CHSI_State_Name","CHSI_County_Name", "Population_Size", "Poverty", "Age_19_Under", "Age_19_64", "Age_65_84", "Age_85_and_Over", "No_HS_Diploma", "Unemployed","Sev_Work_Disabled", "Major_Depression", "Recent_Drug_Use","No_Exercise", "Few_Fruit_Veg", "Obesity", "Smoker", "Diabetes", "Uninsured", "Elderly_Medicare", "Disabled_Medicare", "High_Blood_Pres", "Toxic_Chem", "Carbon_Monoxide_Ind","Nitrogen_Dioxide_Ind","Sulfur_Dioxide_Ind", "Ozone_Ind", "Particulate_Matter_Ind", "Lead_Ind", "Disabled_Medicare", "Prim_Care_Phys_Rate", "Dentist_Rate", "Community_Health_Center_Ind","ALE", "US_ALE","US_Unhealthy_Days","Suicide", "Premature", "Under_18", "Over_40", "Unmarried", "Late_Care", "Infant_Mortality", "IM_Hisp", "IM_Neonatal", "IM_Postneonatal", "Brst_Cancer", "Col_Cancer", "CHD", "Homicide", "Lung_Cancer", "MVA", "Stroke", "Injury" ,  "Total_Deaths", "US_All_Death", "All_Death", "US_Health_Status", "Unhealthy_Days")]

ALEdiminishers$Population_Size <- as.numeric(ALEdiminishers$Population_Size)

ALEdiminishers$All_Death <- as.numeric(ALEdiminishers$All_Death)


ALEdiminishers$Total_Deaths <- as.numeric(ALEdiminishers$Total_Deaths)

ALEdiminishers$state = tolower(ALEdiminishers$CHSI_State_Name)
ALEdiminishers$county = tolower(ALEdiminishers$CHSI_County_Name)
ALEdiminishers$data = tolower(ALEdiminishers$Population_Size)
ALEdiminishers$data2 = tolower(ALEdiminishers$All_Death)
ALEdiminishers$data3 = tolower(ALEdiminishers$Total_Deaths)
ALEdiminishers$ID = paste(ALEdiminishers$state,ALEdiminishers$county)
ALEdiminishers$data.1 = paste(ALEdiminishers$Population_Size, ALEdiminishers$All_Death, ALEdiminishers$Total_Deaths )


counties <- st_as_sf(map("county", plot=FALSE, fill=TRUE))

 ALEdiminDf.map = left_join(counties, ALEdiminishers)
 str(counties)
 str(ALEdiminDf.map)
#
#ALEdiminDf.map <- left_join(ALEdiminishers %>% as.data.frame(), counties %>% as.data.frame(), by = "ID") %>%
#ALEdiminDf.map = st_sf(sf_column_name = 'geometry')


str(ALEdiminDf.map)

#create new object that combines the counties sf with the smoker dataset
noHSDIPLOMA1.map <- left_join(counties,noHSDiploma)

plot(noHSDIPLOMA1.map)

#zoomAmount <- 3 
#knoxville x=-83.99,y=35.82 Pick your favorite city
#centerx <- 43.0481 
#centery <- -76.1474 

#ylimit <- c(centery-zoomAmount, centery+zoomAmount)
#xlimit <- c(centerx-zoomAmount*2, centerx+zoomAmount*2) 

#inherits xlimit and ylimit from above; change as desired 
ggplot(data = world) + 
  geom_sf() + 
  geom_sf(data=noHSDIPLOMA1.map,aes(fill=No_HS_Diploma),color=gray(0.1)) + 
  geom_sf(data=states,fill=NA,size=0.8,color=gray(0.1)) + 
  coord_sf(xlim=xlimit,ylim=ylimit, expand=FALSE)
plot(noHSDIPLOMA1.map)

nohsD.map1 = tm_shape(noHSDIPLOMA1.map) + tm_fill("No_HS_Diploma", legend.show = TRUE, id = "county", palette = "Greens" ) +  tm_polygons("county", "orange", style = "cont",  n = 2, alpha = 1 , stretch.palette = TRUE, popup.vars = NA, convert2density = TRUE, midpoint = FALSE)
tmap_mode("view")
nohsD.map1


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
#zoomAmount <- 3 
##knoxville x=-83.99,y=35.82 Pick your favorite city
#centerx <- -83.99 
#centery <- 35.82 
#
#ylimit <- c(centery-zoomAmount, centery+zoomAmount)
#xlimit <- c(centerx-zoomAmount*2, centerx+zoomAmount*2) 

#inherits xlimit and ylimit from above; change as desired 
mMap = ggplot(data = world ) + 
  geom_sf() + 
  geom_sf(data=ale.Map,aes(fill="ALE"),color=gray(0.1)) + 
  geom_sf(data=states,fill=NA,size=0.8,color=gray(0.1)) + 
  coord_sf(xlim=xlimit,ylim=ylimit, expand=FALSE) + labs(title='US life Expectancy By County')
mMap
ale.mMap = tm_shape(ale.Map) + tm_fill("ALE", legend.show = TRUE,legend.is.portrait = FALSE, legend.hist	= TRUE,legend.hist.title = "ALE UNITED STATES",
 id = "county", palette = "Greens")  +  tm_polygons("county", "orange", style = "cont",  n = 2, alpha = 1 , stretch.palette = TRUE, popup.vars = NA, convert2density = FALSE, midpoint = FALSE)
tmap_mode("view")
ale.mMap




###################################################################



###### Shiny app maybe###
varlist <- setdiff(names(mMap), "geometry")

runApp(list(
  ui = fluidPage(
    titlePanel("Average Life Expectancies"),
    sidebarLayout(
      sidebarPanel(
        selectInput("var", label = "Variable", choices = varlist, selected = "pop_est_dens")
      ),
      mainPanel(
        leafletOutput("map")
      )
    )
  ),
  server = function(input, output) {
    output$map = renderLeaflet({
      if (packageVersion("tmap") >= 2.0) {
        tm <- tm_basemap(leaflet::providers$Stamen.TerrainBackground) +
          tm_shape(ale.mMap)  +
          tm_polygons(input$var) +
          tm_tiles(leaflet::providers$Stamen.TonerLabels, group = "Labels")
        
      } else {
        tm <- tm_shape(mMap) +
          tm_polygons(input$var) +
          tm_view(basemaps = "Stamen.TerrainBackground")
      }
      
      tmap_leaflet(tm)
    })
  }
))






class(ALEdiminishers)
View(ALEdiminDf.map)

class(ALEdiminDf.map)



library(rsconnect)


################################APP II#################

###### Shiny app 2
varlist <- setdiff(names(ale.1), "geometry")

runApp(list(
  ui = fluidPage(
    titlePanel("CHSI DATASET 2010"),
    sidebarLayout(
      sidebarPanel(
        selectInput("var", label = "Variable", choices = varlist, selected = "pop_est_dens")
      ),
      mainPanel(
        leafletOutput("map")
      )
    )
  ),
  server = function(input, output) {
    output$map = renderLeaflet({
      if (packageVersion("tmap") >= 2.0) {
        tm <- tm_basemap(leaflet::providers$Stamen.TerrainBackground) +
          tm_shape(ale.Map)  +
          tm_polygons(input$var) +
          tm_tiles(leaflet::providers$Stamen.TonerLabels, group = "Labels")
        
      } else {
        tm <- tm_shape(ale.Map) +
          tm_polygons(input$var) +
          tm_view(basemaps = "Stamen.TerrainBackground")
      }
      
      tmap_leaflet(tm)
    })
  }
))






































































###########################################################


####Mapping Code perhaps##### 




#ale.1 <- data.frame(Ale.Contribs.df, stringsAsFactors = FALSE)
#View(ale.1)
#class(ale.1)
# Colnames tolower
#names(ale.1) <- tolower(names(ale.1))

#ale.1$CHSI_County_Name <- tolower(ale.1$chsi_county_name)


#View(ale.1)




#colnames(ale.1) <- c( "NAME","COUNTYFP","no_hs_diploma")
#names(ale.1) <- tolower(names(ale.1))

# Have to add leading zeos to any FIPS code that's less than 5 digits long to get a good match.
# I'm cheating by using C code. sprintf will work as well.
#ale.1$COUNTYFP <- formatC(ale.1$COUNTYFP, width = 5, format = "d", flag = "0")

#View(ale.1)
### End data prep

# Download county shape file from Tiger.
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
#us.map <- readOGR(dsn=path.expand("C://Users//randa//Desktop//Applied Data Science//Final Project//tl_2018_us_county"), layer="tl_2018_us_county", stringsAsFactors = FALSE)


#us.map$COUNTYFP
#us.map$GEOID

#plot(us.map)
# Remove Alaska(2), Hawaii(15), Puerto Rico (72), Guam (66), Virgin Islands (78), American Samoa (60)
#  Mariana Islands (69), Micronesia (64), Marshall Islands (68), Palau (70), Minor Islands (74)
#us.map <- us.map[!us.map$STATEFP %in% c("02", "15", #"72", "66", "78", "60", "69",
                                        "64", "68", #"70"#, "74"),]
# Make sure other outling islands are removed.
#us.map <- us.map[!us.map$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
    #                                    "95", "79"),]
#us.map = us.map$no_hs_diploma
#str(us.map)
#plot(us.map)






#usDataTestMap = geo_join(us.map, ale.1, by= NULL)

         
#usD#ataTestMap

#usDataMap <-   sp::merge(us.map, ale.1, by = c("COUNTYFP","COUNTYFP"))
                         #.x = us.map$COUNTYFP, by.y = ale.1$COUNTYFP)
                         #by.x = by, duplicateGeoms = TRUE)
 # sp::m
  
#usDataMap = sp::merge(us.map, ale.1) 
                      #all.x = TRUE, duplicateGeoms = TRUE) #duplicateGeoms = TRUE

#usDatamap1 = inner_join(us.map, ale.1, by = c("COUNTYFP" = "COUNTYFP"), copy = TRUE)
#View(usDataMap)


 # noHSdiploma.map = tm_shape(usDataMap) + tm_fill("no_hs_diploma", legend.show = TRUE, id = "NAME", palette = "Greens" ) +  tm_polygons("no_hs_diploma", "orange", style = "cont",  n = 2, alpha = 1 , stretch.palette = TRUE, popup.vars = NA, convert2density = FALSE, midpoint = FALSE)
#tmap_mode("view")
#noHSdiploma.map








######################
#tm_shape(usdm.1) + tm_polygons("Poverty", "Red", alpha = #1, palette = "Greens", style = "order") 
#tmap_mode("view")
#################################
#style = c("pretty", "fixed")) 











