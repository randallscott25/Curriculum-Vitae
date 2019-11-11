#Luke Miller's script - Randall Taylor working copy 2.2
#7 Aug 2019 created; Taylor edits, 2.2 begin 4 SEPT
#IST687 group
#This script imports data from the chsi dataset and combines several sheets to
#make it easier to work with. You need to run it from within the folder that
#contains all the individual csv files

# use sqldf for joins
library(sqldf)
library(readr)
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
library(sf)
library(raster)
library(dplyr)
library(stringr)
library(spData)
library(leaflet)
library(sp)
library(tidyr)
library(devtools)
library(ggplot2)
library(tmap)
library(tmaptools)


#!!change to your own directory where the CSV files are located!!
setwd("C://Users//randa//Desktop//Applied Data Science//Final Project//chsi_dataset")

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
birthandeath = read_csv("MEASURESOFBIRTHANDDEATH.csv")
#View(riskfactors)
combined <- sqldf('
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

str(combined)
write_csv(combined,'combined.csv')
View(combined)

#replace NAs with mean of column auto
na_mean_swap <- function(x) {
  replace(x, is.na(x),mean(as.numeric(x),na.rm=TRUE))
}

mean_clean <- cbind(combined[,1:7],replace(combined[,-1:-7],TRUE, lapply(combined[,-1:-7], na_mean_swap)))
str(mean_clean)

#data set = mean_clean
dim(mean_clean) #two dimmensional rows 3141 columns 140 
nrow(mean_clean)#how many rows 
ncol(mean_clean)#how many columns
class(mean_clean)#what is this objects classification 
#make Randall dataset based upon dplyr, tidyr: for first correlation runs: suicide
View(mean_clean)
#rstDataset = mean_clean %>% 
 # select(State_FIPS_Code, CHSI_County_Name,  Population_Size, Poverty, Age_19_Under, Age_19_64, Age_65_84, Age_85_and_Over, No_HS_Diploma, Unemployed,Sev_Work_Disabled, Major_Depression, Recent_Drug_Use,No_Exercise, Few_Fruit_Veg, Obesity, Smoker, Diabetes, Uninsured, Elderly_Medicare, Disabled_Medicare,C_Wh_Suicide,)


#suicide study

sDemo.DF =  mean_clean %>%
  select(County_FIPS_Code, State_FIPS_Code, CHSI_County_Name, CHSI_State_Name, C_Wh_Suicide, C_Bl_Suicide, C_Ot_Suicide, C_Hi_Suicide, Population_Size )
View(sDemo.DF)


sContribs.df = mean_clean %>% 
  select(State_FIPS_Code, CHSI_County_Name, Number_Counties, Population_Size, Poverty, Age_19_Under, Age_19_64, Age_65_84, Age_85_and_Over, No_HS_Diploma, Unemployed,Sev_Work_Disabled, Major_Depression, Recent_Drug_Use,No_Exercise, Few_Fruit_Veg, Obesity, Smoker, Diabetes, Uninsured, Elderly_Medicare, Disabled_Medicare, High_Blood_Pres, Toxic_Chem, Carbon_Monoxide_Ind,Nitrogen_Dioxide_Ind,Sulfur_Dioxide_Ind, Ozone_Ind, Particulate_Matter_Ind, Lead_Ind, Disabled_Medicare, Prim_Care_Phys_Rate, Dentist_Rate, Community_Health_Center_Ind, Suicide)
View(sContribs.df)
library(Rcmdr)

#County Data Ages 15 - 24 suicide, black americans

s.black = with(sDemo.DF, tapply(C_Bl_Suicide, list(CHSI_State_Name), mean, na.rm=TRUE))

#County Data Ages 15 - 24 suicide, hispanic americans
hist(with(sDemo.DF, tapply(C_Hi_Suicide, list(CHSI_State_Name), mean, na.rm=TRUE)))
# County Data Ages 15 - 24, listed as OTHER (than black, hispanic, white)
hist(with(sDemo.DF, tapply(C_Ot_Suicide, list(CHSI_State_Name), mean, na.rm=TRUE)))
#County Data Ages 15 - 24 suicide, white americans
with(sDemo.DF, tapply(C_Wh_Suicide, list(CHSI_State_Name), mean, na.rm=TRUE))
# Percentages of suicide, to the total, per state

stateLVL.s.DF = local({
  .Table <- with(sDemo.DF, table(CHSI_State_Name))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})


#plot the percentages 
plot(stateLVL.s.DF)                     
plot


##########Sucide to Uninsured 

#scatterplot  Suicide to Uninsured 
scatterplot(Suicide~Uninsured, regLine=TRUE, smooth=FALSE, boxplots=FALSE, 
            data=sContribs.df)
#correlation Suicide to Uninsured 
with(sContribs.df, cor.test(Suicide, Uninsured, 
                            alternative="two.sided", method="pearson"))
                     
                     
                     
                     
                     
                     

