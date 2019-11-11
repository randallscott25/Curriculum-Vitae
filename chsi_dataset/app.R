#load required packages using a bulk check/install function from professor
packages <- c("readr","sqldf","cowplot", "googleway", "ggplot2", "ggrepel",  "ggspatial", "lwgeom", "sf", "rnaturalearth", "rnaturalearthdata","maps","dplyr","stringr","spData","leaflet","sp","tidyr","devtools","tmap","tmaptools","corrplot","rgeos","rgdal","maptools","scales","shiny","Rcmdr")
package.check <- lapply(packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
    }
})

d <- read.csv("https://data.world/data-society/health-status-indicators/workspace/file?filename=DEMOGRAPHICS.csv",sep=",",quote="\"",header=T,fill=T)

v <- read.csv(url("https://data.world/data-society/health-status-indicators/workspace/file?filename=VUNERABLEPOPSANDENVHEALTH.csv"))
r <- read.csv(url("https://data.world/data-society/health-status-indicators/workspace/file?filename=RISKFACTORSANDACCESSTOCARE.csv"))
l <- read.csv(url("https://data.world/data-society/health-status-indicators/workspace/file?filename=LEADINGCAUSESOFDEATH.csv"))
s <- read.csv(url("https://data.world/data-society/health-status-indicators/workspace/file?filename=SUMMARYMEASURESOFHEALTH.csv"))
b <- read.csv(url("https://data.world/data-society/health-status-indicators/workspace/file?filename=MEASURESOFBIRTHANDDEATH.csv"))

#this is the one that explains what columns mean
key = read_csv("https://data.world/data-society/health-status-indicators/workspace/file?filename=DATAELEMENTDESCRIPTION.csv")

#
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

#View(mean_clean)

Ale.Contribs.df = mean_clean %>% 
    select(CHSI_State_Name,CHSI_County_Name, County_FIPS_Code,No_HS_Diploma, ALE,Population_Size)

ale.1 <- Ale.Contribs.df[,c('CHSI_State_Name','CHSI_County_Name','County_FIPS_Code','No_HS_Diploma', 'Population_Size','ALE')] 
ale.1$state <- tolower(ale.1$CHSI_State_Name)
ale.1$county<- tolower(ale.1$CHSI_County_Name)
ale.1$ID <- paste(ale.1$state,ale.1$county,sep=',')

#View(ale.1)

###### Community Health Status Indicators 

varlist <- setdiff(names(ale.1), "geometry")

runApp(list(
    ui = fluidPage(
        titlePanel("Community Health Status Indicators "),
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
}

# Create Shiny object
shinyApp(ui = ui, server = server)
