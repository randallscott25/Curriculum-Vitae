# Linear model
# After you've imported CHSI

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
                ,All_Death 
                ,US_All_Death 
              from combined')

str(vuln)

vuln$No_HS_Diploma <- vuln$No_HS_Diploma/vuln$Population_Size*100
vuln$Unemployed <- vuln$Unemployed/vuln$Population_Size*100
vuln$Sev_Work_Disabled <-vuln$Sev_Work_Disabled/vuln$Population_Size*100
vuln$Major_Depression <- vuln$Major_Depression/vuln$Population_Size*100
vuln$Recent_Drug_Use <- vuln$Recent_Drug_Use/vuln$Population_Size*100
vuln$Uninsured <- vuln$Uninsured/vuln$Population_Size*100
vuln$Elderly_Medicare <- vuln$Elderly_Medicare/vuln$Population_Size*100
vuln$Disabled_Medicare <- vuln$Disabled_Medicare/vuln$Population_Size*100
vuln$DeathRate <- vuln$All_Death/vuln$Population_Size*100

v_lm <- lm(DeathRate ~ Poverty +No_HS_Diploma +Unemployed +Sev_Work_Disabled +Major_Depression +Recent_Drug_Use +No_Exercise +Few_Fruit_Veg +Obesity +High_Blood_Pres +Smoker +Diabetes +Uninsured +Elderly_Medicare +Disabled_Medicare +Prim_Care_Phys_Rate +Dentist_Rate
,data=vuln)
summary(v_lm)

v_lm1 <- lm(DeathRate ~ Unemployed +Major_Depression +Recent_Drug_Use +No_Exercise +High_Blood_Pres +Smoker +Diabetes +Uninsured +Elderly_Medicare +Disabled_Medicare +Dentist_Rate
,data=vuln)
summary(v_lm1)
