library(dplyr)
library(zipzcta)
library(dplyr)
library(zcta)
library(tigris)
library(gaze)
library(gsheet)

# get uds mapper
df <- read.csv('data/uds_mapper.csv',stringsAsFactors = F)
df <- df %>% filter(State !='AL')

# (or map to country with github package (zcta, zipzcta))
zctacounty = zcta::zcta_county_rel_10 %>%
  select(zcta5, state, county, geoid, poppt, zpoppct) %>%
  group_by(zcta5) %>%
  slice(which.max(zpoppct)) %>%
  left_join(gaze::county10, by = "geoid") %>%
  select(zcta5, state, usps, county, geoid, name) 

# here need to group by name (county) and aggregate to county
df <- inner_join(df, zctacounty, by = c('ZCTA' = 'zcta5')) 
names(df) <- gsub(pattern = '....', '_',names(df), fixed = TRUE)  
names(df) <- gsub(pattern = '...', '_',names(df), fixed = TRUE)  
names(df) <- gsub(pattern = '..', '_',names(df), fixed = TRUE)  
names(df) <-gsub(pattern = '.', '_',names(df), fixed = TRUE)
names(df) <-gsub(pattern = '__', '_',names(df), fixed = TRUE)  
names(df) <-gsub(pattern = '_$', '_',names(df))  

# replace null with NA
df[df=='null'] <- NA
df[df==9999] <- NA

df <- df %>% select(county = name, HCP_Dominant_Health_Center_2019,HCP_Dominant_Health_Center_Serving_ZCTA_Share_of_Patients_2019, HCP_2_Health_Center_Serving_ZCTA_2019, HCP_2_Health_Center_Share_of_Patients_2019,HCP_3_Health_Center_Serving_ZCTA_2019,HCP_3_Health_Center_Share_of_Patients_2019,HCP_4_Health_Center_Serving_ZCTA_2019, HCP_4_Health_Center_Serving_ZCTA_Share_of_Patients_2019,HCP_5_Health_Center_Serving_ZCTA_2019, HCP_5_Health_Center_Share_of_Patients_2019   ,Pop_Total_2015_2019, HCP_Health_Center_Count_Combined_2019, HCP_Total_Patients_2019, HCP_Penetration_of_Low_Income_, HCP_Penetration_of_Total_Population_, HCP_Penetration_of_Uninsured_Population_, HCP_Penetration_of_Medicaid_Public_Ins_, HCP_Penetration_of_Medicare_Private_Ins_)

df$HCP_Penetration_of_Low_Income_ <- as.numeric(df$HCP_Penetration_of_Low_Income_)
df$HCP_Penetration_of_Total_Population_ <- as.numeric(df$HCP_Penetration_of_Total_Population_)
df$HCP_Penetration_of_Medicaid_Public_Ins_<- as.numeric(df$HCP_Penetration_of_Medicaid_Public_Ins_)
df$HCP_Penetration_of_Uninsured_Population_ <- as.numeric(df$HCP_Penetration_of_Uninsured_Population_)
df$HCP_Penetration_of_Medicare_Private_Ins_<- as.numeric(df$HCP_Penetration_of_Medicare_Private_Ins_)

temp <- df %>% group_by(county) %>%
  summarise(first_fqhc = first(HCP_Dominant_Health_Center_2019),
            tot_pop = sum(Pop_Total_2015_2019, na.rm = T),
            num_fqhc = sum(HCP_Health_Center_Count_Combined_2019, na.rm = T),
            num_patients = sum(HCP_Total_Patients_2019, na.rm = T),
            low_income_pen = weighted.mean(HCP_Penetration_of_Low_Income_, w = Pop_Total_2015_2019),
            tot_pop_pen = weighted.mean(HCP_Penetration_of_Total_Population_, w = Pop_Total_2015_2019),
            uninsured_pen = weighted.mean(HCP_Penetration_of_Uninsured_Population_, w = Pop_Total_2015_2019),
            medicaid_pen = weighted.mean(HCP_Penetration_of_Medicaid_Public_Ins_, w = Pop_Total_2015_2019),
            medicare_pen = weighted.mean(HCP_Penetration_of_Medicare_Private_Ins_, w = Pop_Total_2015_2019),
            dominant_serving = weighted.mean(HCP_Dominant_Health_Center_Serving_ZCTA_Share_of_Patients_2019, w = Pop_Total_2015_2019))


write.csv(df,'cleaned_udsm.csv')


#### FQHC level data
# TN_awardess
fqhc_demo <- readxl::read_xlsx('TN_Awardees.xlsx', sheet = 1)
fqhc_patient <- readxl::read_xlsx('TN_Awardees.xlsx', sheet = 2)
fqhc_services <- readxl::read_xlsx('TN_Awardees.xlsx', sheet =3)
fqhc_clin <- readxl::read_xlsx('TN_Awardees.xlsx', sheet = 4)
fqhc_cost <- readxl::read_xlsx('TN_Awardees.xlsx', sheet = 5)

# join 
temp <- merge(fqhc_demo, fqhc_services)
temp <- merge(temp, fqhc_cost)
temp <- merge(temp, fqhc_clin)
temp <- merge(temp, fqhc_patient)

write.csv(temp, file = 'fqhc_data.csv')





