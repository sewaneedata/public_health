library(dplyr)
library(zipzcta)
library(dplyr)
library(zcta)
# library(tigris)
library(gaze)
library(gsheet)

# get uds mapper
df <- read.csv('uds_mapper.csv',stringsAsFactors = F)
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
names(df) <-gsub(pattern = '_$', '',names(df))  

# replace null with NA
df[df=='null'] <- NA
df[df==9999] <- NA

# remove first row
df <- df[-1,]

# df <- df %>% select(county = name,
#                     HCP_Dominant_Health_Center_2019,HCP_Dominant_Health_Center_Serving_ZCTA_Share_of_Patients_2019, HCP_2_Health_Center_Serving_ZCTA_2019, HCP_2_Health_Center_Share_of_Patients_2019,HCP_3_Health_Center_Serving_ZCTA_2019,HCP_3_Health_Center_Share_of_Patients_2019,HCP_4_Health_Center_Serving_ZCTA_2019, HCP_4_Health_Center_Serving_ZCTA_Share_of_Patients_2019,HCP_5_Health_Center_Serving_ZCTA_2019, HCP_5_Health_Center_Share_of_Patients_2019 ,Pop_Total_2015_2019, HCP_Health_Center_Count_Combined_2019, HCP_Total_Patients_2019, HCP_Penetration_of_Low_Income, HCP_Penetration_of_Total_Population, HCP_Penetration_of_Uninsured_Population, HCP_Penetration_of_Medicaid_Public_Ins, HCP_Penetration_of_Medicare_Private_Ins)

# convert character to number

df$HCP_Penetration_of_Low_Income <- as.numeric(df$HCP_Penetration_of_Low_Income)
df$HCP_Penetration_of_Total_Population <- as.numeric(df$HCP_Penetration_of_Total_Population)
df$HCP_Penetration_of_Medicaid_Public_Ins<- as.numeric(df$HCP_Penetration_of_Medicaid_Public_Ins)
df$HCP_Penetration_of_Uninsured_Population <- as.numeric(df$HCP_Penetration_of_Uninsured_Population)
df$HCP_Penetration_of_Medicare_Private_Ins<- as.numeric(df$HCP_Penetration_of_Medicare_Private_Ins)


df$HCP_Uninsured_Not_Served_by_Health_Centers <- as.numeric(df$HCP_Uninsured_Not_Served_by_Health_Centers)
df$HCP_Medicaid_Public_Ins_Not_Served_by_Health_Centers <- 
  as.numeric(df$HCP_Medicaid_Public_Ins_Not_Served_by_Health_Centers)
df$HCP_Medicare_Private_Ins_Not_Served_by_Health_Centers <- 
  as.numeric(df$HCP_Medicare_Private_Ins_Not_Served_by_Health_Centers)
df$Pop_Poverty_2015_2019 <- as.numeric(df$Pop_Poverty_2015_2019)
df$Pop_Low_Income_2015_2019_1 <- as.numeric(df$Pop_Low_Income_2015_2019_1)
df$Pop_Racial_Ethnic_Minority_2015_2019 <- as.numeric(df$Pop_Racial_Ethnic_Minority_2015_2019)
df$Pop_Hispanic_Latino_2015_2019 <- as.numeric(df$Pop_Hispanic_Latino_2015_2019)
df$Pop_Black_2015_2019 <- as.numeric(df$Pop_Black_2015_2019)
df$Pop_Asian_2015_2019 <- as.numeric(df$Pop_Asian_2015_2019)
df$Pop_Native_Hawaiian_2015_2019 <- as.numeric(df$Pop_Native_Hawaiian_2015_2019)
df$Pop_Other_Pacific_Islander_2015_2019 <- as.numeric(df$Pop_Other_Pacific_Islander_2015_2019)
df$Pop_White_2015_2019 <- as.numeric(df$Pop_White_2015_2019)
df$Pop_More_than_One_Race_2015_2019 <- as.numeric(df$Pop_More_than_One_Race_2015_2019)
df$Pop_Uninsured_Est_2019_1 <- as.numeric(df$Pop_Uninsured_Est_2019_1)
df$Pop_Medicaid_Public_Ins_Est_2019_1 <- as.numeric(df$Pop_Medicaid_Public_Ins_Est_2019_1)
df$Pop_Medicare_Private_Ins_Est_2019_1 <- as.numeric(df$Pop_Medicare_Private_Ins_Est_2019_1)
df$Pop_Under_18_2015_2019 <- as.numeric(df$Pop_Under_18_2015_2019)
df$Pop_School_Aged_Children_2015_2019 <- as.numeric(df$Pop_School_Aged_Children_2015_2019)
df$Pop_18_64_2015_2019 <- as.numeric(df$Pop_18_64_2015_2019)
df$Pop_65_and_older_2015_2019 <- as.numeric(df$Pop_65_and_older_2015_2019)
df$Pop_Households_with_Limited_English_Proficiency_2015_2019 <- 
  as.numeric(df$Pop_Households_with_Limited_English_Proficiency_2015_2019)
df$Pop_Veterans_2015_2019 <- as.numeric(df$Pop_Veterans_2015_2019)
df$Pop_Disability_2015_2019 <- as.numeric(df$Pop_Disability_2015_2019)
df$Pop_Uninsured_Population_below_138_FPL_2015_2019 <- 
  as.numeric(df$Pop_Uninsured_Population_below_138_FPL_2015_2019)
df$Pop_Uninsured_Population_below_200_FPL_2015_2019 <- 
  as.numeric(df$Pop_Uninsured_Population_below_200_FPL_2015_2019)
df$Pop_Uninsured_Population_at_138_400_FPL_2015_2019 <- 
  as.numeric(df$Pop_Uninsured_Population_at_138_400_FPL_2015_2019)
# df$HCP_2_Health_Center_Serving_ZCTA_2019 <- as.numeric(df$HCP_2_Health_Center_Serving_ZCTA_2019)
# df$HCP_3_Health_Center_Serving_ZCTA_2019 <- as.numeric(df$HCP_3_Health_Center_Serving_ZCTA_2019)
# df$HCP_4_Health_Center_Serving_ZCTA_2019 <- as.numeric(df$HCP_4_Health_Center_Serving_ZCTA_2019)
# df$HCP_5_Health_Center_Serving_ZCTA_2019 <- as.numeric(df$HCP_5_Health_Center_Serving_ZCTA_2019)

# remove change variables 
df <- df[, !grepl('Change', names(df))]
df <- df[, !grepl('2017', names(df))]
df <- df[, !grepl('_1', names(df))]

df$state <- df$usps <- df$geoid <- df$county <- NULL

temp <- df %>% group_by(county=name) %>%
  summarise(first_fqhc = first(na.omit(HCP_Dominant_Health_Center_2019)),
            first_share = weighted.mean(HCP_Dominant_Health_Center_Serving_ZCTA_Share_of_Patients_2019, w = Pop_Total_2015_2019),
            second_fqhc = first(na.omit(HCP_2_Health_Center_Serving_ZCTA_2019)),
            second_share = weighted.mean(HCP_2_Health_Center_Share_of_Patients_2019, w = Pop_Total_2015_2019),
            third_fqhc = first(na.omit(HCP_3_Health_Center_Serving_ZCTA_2019)),
            third_share = weighted.mean(HCP_3_Health_Center_Share_of_Patients_2019, w = Pop_Total_2015_2019),
            fourth_fqhc = first(na.omit(HCP_4_Health_Center_Serving_ZCTA_2019)),
            fourth_share = weighted.mean(HCP_4_Health_Center_Serving_ZCTA_Share_of_Patients_2019, w = Pop_Total_2015_2019),
            fifth_fqhc = first(na.omit(HCP_5_Health_Center_Serving_ZCTA_2019)),
            fifth_share = weighted.mean(HCP_5_Health_Center_Share_of_Patients_2019, w = Pop_Total_2015_2019),
            
            tot_pop = sum(Pop_Total_2015_2019, na.rm = T),
            num_fqhc = sum(HCP_Health_Center_Count_Combined_2019, na.rm = T),
            num_patients = sum(HCP_Total_Patients_2019, na.rm = T),
            pop_low_income = sum(Pop_Low_Income_2015_2019, na.rm = T),
            unserved_low_income = sum(HCP_Low_Income_Not_Served_by_Health_Centers, na.rm = T),
            unserved_total_pop = sum(HCP_Total_Population_Not_Served_by_Health_Centers, na.rm = T),
            unserved_uninsured = sum(HCP_Uninsured_Not_Served_by_Health_Centers, na.rm = T),
            unserved_medicaid_public = sum(HCP_Medicaid_Public_Ins_Not_Served_by_Health_Centers, na.rm = T),
            unserved_medicaid_private = sum(HCP_Medicare_Private_Ins_Not_Served_by_Health_Centers, na.rm = T),
            
            
            low_income_pen = weighted.mean(HCP_Penetration_of_Low_Income, w = Pop_Total_2015_2019, na.rm=TRUE),
            tot_pop_pen = weighted.mean(HCP_Penetration_of_Total_Population, w = Pop_Total_2015_2019),
            uninsured_pen = weighted.mean(HCP_Penetration_of_Uninsured_Population, w = Pop_Total_2015_2019),
            medicaid_pen = weighted.mean(HCP_Penetration_of_Medicaid_Public_Ins, w = Pop_Total_2015_2019),
            uninsured = weighted.mean(HCP_Uninsured_2019, w = Pop_Total_2015_2019),
            medicaid_pub_ins = weighted.mean(HCP_Medicaid_Public_Ins_2019, w = Pop_Total_2015_2019),
            medicaid_priv_ins = weighted.mean(HCP_Medicare_Private_Ins_2019, w = Pop_Total_2015_2019),
            pop_poverty = weighted.mean(Pop_Poverty_2015_2019, w = Pop_Total_2015_2019),
            pop_minority = weighted.mean(Pop_Racial_Ethnic_Minority_2015_2019, w = Pop_Total_2015_2019),
            pop_hispanic_latino = weighted.mean(Pop_Hispanic_Latino_2015_2019, w = Pop_Total_2015_2019),
            pop_black = weighted.mean(Pop_Black_2015_2019, w = Pop_Total_2015_2019),
            pop_asian = weighted.mean(Pop_Asian_2015_2019, w = Pop_Total_2015_2019),
            pop_ai_an = weighted.mean(Pop_American_Indian_Alaska_Native_2015_2019, w = Pop_Total_2015_2019),
            pop_nh = weighted.mean(Pop_Native_Hawaiian_2015_2019, w = Pop_Total_2015_2019),
            pop_opi = weighted.mean(Pop_Other_Pacific_Islander_2015_2019, w = Pop_Total_2015_2019),
            pop_white = weighted.mean(Pop_White_2015_2019, w = Pop_Total_2015_2019),
            pop_mixed = weighted.mean(Pop_More_than_One_Race_2015_2019, w = Pop_Total_2015_2019),
            pop_uninsured = weighted.mean(Pop_Uninsured_Est_2019, w = Pop_Total_2015_2019),
            pop_medicaid_pub = weighted.mean(Pop_Medicaid_Public_Ins_Est_2019, w = Pop_Total_2015_2019),
            pop_medicaid_priv = weighted.mean(Pop_Medicare_Private_Ins_Est_2019, w = Pop_Total_2015_2019),
            pop_school_age = weighted.mean(Pop_School_Aged_Children_2015_2019, w = Pop_Total_2015_2019),
            pop_65_and_up = weighted.mean(Pop_65_and_older_2015_2019, w = Pop_Total_2015_2019),
            pop_unemployed = weighted.mean(Pop_Not_Employed_2015_2019, w = Pop_Total_2015_2019),
            pop_houses_limited_english = 
              weighted.mean(Pop_Households_with_Limited_English_Proficiency_2015_2019, w = Pop_Total_2015_2019),
            pop_less_than_high_school = weighted.mean(Pop_Less_Than_High_School_Education_2015_2019, w = Pop_Total_2015_2019),
            pop_vets = weighted.mean(Pop_Veterans_2015_2019, w = Pop_Total_2015_2019),
            pop_disability = weighted.mean(Pop_Disability_2015_2019, w = Pop_Total_2015_2019),
            pop_unins_under_200 = weighted.mean(Pop_Uninsured_Population_below_200_FPL_2015_2019, w = Pop_Total_2015_2019)
)

write.csv(temp,'cleaned_udsm.csv')

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


temp$`American Indian/Alaska Native` <- as.numeric(temp$`American Indian/Alaska Native`)
temp$`Native Hawaiian / Other Pacific Islander` <- as.numeric(temp$`Native Hawaiian / Other Pacific Islander`)
temp$`Best Served in another language` <- as.numeric(temp$`Best Served in another language`)
temp$`Mental Health` <- as.numeric(temp$`Mental Health`)
temp$`Substance Abuse` <- as.numeric(temp$`Substance Abuse`)
temp$Vision <- as.numeric(temp$Vision)
temp$Enabling <- as.numeric(temp$Enabling)
temp$`Prenatal Patients` <- as.numeric(temp$`Prenatal Patients`)
temp$`Prenatal Patients who Delivered` <- as.numeric(temp$`Prenatal Patients who Delivered`)
temp$`Access to Prenatal Care (First Prenatal Visit in 1st Trimester)` <- as.numeric(temp$`Access to Prenatal Care (First Prenatal Visit in 1st Trimester)`)
temp$`Low Birth Weight` <- as.numeric(temp$`Low Birth Weight`)
temp$`Dental Sealants` <- as.numeric(temp$`Dental Sealants`)
temp$`HIV Linkage to Care` <- as.numeric(temp$`HIV Linkage to Care`)


write.csv(temp, file = 'fqhc_data.csv')





