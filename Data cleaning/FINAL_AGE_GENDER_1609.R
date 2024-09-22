# Load the packages
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(BSDA)
library(janitor)
library(descriptr)
library(epitools)
library(finalfit)
library(RColorBrewer)
library(reportROC)
library(knitr)
library(broom)
library(ggplot2)
library(gtsummary)
library(magrittr)
library(naniar)

AGE_GENDER_ALL <- read_csv("C:/Users/SST24004/OneDrive - NHS Scotland/Documents/MSc Project/2_Data/1_BINDED_FILES\\AGE_GENDER_ALL.csv", guess_max=100000)


glimpse(AGE_GENDER_ALL)


## RENAME COLUMNS FOR EASIER INTERPRETATION
AGE_GENDER_ALL <-  AGE_GENDER_ALL %>% 
  rename(
    AGE = Pat_Info_Calculated_Age,
    GENDER  = Pat_Info_Gender,
    Postcode = Pat_Info_Postcode,
    Ethnicity = Pat_Info_Ethnicity)

glimpse(AGE_GENDER_ALL)


######################### POSTCODE

#IMPORT DEP_SCOT FILE

DEP_SCOT = read.csv("depr_scot.csv")


##### REMOVE SPACES FROM POSTCODES TO BE LINKED

DEP_SCOT = DEP_SCOT %>%
  mutate(POSTCODE_NEW = str_replace_all(Postcode, " ", ""))

##### REMOVE SPACES FROM POSTCODES TO BE LINKED

AGE_GENDER_ALL = AGE_GENDER_ALL %>%
  mutate(POSTCODE_NEW = str_replace_all(Postcode, " ", ""))


#GET DEPRIVATION LEVEL FOR POSTCODE
AGE_GENDER_ALL <- left_join(AGE_GENDER_ALL, DEP_SCOT, by = "POSTCODE_NEW")

# DEPRIVATION=1 (MOST DEPRIVED)
# DEPRIVATION =5 (LEAST DEPRIVED)
# GENERATE DEPRIVATION ZONE CATEGORIES
AGE_GENDER_ALL <- AGE_GENDER_ALL %>%
  mutate(DEP_CAT = case_when(SIMD2020_Decile >=1  & SIMD2020_Decile <= 4 ~ 'MOST_DEP',
                             SIMD2020_Decile >= 5  & SIMD2020_Decile <= 7 ~ 'MEDIUM_DEP',
                             SIMD2020_Decile >= 8  & SIMD2020_Decile <= 10 ~ 'LEAST_DEP'))

glimpse(AGE_GENDER_ALL)

###############     CREATING AGE GROUPS    ########################

#age to categorical variable with 5 levels
AGE_GENDER_ALL <- AGE_GENDER_ALL %>%
  mutate(AGE_CATEGORY = case_when(AGE <= 29 ~ '<30',
                                  AGE >= 30  & AGE <= 49 ~ '30-49',
                                  AGE >= 50  & AGE <= 59 ~ '50-59',
                                  AGE >= 60  & AGE <= 69 ~ '60-69',
                                  AGE >= 70 ~ '>69'))


## OPTIONAL
AGE_GENDER_FINAL = subset(AGE_GENDER_ALL, 
                           select = -c(DZ,
                                       SIMD2020_Rank,
                                       SIMD2020_Vigintile,
                                       SIMD2020_Decile,
                                       POSTCODE_NEW,
                                       Postcode.y,
                                       Ethnicity))


### replace other with NA

## convert Unknown to NA
AGE_GENDER_FINAL = AGE_GENDER_FINAL %>% 
  replace_with_na(replace = list(GENDER = "Unknown"))

## convert Other to NA
AGE_GENDER_FINAL = AGE_GENDER_FINAL %>% 
  replace_with_na(replace = list(GENDER = "Other"))

## replaced zero and below zero values with NA
AGE_GENDER_FINAL = AGE_GENDER_FINAL %>% 
  replace_with_na(replace = list(AGE = c(0,0.003,0.860,0.016,0.014)))

unique(AGE_GENDER_FINAL$GENDER)
unique(AGE_GENDER_FINAL$AGE)

## rename Postcode.x
AGE_GENDER_FINAL = AGE_GENDER_FINAL %>% 
  rename(Home_Postcode = Postcode.x)


write.csv(AGE_GENDER_FINAL, "C:/Users/SST24004/OneDrive - NHS Scotland/Documents/MSc Project/\\AGE_GENDER_FINAL_1609.csv", row.names=FALSE)


