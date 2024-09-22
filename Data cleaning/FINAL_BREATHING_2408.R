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

BREATHING_ALL <- read_csv("C:/Users/SST24004/OneDrive - NHS Scotland/Documents/MSc Project/2_Data/1_BINDED_FILES\\BREATHING_ALL.csv", guess_max=100000)


######################## 1. MODIFY VARIABLES
## GROUPING

# Respiratory_Rate to categorical variable with 3 levels

BREATHING_FINAL <- BREATHING_ALL %>%
  mutate(Respiratory_category = case_when(Respiratory_Rate <= 12 ~ 'Low',
                                  Respiratory_Rate > 12  & Respiratory_Rate <= 20 ~ 'Normal',
                                  Respiratory_Rate > 20  ~ 'High'))


#check levels
levels(BREATHING_FINAL$Respiratory_category)

#SPO2 to categorical variable with 3 levels

BREATHING_FINAL <- BREATHING_FINAL %>%
  mutate(SPO2_category = case_when(SPO2 <= 88 ~ 'Very low',
                              SPO2 > 88  & SPO2 <= 95 ~ 'Low',
                              SPO2 > 95  ~ 'Normal'))



#check levels
levels(BREATHING_FINAL$SPO2_category)

# RECODE LEVELS of factors to YES & NO

BREATHING_FINAL <- BREATHING_FINAL %>%
  mutate(Chest_Sign_Cough_new = recode(Chest_Sign_Cough,"0"="No","1"="Yes"),
         Chest_Sign_Accessory_Muscles_new = recode(Chest_Sign_Accessory_Muscles,"0"="No","1"="Yes"),
         Chest_Sign_Grunt_new = recode(Chest_Sign_Grunt,"0"="No","1"="Yes"),
         Chest_Sign_Tracheal_Tug_new = recode(Chest_Sign_Tracheal_Tug,"0"="No","1"="Yes"),
         Chest_Sign_Surgical_Emphysema_new = recode(Chest_Sign_Surgical_Emphysema,"0"="No","1"="Yes"),
         Chest_Sign_Intercostal_Recession_new = recode(Chest_Sign_Intercostal_Recession,"0"="No","1"="Yes"),
         Chest_Sign_Subcostal_Recession_new = recode(Chest_Sign_Subcostal_Recession,"0"="No","1"="Yes"),
         Chest_Sign_Crepitus_new = recode(Chest_Sign_Crepitus,"0"="No","1"="Yes"),
         Chest_Sign_Flail_new = recode(Chest_Sign_Flail,"0"="No","1"="Yes"),
         Chest_Sign_Haemoptysis_new = recode(Chest_Sign_Haemoptysis,"0"="No","1"="Yes"),
         Chest_Sign_Stridor_new = recode(Chest_Sign_Stridor,"0"="No","1"="Yes"))
        

BREATHING_FINAL = subset(BREATHING_FINAL, 
                         select = -c(Chest_Sign_Other,
                                     Trachea_Deviated,
                                     Trachea_Deviated_Location,
                                     Needle_Decompression,
                                     Needle_Decompression_Location,
                                     Needle_Decompression_Location_Right,
                                     Needle_Decompression_Location_Left ))

glimpse(BREATHING_FINAL)

write.csv(BREATHING_FINAL, "C:/Users/SST24004/OneDrive - NHS Scotland/Documents/MSc Project\\BREATHING_FINAL_2408.csv", row.names=FALSE)

