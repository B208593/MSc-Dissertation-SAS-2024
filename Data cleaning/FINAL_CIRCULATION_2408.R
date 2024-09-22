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

CIRCULATION_ALL <- read_csv("C:/Users/SST24004/OneDrive - NHS Scotland/Documents/MSc Project/2_Data/1_BINDED_FILES\\CIRCULATION_ALL.csv", guess_max=100000)


#separate SYSTOLIC-DIASTOLIC mm Hg

#CIRCULATION$new <- strsplit(CIRCULATION$Cir_Blood_Pressure,split = '/')
#str_split_fixed(CIRCULATION$Cir_Blood_Pressure, "/", 2)

CIRCULATION_FINAL = CIRCULATION_ALL %>% 
  separate_wider_delim(Cir_Blood_Pressure, "/", names = c("SYS", "DIAS"))


#CREATE CATEGORIES

CIRCULATION_FINAL <- CIRCULATION_FINAL %>%
  mutate(Blood_pressure = case_when(
    SYS < 100  & DIAS < 60 ~ 'Low',
    SYS <= 120 & DIAS <= 80 ~ 'Normal',
    SYS >= 120 & SYS <= 129 & DIAS <= 80 ~ 'Elevated',
    SYS >= 130 & SYS <= 139 | DIAS <= 90 ~ 'Hypertension_ST1',
    SYS >= 140 | DIAS <= 90 ~ 'Hypertension_ST2',
    SYS >= 180  | DIAS >= 100 ~ 'Hypertensive_Crisis'
  ))


CIRCULATION_FINAL %>% 
  select(Blood_pressure) %>%
  tbl_summary()
    
#RENAME ECG RHYTHM

CIRCULATION_FINAL = CIRCULATION_FINAL %>% 
  mutate(ECG_rhythm = case_when(
    Cir_ECG_Rhythm %in% c("AF", "Atrial Flutter") ~ "Arrhytmia",
    Cir_ECG_Rhythm %in% c("Asystole") ~ "Asystole",
    Cir_ECG_Rhythm %in% c("LBBB") ~ "Left bundle branch block",
    Cir_ECG_Rhythm %in% c("RBBB") ~ "Right bundle branch block",
    Cir_ECG_Rhythm %in% c("Sinus Brady") ~ "Bradycardia",
    Cir_ECG_Rhythm %in% c("1Heart Block", "2.2Heart Block", "3Heart Block", "2.1Heart Block") ~ "Heart_block",
    Cir_ECG_Rhythm %in% c("Sinus Tachy", "Narrow Complex Tachy", "VT", "Broad Complex Tachy") ~ "Tachycardia",
    Cir_ECG_Rhythm %in% c("STEMI") ~ "STEMI",   
    Cir_ECG_Rhythm %in% c("Sinus Rhythm") ~ "Sinus Rhythm", 
    Cir_ECG_Rhythm %in% c("VF") ~ "Ventricular fibrillation"))


# CREATE SUBSET

CIRCULATION_FINAL = subset(CIRCULATION_FINAL, 
                         select = -c(Cir_ECG_Other,
                                     Cir_IV_Right_Hand,
                                     Cir_IV_Right_Forearm,
                                     Cir_IV_Right_ACF,
                                     Cir_IV_Right_Foot,
                                     Cir_IV_Right_Other,
                                     Cir_IV_Left_Hand,
                                     Cir_IV_Left_Forearm,
                                     Cir_IV_Left_ACF,
                                     Cir_IV_Left_Foot,
                                     Cir_IV_Left_Other,
                                     Cir_PVC_Indicated,
                                     Cir_PVC_Hand_Hygiene,
                                     Cir_PVC_Skin_Cleansed,
                                     Cir_PVC_Catheter_Site_Covered,
                                     Cir_IO_Right,
                                     Cir_IO_Left))



glimpse(CIRCULATION_FINAL)

# RECODE LEVELS of factors to YES & NO

CIRCULATION_FINAL <- CIRCULATION_FINAL %>%
  mutate(Cir_ECG_3_Lead_new = recode(Cir_ECG_3_Lead,"0"="No","1"="Yes"),
         Cir_ECG_12_Lead_new = recode(Cir_ECG_12_Lead,"0"="No","1"="Yes"))
         

glimpse(CIRCULATION_FINAL)
unique(CIRCULATION_FINAL$Cir_ECG_Rhythm)

write.csv(CIRCULATION_FINAL, "C:/Users/SST24004/OneDrive - NHS Scotland/Documents/MSc Project\\CIRCULATION_FINAL_2408.csv", row.names=FALSE)

