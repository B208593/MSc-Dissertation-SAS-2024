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


################ CREATE SUBSET
## exclude columns contaiing no data

AIRWAY_FINAL = subset(AIRWAY_ALL, 
                         select = -c(Treatment_BackBlow,
                                     Treatment_AbdominalThrust,
                                     Treatment_ChestThrust,
                                     Treatment_Other,
                                     FurtherTreatment_Intubation_ETCO2,
                                     FurtherTreatment_Intubation_HospIntub,
                                     FurtherTreatment_Cricothyrotomy_Attempts,
                                     FurtherTreatment_Cricothyrotomy_Successful))



#### recode values to Y/N

AIRWAY_FINAL <- AIRWAY_FINAL %>%
  mutate(Treatment_Positioning_N = recode(Treatment_Positioning,"0" ="No", "1" ="Yes"),
         Treatment_Suction_N = recode(Treatment_Suction,"0" ="No", "1" ="Yes"),
         Treatment_HeadTilt_N = recode(Treatment_HeadTilt,"0" ="No", "1" ="Yes"),
         Treatment_JawThrust_N = recode(Treatment_JawThrust,"0" ="No", "1" ="Yes"),
         Treatment_OPA_N = recode(Treatment_OPA,"0" ="No", "1" ="Yes"),
         Treatment_NPA_N = recode(Treatment_NPA,"0" ="No", "1" ="Yes"),
         Treatment_Cough_N = recode(Treatment_Cough,"0" ="No", "1" ="Yes"),
         Treatment_LaryngoscopyForceps_N = recode(Treatment_LaryngoscopyForceps,"0" ="No", "1" ="Yes"))


AIRWAY_FINAL = AIRWAY_FINAL %>%  
  rename(AIRWAY_ASSESSMENT=Assessment)
         
write.csv(AIRWAY_FINAL, "C:/Users/SST24004/OneDrive - NHS Scotland/Documents/MSc Project\\AIRWAY_FINAL_1808.csv", row.names=FALSE)


