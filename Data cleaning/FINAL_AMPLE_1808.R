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
library(writexl)


# RENAME PMH_Oth TO SuicideAttempts_HowMany/DIABETES
## NEW NAME = OLD NAME
AMPLE_ALL <- AMPLE_ALL %>% 
  rename(SuicideAttempts_HowMany = PMH_Oth,
         PMH_G_Diabetes = PMH_Other_SuicideAttempts_When)

unique(AMPLE_ALL$PMH_G_Diabetes)
typeof(AMPLE_ALL$SuicideAttempts_HowMany)

### subset
AMPLE_FINAL = subset(AMPLE_ALL, 
                     select = c(INCIDENTID,
                                PMH_HD_MI_HowMany,
                                PMH_HD_PCI_HowMany,
                                PMH_ND_Stroke_HowMany,
                                PMH_ND_TIA_HowMany,
                                PMH_Other_Overdose_HowMany,
                                SuicideAttempts_HowMany,
                                PMH_HD_Hypertension,
                                PMH_HD_Hypotension,
                                PMH_HD_Angina,
                                PMH_RD_COPD,
                                PMH_RD_Asthma,
                                PMH_ND_Epilepsy,
                                PMH_G_Diabetes))


## CREATE NEW COLUMNS
AMPLE_FINAL <- AMPLE_FINAL %>%
  mutate(PMH_HD_MI = ifelse(PMH_HD_MI_HowMany != "NA", "Yes"),
         PMH_HD_MI = if_else(is.na(PMH_HD_MI), "No", PMH_HD_MI),
         PMH_HD_PCI = ifelse(PMH_HD_PCI_HowMany != "NA", "Yes"),
         PMH_HD_PCI = if_else(is.na(PMH_HD_PCI), "No", PMH_HD_PCI),
         PMH_ND_Stroke = ifelse(PMH_ND_Stroke_HowMany != "NA", "Yes"),
         PMH_ND_Stroke = if_else(is.na(PMH_ND_Stroke), "No", PMH_ND_Stroke),
         PMH_ND_TIA = ifelse(PMH_ND_TIA_HowMany != "NA", "Yes"),
         PMH_ND_TIA = if_else(is.na(PMH_ND_TIA), "No", PMH_ND_TIA),
         PMH_GEN_Overdose = ifelse(PMH_Other_Overdose_HowMany != "NA", "Yes"),
         PMH_GEN_Overdose = if_else(is.na(PMH_GEN_Overdose), "No", PMH_GEN_Overdose),
         PMH_GEN_SuicideAttempts = ifelse(SuicideAttempts_HowMany != "NA", "Yes"),
         PMH_GEN_SuicideAttempts = if_else(is.na(PMH_GEN_SuicideAttempts), "No", PMH_GEN_SuicideAttempts),
         PMH_GEN_Diabetes = ifelse(PMH_G_Diabetes != "NA", "Yes"),
         PMH_GEN_Diabetes = if_else(is.na(PMH_GEN_Diabetes), "No", PMH_GEN_Diabetes))
         

AMPLE_FINAL %>% 
  select(PMH_HD_MI,PMH_HD_PCI,PMH_HD_Angina_N )%>%
  tbl_summary(sort = list(everything() ~ "frequency"),
              digits = list(all_categorical() ~ c(0, 2)))



## recode 0 & 1 to No & Yes
AMPLE_FINAL <- AMPLE_FINAL %>%
  mutate(PMH_HD_Hypertension_N = ifelse(PMH_HD_Hypertension != 0, "Yes",  "No"),
         PMH_HD_Hypotension_N = ifelse(PMH_HD_Hypotension != 0, "Yes",  "No"),
         PMH_HD_Angina_N = ifelse(PMH_HD_Angina != 0, "Yes",  "No"),
         PMH_RD_COPD_N = ifelse(PMH_RD_COPD != 0, "Yes",  "No"),
         PMH_RD_Asthma_N = ifelse(PMH_RD_Asthma != 0, "Yes",  "No"),
         PMH_ND_Epilepsy_N = ifelse(PMH_ND_Epilepsy != 0, "Yes",  "No"))

unique(AMPLE_FINAL$PMH_HD_Hypertension_N)
 
########### CREATE CATEGORIES FOR HOW MANY 

## CHECK VALUES

unique(AMPLE_FINAL$PMH_RD_Asthma)


### PMH_HD_MI_HowMany
AMPLE_FINAL <- AMPLE_FINAL %>%
  mutate(NUMBER_OF_MI = case_when(PMH_HD_MI_HowMany == 1 ~ '1',
                                 PMH_HD_MI_HowMany > 1  & PMH_HD_MI_HowMany <= 5 ~ '1-5',
                                 PMH_HD_MI_HowMany > 5  & PMH_HD_MI_HowMany <= 10 ~ '6-10',
                                 PMH_HD_MI_HowMany > 10 ~ '10<'))

### PMH_HD_PCI_HowMany
AMPLE_FINAL <- AMPLE_FINAL %>%
  mutate(NUMBER_OF_PCI = case_when(PMH_HD_PCI_HowMany == 1 ~ '1',
                                  PMH_HD_PCI_HowMany > 1  & PMH_HD_PCI_HowMany <= 5 ~ '1-5',
                                  PMH_HD_PCI_HowMany > 5  & PMH_HD_PCI_HowMany <= 10 ~ '6-10',
                                  PMH_HD_PCI_HowMany > 10 ~ '10<'))


### PMH_ND_Stroke_HowMany
AMPLE_FINAL <- AMPLE_FINAL %>%
  mutate(NUMBER_OF_STROKE = case_when(PMH_ND_Stroke_HowMany == 1 ~ '1',
                                   PMH_ND_Stroke_HowMany > 1  & PMH_ND_Stroke_HowMany <= 5 ~ '1-5',
                                   PMH_ND_Stroke_HowMany > 5  & PMH_ND_Stroke_HowMany <= 10 ~ '6-10',
                                   PMH_ND_Stroke_HowMany > 10 ~ '10<'))

### PMH_ND_TIA_HowMany
AMPLE_FINAL <- AMPLE_FINAL %>%
  mutate(NUMBER_OF_TIA = case_when(PMH_ND_TIA_HowMany == 1 ~ '1',
                                      PMH_ND_TIA_HowMany > 1  & PMH_ND_TIA_HowMany <= 5 ~ '1-5',
                                      PMH_ND_TIA_HowMany > 5  & PMH_ND_TIA_HowMany <= 10 ~ '6-10',
                                      PMH_ND_TIA_HowMany > 10 ~ '10<'))


### PMH_Other_Overdose_HowMany
AMPLE_FINAL <- AMPLE_FINAL %>%
  mutate(NUMBER_OF_OVERDOSE = case_when(PMH_Other_Overdose_HowMany == 1 ~ '1',
                                   PMH_Other_Overdose_HowMany > 1  & PMH_Other_Overdose_HowMany <= 5 ~ '1-5',
                                   PMH_Other_Overdose_HowMany > 5  & PMH_Other_Overdose_HowMany <= 10 ~ '6-10',
                                   PMH_Other_Overdose_HowMany > 10 ~ '10<'))

### SuicideAttempts_HowMany
AMPLE_FINAL <- AMPLE_FINAL %>%
  mutate(NUMBER_OF_SUICIDE_ATTEMPT = case_when(SuicideAttempts_HowMany == 1 ~ '1',
                                        SuicideAttempts_HowMany >= 1  & SuicideAttempts_HowMany <= 5 ~ '1-5',
                                        SuicideAttempts_HowMany > 5  & SuicideAttempts_HowMany <= 10 ~ '6-10',
                                        SuicideAttempts_HowMany > 10 ~ '10<'))


summary(AMPLE_FINAL$PMH_HD_PCI_HowMany) 

### subset
AMPLE_FINAL = subset(AMPLE_FINAL, 
                     select = -c(PMH_HD_MI_HowMany,
                                 PMH_HD_PCI_HowMany,
                                 PMH_ND_Stroke_HowMany,
                                 PMH_ND_TIA_HowMany,
                                 PMH_Other_Overdose_HowMany,
                                 SuicideAttempts_HowMany))
                                 
                       
glimpse(AMPLE_FINAL)


unique(AMPLE_FINAL$PMH_HD_Hypertension)

write.csv(AMPLE_FINAL, "C:/Users/SST24004/OneDrive - NHS Scotland/Documents/MSc Project\\AMPLE_FINAL_1808.csv", row.names=FALSE)

AMPLE_FINAL_1808 %>% 
  select(NUMBER_OF_MI,
         NUMBER_OF_PCI,
         NUMBER_OF_STROKE,
         NUMBER_OF_TIA,
         NUMBER_OF_OVERDOSE,
         NUMBER_OF_SUICIDE_ATTEMPT)%>%
  tbl_summary(sort = list(everything() ~ "frequency"),
              digits = list(all_categorical() ~ c(0, 2)))
