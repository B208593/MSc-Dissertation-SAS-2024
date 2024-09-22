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

SOCRATES_ALL <- read_csv("C:/Users/SST24004/OneDrive - NHS Scotland/Documents/MSc Project/2_Data/1_BINDED_FILES\\SOCRATES_ALL.csv", guess_max=100000)

unique(SOCRATES_ALL$Onset_Datetime)

# count duplicates
sum(table(SOCRATES_ALL$INCIDENTID)-1)

# count disticnt values - UNABLE TO ANALYSE DUE TO HIGH NUMBER OF VALUES
n_distinct(SOCRATES_ALL$Radiates_RadiatesTo)
head(SOCRATES_NEW$Radiates_RadiatesTo)

#CREATE SUBSETS OF DATA
SOCRATES_FINAL = subset(SOCRATES_ALL, 
                 select = c(INCIDENTID,
                            Character_ConstIntermit,
                            Character_ImprovWorse,
                            Radiates_YesNo,
                            Radiates_RadiatesTo,
                            AssocSymptoms_Nausea,
                            AssocSymptoms_Vomiting,
                            AssocSymptoms_Dizziness,
                            AssocSymptoms_Diarrhoea))


unique(SOCRATES_FINAL$Character_ImprovWorse)

glimpse(SOCRATES_FINAL)


## remove "" from text
SOCRATES_FINAL  <- SOCRATES_FINAL %>%
  mutate(across(everything(), ~ str_remove_all(., '"')))


## CHECK IF IT WORKED
unique(SOCRATES_FINAL$AssocSymptoms_Nausea)  

### RENAME VALUES
############### AssocSymptoms_Nausea
SOCRATES_FINAL <- SOCRATES_FINAL %>%
  mutate(AssocSymptoms_Nausea = recode(AssocSymptoms_Nausea, 
                                       "UNDER ARMPIT,ULQ ABDO PAIN" = '0',
                                       "ASPIRIN RELIVED BRIEFLY" = '0',
                                       "BACK" = '0',
                                       "JAW, TO BACK." = '0',
                                       "BACK (AT BRA LINE)" = '0'))

unique(SOCRATES_FINAL$AssocSymptoms_Nausea)

################# AssocSymptoms_Vomiting

SOCRATES_FINAL <- SOCRATES_FINAL %>%
  mutate(AssocSymptoms_Vomiting = recode(AssocSymptoms_Vomiting, 
                                       "10 THEN AFTER GTN 4 BUT BACK TO 5 ON ROUTE TO HOSPITAL" = '0'))


unique(SOCRATES_FINAL$AssocSymptoms_Vomiting)


################# AssocSymptoms_Dizziness

SOCRATES_FINAL <- SOCRATES_FINAL %>%
  mutate(AssocSymptoms_Dizziness = recode(AssocSymptoms_Dizziness, 
                                         "L BREAST." = '0'))


unique(SOCRATES_FINAL$AssocSymptoms_Dizziness)


################# Radiates_YesNo

SOCRATES_FINAL <- SOCRATES_FINAL %>%
  mutate(Radiates_YesNo = recode(Radiates_YesNo, 
                                          "PRESSURE" = 'No',
                                 "0" = 'No',
                                 "Improving" = 'No'))


unique(SOCRATES_FINAL$Radiates_YesNo)

################# Character_ConstIntermit

SOCRATES_FINAL <- SOCRATES_FINAL %>%
  mutate(Character_ConstIntermit = recode(Character_ConstIntermit, 
                                 "SORE" = 'Constant',
                                 "ANNOYING" = 'Constant',
                                 "PAIN WORSE ON DEEP INHALATION & PALPATION OF EPIGASTRIC REGION. PAIN RELIEVED AFTER MORNING MEDS INC; LANSOPRAZOLE & CODRYAMOL." = 'Constant',
                                 "PULSATING...SHARP" = 'Constant',
                                 "SOMETHING STUCK" = 'Constant',
                                 "TINGLING ON RIGHT ARM" = 'Constant',
                                 "HEAVY ACHE" = 'Constant',
                                 "NOT A PAIN" = 'NO PAIN',
                                 "0" = 'NO PAIN',
                                 ",Intermittent,,Yes,NOT CONSTANT - HEAVY FEELING IN L ARM" = 'Intermittent'))


unique(SOCRATES_FINAL$Character_ConstIntermit)

################# Character_ImprovWorse

SOCRATES_FINAL <- SOCRATES_FINAL %>%
  mutate(Character_ImprovWorse = recode(Character_ImprovWorse, 
                                          "SQUEEZING" = 'Constant',
                                          "8/10 AT WORST, NOW PAIN FREE." = 'Improving',
                                          "PAIN IN L SIDE NECK" = 'Constant'))


unique(SOCRATES_FINAL$Character_ConstIntermit)
unique(SOCRATES_FINAL$Character_ImprovWorse)
unique(SOCRATES_FINAL$Radiates_YesNo)
unique(SOCRATES_FINAL$AssocSymptoms_Nausea)
unique(SOCRATES_FINAL$AssocSymptoms_Vomiting)
unique(SOCRATES_FINAL$Character_ImprovWorse)


################### RECODE LEVELS of factors to YES & NO
library(naniar)

SOCRATES_FINAL <- SOCRATES_FINAL %>%
  mutate(AssocSymptoms_Nausea = recode(AssocSymptoms_Nausea,"0"="No","1"="Yes"),
         AssocSymptoms_Vomiting = recode(AssocSymptoms_Vomiting,"0"="No","1"="Yes"),
         AssocSymptoms_Dizziness = recode(AssocSymptoms_Dizziness,"0"="No","1"="Yes"),
         AssocSymptoms_Diarrhoea = recode(AssocSymptoms_Diarrhoea,"0"="No","1"="Yes"))

unique(SOCRATES_FINAL$AssocSymptoms_Nausea)
unique(SOCRATES_FINAL$AssocSymptoms_Vomiting)
unique(SOCRATES_FINAL$AssocSymptoms_Dizziness)
unique(SOCRATES_FINAL$AssocSymptoms_Diarrhoea)
unique(SOCRATES_FINAL$Character_ImprovWorse)
unique(SOCRATES_FINAL$Character_ConstIntermit)
unique(SOCRATES_FINAL$Character_ImprovWorse)


SOCRATES_FINAL <- SOCRATES_FINAL %>%
  replace_with_na(replace = list(Character_ImprovWorse = "NA"))


##########################################################################


write.csv(SOCRATES_FINAL, "C:/Users/SST24004/OneDrive - NHS Scotland/Documents/MSc Project\\SOCRATES_FINAL_2408.csv", row.names=FALSE)
