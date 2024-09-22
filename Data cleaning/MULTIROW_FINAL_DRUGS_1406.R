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


DRUGTABLE_ALL <- read_csv("C:/Users/SST24004/OneDrive - NHS Scotland/Documents/MSc Project/2_Data/1_BINDED_FILES\\DRUGTABLE_ALL.csv", guess_max=100000)


glimpse(DRUGTABLE_ALL)

#check duplicates
sum(table(DRUGTABLE_ALL$INCIDENTID)-1)

#SELECT CERTAIN COLUMNS
DRUGTABLE_NEW = select(DRUGTABLE_ALL, 
                       INCIDENTID, 
                       RowIndex,
                       Drugname, 
                       Dosage,
                       Units,
                       Route,
                       PainBefore,
                       PainAfter)

#1. RENAME DRUGS

###################################################################################
#RENAME DRUGS
DRUGS_NEW_NAMES = DRUGTABLE_NEW %>%
  mutate(Drugname = case_when(
    str_detect(Drugname, pattern = "aspirin") ~ "Aspirin",
    str_detect(Drugname, pattern = "Aspirin") ~ "Aspirin",
    str_detect(Drugname, pattern = "Asprin") ~ "Aspirin",
    str_detect(Drugname, pattern = "asprin") ~ "Aspirin",
    str_detect(Drugname, pattern = "ASPRIN") ~ "Aspirin",
    str_detect(Drugname, pattern = "Apirin") ~ "Aspirin",
    str_detect(Drugname, pattern = "paracetamol")  ~ "Paracetamol",
    str_detect(Drugname, pattern = "paracetomol")  ~ "Paracetamol",
    str_detect(Drugname, pattern = "Co-codamol")  ~ "Co-codamol",
    str_detect(Drugname, pattern = "co-Codamol")  ~ "Co-codamol",
    str_detect(Drugname, pattern = "Co-Codamol")  ~ "Co-codamol",
    str_detect(Drugname, pattern = "co-codamol")  ~ "Co-codamol",
    str_detect(Drugname, pattern = "cocodamol")  ~ "Co-codamol",
    str_detect(Drugname, pattern = "Cocodamol")  ~ "Co-codamol",
    str_detect(Drugname, pattern = "GTN")  ~ "Glyceryl trinitrate (GTN)",
    str_detect(Drugname, pattern = "Gtn")  ~ "Glyceryl trinitrate (GTN)",
    str_detect(Drugname, pattern = "gtn")  ~ "Glyceryl trinitrate (GTN)",
    str_detect(Drugname, pattern = "Adrenaline")  ~ "Adrenaline",
    str_detect(Drugname, pattern = "Diazapam")  ~ "Diazepam",
    str_detect(Drugname, pattern = "Diclofenac Sodium")  ~ "Diclofenac",
    str_detect(Drugname, pattern = "diclofenic")  ~ "Diclofenac",
    str_detect(Drugname, pattern = "Diazepam")  ~ "Diazepam",
    str_detect(Drugname, pattern = "Gaviscon")  ~ "Gaviscon",
    str_detect(Drugname, pattern = "Glucose")  ~ "Glucose",
    str_detect(Drugname, pattern = "ibuprofen")  ~ "Ibuprofen",
    str_detect(Drugname, pattern = "Oramorph")  ~ "Oramorph",
    str_detect(Drugname, pattern = "Oromorph")  ~ "Oramorph",
    str_detect(Drugname, pattern = "oramorph")  ~ "Oramorph",
    str_detect(Drugname, pattern = "Peptac")  ~ "Peptac",
    str_detect(Drugname, pattern = "tramadol")  ~ "Tramadol",
    str_detect(Drugname, pattern = "Saline")  ~ "Saline flush",
    str_detect(Drugname, pattern = "Naproxen")  ~ "Naproxen",
    str_detect(Drugname, pattern = "TICAGRELOR")  ~ "Ticagrelor",
    TRUE ~ Drugname
  ))

write.csv(DRUGS_NEW_NAMES, "C:/Users/SST24004/OneDrive - NHS Scotland/Documents/MSc Project\\DRUGS_NEW_NAMES.csv", row.names=FALSE)
##########################################################################################################################
                                
#2. GROUP THEM
#create new column with drug groups (containing aspirin etc)
#create groups for drugs

Antibiotic <- "Amoxicillin|Cefotaxime|Doxycycline|Nitrofurantoin"
Anticoagulant <- "Aspirin|Enoxaparin|Heparin"
Antihistamine <- "Cyclizine"
Anxiety <- "Diazepam|Midazolam|Prochlorperazine"
Blood_sugar <- "Glucagon|Glucose|Hypostop"
Cardiac <- "Adrenaline|Amiodarone|Atropine|Clopidogrel|Glyceryl trinitrate (GTN)|Tenecteplase|Ticagrelor"
Gastric <-"Gaviscon|Hyoscine Butylbromide|Metoclopramide (Maxolon)|Omeprazole|Ondansetron|Peptac"
Opioid_antagonist <-"Naloxone (Narcan)"
Pulmonary <- "Ipratropium Bromide (Atrovent)|Oxygen|Salbutamol"
Saline <- "Saline flush"
Steroid <- "Prednisolone"

#test
DRUGS_NEW_TYPES = DRUGS_NEW_NAMES %>% 
  mutate(DRUG_TYPE = case_when(
    Drugname %in% c("Amoxicillin", "Cefotaxime", "Doxycycline", "Nitrofurantoin") ~ "Antibiotic",
    Drugname %in% c("Aspirin", "Enoxaparin", "Heparin") ~ "Anticoagulant",
    Drugname %in% c("Cyclizine") ~ "Antihistamine",
    Drugname %in% c("Diazepam", "Midazolam", "Prochlorperazine") ~ "Psychotropic medication",
    Drugname %in% c("Glucagon", "Glucose", "Hypostop") ~ "Diabetes medication",
    Drugname %in% c("Adrenaline", "Amiodarone", "Atropine", "Clopidogrel", "Glyceryl trinitrate (GTN)", "Tenecteplase", "Ticagrelor") ~ "Cardiovascular medication",
    Drugname %in% c("Gaviscon","Hyoscine Butylbromide", "Metoclopramide (Maxolon)", "Omeprazole","Ondansetron", "Peptac") ~ "Gastrointestinal medication",
    Drugname %in% c("Naloxone (Narcan)") ~ "Opioid_antagonist",
    Drugname %in% c("Ipratropium Bromide (Atrovent)", "Oxygen", "Salbutamol") ~ "Respiratory medication",
    Drugname %in% c("Saline flush") ~ "Saline",
    Drugname %in% c("Prednisolone") ~ "Steroid",
    Drugname %in% c("Calpol",
                    "Chlorphenamine (Piriton)",
                    "Co-codamol",
                    "Codeine Phosphate",
                    "Codipar",
                    "Diclofenac",
                    "Dihydrocodine",
                    "Disprin",
                    "Entonox",
                    "Hydrocortisone",
                    "Ibuprofen",
                    "Morphine",
                    "Naproxen",
                    "Oramorph",
                    "Oxycodone",
                    "Paracetamol",
                    "Penthrox (Methoxyflurane)",
                    "Shortec",
                    "Tramadol") ~ "Painkiller medication",
    TRUE ~ NA_character_
  ))

write.csv(DRUGS_NEW_TYPES, "C:/Users/SST24004/OneDrive - NHS Scotland/Documents/MSc Project\\DRUGS_NEW_TYPES_1406.csv", row.names=FALSE)

################################################################################

#3. NEXT DELETE DUPLICATES FOR DRUGTYPES
#FIRST CONCATENATE IDs with DRUGtypes 

DRUGS_NEW_TYPES$conc <- paste(DRUGS_NEW_TYPES$INCIDENTID,DRUGS_NEW_TYPES$DRUG_TYPE)

#check duplicates
sum(table(DRUGS_NEW_TYPES$conc)-1)

#then delete duplicates
DRUGS_NEW_TYPES = DRUGS_NEW_TYPES[!duplicated(DRUGS_NEW_TYPES$conc), ]

# or
#DRUGS_NEW_TYPES2 = DRUGS_NEW_TYPES %>% distinct(conc, .keep_all = TRUE)

#check duplicates
sum(table(DRUGS_NEW_TYPES$conc)-1)

###### ADD NEW COLUMN filled WITH YES for wide table####

DRUGS_NEW_TYPES$number <- c("Yes")

#remove COLUMN conc & Drugname
DRUGS_NEW_TYPES_wide = subset(DRUGS_NEW_TYPES, select = c(INCIDENTID, DRUG_TYPE, number))

#4. USE pivot_wider()
# DRUGTYPE & ROWINDEX
DRUG_WIDE_CHECK = DRUGS_NEW_TYPES_wide %>%
  pivot_wider(names_from = DRUG_TYPE, values_from = number)

## REPLACE NA WITH NO
DRUG_WIDE_CHECK[is.na(DRUG_WIDE_CHECK)] <- "No"

## REMOVE NA COLUMN
DRUG_WIDE_FINAL = DRUG_WIDE_CHECK %>%
  select(-c("NA"))

write.csv(DRUG_WIDE_FINAL, "C:/Users/SST24004/OneDrive - NHS Scotland/Documents/MSc Project\\DRUG_WIDE_FINAL_1406.csv", row.names=FALSE)

# DRUGTYPE & DRUGNAME

DRUGS_NEW_TYPES_wide2 = subset(DRUGS_NEW_TYPES, select = c(INCIDENTID, DRUG_TYPE,Drugname ))

DRUGS_WIDE_DRUGNAME = DRUGS_NEW_TYPES_wide2 %>%
  pivot_wider(names_from = DRUG_TYPE, values_from = Drugname)

write.csv(DRUGS_WIDE_DRUGNAME, "C:/Users/SST24004/OneDrive - NHS Scotland/Documents/MSc Project\\DRUGS_WIDE_DRUGNAME_1604.csv", row.names=FALSE)


