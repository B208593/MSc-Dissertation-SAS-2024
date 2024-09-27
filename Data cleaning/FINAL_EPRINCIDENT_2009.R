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
library(ggsankey)
library(naniar)
library(stringr)  


## LOAD FILE
despatch_codes <- read_csv("despatch_codes.csv", guess_max=100000)
MPDS_CODES <- read_csv("MPDS_CODES_1609.csv", guess_max=100000)

# CREATE SUBSET

EPRINCIDENT_FINAL = subset(EPRINCIDENT_ALL, 
                         select = c(INCIDENTID,
                                    Call_Started_Datetime,
                                    Incident_Postcode,
                                    C3_Call_Colour,
                                    Despatch_code,
                                    CLR_Diagnostic_Code,
                                    Receiving_Hospital,
                                    Conveyed_YN))



#################################### CREATE NEW VARIABLES
### 1. DATETIME, season

#TRANSFROM CHARACTER INTO DATETIME FORMAT
EPRINCIDENT_FINAL$DATETIME = dmy_hms(EPRINCIDENT_FINAL$Call_Started_Datetime)

#CREATE SEASON VARIABLE
EPRINCIDENT_FINAL = EPRINCIDENT_FINAL %>% 
  mutate(
    SEASON = case_when(
      month(DATETIME) %in% c(12, 1, 2) ~ 'Winter',
      month(DATETIME) %in% 3:5 ~ 'Spring',
      month(DATETIME) %in% 6:8 ~ 'Summer',
      month(DATETIME) %in% 9:11 ~ 'Autumn'))

#CHECK NEW VALUES
unique(EPRINCIDENT_FINAL$SEASON)

#CREATE DAYTIME VARIABLE
EPRINCIDENT_FINAL = EPRINCIDENT_FINAL %>%
  mutate(
    TIME_OF_DAY = case_when(
      hour(DATETIME) >= 6 & hour(DATETIME) < 12 ~ 'Morning', 
      hour(DATETIME) >= 12 & hour(DATETIME) < 18 ~ 'Afternoon', 
      hour(DATETIME) >= 18 & hour(DATETIME) <= 23  ~ 'Evening',
      hour(DATETIME) >= 0 & hour(DATETIME) < 6  ~ 'Night'))

#CHECK NEW VALUES
unique(EPRINCIDENT_FINAL$TIME_OF_DAY)
unique(EPRINCIDENT_FINAL$SEASON)


#### 2. Create new variable for MONTHS FOR LINE PLOT

## SEPARATE DATE AND TIME

EPRINCIDENT_FINAL$Date_MONTH <- format(as.POSIXct(EPRINCIDENT_FINAL$DATETIME,format="%Y-%m-%d %H:%M:%S"),"%Y-%m")

unique(EPRINCIDENT_FINAL$Date_MONTH)

############################################ DESPATCH CODE

#CREATE NEW COLUMN FROM DISPATCH CODES WITH ONLY THE FIRST 2 CHARACTERS

###### CORRECT DESPTACH CODES (MISSPELLING)
### DESPATCH CODE NEW

EPRINCIDENT_FINAL = EPRINCIDENT_FINAL %>%
  mutate(Despatch_code = if_else(Despatch_code == "APY", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "AMBER", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "APA", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "APCARY", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "APP", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "APR", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "APUTT", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "APY", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "CDIS", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "CLINICAL1", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "CLINICAL2", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "CSDA", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "CSDG-SP", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "CSDP", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "CSDR", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "CSDY", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "CSDY-SP", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "CSDY-TA", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "DETA", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "DETY - TA", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "ICHA", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "ICHR", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "ICHY", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "NA", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "SSD2", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "SSD4", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "SSD9", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "SSDA", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "SSDR", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "SSDY", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "TDR", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "UTAA", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "UTAR", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "UTAY", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "YELLOW", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "PURPLE", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "RED", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "GREEN", "OTHER", Despatch_code),
         Despatch_code = if_else(Despatch_code == "33C04.", "33C04", Despatch_code),
         Despatch_code = if_else(Despatch_code == "09B01A", "09B01a", Despatch_code),
         Despatch_code = if_else(Despatch_code == "29D02P", "29D02p", Despatch_code))





################# MERGE WITH LONG DESPATCH CODE TABLE

#RENAME CODE TO DISPATCH_CODE
#In tidyverse, use rename(New_Name = Old_Name):
despatch_codes <- despatch_codes %>% 
  rename(Despatch_code = "Desp Code")

MPDS_CODES <- MPDS_CODES %>% 
  rename(DESPATCH_CODE_SHORT = "CODE")

## CREATE NEW COLUMN FROM DISPATCH CODES WITH ONLY THE FIRST 2 CHARACTERS
EPRINCIDENT_FINAL$DESPATCH_CODE_SHORT<- 
  substr(EPRINCIDENT_FINAL$Despatch_code, 1, 2)

## CREATE NEW COLUMN FROM short DISPATCH CODES WITH the last CHARACTER (letter)
EPRINCIDENT_FINAL$DESPATCH_CODE_letter<- 
  str_sub(EPRINCIDENT_FINAL$Despatch_code, 3, 3)


## SUBSTRACT FIRST 2 CHARACTERS
EPRINCIDENT_FINAL$DIAGNOSTIC_CODE_SHORT<- 
  substr(EPRINCIDENT_FINAL$CLR_Diagnostic_Code, 1, 2)

##typeof(MPDS_CODES$DESPATCH_CODE_SHORT)

##MPDS$DESPATCH_CODE_SHORT<-as.character(MPDS$DESPATCH_CODE_SHORT)

#MERGE THE 2 TABLES
EPRINCIDENT_FINAL <- left_join(EPRINCIDENT_FINAL, 
                               despatch_codes, 
                               by = "Despatch_code")


EPRINCIDENT_FINAL <- left_join(EPRINCIDENT_FINAL, 
                               MPDS_CODES, 
                               by = "DESPATCH_CODE_SHORT")


################## MAKE COMPARISON

## COMPARE DESPATCH AND DIAGNOSTIC CODES
EPRINCIDENT_FINAL$Code_match = ifelse(
  EPRINCIDENT_FINAL$DESPATCH_CODE_SHORT == EPRINCIDENT_FINAL$DIAGNOSTIC_CODE_SHORT, 
  "TRUE", 
  "FALSE")


## rename
EPRINCIDENT_FINAL<- EPRINCIDENT_FINAL  %>%
  mutate(MEANING = if_else((DESPATCH_CODE_SHORT== "OT"),
                           "Clinical triage",MEANING))

EPRINCIDENT_FINAL<- EPRINCIDENT_FINAL  %>%
  mutate(MEANING = if_else((DESPATCH_CODE_SHORT== "45"),
                           "Medically unwell patients",MEANING))

EPRINCIDENT_FINAL<- EPRINCIDENT_FINAL  %>%
  mutate(MEANING = if_else((DESPATCH_CODE_SHORT== "46"),
                           "Medically unwell patients",MEANING))

EPRINCIDENT_FINAL %>% 
  select(MEANING)%>%
  tbl_summary(sort = list(everything() ~ "frequency"),
              digits = list(all_categorical() ~ c(0, 2)))


########################## COLOUR CODING

# RENAME COLOUR CODES
EPRINCIDENT_FINAL <- EPRINCIDENT_FINAL %>%
  mutate(C3_Call_Colour_new = recode(C3_Call_Colour, "Lime" = 'Green'))

library(naniar)

EPRINCIDENT_FINAL <- EPRINCIDENT_FINAL %>%
  replace_with_na(replace = list(C3_Call_Colour_new ="Unknown"))

unique(EPRINCIDENT_FINAL$C3_Call_Colour_new)
########################## create new TRANSPORT TO HOSPITAL VARIABLE

EPRINCIDENT_FINAL<- EPRINCIDENT_FINAL  %>%
  mutate(NEW_TRANSPORT_TO_HOSPITAL = if_else(is.na(Receiving_Hospital), "No",  "Yes"))
          
         
##################### HOSPITAL HEALTH BOARD

# LOAD HB TABLE

HEALTH_BOARDS <- read_csv("HEALTH_BOARDS.csv", guess_max=100000)


# MERGE MAIN TABLE WITH HEALTHBOARD
EPRINCIDENT_FINAL <- EPRINCIDENT_FINAL %>%
  left_join(HEALTH_BOARDS, by='Receiving_Hospital')

glimpse(EPRINCIDENT_FINAL)


write.csv(EPRINCIDENT_FINAL, "C:/Users/SST24004/OneDrive - NHS Scotland/Documents/MSc Project\\EPRINCIDENT_FINAL_2009.csv", row.names=FALSE)


