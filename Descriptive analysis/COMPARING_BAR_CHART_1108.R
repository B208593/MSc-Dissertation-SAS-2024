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

library(unhcrthemes)


SAS_DATASET_FINAL <- read_csv("SAS_DATASET_FINAL_0607.csv", guess_max=100000)
DRUGS_NEW_TYPES <- read_csv("DRUGS_NEW_TYPES_1406.csv", guess_max=100000)


## CREATE FACTORS

catcols <- c("Call_Colour",
             "TRANSPORTED_TO_HOSPITAL",
             "SEASON",
             "TIME_OF_DAY",
             "Date_MONTH",
             "Despatch_code_description",
             "Code_match",
             "DESPATCH_CODE_NEW",
             "HEALTH_BOARD_NAME",
             "Health_board_region",
             "Symptoms_CentralChestPain",
             "Symptoms_Back",
             "Symptoms_Arm",
             "Symptoms_Neck",
             "Symptoms_Jaw",
             "Symptoms_Shoulder",
             "Symptoms_UpperAbdomen",
             "Symptoms_None",
             "AssocSymptoms_Sweating",
             "AssocSymptoms_Pallor",
             "AssocSymptoms_Breathlessness",
             "AssocSymptoms_ClammySkin",
             "AssocSymptoms_None",
             "Management_MISTEMI_ECGsent",
             "Management_MISTEMI_DiscussedECG",
             "Management_MISTEMI_DirectTriage",
             "Management_MISTEMI_PreAlert",
             "Management_MISTEMI_PatientThrombolysed",
             "Management_ThromboCheck_Ischaemic",
             "Management_ThromboCheck_GoodHistory",
             "Management_ThromboCheck_ECGtoCCU",
             "Management_ThromboCheck_PreviousStroke6m",
             "Management_ThromboCheck_PreviousStrokeANY",
             "Management_ThromboCheck_CNSTrauma",
             "Management_ThromboCheck_AorticDissection",
             "Management_ThromboCheck_Pregnant",
             "Management_ThromboCheck_LiverDisease",
             "Management_ThromboCheck_PepticUlcer",
             "Management_ThromboCheck_PainFree",
             "Management_ThromboCheck_AnticoagulantTherapy",
             "Management_ThromboCheck_HOSPITAL",
             "CARDIAC_PROBLEM",
             "AGE_CATEGORY",
             "GENDER",
             "SIMD2020_Quintile",
             "Deprivation_category",
             "PMH_HD_Hypertension",
             "PMH_HD_Hypotension",
             "PMH_HD_Angina",
             "PMH_RD_COPD",
             "PMH_RD_Asthma",
             "PMH_ND_Epilepsy",
             "PMH_HD_MI",
             "PMH_HD_PCI",
             "PMH_ND_Stroke",
             "PMH_ND_TIA",
             "PMH_GEN_Overdose",
             "PMH_GEN_SuicideAttempts",
             "PMH_GEN_Diabetes",
             "Cardiovascular_history",
             "Respiratory_history",
             "Cerebrovascular_history",
             "General_medical_history",
             "Mental_health_history",
             "Breathing_YN",
             "SPO2_On_Air_Or_O2",
             "Oxygen_Given",
             "RESP_CAT",
             "SPO2_CAT",
             "Cir_Pulse_Rhythm",
             "Cir_Most_Distal_Pulse",
             "Cir_Cap_Refill",
             "Cir_Central_Peripheral",
             "Cir_Skin_Appearance",
             "Cir_Skin_Texture",
             "Cir_Skin_Temperature",
             "Cir_Blood_Pressure_Arm",
             "Cir_ECG_3_Lead",
             "Cir_ECG_12_Lead",
             "Cir_ECG_Rhythm",
             "Blood_pressure",
             "ECG_rhythm",
             "BLOOD_SUGAR",
             "Character_ConstIntermit",
             "Character_ImprovWorse",
             "Radiates_YesNo",
             "Radiates_RadiatesTo",
             "AssocSymptoms_Nausea",
             "AssocSymptoms_Vomiting",
             "AssocSymptoms_Dizziness",
             "AssocSymptoms_Diarrhoea")

SAS_DATASET_FINAL <- SAS_DATASET_FINAL %>%
  mutate(across(all_of(catcols), as_factor))

### colour palette
NEWPalette2 <- c("#da1c04", "#CC79A7","#ffa500","#56B4E9", "chartreuse3", "#808080")

################# bar plot HOSPITAL - GENDER ######################################
#################################################################
SAS_DATASET_FINAL %>%
  filter(GENDER != "Other"|NA) %>%
  count(TRANSPORTED_TO_HOSPITAL, GENDER)%>%
  ggplot(aes(x=TRANSPORTED_TO_HOSPITAL, y=n, fill=GENDER)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=scales::comma(n)), 
            position=position_dodge(width=0.9), 
            vjust=-0.25,
            size = 3)+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "HOSPITAL",
       y = "Number of individuals") +
  labs(title = "Distribution of GENDER by HOSPITAL")

################# bar plot AGE-GENDER ######################################
#################################################################
SAS_DATASET_FINAL$AGE_CATEGORY = factor(SAS_DATASET_FINAL$AGE_CATEGORY,
                                        levels=c("<18", 
                                                 "18-35",
                                                 "35-45", 
                                                 "45-60", 
                                                 "60-75",
                                                 "75<"))

SAS_DATASET_FINAL %>%
  filter(GENDER != "Other"|NA,
         AGE_CATEGORY != "NA") %>%
  count(AGE_CATEGORY, GENDER)%>%
  ggplot(aes(x=AGE_CATEGORY, y=n, fill=GENDER)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=scales::comma(n)), 
            position=position_dodge(width=0.9), 
            vjust=-0.25,
            size = 3)+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Age categories",
       y = "Number of individuals") +
  labs(title = "Distribution of Gender by Age categories")

################# bar plot PMH ######################################
#################################################################
DATASET_RQ2_long2 <- SAS_DATASET_FINAL %>%
  select(GENDER, AGE_CATEGORY,
         TRANSPORTED_TO_HOSPITAL,
         Cardiovascular_history,
         Respiratory_history,
         Cerebrovascular_history,
         General_medical_history,
         Mental_health_history)


data_long2 <- gather(DATASET_RQ2_long2, PMH, measurement, Cardiovascular_history:Mental_health_history, factor_key=TRUE)

data_long2 %>%
  filter(measurement != "NA",
         GENDER != "Other"|NA)%>%
  count(PMH, GENDER)%>%
  ggplot(aes(x=PMH, y=n, fill=GENDER)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=scales::comma(n)), 
            position=position_dodge(width=0.9), 
            vjust=-0.25,
            size = 3)+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Past Medical History",
       y = "Number of individuals") +
  labs(title = "Distribution of GENDER by Past Mecial History")+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12))

#################################################################
data_long2 %>%
  filter(measurement != "NA")%>%
  count(PMH, TRANSPORTED_TO_HOSPITAL)%>%
  ggplot(aes(x=PMH, y=n, fill=TRANSPORTED_TO_HOSPITAL)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=scales::comma(n)), 
            position=position_dodge(width=0.9), 
            vjust=-0.25,
            size = 3)+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Past Medical History",
       y = "Number of individuals") +
  labs(title = "Distribution of patients PMH whether transported to hospital")+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12))

#################################################################
SAS_DATASET_FINAL$SEASON = factor(SAS_DATASET_FINAL$SEASON,
                                  levels=c("Spring", 
                                           "Summer",
                                           "Autumn", 
                                           "Winter"))

SAS_DATASET_FINAL$TIME_OF_DAY = factor(SAS_DATASET_FINAL$TIME_OF_DAY,
                                  levels=c("Morning", 
                                           "Afternoon",
                                           "Evening", 
                                           "Night"))


#################################################################
SAS_DATASET_FINAL %>% 
  filter(Blood_pressure!="NA")%>%
  count(TRANSPORTED_TO_HOSPITAL, Blood_pressure)%>%
  ggplot(aes(x=Blood_pressure, y=n, fill=TRANSPORTED_TO_HOSPITAL)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=scales::comma(n)), 
            position=position_dodge(width=0.9), 
            vjust=-0.25,
            size = 3)+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Blood pressure categories",
       y = "Number of individuals") +
  labs(title = "Blood pressure of individuals transported to hospital")+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x=element_text(size=10),
        axis.text.y = element_text(size=12))

#################################################################
### BP & AGE CAT

SAS_DATASET_FINAL$AGE_CATEGORY = factor(SAS_DATASET_FINAL$AGE_CATEGORY,
                                        levels=c("<18", 
                                                 "18-35",
                                                 "35-45", 
                                                 "45-60", 
                                                 "60-75",
                                                 "75<"))

SAS_DATASET_FINAL$Blood_pressure = factor(SAS_DATASET_FINAL$Blood_pressure,
                                          levels=c("Low",
                                                   "Normal", 
                                                   "Elevated",
                                                   "Hypertension_ST1", 
                                                   "Hypertension_ST2",
                                                   "Hypertensive_Crisis"
                                                   ))

age_colour = c("#ded3ed",
               "#bea9dc",
               "#9e7fcc",
               "#623d98",
               "#472c6e",
               "#231636")

age_colour2 = c("#d8ffd8",
                "#b1ffb1",
                "#00ff00",
                "#00b100",
                "#008900",
                "#001400")



SAS_DATASET_FINAL$AGE_CATEGORY = factor(SAS_DATASET_FINAL$AGE_CATEGORY,
                                        levels=c("<18", 
                                                 "18-35",
                                                 "35-45", 
                                                 "45-60", 
                                                 "60-75",
                                                 "75<"))

SAS_DATASET_FINAL %>% 
  filter(Blood_pressure!="NA",
         AGE_CATEGORY!="NA")%>%
  count(AGE_CATEGORY, Blood_pressure)%>%
  ggplot(aes(x=Blood_pressure, y=n, fill=AGE_CATEGORY)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=scales::comma(n)), 
            position=position_dodge(width=0.9), 
            vjust=-0.25,
            size = 3)+
  scale_fill_manual(values=age_colour2)+
  #scale_fill_manual(values=blue_colour)+
  #scale_fill_manual(values=blue_colour)+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Blood pressure categories",
       y = "Number of patients") +
  #labs(title = "Blood pressure by age")+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.text.y = element_text(size=14))

#################################################################
### BP & SIMD

simd_colour <- c("paleturquoise",
               "skyblue",
               "deepskyblue",
               "deepskyblue3",
               "deepskyblue4",
               "#153E7E")

blue_colour = c("#00fefe",
                "#00eaea",
                "#00d6d6",
                "#00c3c3",
                "#00afaf",
                "#009b9b",
                "#008888",
                "#007474",
                "#006161",
                "#004d4d",
                "#003939",
                "#002626",
                "#001212",)

SAS_DATASET_FINAL %>% 
  filter(Blood_pressure!="NA",
         SIMD2020_Quintile!="NA")%>%
  count(SIMD2020_Quintile, Blood_pressure)%>%
  ggplot(aes(x=Blood_pressure, y=n, fill=SIMD2020_Quintile)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=scales::comma(n)), 
            position=position_dodge(width=0.9), 
            vjust=-0.25,
            size = 3)+
  scale_fill_manual(values=simd_colour)+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Blood pressure categories",
       y = "Number of calls") +
  labs(title = "Blood pressure by SIMD")+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x=element_text(size=10),
        axis.text.y = element_text(size=12))

#################################################################
SAS_DATASET_FINAL %>% 
  filter(Blood_pressure!="NA")%>%
  count(TRANSPORTED_TO_HOSPITAL,)%>%
  ggplot(aes(x=Blood_pressure, y=n, fill=TRANSPORTED_TO_HOSPITAL)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=scales::comma(n)), 
            position=position_dodge(width=0.9), 
            vjust=-0.25,
            size = 3)+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Blood pressure categories",
       y = "Number of individuals") +
  labs(title = "Blood pressure of individuals transported to hospital")+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x=element_text(size=10),
        axis.text.y = element_text(size=12))

#################################################################

########################AGE & HOSPITAL #########################################
SAS_DATASET_FINAL$AGE_CATEGORY = factor(SAS_DATASET_FINAL$AGE_CATEGORY,
                                        levels=c("<18", 
                                                 "18-35",
                                                 "35-45", 
                                                 "45-60", 
                                                 "60-75",
                                                 "75<"))

SAS_DATASET_FINAL %>% 
  select(AGE_CATEGORY, TRANSPORTED_TO_HOSPITAL)%>%
  filter(AGE_CATEGORY !="NA")%>%
  count(AGE_CATEGORY, TRANSPORTED_TO_HOSPITAL,)%>%
  ggplot(aes(x=AGE_CATEGORY, y=n, fill=TRANSPORTED_TO_HOSPITAL)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=scales::comma(n)), 
            position=position_dodge(width=0.9), 
            vjust=-0.25,
            size = 3)+
  scale_fill_brewer(palette="Paired", name = "Hospitalized")+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Age categories",
       y = "Number of calls") +
  labs(title = "Patients transported to hospital")+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x=element_text(size=10),
        axis.text.y = element_text(size=12))

#################################################################
SAS_DATASET_FINAL %>% 
  count(TRANSPORTED_TO_HOSPITAL, TIME_OF_DAY)%>%
  ggplot(aes(x=TRANSPORTED_TO_HOSPITAL, y=n, fill=TIME_OF_DAY)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=scales::comma(n)), 
            position=position_dodge(width=0.9), 
            vjust=-0.25,
            size = 3)+
  scale_fill_brewer(palette="YlGnBu")+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Transported to hospital",
       y = "Number of individuals") +
  labs(title = "Distribution of number of individuals transported to hospital by time of day")+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12))

#################################################################
NEWPalette2 <- c("#da1c04",
                 "#CC79A7",
                 "#ffa500",
                 "#56B4E9",
                 "chartreuse3",
                 "#808080")

NEWPalette_colour <- c("chartreuse3",
                       "yellow",
                       "#ffbf00",
                       "#da1c04",
                       "purple")

SAS_DATASET_FINAL %>% 
  filter(Call_Colour != "NA")%>%
  count(Call_Colour, SEASON)%>%
  ggplot(aes(x=SEASON, y=n, fill=Call_Colour)) + 
  geom_bar(position = 'dodge', colour="black",stat='identity') +
  geom_text(aes(label=scales::comma(n)), 
            position=position_dodge(width=0.9), 
            vjust=-0.25,
            size = 3)+
  scale_fill_manual(values=NEWPalette_colour, name ="Call colour" )+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Season",
       y = "Number of CALLS") +
  labs(title = "Call colour by season")+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12))


#################################################################
SAS_DATASET_FINAL %>% 
  filter(Call_Colour != "NA")%>%
  count(Call_Colour, TIME_OF_DAY)%>%
  ggplot(aes(x=TIME_OF_DAY, y=n, fill=Call_Colour)) + 
  geom_bar(position = 'dodge', colour="black",stat='identity') +
  geom_text(aes(label=scales::comma(n)), 
            position=position_dodge(width=0.9), 
            vjust=-0.25,
            size = 3)+
  scale_fill_manual(values=NEWPalette_colour, name ="Call colour" )+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "TIME_OF_DAY",
       y = "Number of CALLS") +
  labs(title = "Call colour by TIME_OF_DAY")+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12))


#################################################################
SAS_DATASET_FINAL %>% 
  filter(GENDER != "Other")%>%
  count(GENDER, SEASON)%>%
  ggplot(aes(x=SEASON, y=n, fill=GENDER)) + 
  geom_bar(position = 'dodge', colour="black",stat='identity') +
  geom_text(aes(label=scales::comma(n)), 
            position=position_dodge(width=0.9), 
            vjust=-0.25,
            size = 3)+
  scale_fill_manual(values=NEWPalette_colour, name ="Call colour" )+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "TIME_OF_DAY",
       y = "Number of individuals") +
  labs(title = "Call by gender")+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12))

############################### BAR PLOT symptoms by despatch code ######################################
###############################################################################

SAS_DATASET_FINAL %>% 
  filter(DESPATCH_CODE_NEW == "Clinical triage")%>%
  count(DESPATCH_CODE_NEW, AGE_CATEGORY)%>%
  ggplot(aes(x=DESPATCH_CODE_NEW, y=n, fill=AGE_CATEGORY)) + 
  geom_bar(position = 'dodge', colour="black",stat='identity') +
  geom_text(aes(label=scales::comma(n)), 
            position=position_dodge(width=0.9), 
            vjust=-0.25,
            size = 3)+
  scale_fill_manual(values=NEWPalette_colour, name ="Call colour" )+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "TIME_OF_DAY",
       y = "Number of individuals") +
  labs(title = "Call by gender")+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12))

#################################################################
SAS_DATASET_FINAL %>% 
  filter(ECG_rhythm != "Sinus Rhythm",
         ECG_rhythm!="No")%>%
  count(TRANSPORTED_TO_HOSPITAL, ECG_rhythm)%>%
  filter(n > 10)%>%
  #ggplot(aes(x=ECG_rhythm, y=n, fill=TRANSPORTED_TO_HOSPITAL)) + 
  ggplot(aes(x = reorder(ECG_rhythm, n), n, fill=TRANSPORTED_TO_HOSPITAL))+
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=scales::comma(n)), 
            position=position_dodge(width=0.9), 
            vjust=-0.25,
            size = 3)+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "ECG",
       y = "Number of individuals") +
  labs(title = "ECG vs transported to hospital")+
  coord_flip()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12))
#################################################################

SAS_DATASET_FINAL %>% 
  filter(ECG_rhythm != "Sinus Rhythm",
         ECG_rhythm!="No")%>%
  count(TRANSPORTED_TO_HOSPITAL, ECG_rhythm)%>%
  mutate(pct = prop.table(n)) %>% 
  filter(n > 10)%>%
  #ggplot(aes(x=ECG_rhythm, y=n, fill=TRANSPORTED_TO_HOSPITAL)) + 
  ggplot(aes(x = reorder(ECG_rhythm, pct), pct, fill=TRANSPORTED_TO_HOSPITAL))+
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=scales::percent(pct, accuracy=0.1)), 
            position=position_dodge(width=0.9), 
            vjust=0.25,
            hjust=-0.05,
            size = 3)+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "ECG",
       y = "Number of individuals") +
  labs(title = "ECG vs transported to hospital")+
  coord_flip()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12))

#################################################################
SAS_DATASET_FINAL$Blood_pressure = factor(SAS_DATASET_FINAL$Blood_pressure,
                                  levels=c("Low",
                                           "Normal", 
                                           "Elevated",
                                           "Hypertension_ST1", 
                                           "Hypertension_ST2",
                                           "Hypertensive_Crisis",
                                           "NA"))

SAS_DATASET_FINAL$TRANSPORTED_TO_HOSPITAL = factor(SAS_DATASET_FINAL$TRANSPORTED_TO_HOSPITAL,
                                                   levels=c("No", 
                                                            "Yes"))

SAS_DATASET_FINAL %>% 
  #filter(ECG_rhythm != "Sinus Rhythm")%>%
  count(TRANSPORTED_TO_HOSPITAL, Blood_pressure)%>%
  filter(n > 10)%>%
  #ggplot(aes(x=Blood_pressure, y=n, fill=TRANSPORTED_TO_HOSPITAL)) + 
  ggplot(aes(x = Blood_pressure, n, fill=TRANSPORTED_TO_HOSPITAL))+
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=scales::comma(n)), 
            position=position_dodge(width=0.9), 
            vjust=-0.25,
            size = 3)+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Blood_pressure",
       y = "Number of individuals") +
  labs(title = "Blood_pressure vs transported to hospital")+
  coord_flip()+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12))

#################################################################

SAS_DATASET_FINAL %>% 
  filter(Management_ThromboCheck_ECGtoCCU != "NA",
         Code_match != "NA") %>%
  count(Code_match, Management_ThromboCheck_ECGtoCCU)%>%
  ggplot(aes(x=Management_ThromboCheck_ECGtoCCU, y=n, fill=Code_match)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=scales::comma(n)), 
            position=position_dodge(width=0.9), 
            vjust=-0.25,
            size = 3)+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "ECG Heart Problem",
       y = "Number of individuals") +
  labs(title = "Heart problem despatch code difference")

#################################################################

SAS_DATASET_FINAL %>% 
  count(Code_match, SEASON)%>%
  ggplot(aes(x=SEASON, y=n, fill=Code_match)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=scales::comma(n)), 
            position=position_dodge(width=0.9), 
            vjust=-0.25,
            size = 3)+
  scale_fill_brewer(palette="YlGnBu")+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "HOSPITAL",
       y = "Number of individuals") +
  labs(title = "Distribution of GENDER by HOSPITAL")

#################################################################
#################################################################
##   drugtable ##########################

SAS_DRUGTABLE <- SAS_DATASET_FINAL %>%
  left_join(DRUG_WIDE_FINAL_1406, by='INCIDENTID')

SAS_DRUGTABLE = SAS_DRUGTABLE %>% 
  rename(Cardiovascular_medication = "Cardiovascular medication",
         Painkiller_medication = "Painkiller medication",
         Respiratory_medication = "Respiratory medication",
         Gastrointestinal_medication = "Gastrointestinal medication",
         Psychotropic_medication = "Psychotropic medication")

SAS_DRUGTABLE_new = SAS_DRUGTABLE%>%
  select(TRANSPORTED_TO_HOSPITAL,
    Cardiovascular_medication,
         Painkiller_medication,
         Respiratory_medication,
         Gastrointestinal_medication,
         Psychotropic_medication)

SAS_DRUGTABLE_new_long <- SAS_DRUGTABLE_new %>%
  make_long(TRANSPORTED_TO_HOSPITAL,
            Cardiovascular_medication,
            Painkiller_medication,
            Respiratory_medication,
            Gastrointestinal_medication,
            Psychotropic_medication)

SAS_DRUGTABLE_new_long <- gather(SAS_DRUGTABLE_new, condition, measurement, 
                    Cardiovascular_medication:Psychotropic_medication, 
                    factor_key=TRUE)

SAS_DRUGTABLE_transport2 = SAS_DRUGTABLE_new_long %>%
  filter(measurement == "Yes")%>%
  group_by(condition, TRANSPORTED_TO_HOSPITAL)%>%
  count(condition, TRANSPORTED_TO_HOSPITAL)

SAS_DRUGTABLE_new_long$TRANSPORTED_TO_HOSPITAL = factor(SAS_DRUGTABLE_new_long$TRANSPORTED_TO_HOSPITAL,
                                                   levels=c("Yes", 
                                                            "No"))
SAS_DRUGTABLE_new_long %>% 
  filter(measurement == "Yes")%>%
  group_by(condition,TRANSPORTED_TO_HOSPITAL)%>%
  count(condition,TRANSPORTED_TO_HOSPITAL)%>%
  ggplot(aes(x=condition, y=n, fill=TRANSPORTED_TO_HOSPITAL)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=scales::comma(n)), 
            position=position_dodge(width=0.9), 
            vjust=-0.25,
            size = 3)+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Pre-hospital medication",
       y = "Number of medication") +
  labs(title = "Distribution of medication by HOSPITAL")

##################################################################
#### create long table

DATASET_long <- SAS_DATASET_FINAL %>%
  select(GENDER, AGE_CATEGORY,
         TRANSPORTED_TO_HOSPITAL,
         AssocSymptoms_Sweating,
         AssocSymptoms_Pallor,
         AssocSymptoms_Breathlessness,
         AssocSymptoms_ClammySkin)

DATASET_long2 <- SAS_DATASET_FINAL %>%
  select(GENDER, AGE_CATEGORY,
         TRANSPORTED_TO_HOSPITAL,
         Symptoms_CentralChestPain,
         Symptoms_Back,
         Symptoms_Arm,
         Symptoms_Neck,
         Symptoms_Jaw,
         Symptoms_Shoulder,
         Symptoms_UpperAbdomen,
         Symptoms_None)


data_long <- gather(DATASET_long, PMH, measurement, AssocSymptoms_Sweating:AssocSymptoms_ClammySkin, factor_key=TRUE)
data_long2 <- gather(DATASET_long2, PMH, measurement, Symptoms_CentralChestPain:Symptoms_None, factor_key=TRUE)

##################################################################
hospital_colour <- c("#008080",
                 "#800000")

data_long$TRANSPORTED_TO_HOSPITAL = factor(data_long$TRANSPORTED_TO_HOSPITAL,
                                           levels=c("Yes", 
                                                    "No"))

data_long %>% 
  filter(measurement == "Yes")%>%
  group_by(PMH,TRANSPORTED_TO_HOSPITAL)%>%
  count(PMH,TRANSPORTED_TO_HOSPITAL)%>%
  ggplot(aes(x=PMH, y=n, fill=TRANSPORTED_TO_HOSPITAL)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=scales::comma(n)), 
            position=position_dodge(width=0.9), 
            vjust=-0.25,
            size = 3)+
  scale_fill_manual(values=PMH_colour2, name ="Hospitalization" )+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Associated symptoms",
       y = "Number of patients") +
  labs(title = "Associated symptoms vs transported to hospital")

data_long %>% 
  filter(measurement == "Yes",
         GENDER != "Other")%>%
  group_by(PMH,GENDER)%>%
  count(PMH,GENDER)%>%
  ggplot(aes(x=PMH, y=n, fill=GENDER)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=scales::comma(n)), 
            position=position_dodge(width=0.9), 
            vjust=-0.25,
            size = 3)+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Associated symptoms",
       y = "Number of patients") +
  labs(title = "Associated symptoms vs GENDER")

##################################################################
data_long2 %>% 
  filter(measurement == "Yes")%>%
  group_by(PMH,TRANSPORTED_TO_HOSPITAL)%>%
  count(PMH,TRANSPORTED_TO_HOSPITAL)%>%
  ggplot(aes(x=PMH, y=n, fill=TRANSPORTED_TO_HOSPITAL)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=scales::comma(n)), 
            position=position_dodge(width=0.9), 
            vjust=-0.25,
            size = 3)+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Symptoms",
       y = "Number of patients") +
  labs(title = "Symptoms vs transported to hospital")

data_long2 %>% 
  filter(measurement == "Yes",
         GENDER != "Other")%>%
  group_by(PMH,GENDER)%>%
  count(PMH,GENDER)%>%
  ggplot(aes(x=PMH, y=n, fill=GENDER)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=scales::comma(n)), 
            position=position_dodge(width=0.9), 
            vjust=-0.25,
            size = 3)+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Symptoms",
       y = "Number of patients") +
  labs(title = "Symptoms vs gender")
