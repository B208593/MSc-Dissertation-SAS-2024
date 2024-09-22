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

######### RQ1 - what do call handlers think is wrong with patient at the point of call?

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


######################################################################
############################# BASIC 1 ###################################
SAS_DATASET_FINAL %>%
  filter(TRANSPORTED_TO_HOSPITAL == "Yes")%>%
  count(HEALTH_BOARD_NAME, SIMD2020_Quintile) %>%       
  group_by(HEALTH_BOARD_NAME) %>%
  mutate(percentage = round(n / sum(n) * 100), digits = 1)%>%
  ggplot(aes(x= reorder(HEALTH_BOARD_NAME, n), y = n,fill = SIMD2020_Quintile))+ 
  geom_bar(stat="identity", position ="fill",color="black")+
  geom_text(aes(label=paste0(sprintf("%1.1f", percentage),"%")),
            position=position_fill(vjust=0.5), 
            colour="white")+
  theme_minimal()+
  scale_fill_manual(values=NEWPalette2, name ="Deprivation zone" )+
  scale_y_continuous(labels = scales::label_percent())+
  labs(title ="Patients transported to hospital")+
  labs(subtitle =" by healthboard")+
  labs(caption ="SAS DATASET")+
  labs(x ="Health Board", y = "Percentage") +
  coord_flip()

######################################################################
############################# SIMD & AGE ###################################

text_colour <- c("white",
                 "white",
                 "white",
                 "white",
                 "black")

BLUEPALETTE = c("lightsteelblue",
                "deepskyblue",
                "royalblue",
                "steelblue",
                "#153E7E")


SAS_DATASET_FINAL %>%
  filter(AGE_CATEGORY!= "NA",
         AGE_CATEGORY!= "<18",
         SIMD2020_Quintile!= "NA")%>% 
  count(SIMD2020_Quintile, AGE_CATEGORY) %>% 
  group_by(SIMD2020_Quintile) %>%
  mutate(percentage = round(n / sum(n) * 100), digits = 1)%>%
  ggplot(aes(x= reorder(SIMD2020_Quintile, n), y = n,fill = AGE_CATEGORY))+ 
  geom_bar(stat="identity", position ="fill",color="black")+
  geom_text(aes(label=paste(percentage,"%")),
            position=position_fill(vjust=0.5), 
            colour="white")+
  theme_minimal()+
  #scale_fill_manual(values=NEWPalette2, name ="Age categories" )+
  #scale_fill_brewer(palette="Blues")+
  scale_fill_manual(values=BLUEPALETTE, name ="Age categories" )+
  scale_y_continuous(labels = scales::label_percent())+
  labs(title ="SIMD by Age")+
  #labs(subtitle =" by healthboard")+
  labs(caption ="SAS DATASET")+
  labs(x ="SIMD", y = "Percentage") +
  coord_flip()

geom_text(
  aes(label=paste(value,"%")), size=5,
  color=ifelse(df$category=="A", 'white', 'black'),
  position=position_stack(vjust=0.5))


############## colours###############
#scale_fill_manual(values = c("steelbluehttp://127.0.0.1:18413/graphics/8c537147-7d38-4c95-aa4e-1c5a38767a81.png", 
                            # "#C0C0C0", "#98AFC7", "#6698FF", "#153E7E"))+
 scale_fill_manual(values = c("steelblue",
                              "royalblue",
                              "deepskyblue",
                              "paleturquoise",
                              "cyan3",
                              "cyan4",
                              "aquamarine3",
                              "darkcyan",
                              "darkblue",
                              "paleturquoise3",
                              "skyblue1",
                              "skyblue3",
                              "deepskyblue2",
                              "deepskyblue3",
                              "deepskyblue4", 
                              "cadetblue3", 
                              "dodgerblue", 
                              "#153E7E",
                              "lightsteelblue"))+
  

  
######################################################################
############################# BASIC 2 ###################################
SAS_DATASET_FINAL %>%
  filter(TRANSPORTED_TO_HOSPITAL == "Yes")%>%
  count(HEALTH_BOARD_NAME, SIMD2020_Quintile) %>%       
  group_by(HEALTH_BOARD_NAME)%>%
  ggplot(aes(x = reorder(HEALTH_BOARD_NAME, n), y = n,
             fill = SIMD2020_Quintile, label = n)) +
  geom_bar(stat = "identity",color="black") + 
  geom_text(aes(label = scales::comma(n)),
    size = 3, position = position_stack(vjust = 0.5),colour = "white")+
  scale_fill_manual(values=NEWPalette2, name ="Deprivation zone" )+
  scale_y_continuous(labels = scales::comma)+
  coord_flip()  +
  labs(title = "Patients transported to hospital", 
       x= "HEALTH BOARD NAME", 
       y= "Number of cases") +
  theme(plot.title = element_text(hjust = 0.5))


######################################################################
############################# BASIC 3 ###################################
### using 1,000 separator
SAS_DATASET_FINAL  %>%
  filter(TRANSPORTED_TO_HOSPITAL == "Yes")%>%
  count(HEALTH_BOARD_NAME) %>%
  ggplot(aes(x = reorder(HEALTH_BOARD_NAME, n), n)) + 
  geom_bar(stat = "identity", color="black",fill='steelblue') +
  geom_text(aes(label = scales::comma(n), hjust = -0.05))+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "HEALTH_BOARD",
       y = "Number of individuals") +
  theme_classic(base_size = 14)+
  labs(title = "HEALTH_BOARD")+
  coord_flip()

##############################################################################
##############################################################################
############################# REGIONS 1 ######################################

SAS_DATASET_FINAL <- SAS_DATASET_FINAL %>%
  mutate(Health_board_region = if_else(is.na(Health_board_region), "Unknown", Health_board_region))

SAS_DATASET_FINAL %>%
  filter(TRANSPORTED_TO_HOSPITAL == "Yes")%>%
  count(Health_board_region, SIMD2020_Quintile) %>%       
  group_by(Health_board_region) %>%
  mutate(percentage = round(n / sum(n) * 100), digits = 1)%>%
  ggplot(aes(x= reorder(Health_board_region, n), y = n, fill = SIMD2020_Quintile))+ 
  geom_bar(stat="identity", position ="fill",color="black")+
  geom_text(aes(label=paste0(sprintf("%1.1f", percentage),"%")),
            position=position_fill(vjust=0.5), colour="white")+
  theme_minimal()+
  scale_fill_manual(values=NEWPalette2, name ="Deprivation zone" )+
  scale_y_continuous(labels = scales::label_percent())+
  labs(title ="Patients transported to hospital")+
  labs(subtitle =" by healthboard")+
  labs(caption ="SAS DATASET")+
  labs(x ="Division", y = "Percentage") +
  coord_flip()


######################################################################
############################# REGIONS 2 ###################################

SAS_DATASET_FINAL %>%
  filter(TRANSPORTED_TO_HOSPITAL == "Yes")%>%
  count(Health_board_region, SIMD2020_Quintile) %>%       
  group_by(Health_board_region)%>%
  ggplot(aes(x = reorder(Health_board_region, n), y = n,
             fill = SIMD2020_Quintile, label = n)) +
  geom_bar(stat = "identity",color="black") + 
  geom_text( 
    size = 3, position = position_stack(vjust = 0.5),colour = "white")+
  scale_fill_manual(values=NEWPalette2, name ="Deprivation zone" )+
  scale_y_continuous(labels = scales::comma)+
  coord_flip()  +
  labs(title = "Patients transported to hospital", 
       x= "HEALTH BOARD NAME", 
       y= "Number of cases") +
  theme(plot.title = element_text(hjust = 0.5))
