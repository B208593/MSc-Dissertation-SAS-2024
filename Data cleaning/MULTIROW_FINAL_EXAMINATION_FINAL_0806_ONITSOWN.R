# Load the packages
library(tidyverse)
library(dplyr)
library(strex)

install.packages("strex")


#check duplicates
sum(table(EXAM_NEW$INCIDENTID)-1)

#SELECT CERTAIN COLUMNS
EXAM_NEW = select(EXAM_NEW, INCIDENTID, Exa_Pan_BodyPart, Exa_Pan_PainScore)

###################################################################################
#1. GROUP BODY PARTS
#create new column with BODY PARTS groups (containing LEFT ARM etc)
EXAM_NEW = EXAM_NEW %>%
  mutate(BODY_PART = case_when(
    str_detect(Exa_Pan_BodyPart, pattern = "Abdomen") ~ "ABDOMEN",
    str_detect(Exa_Pan_BodyPart, pattern = "Body") ~ "BODY",
    str_detect(Exa_Pan_BodyPart, pattern = "Head") ~ "HEAD",
    str_detect(Exa_Pan_BodyPart, pattern = "Foot") ~ "FOOT",
    str_detect(Exa_Pan_BodyPart, pattern = "Hand") ~ "HAND",
    str_detect(Exa_Pan_BodyPart, pattern = "Leg") ~ "LEG",
    str_detect(Exa_Pan_BodyPart, pattern = "Neck|neck") ~ "NECK",
    str_detect(Exa_Pan_BodyPart, pattern = "Chest") ~ "CHEST",
    str_detect(Exa_Pan_BodyPart, pattern = "Groin") ~ "GROIN",
    str_detect(Exa_Pan_BodyPart, pattern = "Buttock") ~ "BUTTOCK",
    str_detect_all(Exa_Pan_BodyPart, c("Left", "Arm"))~ "LEFT_ARM",
    str_detect_all(Exa_Pan_BodyPart, c("Right", "Arm"))~ "RIGHT_ARM",
    TRUE ~ Exa_Pan_BodyPart
  ))

#DETECT 2 OR MORE WORDS IN THE SAME STRING str_detect_all
#EXAM_NEW_PART_test = EXAM_NEW %>%
#  mutate(BODY_PART3 = case_when(
#str_detect_all(Exa_Pan_BodyPart, c("Left", "Arm"))~ "LEFT_ARM",
#str_detect_all(Exa_Pan_BodyPart, c("Right", "Arm"))~ "RIGHT_ARM",
#TRUE ~ Exa_Pan_BodyPart))


############### CREATE PAIN CATEGORIES

EXAM_NEW <- EXAM_NEW %>%
  mutate(PAIN_CATEGORY = case_when(Exa_Pan_PainScore < 1 ~ 'NO_PAIN',
                                  Exa_Pan_PainScore >= 1  & Exa_Pan_PainScore <= 3 ~ '1-3',
                                  Exa_Pan_PainScore > 3  & Exa_Pan_PainScore <=6 ~ '4-6',
                                  Exa_Pan_PainScore > 6  ~ '7-10'))%>%
  mutate(PAIN_CATEGORY = as_factor(PAIN_CATEGORY))

#no pain = 0, mild pain. = 1-3, moderate pain = 4-6, severe pain = 7-10

EXAM_NEW <- EXAM_NEW %>%
  mutate(PAIN_LEVEL = case_when(Exa_Pan_PainScore < 1 ~ 'NO_PAIN',
                                   Exa_Pan_PainScore >= 1  & Exa_Pan_PainScore <= 3 ~ 'MILD_PAIN',
                                   Exa_Pan_PainScore > 3  & Exa_Pan_PainScore <=6 ~ 'MODERATE_PAIN',
                                   Exa_Pan_PainScore > 6  ~ 'SEVERE_PAIN'))%>%
  mutate(PAIN_LEVEL = as_factor(PAIN_LEVEL))

#order changed
EXAM_NEW$PAIN_CATEGORY = factor(EXAM_NEW$PAIN_CATEGORY,
                                     levels=c("NO_PAIN", "1-3", "4-6", "7-10"))


EXAM_NEW$PAIN_LEVEL = factor(EXAM_NEW$PAIN_LEVEL,
                                     levels=c("NO_PAIN", "MILD_PAIN", "MODERATE_PAIN", "SEVERE_PAIN"))



### CSV FILE FOR PAIN CATEGORIES
write.csv(EXAM_NEW, "C:/Users/SST24004/OneDrive - NHS Scotland/Documents/MSc Project\\EXAMINATION_FINAL_1506.csv", row.names=FALSE)

glimpse(EXAM_NEW)

################################################################################
#3. NEXT DELETE DUPLICATES
#FIRST CONCATENATE IDs with BODY_PART

EXAM_NEW$BODY_PART_PAIN_LEVEL <- gsub(" ", "", paste(EXAM_NEW$BODY_PART,"_",EXAM_NEW$PAIN_LEVEL))


EXAM_NEW$conc <- paste(EXAM_NEW$INCIDENTID,EXAM_NEW$BODY_PART_PAIN_LEVEL)

#check duplicates
sum(table(EXAM_NEW$conc)-1)

#then delete duplicates
EXAM_NEW = EXAM_NEW[!duplicated(EXAM_NEW$conc), ]

#check duplicates
sum(table(EXAM_NEW$conc)-1)

glimpse(EXAM_NEW)

#select needed COLUMNS
EXAM_NEW2 = subset(EXAM_NEW, select = c(INCIDENTID, BODY_PART_PAIN_LEVEL))

###### ADD NEW COLUMN filled WITH YES ####

EXAM_NEW2$NUMBER <- c("YES")

#################################################################################
#4. USE pivot_wider()
# BODY_PART & Exa_Pan_PainScore
EXAM_WIDE = EXAM_NEW2 %>%
  pivot_wider(names_from = BODY_PART_PAIN_LEVEL, values_from =NUMBER )

# CHECK DUPLICATES
sum(table(EXAM_WIDE$INCIDENTID)-1)

glimpse(EXAM_WIDE)

## REPLACE NA WITH NO
EXAM_WIDE[is.na(EXAM_WIDE)] <- "NO"

write.csv(EXAM_WIDE, "C:/Users/SST24004/OneDrive - NHS Scotland/Documents/MSc Project\\EXAM_WIDE_1506.csv", row.names=FALSE)



