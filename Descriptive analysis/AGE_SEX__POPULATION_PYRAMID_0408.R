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
library(plotrix)

########## Age-sex pyramid
AGE_GENDER_FINAL_1406 <- read_csv("AGE_GENDER_FINAL_1406.csv", guess_max=100000)

glimpse(AGE_GENDER_FINAL_1406)

## FILTER OUT AGES BELOW 4
SAS_DATASET_FINAL2 = SAS_DATASET_FINAL %>%
  filter(AGE >= 18, 
         GENDER != "Other")

unique(AGE_GENDER_FINAL_1406$GENDER)

############### create new age cat
SAS_DATASET_FINAL2 = SAS_DATASET_FINAL2 %>%
  age_groups(AGE, 1:10 * 10)

## create dataset
AGE_SEX_PYRAMID2 = SAS_DATASET_FINAL2 %>% 
  select(AGE_CATEGORY, GENDER) %>%
  group_by(GENDER, AGE_CATEGORY) %>% 
  dplyr::summarise(N=n(), .groups="drop")  %>% 
  mutate(PlotN=ifelse(GENDER=="Female", -N, N)) 

AGE_SEX_PYRAMID2$AGE_CATEGORY = factor(AGE_SEX_PYRAMID2$AGE_CATEGORY,
                                        levels=c("18-35",
                                                 "35-45", 
                                                 "45-60", 
                                                 "60-75",
                                                 "75<"))

AGE_SEX_PYRAMID2$GENDER = factor(AGE_SEX_PYRAMID2$GENDER,
                                       levels=c("Female",
                                                "Male"))

### plot1
AGE_SEX_PYRAMID2 %>% ggplot(aes(fill=GENDER, x=AGE_CATEGORY, y=PlotN)) +
  geom_col(width = 0.5) +
  scale_y_continuous(breaks = c(-10000, -5000, 0, 5000, 10000),
                     labels = c(10000, 5000, 0, 5000, 10000)) +
  #scale_x_continuous(breaks=seq(0,110,10))+
  labs(y="Number of patients", x="Age") +
  scale_fill_brewer(palette = "Paired", name="GENDER")+
  #scale_fill_manual(values = c("#800080", "#0000b3"), name = "GENDER", 
                    #guide = guide_legend(reverse = TRUE)
                    #)+
  #theme(axis.text=element_text(size=12),
  #      axis.title=element_text(size=14,face="bold"))
  theme_grey(base_size = 12)+
  #ggtitle("Distribution of Age and Gender of chest pain patients")+
  coord_flip()+
  geom_text(aes(label = scales::comma(N)),
            size = 4,
            position = position_stack(vjust = 0.5),
            colour = "white")
  
#scale_fill_manual(values = c("#800080", "#0000b3"), name = "GENDER", 
#guide = guide_legend(reverse = TRUE)
#)+
## factors
catcols <- c("GENDER",
             "AGE_CATEGORY")

SAS_DATASET_FINAL <- SAS_DATASET_FINAL %>%
               mutate(across(all_of(catcols), as_factor))

### plot2

## create dataset
AGE_SEX_PYRAMID3 = SAS_DATASET_FINAL %>% 
  select(AGE_CATEGORY, GENDER) %>%
  group_by(GENDER, AGE_CATEGORY) %>% 
  dplyr::summarise(N=n(), .groups="drop")  %>% 
  mutate(PlotN=ifelse(GENDER=="Female", -N, N))

AGE_SEX_PYRAMID3 %>% ggplot() +
  geom_col(aes(fill=GENDER, x=AGE_CATEGORY, y=PlotN)) +
  scale_y_continuous() +
  labs(y="Population", x="Age group") +
  scale_fill_discrete(name="Sex") +
  coord_flip()

