install.packages("vtable")

# Load the packages
library(tidyverse)
library(epitools)
library(finalfit)
library(RColorBrewer)
library(reportROC)
library(knitr)
library(broom)
library(epitools)
library(ggplot2)
library(gtsummary)
library(magrittr)
library(lubridate)
library(stringi)
library(janitor)
library(vtable)

##############################################################################
##############               GENERATE DATASET          #######################

set.seed(100)

#Scottish Index of Multiple Deprivation file downloaded from: 
#https://www.gov.scot/publications/scottish-index-of-multiple-deprivation-2020v2-postcode-look-up/

SAMPLEDATASET = data.frame(CALL_NUMBER = 1:600, 
                     CALL_time=sample(seq(as.POSIXct('2021/01/01'), 
                                          as.POSIXct('2022/01/01'), 
                                          by="15 mins"), 600),
                     DISPATCH_CODE = sample(x = 1:5, 
                                            size = 600, 
                                            replace = TRUE),
                     DIAGNOSTIC_CODE = sample(x = 1:5, 
                                             size = 600, 
                                             replace = TRUE),
                     GENDER = sample(c(rep("Male", 300), 
                                       rep("Female", 300))),
                     AGE= round(runif(n = 600, 
                                      min = 19, 
                                      max = 91)),
                     AGE2 = sample(x = 19:91, 
                                  size = 600, 
                                  replace = TRUE),
                     POSTCODE = sample(x=depr_scot$Postcode, 
                                       size=10, 
                                       replace=TRUE),
                     PULSE = sample(x = 50:150, 
                                    size = 600, 
                                    replace = TRUE),
                     MED_HIST = sample(x= c("Diabetes mellitus", 
                                         "Hypertension",
                                         "Hypercholesterolemia",
                                         "Cerebrovascular disease",
                                         "Myocardial infarction",
                                         "Ischemic heart disease",
                                         "Smoker"),
                                    size = 5,
                                    replace = TRUE),
                     Prior_AMI = sample(x= c("YES", 
                                             "NO"),
                                        size = 8,
                                        replace = TRUE),
                     DRUGS = sample(x= c("Aspirin", 
                                         "Clopidogrel",
                                         "Other P2Y12 Inhibitor",
                                         "Statin",
                                         "ACE inhibitor",
                                         "Oral anticoagulant",
                                         "Beta-blocker"),
                                      size = 12,
                                      replace = TRUE),
                     OUTCOME = sample(x= c("angina pectoris", 
                                           "atrial fibrillation", 
                                           "COPD", 
                                           "psychiatric",
                                           "death"),
                                      size = 25,
                                      replace = TRUE))


#generate age groups(<=29, 30-49, 50-59, 60-69, over 70 years)
SAMPLEDATASET <- SAMPLEDATASET %>%
  mutate(AGE_CAT = case_when(AGE <= 29 ~ '29>',
                                  AGE >= 30  & AGE <= 49 ~ '30-49',
                                  AGE >= 50  & AGE <= 59 ~ '50-59',
                                  AGE >= 60  & AGE <= 70 ~ '60-70',
                                  AGE > 70 ~ '70<'))%>%
  mutate(AGE_CAT = as_factor(AGE_CAT))

SAMPLEDATASET$AGE_CAT = factor(SAMPLEDATASET$AGE_CAT,
                                        levels=c("29>", "30-49", "50-59", "60-70", "70<"))


glimpse(SAMPLEDATASET)

#RENAME COLUMN ON DEPR_SCOT CSV
dp_2 = depr_scot %>% 
  rename(
    POSTCODE = Postcode
  )

#GET DEPRIVATION LEVEL FOR POSTCODE
SAMPLEDATASET_2 <- left_join(SAMPLEDATASET, dp_2, by = "POSTCODE")

head(SAMPLEDATASET_2)

# DEPRIVATION=1 (MOST DEPRIVED)
# DEPRIVATION =5 (LEAST DEPRIVED)
# GENERATE DEPRIVATION ZONE CATEGORIES
SAMPLEDATASET_2 <- SAMPLEDATASET_2 %>%
  mutate(DEP_CAT = case_when(SIMD2020_Decile >=1  & SIMD2020_Decile <= 4 ~ 'MOST_DEP',
                             SIMD2020_Decile >= 5  & SIMD2020_Decile <= 7 ~ 'MEDIUM_DEP',
                             SIMD2020_Decile >= 8  & SIMD2020_Decile <= 10 ~ 'LEAST_DEP'
                             ))%>%
  mutate(DEP_CAT = as_factor(DEP_CAT))

glimpse(SAMPLEDATASET_2)

# CREATE SEASONS & DAY
SAMPLEDATASET_2 = SAMPLEDATASET_2 %>% 
  mutate(
    SEASON = case_when(
      month(CALL_time) %in% c(12, 1, 2) ~ 'Winter',
      month(CALL_time) %in% 3:5 ~ 'Spring',
      month(CALL_time) %in% 6:8 ~ 'Summer',
      month(CALL_time) %in% 9:11 ~ 'Autumn'))

SAMPLEDATASET_2 = SAMPLEDATASET_2 %>%
  mutate(
    Daytime = case_when(
      hour(CALL_time) < 12 ~ 'Morning', 
      hour(CALL_time) < 18 ~ 'Afternoon', 
      hour(CALL_time) >= 18 ~ 'Evening'))


#COMPARE DISPATCH AND DIAGNOSTIC CODES
SAMPLEDATASET_2$CODE = ifelse(SAMPLEDATASET_2$DISPATCH_CODE == SAMPLEDATASET_2$DIAGNOSTIC_CODE, "TRUE", "FALSE")

#write csv file
write.csv(SAMPLEDATASET_2, "C:\\Users\\Kati\\Documents\\DATAFRAME_2303.csv", row.names=FALSE)

glimpse(SAMPLEDATASET_2)


#write csv file
write.csv(SAMPLEDATASET_2, "C:\\Users\\Kati\\Documents\\DATAFRAME_new_2.csv", row.names=FALSE)

##############################################################################
###########                      ANALYSIS                #####################


#CREATING SUBGROUPS FOR ANALYSIS
NEWDATASET = SAMPLEDATASET_2[c("GENDER","DEP_CAT","OUTCOME")]

NEWDATASET = DATAFRAME_2303[c("GENDER","DEP_CAT","OUTCOME")]

#SUMMARY TABLE ORIGINAL
st(DATAFRAME_2303, col.breaks = 4,
   summ = list(
     c('notNA(x)','mean(x)','sd(x^2)','min(x)','max(x)'),
     c('notNA(x)','mean(x)')
   ),
   summ.names = list(
     c('N','Mean','SD of X^2','Min','Max'),
     c('Count','Percent')
   ))

st(NEWDATASET, col.breaks = 4,
        summ = list(
          c('mean(x)','sd(x^2)','min(x)','max(x)')
        ),
        summ.names = list(
          c('N','Mean','SD of X^2','Min','Max'),
          c('Count','Percent')
        ))

############################################################
#MAIN TABLE
#https://cran.r-project.org/web/packages/vtable/vignettes/sumtable.html
CONTTABLE = sumtable(DATAFRAME_2303,
         summ=c('notNA(x)',
                'mean(x)',
                'median(x)',
                'propNA(x)',
                'sd(x^2)',
                'min(x)',
                'max(x)'),
         out = 'return')

png("CONTABLE.png")
p<-tableGrob(CONTTABLE)
grid.arrange(p)


#how many people in each group
table(SAMPLEDATASET_2$AGE_CAT)
str(SAMPLEDATASET_2)
#AGE DISTRIBUTION BY GENDER
table = DATAFRAME_2303 %>%
  group_by(GENDER) %>%
  summarise(mean_age = mean(AGE, na.rm = TRUE),
            median_age = median(AGE, na.rm = TRUE),
            sd_age = sd(AGE, na.rm = TRUE),
            iqr_age = IQR(AGE, na.rm = TRUE))


#CATEGORICAL VS CATEGORICAL
SAMPLEDATASET_2 %>% 
  tabyl(AGE_CAT, GENDER) %>% 
  knitr::kable()

#Contingency table for AGE-GENDER
SAMPLEDATASET_3 <- SAMPLEDATASET_2 %>%
  drop_na(GENDER,AGE_CAT) %>%
  select(GENDER,AGE_CAT)

SAMPLEDATASET_3 %>%
  tbl_cross(row = GENDER, 
            col = AGE_CAT, 
            percent = "row") %>%
  bold_labels()

#Contingency table for DEPR-SEASON
SAMPLEDATASET_4 <- SAMPLEDATASET_2 %>%
  drop_na(DEP_CAT,SEASON) %>%
  select(DEP_CAT,SEASON)

SAMPLEDATASET_4 %>%
  tbl_cross(row = DEP_CAT, 
            col = SEASON, 
            percent = "row") %>%
  bold_labels()


#PLOTS
DATAFRAME_2303 %>%
  ggplot() +
  geom_bar(aes(x = DRUGS, fill = AGE_CAT), position = "dodge")+
  labs(title = "Age group distribution by GIVEN DRUGS",
       x = "DRUGS",
       y = "Number of people")

#FIGURES (X=CONTINOUS VARIABLE, facet_wrap(~CATEGORICAL VARIABLE)
DATAFRAME_2303 %>%
  ggplot(aes(x=SIMD2020_Decile)) +
  geom_histogram(aes(y=..density.., fill=..count..),binwidth=2)+
  geom_density(aes(y=..density..)) +
  facet_wrap(~CODE) +
  theme_classic(base_size = 12)+
  theme(panel.grid=element_blank())+
  labs(title = "Distribution of CODE CORRECTION by DISPATCH CODE")

#CHI SQUARE TEST (if there are NAs it won't work)
contingency_table <- as.data.frame.matrix(table(DATAFRAME_2303$GENDER, DATAFRAME_2303$AGE_CAT))
chisq.test(contingency_table)

glimpse(DATAFRAME_2303)

#LOGISTIC REGRESSION ANALYSIS
##### ALL VARIABLES NEEDS TO BE FACTORS TO WORK  ######
assess_lr <- glm(GENDER~AGE_CAT+
                   OUTCOME+
                   SEASON+
                   DEP_CAT,
                 data=DATAFRAME_2303,family=binomial("logit"))

assess_lr <- glm(GENDER~AGE_CAT,
                 data=DATAFRAME_2303,family=binomial("logit"))

summary(assess_lr)

#CI/T TEST
SAMPLEDATASET_2 %>%
  drop_na(AGE) %>%
  count(GENDER) %>%
  summarise(t_stat=t.test(SAMPLEDATASET_2$AGE~SAMPLEDATASET_2$GENDER)$statistic,
            pval=t.test(SAMPLEDATASET_2$AGE~SAMPLEDATASET_2$GENDER)$p.value,
            lowerCI=t.test(SAMPLEDATASET_2$AGE~SAMPLEDATASET_2$GENDER)$conf.int[1],
            upperCI=t.test(SAMPLEDATASET_2$AGE~SAMPLEDATASET_2$GENDER)$conf.int[2])

##############################################################################
###########                      USEFUL                #######################
# Transform categorical variables into factors
catcols <- c("GENDER","HISTORY","DRUGS")

SAMPLEDATASET_3 <- SAMPLEDATASET_2 %>%
  mutate(across(all_of(catcols), as_factor))

# WHEN VALUES NEED TO BE CHANGED
#Recode the levels of some factors for easier interpretation:
#"new_value" = "old_value"
SAMPLEDATASET_3 <- SAMPLEDATASET_3 %>%
  mutate(GENDER = fct_recode(GENDER,"MALE"="0","FEMALE"="1"),
         HISTORY = fct_recode(HISTORY,"angina pectoris"="0",
                              "atrial fibrillation"="1",
                              "COPD"="3",
                              "psychiatric"="4"),
         DRUGS = fct_recode(DRUGS,"DRUG_A"="1",
                                "DRUG_B"="2",
                                "DRUG_C"="3")
  )


#reduce number of values for xx variable (HERE 2 categories from 5:POOR & GOOD)
SAMPLEDATASET_3 <- SAMPLEDATASET_3  %>%
  mutate(NEW_COLUMN = fct_collapse(OLD_COLUMN, Poor = c("Poor", "Fair"),
                                     Good = c("Good","Very good", "Excellent")))

#CHECK LEVELS OF VARIABLES
levels(SAMPLEDATASET_3$GENDER)

#LOGISTIC REGRESSION ANALYSIS
assess_lr <- glm(DRUGS~GENDER+
                   AGE+
                   HISTORY+
                   AGE_CAT,
                 data=SAMPLEDATASET_3,family=binomial("logit"))

summary(assess_lr)

#FIGURES (X=CONTINOUS VARIABLE, facet_wrap(~CATEGORICAL VARIABLE)
SAMPLEDATASET_3 %>%
  ggplot(aes(x=DESPATCH_CODE)) +
  geom_histogram(aes(y=..density.., fill=..count..),binwidth=2)+
  geom_density(aes(y=..density..)) +
  facet_wrap(~DRUGS) +
  theme_classic(base_size = 12)+
  theme(panel.grid=element_blank())+
  labs(title = "Distribution of DRUGS by DISPATCH CODE")
