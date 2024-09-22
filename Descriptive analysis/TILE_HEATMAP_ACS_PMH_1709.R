#### PMH
DATASET_RQ2_long <- SAS_DATASET_LARGE %>%
  select(AGE_CATEGORY,
         PMH_HD_Hypertension_N,
         PMH_HD_Angina_N,
         PMH_RD_COPD_N,
         PMH_RD_Asthma_N,
         PMH_ND_Epilepsy_N,
         PMH_HD_MI,
         PMH_HD_PCI,
         PMH_ND_Stroke,
         PMH_ND_TIA,
         PMH_GEN_Overdose,
         PMH_GEN_SuicideAttempts,
         PMH_GEN_Diabetes)




DATASET_RQ2_long <- DATASET_RQ2_long %>%
  rename(Hypertension=PMH_HD_Hypertension_N,
         Angina=PMH_HD_Angina_N,
         COPD=PMH_RD_COPD_N,
         Asthma=PMH_RD_Asthma_N,
         Epilepsy=PMH_ND_Epilepsy_N,
         MI=PMH_HD_MI,
         PCI=PMH_HD_PCI,
         Stroke=PMH_ND_Stroke,
         TIA= PMH_ND_TIA,
         Overdose=PMH_GEN_Overdose,
         SuicideAttempts=PMH_GEN_SuicideAttempts,
         Diabetes=PMH_GEN_Diabetes)


data_long <- gather(DATASET_RQ2_long, PMH, measurement, Hypertension:Diabetes, factor_key=TRUE)

data_long = data_long %>%
  filter(measurement=="Yes", is.na(AGE_CATEGORY))%>%
  count(PMH, AGE_CATEGORY)



data_long$AGE_CATEGORY = factor(data_long$AGE_CATEGORY,
                                         levels=c("<30",
                                                  "30-49", 
                                                  "50-59",
                                                  "60-69", 
                                                  ">69"))

data_long$group<-cut(data_long$n,
                     breaks = c(0,100,1000,2000,5000,10000))

data_long %>%
  filter(!is.na(AGE_CATEGORY))%>%
  ggplot(aes(x = AGE_CATEGORY, y= PMH , fill = group)) +
  geom_tile(color = "darkblue") + # Create heatmap
  scale_fill_manual(breaks = levels(data_long$group),
                    values = c("white",
                               "lightcyan", 
                               "lightblue",
                               "steelblue2",
                               "steelblue"),
                    name = "Number of patients",
                    labels = c("0-100",
                               "100-1000",
                               "1000-2000",
                               "2000-5000",
                               "5000-10000"))+ theme_minimal() + # Set theme
  labs(x = "Age groups", 
       y = "Past Medical History") + # Labels
  theme(axis.text.x = element_text(hjust = 1)) +
  geom_text(aes(label = scales::comma(n)), color = "black", size = 4)
#scale_x_discrete(labels= pain_label)

################################################################################
### ACS
DATASET_RQ2_long2 <- SAS_DATASET_0609 %>%
  select(INCIDENTID,
         Management_MISTEMI_ECGsent,
         Management_MISTEMI_DiscussedECG,
         Management_MISTEMI_DirectTriage,
         Management_MISTEMI_PreAlert,
         Management_MISTEMI_PatientThrombolysed,
         Management_ThromboCheck_Ischaemic,
         Management_ThromboCheck_GoodHistory,
         Management_ThromboCheck_ECGtoCCU)

DATASET_RQ2_long2 <- DATASET_RQ2_long2 %>%
  rename(MISTEMI_ECGsent= Management_MISTEMI_ECGsent,
         DiscussedECG=Management_MISTEMI_DiscussedECG,
         MISTEMI_DirectTriage=Management_MISTEMI_DirectTriage,
         MISTEMI_PreAlert=Management_MISTEMI_PreAlert,
         PatientThrombolysed=Management_MISTEMI_PatientThrombolysed,
         ThromboCheck_Ischaemic=Management_ThromboCheck_Ischaemic,
         ThromboCheck_GoodHistory=Management_ThromboCheck_GoodHistory)

data_long2 <- gather(DATASET_RQ2_long2, AMI, measurement, MISTEMI_ECGsent:ThromboCheck_GoodHistory, factor_key=TRUE)


data_long2 = data_long2 %>%
  filter(measurement=="Yes")

PMH_ACS <- data_long %>%
  left_join(data_long2, by='INCIDENTID')

PMH_ACS = PMH_ACS %>%
  count(PMH, AMI)%>%
  filter(!is.na(AMI))

PMH_ACS$group<-cut(PMH_ACS$n,
                   breaks = c(0,100,200,400,600,700))

ggplot(PMH_ACS, aes(x = PMH, y = AMI, fill = group)) +
  geom_tile(color = "darkblue") + # Create heatmap
  scale_fill_manual(breaks = levels(PMH_ACS$group),
                    values = c("white",
                               "lightcyan", 
                               "lightblue",
                               "steelblue2",
                               "steelblue"),
                    name = "Number of incidents",
                    labels = c("0-100",
                               "100-200",
                               "200-400",
                               "400-600",
                               "600-700"))+ theme_minimal() + # Set theme
  labs(x = "SIMD", 
       y = "Past Medical History", 
       title = "Heatmap for past medical history by SIMD") + # Labels
  theme(axis.text.x = element_text(hjust = 1)) +
  geom_text(aes(label = scales::comma(n)), color = "black", size = 4)
#scale_x_discrete(labels= pain_label)













