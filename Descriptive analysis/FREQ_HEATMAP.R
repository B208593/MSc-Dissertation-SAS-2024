## REORDER LEVELS OF FACTORS
#SAS_DATASET_LARGE2$C3_Call_Colour_new = factor(SAS_DATASET_LARGE2$C3_Call_Colour_new,
#                                              levels=c("No Colour",
#                                                       "Green", 
#                                                       "Yellow",
#                                                       "Amber", 
#                                                       "Red",
#                                                       "Purple"))


NEW_CODES$AGE_CATEGORY = factor(NEW_CODES$AGE_CATEGORY,
                                            levels=c("<30",
                                                     "30-49", 
                                                     "50-59",
                                                     "60-69", 
                                                     ">69"))


## CREATE CATEGORIES FOR COLOURS 
NEW_CODES$group<-cut(NEW_CODES$n,
                              breaks = c(0,150,500,1000,5000,15000))

## TILE PLOT
ggplot(NEW_CODES, aes(x = MEANING, y = AGE_CATEGORY, fill = group)) +
  geom_tile(color = "darkblue") + # Create heatmap
  scale_fill_manual(breaks = levels(NEW_CODES$group),
                    values = c("white",
                               "lightcyan", 
                               "lightblue",
                               "steelblue2",
                               "steelblue"),
                    name = "Number of incidents",
                    labels = c("0-150",  
                               "150-500",
                               "500-1,000",
                               "1,000-5,000",
                               "5,000-15,000"))+ theme_minimal() + # Set theme
  labs(x = "Despatch category", y = "Age groups") + # Labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = scales::comma(n)), color = "black", size = 4)+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.text.y = element_text(size=12))


