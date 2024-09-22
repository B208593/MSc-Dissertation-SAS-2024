################################ LINE GRAPH FOR TIME

MONTHLY_counts <- SAS_DATASET_FINAL %>% 
  select(TRANSPORTED_TO_HOSPITAL, Date_MONTH)%>% 
  count(Date_MONTH)

MONTHLY_counts_hosp <- SAS_DATASET_FINAL %>% 
  select(TRANSPORTED_TO_HOSPITAL, Date_MONTH)%>%
  filter(TRANSPORTED_TO_HOSPITAL == "Yes")%>%
  count(Date_MONTH, TRANSPORTED_TO_HOSPITAL)%>%
  rename(Transport = "n")

LINE_PLOT <- MONTHLY_counts %>%
  left_join(MONTHLY_counts_hosp, by='Date_MONTH')


LINE_PLOT = LINE_PLOT %>% 
  mutate(percentage = Transport/n)

###########################################################
##working
LINE_PLOT %>% 
  ggplot(aes(x= Date_MONTH, group = 1, color = "Status")) +
  geom_line(aes(y = n, color = "All calls")) +
  geom_line(aes(y = Transport, color = "Hospitalized")) +
  geom_line(aes(y = percentage*5000, 
                color = "Percentage"), size = 2) +
  expand_limits(x = 0, y = 3000)+
  labs(x = "Date", y = "Number of cases", 
       title = "Emergency calls per month")+
  scale_color_discrete(name = "Status", 
                       labels = c("All calls", 
                                  "Hospitalized",
                                  "Percentage"))+
  geom_text(aes(y = n, label= n), 
            hjust= 0.5, 
            vjust = -1, 
            size= 2.0, 
            color= "black")+
  geom_text(aes(y = Transport, label= Transport), 
            hjust= 0.5, 
            vjust = -1, 
            size= 2.0, 
            color= "black")+
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.x=element_text(size=10),
        axis.text.y = element_text(size=10))+
  #scale_x_continuous(breaks = seq(2000, 6000, 1000)) +
  scale_y_continuous(sec.axis=sec_axis(~.*0.0002,
                                       name="Percentage of patients hospitalized",
                                      labels=scales::percent))

5000 = 0.01

### DUAL AXES  
## https://finchstudio.io/blog/ggplot-dual-y-axes/



pkpd <- ggplot(res, aes(x = time, y = CP)) +
  geom_line(aes(color = "Drug Concentration")) +
  geom_line(aes(y = percentage*scale, color = "Percentage")) +
  scale_x_continuous(breaks = seq(0, 336, 24)) +
  scale_y_continuous(sec.axis = sec_axis(~.*scale, name="Percentage")) +
  labs(x = "Time (hr)", y = "Concentration (mg/L)", color = "") +
  scale_color_manual(values = c("orange2", "gray30"))


#scale_y_continuous(sec.axis = sec_axis(~ . / ratio, 
#  labels = scales::label_percent(),
# breaks = seq(0, 100, by = 20)))






