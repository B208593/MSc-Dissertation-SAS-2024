#### https://r-graph-gallery.com/327-chloropleth-map-from-geojson-with-ggplot2.html

library(sf)

tmp_geojson <- tempfile(fileext = ".geojson")

### READ FILE
my_sf2 <- read_sf("Local_Authority_Districts_May_2024_Boundaries_UK_BFC_8443829521533055349.geojson")

## BLANK MAP
ggplot(my_sf2) +
  geom_sf(fill = "white", color = "black", linewidth = 0.3) +
  theme_void()

EPRINCIDENT_FINAL_2407 <- read_csv("EPRINCIDENT_FINAL_2407.csv", guess_max=100000)
SmallUser <- read_csv("SmallUser.csv", guess_max=100000)



#### MERGE SAS DATASET WITH POSTCODE FILE
#### rename column
EPRINCIDENT_FINAL_2407 = EPRINCIDENT_FINAL_2407_2 %>%
  rename("Postcode" = Incident_Postcode)

## delete rows which coordinates were deleted
SmallUser2 <- SmallUser[is.na(SmallUser$DateOfDeletion), ]

DATA_merged <- EPRINCIDENT_FINAL_2407 %>%
  left_join(SmallUser2, by = c("Postcode"))

DATA_merged = DATA_merged %>%
  filter(Postcode!="NA")

DATA_merged = DATA_merged %>%
  rename(id = CouncilArea2019Code)

DATA_merged = DATA_merged %>%
  rename(LAD24CD = id)


## create new column, count frequency
DATA_merged2 = DATA_merged %>%
  mutate(newcol = 1)%>%
  select(LAD24CD, newcol)%>%
  count(LAD24CD, sort=TRUE)

### merge with sf file
my_sf_merged <- my_sf2 %>%
  left_join(DATA_merged2, by = "LAD24CD")

## filter to Scotland
my_sf_merged = my_sf_merged%>%
  filter(substr(LAD24CD, 1, 1) == "S")

### plot map
ggplot(my_sf_merged) +
  geom_sf(aes(fill = n)) +
  theme_void()

## print map
p <-
  ggplot(my_sf_merged) +
  geom_sf(aes(fill = n)) +
  theme_void()+  
  png("Covid-19_Irish_Cases_vaccinated_confirmed_2021.png")
print(p)
dev.off()

############### fancy map
ggplot(my_sf_merged) +
  geom_sf(aes(fill = n), linewidth = 0, alpha = 0.9) +
  #guides(fill = guide_colorbar(reverse = TRUE))+
  theme_void() +
  scale_fill_viridis_c(
    name = "Number of calls",
    option="viridis",
    direction = -1)

### labels
+
  labs(
    title = "Chest pain patients",
    subtitle = "Number of calls per Council Area",
     ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(
      size = 15, hjust = 0.01, color = "#4e4d47"
    ),
    plot.subtitle = element_text(
      size = 10, hjust = 0.01,
      color = "#4e4d47"),
    plot.caption = element_text(
      size = 10,
      color = "#4e4d47"))

#########################################
####### per 10,000 population
## https://www.scotlandscensus.gov.uk/documents/scotland-s-census-2022-rounded-population-estimates-data/

Scotland_Census_2022 <- read_csv("Scotland_Census_2022.csv", guess_max=100000)

### merge with sf file
DATA_merged_pop = DATA_merged2 %>%
  rename(Area_code = LAD24CD)


DATA_merged_pop <- DATA_merged_pop %>%
  left_join(Scotland_Census_2022, by = "Area_code")


DATA_merged_pop = DATA_merged_pop %>%
  mutate(perpop= 10000/All_persons*n)


#### merge

### merge with sf file

my_sf2 = my_sf2 %>%
  rename(Area_code = LAD24CD)


my_sf_merged <- my_sf2 %>%
  left_join(DATA_merged_pop, by = "Area_code")

## filter to Scotland
my_sf_merged = my_sf_merged%>%
  filter(substr(Area_code, 1, 1) == "S")


ggplot(my_sf_merged) +
  geom_sf(aes(fill = perpop), linewidth = 0, alpha = 0.9) +
  theme_void() +
  scale_fill_viridis_c(
    name = "Number of calls per 10,000",
    option="viridis",
    direction = -1)+
  theme(
    legend.text = element_text(size = 8), 
    legend.title = element_text(size = 10)
  )
