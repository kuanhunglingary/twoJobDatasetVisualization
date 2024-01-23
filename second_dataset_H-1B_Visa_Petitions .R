library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(reshape2)
library(RColorBrewer)
library(lubridate)
library(tidytext)
library(devtools)
library(formattable)
library(stringr)
library(plotly)
library(wordcloud)
library(tm)
library(qdap)
library(viridis)
library(leaflet)

# Read Data
df = read.csv("h1b_kaggle.csv")

# View Data
df$X1 <- NULL
View(df)
glimpse(df)

# Text Cleaning
df_data_related <- df %>% filter(str_detect(JOB_TITLE,"DATA"))

df_job <- df_data_related %>% 
  mutate(job = ifelse(str_detect(JOB_TITLE, "SCIENTIST"),"Data Scientist",
               ifelse(str_detect(JOB_TITLE, "ANALYST"),"Data Analyst",
               ifelse(str_detect(JOB_TITLE, "ENGINEER"),"Data Engineer",
                      "OTHERS"))))%>%
  mutate(level = ifelse(str_detect(JOB_TITLE, "MANAGER"),"MANAGER",
                 ifelse(str_detect(JOB_TITLE, "SENIOR"),"SENIOR",
                 ifelse(str_detect(JOB_TITLE, "PRINCIPAL"),"PRINCIPAL",
                 ifelse(str_detect(JOB_TITLE, "SR"),"SENIOR",
                 ifelse(str_detect(JOB_TITLE, "DIRECTOR"),"DIRECTOR",
                 ifelse(str_detect(JOB_TITLE, "VP"),"VP",
                 ifelse(str_detect(JOB_TITLE, "VICE PRESIDENT"),"VP",
                 ifelse(str_detect(JOB_TITLE, "LEAD"),"LEAD",
                 ifelse(str_detect(JOB_TITLE, "ASSOCIATE"),"ASSOCIATE",
                 ifelse(str_detect(JOB_TITLE, "SPECIALIST"),"SPECIALIST",
                 ifelse(str_detect(JOB_TITLE, "JUNIOR"),"JUNIOR",
                         "UNSPECIFIED"))))))))))))
table(df_job$job, df_job$level)

# Visualization
catpal = colorFactor(viridis(10),df_job$job)
leaflet(data = filter(df_job, !is.na(lat),!is.na(lon),str_detect(df_job$CASE_STATUS,"CERTIFIED"))) %>% 
  addProviderTiles("Stamen.TonerLite") %>% 
  setView(lng = -110, lat = 25, zoom = 3) %>%
  addCircleMarkers(~lon, ~lat, stroke = FALSE, 
                   fillOpacity = 0.3, 
                   radius=2.5, 
                   popup = ~EMPLOYER_NAME, 
                   color = ~catpal(job)) %>%
  addLayersControl(
    baseGroups = c("job", "level"),
    options = layersControlOptions(collapsed = FALSE)
  )%>% 
  addLegend("topleft", pal = catpal, values = df_job$job, 
            title = "Job Title", 
            opacity = .8)