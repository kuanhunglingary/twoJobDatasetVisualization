# VA job
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(reshape2)
library(tidytext)
library(devtools)
library(formattable)
library(stringr)
library(leaflet)

# Read tsv file
myData <- read.delim(file = 'joblistings_merged_parsed_unique_grpbyyear_2016.tsv', header = TRUE)

# View Data
View(myData)
ls(myData)

# Filter Data
#
myData <- myData[,c("title","experienceRequirements","hiringOrganization_organizationName","jobDescription","jobLocation_address_locality","normalizedTitle_onetName","responsibilities","skills")]
View(myData)

write.csv(myData, "VA_Job.csv")

# Clean Data
# Search key words "DataData Scientist","Data Analyst", and "Data Engineer"
myData_related <- myData %>% filter(str_detect(title,"Data"))

myData_job <- myData_related %>% 
  mutate(job = ifelse(str_detect(title, "SCIENTIST"),"Data Scientist",
               ifelse(str_detect(title, "ANALYST"),"Data Analyst",
               ifelse(str_detect(title, "ENGINEER"),"Data Engineer",
                                    "OTHERS"))))%>%
  mutate(level = ifelse(str_detect(title, "MANAGER"),"MANAGER",
                 ifelse(str_detect(title, "SENIOR"),"SENIOR",
                 ifelse(str_detect(title, "PRINCIPAL"),"PRINCIPAL",
                 ifelse(str_detect(title, "SR"),"SENIOR",
                 ifelse(str_detect(title, "DIRECTOR"),"DIRECTOR",
                 ifelse(str_detect(title, "VP"),"VP",
                 ifelse(str_detect(title, "VICE PRESIDENT"),"VP",
                 ifelse(str_detect(title, "LEAD"),"LEAD",
                 ifelse(str_detect(title, "ASSOCIATE"),"ASSOCIATE",
                 ifelse(str_detect(title, "SPECIALIST"),"SPECIALIST",
                 ifelse(str_detect(title, "JUNIOR"),"JUNIOR",
                 "UNSPECIFIED"))))))))))))
table(myData_job$job, myData_job$level)

# Text mining
library(NLP)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)

myData2 <- toString(myData$jobLocation_address_locality)
myData2

# Text mining
myData3 <- Corpus(VectorSource(myData2))

# Remove Punctuation
myData3 <- tm_map(myData3, removePunctuation)

# Lower case
myData3 <- tm_map(myData3, content_transformer(tolower))

myData3 <- tm_map(myData3, PlainTextDocument)

# Remove Stop words
myData3 <- tm_map(myData3, removeWords, stopwords('english'))

myData3
# Word Cloud
wordcloud(myData3, max.words = 100, random.order = FALSE)
