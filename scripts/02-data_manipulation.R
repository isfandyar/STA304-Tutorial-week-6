#### Preamble ####
# Purpose: Visualization - Graph using ggplot2 & map using ggmap
# Author: Isfandyar Virani
# Data: 13 Feb 2022
# Contact: isfandyar.virani@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(opendatatoronto)
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(knitr)
library(vtable)
library(ggmap)
#### Import data from folder ####
raw_data <- read_csv("inputs/data/raw_data.csv")


##### Extracting Event Call type and Location (Lattitute and Longitude)
EventCallType <- raw_data %>% select( Initial.CAD.Event.Call.Type,LONGITUDE, LATITUDE)


unique(EventCallType$Initial.CAD.Event.Call.Type) # Looking at the 8 different types of calls

### Finding the count of each of the 8 types of emergency event and arranging them in decending order
EventCallTypeCount<- EventCallType %>% group_by(Initial.CAD.Event.Call.Type) %>% summarize(count=n()) %>% arrange(desc(count))

#### Graphing 
EventCallTypeCount %>% ggplot(aes(x = reorder(Initial.CAD.Event.Call.Type, count), y = count)) + geom_bar(stat = 'identity', position = position_dodge()) + coord_flip() + 
  labs(x = 'Emergency Type', y = 'Count', title = 'Toronto Fire Services Emergency Incident Calls Type') +
  theme(axis.text.x =  element_text(angle = 27))

EventCallType

EventCallType_reduced <- filter(EventCallType, LATITUDE != 0) %>% filter(LONGITUDE != 0)  

## https://boundingbox.klokantech.com/ <--- used this website to get the bounding box
bbox_toronto <- c(left = -79.629471, bottom = 43.585106, right = -79.121844, top = 43.853512)

toronto_stamen_map <- get_stamenmap(bbox_toronto, zoom = 11, maptype = "toner-lite")

ggmap(toronto_stamen_map) +
  geom_point(data = EventCallType_reduced,
             aes(x = LONGITUDE,
                 y = LATITUDE,
                 colour = Initial.CAD.Event.Call.Type)) +
  labs(x = "Longitude",
       y = "Latitude",
       colour = "Emergency Event Call Type",
       title = "Toronto Fire Services Emergency Incident Calls") +
  theme_minimal() 
