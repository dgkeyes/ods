#### Load packages ####

library(splitstackshape)
library(tidyverse)
library(ggmap)
library(ggplot2)

#### Load themes ####

source("~/Google Drive/Work/R/DK themes.R")

#### Load and clean camp data ####

camps <- read.csv("data/camps.csv")
schools.geodata <- schools.data %>%
  select(one_of(c("School.Name...City", "lon", "lat")))


camps <- camps %>%
  select(-(Camp.Owner:X.Notes.by.Friends.of.Outdoor.School..)) %>%
  select(-(Coordinates)) %>%
  filter(!is.na(lon)) %>%
  cSplit("Schools", sep =",") %>%
  gather(camp, school, Schools_01:Schools_50) %>%
  filter(!is.na(school)) %>%
  select(-(camp)) %>%
  set_names(c("camp", "lon", "lat", "school")) %>%
  left_join(schools.geodata, by = c("school" = "School.Name...City")) %>%
  set_names(c("camp", "lon.camp", "lat.camp", "school", "lon.school", "lat.school")) %>%
  group_by(camp)

distance.to.camp <- mapdist(as.numeric(c("school.lon", "school.lat")), as.numeric(c("camp.lon", "camp.lat")))


#### Get basemaps ####

oregon.map <- map_data("state", "oregon")
ods.states.map <- map_data("state", region = c("oregon", "washington", "idaho"))

#### Plot camps and associated schools ####

ggplot(camps, aes(group = camp)) +
  geom_polygon(data=oregon.map, 
               aes(x=long, y=lat, group=group), 
               fill="#a9a9a9", 
               color = "#a9a9a9",
               alpha=0.5) +
  geom_point(data = camps,
             color = "blue",
             aes(x = lon.camp, y = lat.camp)) +
  geom_point(data = camps,
             color = "red",
             aes(x = lon.school, y = lat.school)) +
  facet_wrap(~camp) +
  theme_bw() + dkmaptheme
  
ggsave("plots/schools per camp.png")
