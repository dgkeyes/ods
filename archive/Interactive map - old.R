#### Packages ####

library('tidyverse')
library('stringr')
library('ggmap')
library('ggplot2')
library('shiny')
library('maps')
library('leaflet')

#### Get data ####

camps.for.map <- camps



#### Create icons ####

camp.icon <- awesomeIcons(
  icon = 'fa-tree',
  iconColor = ods.white,
  library = 'fa',
  markerColor = 'green'
)

school.icon <- awesomeIcons(
  icon = 'fa-university',
  iconColor = ods.white,
  library = 'fa',
  markerColor = 'blue'
)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addAwesomeMarkers(lng=schools.for.map$lon, lat=schools.for.map$lat, 
             popup = schools.for.map$School.Name...City,
              icon = school.icon,
             clusterOptions = markerClusterOptions()) %>%
  clearBounds()

save.image()
  
