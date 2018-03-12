library(htmlwidgets)
library(widgetframe)
library(tidyverse)
library(plotly)
library(leaflet)

schools.data.for.map <- schools.data %>%
  filter(Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year. == "Yes" |
           Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year. == "No") %>%
  select(one_of(c("School.Name", 
                  "Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year.", 
                  "District",
                  "ESD",
                  "Governance",
                  "ods.fifth.grade",
                  "ods.sixth.grade",
                  "ods.students.in.fifth.or.sixth",
                  # "How.many.students.from.your.school.participated.in.Outdoor.School.in.the.2016.2017.school.year.",
                  "Which.camp.or.camps.did.students.at.your.school.attend.for.Outdoor.School.in.the.2016.2017.school.year.",
                  "How.many.DAYS.does.your.Outdoor.School.program.last.",
                  "How.many.NIGHTS.does.your.Outdoor.School.program.last.",
                  "Does.your.Outdoor.School.program.have.an.academic.component.",
                  "To.what.degree.are.you.able.to.make.accommodations.to.ensure.that.the.Outdoor.School.program.is.accessible.for.all.students.",
                  "lon", 
                  "lat"))) %>%
  set_names(c("school", 
              "participation", 
              "district",
              "esd",
              "governance",
              "ods.fifth.grade",
              "ods.sixth.grade",
              "number.of.students",
              "camp.attended",
              "days",
              "nights",
              "academic.component",
              "accommodations",
              "lon", 
              "lat"))

school.icon <- awesomeIcons(
  icon = 'fa-university',
  iconColor = ods.blue,
  library = 'fa',
  markerColor = 'white'
)


school.icons <- icons(
  iconUrl = ifelse(schools.data.for.map$participation == "Yes",
                   "icons/school-blue.png",
                   "icons/school-red.png")
)

school.icon.blue <- icons(
  iconUrl = "icons/school-blue.png")

school.icon.red <- icons(
  iconUrl = "icons/school-red.png")

camp.icon <- icons(
  iconUrl = "icons/camp.png"
)

school.popup.text <- paste("<strong>", schools.data.for.map$school, "</strong>",
                           "<br/>",
                           "District: ", schools.data.for.map$district,
                           "<br/>",
                           "2016-2017 outdoor school participation: ", schools.data.for.map$participation,
                           "<br/>",
                           "Camp(s) attended: ", schools.data.for.map$camp.attended,
                           "<br/>",
                           "Number of days: ", schools.data.for.map$days,
                           "<br/>",
                           "Number of nights: ", schools.data.for.map$nights,
                           "<br/>",
                           "Academic component: ", schools.data.for.map$academic.component,
                           sep = "")

schools.data.for.map.participated.yes <- schools.data.for.map %>%
  filter(participation == "Yes")

schools.data.for.map.participated.no <- schools.data.for.map %>%
  filter(participation == "No")

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(lng=schools.data.for.map.participated.yes$lon, 
             lat=schools.data.for.map.participated.yes$lat, 
             icon = school.icon.blue,
             popup = school.popup.text,
             # clusterOptions = markerClusterOptions(),
             group = "Participated in Outdoor School") %>%
  addMarkers(lng=schools.data.for.map.participated.no$lon, 
             lat=schools.data.for.map.participated.no$lat, 
             icon = school.icon.red,
             popup = school.popup.text,
             # clusterOptions = markerClusterOptions(),
             group = "Did not participate in Outdoor School") %>%
  addLayersControl(
    overlayGroups = c("Participated in Outdoor School", "Did not participate in Outdoor School"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  clearBounds()

htmlwidgets::saveWidget(frameableWidget(ods.leaflet.map),'leaflet.html')
