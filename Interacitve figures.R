library(htmlwidgets)
library(widgetframe)
library(tidyverse)
library(plotly)
library(leaflet)
library(janitor)


# MAKE DATASET ------------------------------------------------------------
load(".RData")



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
                     "FRL",
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
                 "frl",
                 "lon",
                 "lat"))




# LEAFLET V2 -----------------------------------------------------------------


schools.data.for.map.v2 <- schools.data %>%
     select(one_of(c("School.Name",
                     "Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year.",
                     "Sister.school",
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
                     "FRL",
                     "lon",
                     "lat"))) %>%
     set_names(c("school",
                 "participation",
                 "sister.schools",
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
                 "frl",
                 "lon",
                 "lat")) %>%
     filter(participation == "Yes" |
                 participation == "No" |
                 participation == "Sister school did" |
                 is.na(participation)) %>%
     mutate(participation = str_replace(participation, "Yes", "Participated")) %>%
     mutate(participation = str_replace(participation, "No", "Did not participate")) %>%
     mutate(participation = str_replace(participation, "Sister school did", "Sister school participated")) %>%
     mutate(participation = factor(participation,
                                   levels = c("Participated",
                                              "Did not participate",
                                              "Sister school participated"))) %>%
     mutate(participation = str_replace_na(participation, "No info")) %>%
     mutate(mapcolor = participation) %>%
     mutate(mapcolor = str_replace(mapcolor, "Participated", "#004C97")) %>%
     mutate(mapcolor = str_replace(mapcolor, "No info", "#A9A9A9")) %>%
     mutate(mapcolor = str_replace(mapcolor, "Did not participate", "#AF1E2D")) %>%
     mutate(mapcolor = str_replace(mapcolor, "Sister school participated", "#DD8500"))









map.filters <- c("Participated", "Did not participate", "Sister school participated", "No info")

     map = leaflet() %>%
          addProviderTiles(providers$CartoDB.Positron)

     for (i in 4:1) {
          temp = filter(schools.data.for.map.v2, participation == map.filters[i])



          school.popup.text <- paste("<strong>", temp$school, "</strong>",
                                     "<br/>",
                                     "District: ", temp$district,
                                     "<br/>",
                                     "2016-2017 Outdoor School Participation: ", temp$participation,
                                     "<br/>",
                                     "Sister School(s):", temp$sister.schools,
                                     "<br/>",
                                     "Camp(s) Attended: ", temp$camp.attended,
                                     "<br/>",
                                     "Number of Days: ", temp$days,
                                     "<br/>",
                                     "Number of Nights: ", temp$nights,
                                     "<br/>",
                                     "Academic Component: ", temp$academic.component,
                                     sep = "")

          map = map %>%
               addCircleMarkers(lng=temp$lon,
                                lat=temp$lat,
                                color = temp$mapcolor,
                                fill = temp$mapcolor,
                                stroke = temp$mapcolor,
                                fillOpacity = .25,
                                popup = school.popup.text,
                                group = map.filters[i])
     }

     ods.map.all <- map %>%
          addLayersControl(overlayGroups = map.filters,
                           options = layersControlOptions(collapsed = F)) %>%
          clearBounds()

     setwd("maps")
     saveWidget(frameableWidget(ods.map.all), "all-schools.html")




# MAP BY ESD --------------------------------------------------------------

     ods.esd.map.data <- schools.data.for.map.v2


     map.filters <- c("Participated", "Did not participate", "Sister school participated", "No info")




               school.popup.text <- paste("<strong>", temp$school, "</strong>",
                                          "<br/>",
                                          "District: ", temp$district,
                                          "<br/>",
                                          "2016-2017 Outdoor School Participation: ", temp$participation,
                                          "<br/>",
                                          "Sister School(s):", temp$sister.schools,
                                          "<br/>",
                                          "Camp(s) Attended: ", temp$camp.attended,
                                          "<br/>",
                                          "Number of Days: ", temp$days,
                                          "<br/>",
                                          "Number of Nights: ", temp$nights,
                                          "<br/>",
                                          "Academic Component: ", temp$academic.component,
                                          sep = "")





     map = leaflet() %>%
          addProviderTiles(providers$CartoDB.Positron)


               school.popup.text <- paste("<strong>", temp$school, "</strong>",
                                          "<br/>",
                                          "District: ", temp$district,
                                          "<br/>",
                                          "2016-2017 Outdoor School Participation: ", temp$participation,
                                          "<br/>",
                                          "Sister School(s):", temp$sister.schools,
                                          "<br/>",
                                          "Camp(s) Attended: ", temp$camp.attended,
                                          "<br/>",
                                          "Number of Days: ", temp$days,
                                          "<br/>",
                                          "Number of Nights: ", temp$nights,
                                          "<br/>",
                                          "Academic Component: ", temp$academic.component,
                                          sep = "")


               map = map %>%
                    addCircleMarkers(lng=temp$lon,
                                     lat=temp$lat,
                                     color = temp$mapcolor,
                                     fill = temp$mapcolor,
                                     stroke = temp$mapcolor,
                                     fillOpacity = .25,
                                     popup = school.popup.text,
                                     group = map.filters[j],
                                     options = list(zIndex = j * -1))


     for (i in 1:length(esd.list$ESD)) {
          ods.esd.map.data <- schools.data.for.map.v2 %>%
               filter(esd == esd.list$ESD[i])

          dk_ods_map(esd.list$ESD[i])
     }

