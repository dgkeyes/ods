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



# LEAFLET -----------------------------------------------------------------

<<<<<<< HEAD

=======
<<<<<<< HEAD
# load(".RData")
=======
     .load(".RData")
>>>>>>> 5ef48186f1ab1758745f7e4efcaae6e5fffafd56
>>>>>>> 69987282e66310fefa8ab4c2296eb08dd1779488


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

     # setwd("maps")
     saveWidget(frameableWidget(ods.map.all), "all-schools.html")



<<<<<<< HEAD
# MAP BY ESD --------------------------------------------------------------

     ods.esd.map.data <- schools.data.for.map.v2


     map.filters <- c("Participated", "Did not participate", "Sister school participated", "No info")

=======
     # MAP BY ESD --------------------------------------------------------------


     map.filters <- c("Yes", "No", "Sister school did", "No info")
>>>>>>> 69987282e66310fefa8ab4c2296eb08dd1779488


     esd.list <- unique(schools.data$ESD) %>%
          tibble() %>%
          set_names("ESD") %>%
          filter(ESD != "Private" & ESD != "Oregon Department of Education") %>%
          arrange(ESD)



     map = leaflet() %>%
          addProviderTiles(providers$CartoDB.Positron)

<<<<<<< HEAD

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

=======
>>>>>>> 69987282e66310fefa8ab4c2296eb08dd1779488
     dk_ods_map <- function (esd_name){
          for (j in 4:1) {
               temp <- filter(ods.esd.map.data, participation == map.filters[j])



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
          }

          ods.map.by.esd <- map %>%
               addLayersControl(overlayGroups = map.filters,
                                options = layersControlOptions(collapsed = F)) %>%
               clearBounds()


          saveWidget(frameableWidget(ods.map.by.esd), paste(str_replace_all(str_to_lower(esd_name), " ", "-"),
                                                            ".html",
                                                            sep = ""))
     }

     for (i in 1:length(esd.list$ESD)) {
          ods.esd.map.data <- schools.data.for.map.v2 %>%
               filter(esd == esd.list$ESD[i])

          dk_ods_map(esd.list$ESD[i])
     }

