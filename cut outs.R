ods.participation.by.county.animated <- ggplot(ods.participation.by.county, 
                                               aes(x=long, y=lat, 
                                                   group=group, 
                                                   frame = pct.text,
                                                   fill = factor(pct.text))) +
  geom_polygon(color = "white") +
  scale_fill_manual(name = "Percent participation",
                    values = c("#EAEEF1", "#B6CADD", "#6894BF", ods.blue)) +
  ods.map.theme

gganimate(ods.participation.by.county.animated)

## Animated map by year started

ods.participation.map.animated.data <- schools.data %>%
  filter(Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year. == "Yes") %>%
  filter(!is.na(first.year.of.ODS))

ods.participation.map.animated <- ggplot() +
  geom_polygon(data = oregon.map, 
               aes(x=long, y=lat, group=group),
               color = ods.gray,
               fill = ods.light.gray) +
  geom_point(data = ods.participation.map.animated.data),
color = ods.blue,
aes(x = lon, y = lat,
    frame = first.year.of.ODS)) +
  # labs(title = "Schools that participated in Outdoor School in 2016-2017") +
  ods.map.theme

gganimate(ods.participation.map.animated)

ggsave("gff/plots/current programs/ods participation map - participated.png", dpi = 300)



#### Save data for GFF ####

temp <- data.frame((colnames(schools.data)))

schools.data.for.gff <- schools.data %>%
  select(one_of(c("School.Name",
                  "City",
                  "County",
                  "District",
                  "ESD",
                  "Governance",
                  "Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year.",
                  "ods.grades",
                  "Which.camp.or.camps.did.students.at.your.school.attend.for.Outdoor.School.in.the.2016.2017.school.year.",
                  "How.many.DAYS.does.your.Outdoor.School.program.last.",                                                                                                
                  "How.many.NIGHTS.does.your.Outdoor.School.program.last.",  
                  "Does.your.Outdoor.School.program.have.an.academic.component.",
                  "ods.total.students",
                  "curriculum.outside.provider",                                                                                                                         
                  "curriculum.school.district.staff",                                                                                                                    
                  "curriculum.volunteers",
                  "staffing.provider.staff",                                                                                                                             
                  "staffing.hs.mentors",                                                                                                                                 
                  "staffing.parents.volunteers",                                                                                                                         
                  "staffing.community.experts",                                                                                                                          
                  "staffing.other",
                  "ela.numeric",
                  "math.numeric",
                  "science.numeric",
                  "FRL",
                  "X2016.17....White."))) %>%
  set_names(c("school",
              "city",
              "county",
              "district",
              "esd",
              "governance",
              "ods.participation",
              "ods.grades",
              "ods.camp",
              "ods.days",
              "ods.nights",
              "ods.academic.component",
              "ods.total.students",
              "curriculum.outside.provider",                                                                                                                         
              "curriculum.school.district.staff",                                                                                                                    
              "curriculum.volunteers",
              "staffing.provider.staff",                                                                                                                             
              "staffing.hs.mentors",                                                                                                                                 
              "staffing.parents.volunteers",                                                                                                                         
              "staffing.community.experts",                                                                                                                          
              "staffing.other",
              "ela.numeric",
              "math.numeric",
              "science.numeric",
              "frl",
              "pct.white")) %>%
  filter(ods.participation == "Yes" | ods.participation == "No") %>%
  mutate(curriculum.volunteers = as.character(curriculum.volunteers)) %>%
  mutate(curriculum.volunteers = str_replace(curriculum.volunteers, "TRUE", "Yes")) %>%
  mutate(curriculum.volunteers = str_replace(curriculum.volunteers, "FALSE", "No")) %>%
  mutate(curriculum.outside.provider = as.character(curriculum.outside.provider)) %>%
  mutate(curriculum.outside.provider = str_replace(curriculum.outside.provider, "TRUE", "Yes")) %>%
  mutate(curriculum.outside.provider = str_replace(curriculum.outside.provider, "FALSE", "No")) %>%
  mutate(curriculum.school.district.staff = as.character(curriculum.school.district.staff)) %>%
  mutate(curriculum.school.district.staff = str_replace(curriculum.school.district.staff, "TRUE", "Yes")) %>%
  mutate(curriculum.school.district.staff = str_replace(curriculum.school.district.staff, "FALSE", "No")) %>%
  mutate(staffing.provider.staff = as.character(staffing.provider.staff)) %>%
  mutate(staffing.provider.staff = str_replace(staffing.provider.staff, "TRUE", "Yes")) %>%
  mutate(staffing.provider.staff = str_replace(staffing.provider.staff, "FALSE", "No")) %>%
  mutate(staffing.hs.mentors = as.character(staffing.hs.mentors)) %>%
  mutate(staffing.hs.mentors = str_replace(staffing.hs.mentors, "TRUE", "Yes")) %>%
  mutate(staffing.hs.mentors = str_replace(staffing.hs.mentors, "FALSE", "No")) %>%
  mutate(staffing.parents.volunteers = as.character(staffing.parents.volunteers)) %>%
  mutate(staffing.parents.volunteers = str_replace(staffing.parents.volunteers, "TRUE", "Yes")) %>%
  mutate(staffing.parents.volunteers = str_replace(staffing.parents.volunteers, "FALSE", "No")) %>%
  mutate(staffing.community.experts = as.character(staffing.community.experts)) %>%
  mutate(staffing.community.experts = str_replace(staffing.community.experts, "TRUE", "Yes")) %>%
  mutate(staffing.community.experts = str_replace(staffing.community.experts, "FALSE", "No")) %>%
  mutate(staffing.other = as.character(staffing.other)) %>%
  mutate(staffing.other = str_replace(staffing.other, "TRUE", "Yes")) %>%
  mutate(staffing.other = str_replace(staffing.other, "FALSE", "No")) 
# mutate(ods.academic.component = str_replace(ods.academic.component, "Yes\b", "")) %>%
# mutate(curriculum = paste(curriculum.outside.provider, curriculum.school.district.staff, curriculum.volunteers, sep = ", "))
# cSplit("ods.camp", sep = ",", drop = FALSE)

schools.data.for.gff[ods.academic.component == "Yes"]$ods.academic.component <- NA
schools.data.for.gff$ods.camp[51] <- "Camp Magruder, Coastal Discovery Center at Camp Gray"
schools.data.for.gff$ods.camp[85] <- NA
schools.data.for.gff$ods.camp[442] <- NA
schools.data.for.gff$ods.camp[496] <- NA
schools.data.for.gff$ods.camp[517] <- NA
schools.data.for.gff$ods.camp[750] <- NA

schools.data.for.gff <- schools.data.for.gff %>%
  set_names(c("School",
              "City",
              "County",
              "District",
              "ESD",
              "Governance",
              "Did school participate in Outdoor School in 2016-2017",
              "Grades that participated in Outdoor School",
              "Camp attended",
              "Program length (days)",
              "Program length (nights)",
              "Does program have an academic component",
              "Number of students who attended",
              "Did an outside provider help develop the curriculum",                                                                                                                         
              "Did school or district staff help develop the curriculum",                                                                                                                    
              "Did volunteers help develop the curriculum",
              "Did an outside provider staff the program",                                                                                                                             
              "Did high school mentors staff the program",                                                                                                                                 
              "Did volunteers staff the program",                                                                                                                         
              "Did community experts staff the program",                                                                                                                          
              "Did someone else staff the program",
              "School proficiency on reading",
              "School proficiency on math",
              "School proficiency on science",
              "Free and reduced lunch percent",
              "Percent white"))

library(WriteXLS)

temp <- data.frame(schools.data.for.gff$ods.camp)

WriteXLS(schools.data.for.gff, ExcelFileName = "gff/data/schools data.xls")







ods.program.characteristics.graph <- function(dataset, plotname) {
  characteristics.plot <- ggplot(datatset, aes(x = grouping, y = number, 
                                               fill = factor(grouping),
                                               color = factor(grouping))) +
    geom_bar(stat = "identity") +
    geom_text(label = percent(dataset$pct),
              hjust = -.2,
              color = ods.blue,
              size = ods.text.size * .35) +
    # scale_fill_manual(values=(colorscheme)) +
    coord_flip() +
    ods.bar.theme
  
  ggsave(characteristics.plot, 
         file = paste("plots/program characteristics/", plotname, ".png", sep = ""), 
         dpi = 300,
         height = nrow(dataset))
}


## By district

ods.participation.by.district <- schools.data %>%
  select(one_of("School.Name", "District", "ESD", "Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year.")) %>%
  set_names(c("school", "district", "esd", "participation")) %>%
  mutate(participation = str_replace(participation, "No, but have other outdoor ed programming", "No")) %>%
  filter(district != "None") %>%
  filter(participation != "Sister school did") %>%
  filter(!is.na(participation)) %>%
  group_by(district, participation, esd) %>%
  count() %>%
  set_names(c("district", "participation", "esd", "number")) %>%
  ungroup() %>%
  group_by(district) %>%
  mutate(pct = round(prop.table(number), digits = 2)) %>%
  filter(participation == "Yes") %>%
  filter(esd != "Oregon Department of Education")

ggplot(ods.participation.by.district, aes(x = reorder(district, pct), y = pct)) +
  geom_bar(stat = "identity") +
  facet_wrap(~esd, ncol = 2) +
  coord_flip() +
  ods.bar.theme.faceted + theme(axis.text.y = element_blank())

ggsave("plots/ods participation/ods participation by district.png", dpi = 300, height = nrow(number.of.students))




library(googlesheets)

gs_(schools.data.for.map, file = "schools for map.csv")


#### Leaflet testing ####

library(leaflet)

leaflet() %>% addTiles() %>%
  addPopups(-122.327298, 47.597131, temp,
            options = popupOptions(closeButton = FALSE)
  )

#### Leaflet testing ####

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(df.20)
)






states = map("state", fill = TRUE, plot = FALSE)

neStates <- subset(states, states$STUSPS %in% c(
  "CT","ME","MA","NH","RI","VT","NY","NJ","PA"
))

leaflet(neStates) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))

#### Examples ####

ui <- fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with 
               information from the 2010 US Census."),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Percent White", 
                              "Percent Black",
                              "Percent Hispanic", 
                              "Percent Asian"),
                  selected = "Percent White"),
      
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = 100, value = c(0, 100))
      ),
    
    mainPanel(
      textOutput("selected_var"),
      textOutput("min_max")
    )
  )
  )

server <- function(input, output) {
  
  output$selected_var <- renderText({ 
    paste("You have selected", input$var)
  })
  
  output$min_max <- renderText({ 
    paste("You have chosen a range that goes from",
          input$range[1], "to", input$range[2])
  })
  
}

shinyApp(ui, server)




#### Testing ####


camps <- read.csv("data/camps.csv")
camps <- camps %>%
  filter(lat != "")

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  leafletOutput("mymap")
  # p(),
  # actionButton("recalc", "New points")
)

camp.icon <- awesomeIcons(
  icon = 'fa-tree',
  iconColor = 'green',
  library = 'fa',
  markerColor = 'green'
)

school.icon <- awesomeIcons(
  icon = 'fa-pencil',
  iconColor = 'blue',
  library = 'fa',
  markerColor = 'blue'
)

server <- function(input, output, session) {
  
  # points <- eventReactive(input$recalc, {
  #   cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  # }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet(schools.data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addAwesomeMarkers(lng=schools.data$lon, lat=schools.data$lat, 
                        popup=paste("<h3>", schools.data$School.Name, "</h3>", "<br>test", sep=""),
                        icon = school.icon,
                        clusterOptions = markerClusterOptions()) %>%
      addAwesomeMarkers(lng=camps$lon, lat=camps$lat,
                        popup = camps$Camp.Name, icon = camp.icon) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  })
}

shinyApp(ui, server)







schools.data$ods.students.in.fifth[schools.data$ods.fifth.or.sixth.grade == 1] <- schools.data$ods.students.per.grade[schools.data$ods.fifth.or.sixth.grade == 1] / 1
schools.data$ods.students.in.fifth[schools.data$ods.fifth.or.sixth.grade == 2] <- schools.data$ods.students.per.grade[schools.data$ods.fifth.or.sixth.grade == 2] / 2



mutate(ods.students.in.fifth = if_else(ods.fifth.grade > 0, schools.data$ods.students.in.fifth.or.sixth / schools.data$ods.fifth.or.sixth.grade, 0))


temp <- schools.data %>%
  cSplit("ods.grades")



schools.data$ods.students.in.fifth <- 
  
  schools.data$ods.students.in.fifth.or.sixth / schools.data$ods.fifth.or.sixth.grade
schools.data$ods.students.in.fifth <- schools.data$ods.students.in.fifth.or.sixth / schools.data$ods.fifth.or.sixth.grade


temp <- schools.data %>%
  filter(!is.na(Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year.)) %>%
  select(Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year.) %>%
  set_names("response") %>%
  group_by(response) %>%
  count()

256+208+487+1
952+183
  

# Geocode schools without that data ##


temp <- filter(schools.data, is.na(lon))
temp <- temp %>%
  mutate(location = paste(School.Name, City, "Oregon")) %>%
  mutate_geocode(location)

write.csv(temp, "temp.csv")


schools.data$lon[is.na(schools.data$lon)] <- temp$lon.1
schools.data$lat[is.na(schools.data$lat)] <- temp$lat.1



# Geocode camps w/o data

camps <- read.csv("data/camps.csv")
camps.geodata <- camps %>%
  select(one_of(c("Camp.Name", "lon", "lat")))

temp <- filter(camps, is.na(lon))
temp <- temp %>%
  mutate(location = as.character(Camp.physical.address)) %>%
  mutate_geocode(location)

write.csv(temp, "temp.csv", na = "")

## Create new variable that is for 5th/6th ODS participation

# schools.data$participation.ods.fifth.sixth <- schools.data$Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year.
# 
# temp <- schools.data %>%
#   select(one_of(c("ods.fifth.or.sixth.grade", "participation.ods.fifth.sixth"))) %>%
#   filter(participation.ods.fifth.sixth == "No")
# 
# 
# schools.data$participation.ods.fifth.sixth[schools.data$Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year. == "Yes" & schools.data$ods.fifth.or.sixth.grade == 0] <- "No"
# 
# schools.data$participation.ods.fifth.sixth <- ifelse(schools.data$ods.fifth.or.sixth.grade == 0 &
#                                                      schools.data$Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year. == "Yes",
#                                                      "No", schools.data$participation.ods.fifth.sixth)


temp <- schools.data %>%
  select(contains("quartile")) %>%
  count()


schools.data$

temp <- mapdist("Portland, OR", "Yellow Springs, OH", mode = "driving")

register_google()

#### Create camp columns ####

temp <- schools.data
temp <- str_split(temp$Which.camp.or.camps.did.students.at.your.school.attend.for.Outdoor.School.in.the.2016.2017.school.year., ",", simplify = TRUE)
temp <- data.frame(schools.data$School.Name...City, temp)
temp <- temp %>%
  gather(key = "school", value="camp", 2:6) %>%
  select (-school) %>%
  set_names(c("school", "camp")) %>%
  filter(camp != "") %>%
  group_by(school)


temp <- geocodeQueryCheck() 

temp <- cSplit_e(schools.data, 
                 "Which.camp.or.camps.did.students.at.your.school.attend.for.Outdoor.School.in.the.2016.2017.school.year.",
                 type = "character")

temp <- temp[99:171]


#### Misc ####


colnames(schools.data[43])
temp <- data.frame(table(schools.data.governance[39]))



# schools.data$ods.seventh.grade <- NA
# schools.data$ods.eighth.grade <- NA
# schools.data$ods.ninth.grade <- NA
# schools.data$ods.tenth.grade <- NA
# schools.data$ods.eleventh.grade <- NA
# schools.data$ods.tweltfth.grade <- NA
# schools.data$ods.kindergarten <- ifelse (str_detect(schools.data$ods.grades, "K"), "x", "")
# schools.data$ods.first.grade <- ifelse (str_detect(schools.data$ods.grades, "1"), "x", "")
# schools.data$ods.second.grade <- NA
# schools.data$ods.third.grade <- NA
# schools.data$ods.fourth.grade <- NA

# Get data on schools that do ODS

schoolswithods <- subset(schoolsdata, schoolsdata$Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year. == "Yes")

# Filter for just 5th/6th grade (not doing for now)
schoolswithods %>% 
  filter(str_detect(Which.grades.participated.in.Outdoor.School.in.the.2016.2017.school.year., "5th grade"))


# Get data on schools that responded

schoolswithdata <- subset(schoolsdata, schoolsdata$Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year. != "")




# Get basemap

oregonmap <-  get_googlemap(force = FALSE, 
                            maptype="roadmap", 
                            center = "Oregon", 
                            zoom = 6, 
                            color = "bw", 
                            style = c(
                              feature = "poi", element = "labels", visibility = "off"
                            )
)


# ggmap

ODSschoolsmap <- ggmap(oregonmap) +
  
  # Schools 
  geom_point(data = subset(schoolsdata, Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year. == "No"),
             color = "#c63320",
             aes(x = lon, y = lat)) +
  
  
  # Camp points
  geom_point(data = camps,
             color = "#2667a7",
             aes(x = lat, y = lon)) +
  
  dkmaptheme

ODSschoolsmap



ggsave("camps.png", plot = ODSschoolsmap, width = 10, height = 10, units ="in", dpi = 300)


# Testing facets

schoolswithdatatest <- schoolswithdata
schoolswithdatatest <- filter (schoolswithdatatest, schoolswithdatatest$Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year. != "Sister school did")
schoolswithdatatest$participation <- factor(schoolswithdatatest$Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year., levels = c("Yes", "No"))

ODSschoolsmaptest <- ggmap(oregonmap) +
  geom_point(data = schoolswithdatatest, 
             shape = 21,
             fill = "#5c9134",
             color = "#5c9134",
             aes(x = lon, y = lat)) +
  facet_wrap(~participation, ncol = 1) +
  dkmaptheme

ODSschoolsmaptest


#################### SISTER SCHOOLS PAIRING ####################

schools.es <- subset(schoolsdata, Grade.level == "Elementary")
school.pairs.no.info <- subset(schoolsdata, Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year. == "")

school.pairs.no.info <- anti_join (school.pairs.no.info, schools.es, by = c("School.Name...City", "Sister.school"))

school.pairs$School.Name...City
school.pairs <- cbind(schoolsdata[1], schoolsdata[5])
school.pairs$pair.name <- paste(school.pairs$School.Name...City, school.pairs$Sister.school, sep="")



#################### MISC ####################

# Comments

schoolswoods <- subset(schoolsdata, schoolsdata$Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year. == "No")

comments <- data_frame(schoolswoods$Do.you.have.any.other.comments.about.Outdoor.School.)
colnames(comments)[1] <- "text"
comments <- subset(comments, comments$text != "")
write.csv (comments, "comments.csv")


# Sister schools stuff
sister.schools <- schoolsdata
sister.schools <- filter(sister.schools, sister.schools$Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year. == "")
sister.schools.total <- length(sister.schools$School.Name...City)
sister.schools <- filter(sister.schools, sister.schools$Sister.school != "")


##


ggplot(ods.participation.by.science.scores, aes(x = school_test_scores, y = pct)) +
  geom_bar(stat = "identity", fill = ods.green) +
  geom_text(label = percent(ods.participation.by.science.scores$pct),
            nudge_y = .03,
            family = "SourceSansPro-Regular",
            color = ods.green,
            size = ods.text.size) + 
  geom_text(aes(x = ods.participation.by.science.scores$school_test_scores, y = .02),
            label = ods.participation.by.science.scores$school_test_scores,
            family = "SourceSansPro-Regular",
            color = "white",
            size = ods.text.size,
            hjust = 0) +
  coord_flip() +
  ods.bar.theme + theme(axis.text.y = element_blank())


