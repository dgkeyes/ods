#### Packages ####

library('tidyverse')
library('stringr')
library('ggmap')
library('ggplot2')
library('maps')
library('scales')
library('splitstackshape')
library('ggrepel')
library('lettercase')
library('waffle')
library('googlesheets')
library('gganimate')



temp <- route("Yellow Springs, OH", 
              "Portland, OR", 
              mode = "driving",
              output = "all")

us.map <- map_data("state")

ggplot(temp, aes(x = startLon, y = startLat)) +
  geom_polygon(data = us.map, 
               aes(x=long, y=lat, group=group),
               color = ods.gray,
               fill = ods.light.gray) +
  geom_line() +

  coord_map()

#### DELETE DIRECTORIES AND FILES AND THEN RECREATE THEME ####

# plots.directories <- c("camps", "current programs", "future programs",
#                        "interim report", "measure 99", "past programs", 
#                        "program characteristics")
# unlink("plots", recursive = TRUE)
# dir.create("plots")
# 
# dir.create(paste("plots", plots.directories[1], sep = "/"))
# dir.create(paste("plots", plots.directories[2], sep = "/"))
# dir.create(paste("plots", plots.directories[3], sep = "/"))
# dir.create(paste("plots", plots.directories[4], sep = "/"))
# dir.create(paste("plots", plots.directories[5], sep = "/"))
# dir.create(paste("plots", plots.directories[6], sep = "/"))
# dir.create(paste("plots", plots.directories[7], sep = "/"))


#### THEMES ####

library(extrafont)
library(ggplot2)



ods.green <- "#4a7729"
ods.blue <- "#004c97"
ods.red <- "#AF1E2D"
ods.gray <- "#a9a9a9"
ods.dark.gray <- "#909090"
ods.light.gray <- "#eeeeee"
ods.white <- "#ffffff"

## Define sizes 

ods.text.size <- 20

# Import fonts

font_import(pattern="Roboto")

# Define themes

ods.base.theme <- theme(
  panel.background = element_rect(fill = "white"),
  plot.background = element_blank(),
  text = element_text(size=ods.text.size, family="Roboto"),
  plot.title = element_text(size=ods.text.size * 1.25, 
                            family="Roboto", 
                            face = "bold",
                            color = ods.blue),
  plot.subtitle = element_text(size=ods.text.size * .75, 
                               family="Roboto", 
                               face = "italic",
                               color = ods.dark.gray),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  legend.position = "none"
)

ods.map.theme <- theme(
  axis.line = element_blank(),
  axis.line.y = element_blank(),
  axis.title.y = element_blank(),
  axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  panel.grid = element_blank(),
  panel.border = element_blank(),
  plot.background = element_blank(),
  panel.background = element_blank(),
  legend.position = "right",
  legend.title = element_text(size = ods.text.size * .75, family = "Roboto"),
  legend.text = element_text(size = ods.text.size * .75, family = "Roboto"),
  plot.title = element_text(size=ods.text.size * 1.2, 
                            family="Roboto", 
                            face = "bold",
                            color = ods.blue,
                            hjust = 0.5),
  plot.subtitle = element_text(size=ods.text.size * .75, 
                               family="Roboto", 
                               face = "italic",
                               color = ods.dark.gray)
)

ods.bar.theme <- ods.base.theme + theme(
  axis.text.x = element_blank(),
  axis.text.y = element_text(margin = margin(t = 0, r = -15, b = 0, l = 0))
  # panel.grid.major.x = element_line(color = ods.light.gray)
)

ods.bar.theme.faceted <- ods.base.theme + theme(
  strip.background = element_blank(),
  strip.text = element_text(face = "bold", size = ods.text.size, 
                            hjust = .05),
  legend.position = "none",
  panel.grid.major.y = element_blank(),
  panel.grid.major.x = element_blank(),
  # axis.title = element_text(size=ods.text.size, family="Roboto")
  axis.text.x = element_blank()
  # axis.text.y = element_blank()
)


ods.column.theme <- ods.base.theme + theme(
  axis.text.x = element_text(size=ods.text.size, family="SourceSansPro-Regular"),
  axis.text.y = element_text(size=ods.text.size, family="SourceSansPro-Regular"),
  axis.title = element_text(size=ods.text.size, family="SourceSansPro-Regular"),
  # axis.title.y = element_text(size=ods.text.size, family="SourceSansPro-Regular"),
  panel.grid.major.y = element_line(color = ods.light.gray)
)



#### GET DATA ####

schools.data <- read.csv("data/schools.csv", na.strings = "")
schools.data$District <- str_replace_na(schools.data$District, "Private")
schools.data$ESD <- str_replace_na(schools.data$ESD, "Private")



# Remove summary row that Fieldbook exports

schools.data <- schools.data[-as.numeric(nrow(schools.data)),]

## Fix text on academic connections data

schools.data <- schools.data %>%
  mutate("Does.your.Outdoor.School.program.have.an.academic.component." = str_replace(Does.your.Outdoor.School.program.have.an.academic.component., "Yet, but it DOES NOT CONNECT TO STATE STANDARDS", "Yes, but it DOES NOT CONNECT TO STATE STANDARDS"))

# Remove LTCT

schools.data$Type.of.school <- str_replace_na(schools.data$Type.of.school, "Unknown")

schools.data <- schools.data %>%
  filter(Type.of.school != "LTCT" & !is.na(Type.of.school))

## Calculate numbers of students and grades ##

## Total number of ODS students
schools.data$ods.total.students <- schools.data$How.many.students.from.your.school.participated.in.Outdoor.School.in.the.2016.2017.school.year.
schools.data$ods.total.students <- as.numeric(as.character(schools.data$ods.total.students))

## Calculate number of students in 5th and 6th doing ODS

grades.responses <- data.frame(unique(schools.data$Which.grades.participated.in.Outdoor.School.in.the.2016.2017.school.year.))
colnames(grades.responses) <- "response" 
grades.responses$response <- as.character(grades.responses$response)
grades.responses$ods.grades <- c("", 
                                 "6", 
                                 "5", 
                                 "5, 6, 7", 
                                 "6, 7, 8", 
                                 "5, 6", 
                                 "5, 6, 7, 8", 
                                 "4, 5, 6, 7, 8", 
                                 "3, 4, 5",
                                 "1, 2, 3", 
                                 "3, 4, 5, 6, 7, 8", 
                                 "6, 7, 8, 9, 10, 11, 12", 
                                 "3, 4, 5, 6, 7", 
                                 "6, 7", 
                                 "6, 7, 8",
                                 "1, 2, 3, 4, 5, 6, 7, 8",
                                 "1, 2, 3, 4, 5",
                                 "4, 5",
                                 "8",
                                 "3, 8",
                                 "4, 5, 6",
                                 "4",
                                 "3, 4, 5",
                                 "K, 1, 2, 3",
                                 "6, 8",
                                 "5, 7",
                                 "7",
                                 "7, 8",
                                 "K, 1, 2, 3, 4, 5",
                                 "3, 4, 5, 6",
                                 "1, 2, 3, 4",
                                 "6, 7, 8, 9, 10, 11, 12",
                                 "K, 1, 2, 3, 4, 5, 6, 7, 8",
                                 "3, 4, 5, 6, 7, 8")

# Convert grades text into something useful

schools.data <- left_join(schools.data, grades.responses, by = c("Which.grades.participated.in.Outdoor.School.in.the.2016.2017.school.year." = "response"))

# Create grades columns
schools.data$ods.fifth.grade <- ifelse (str_detect(schools.data$ods.grades, "5"), 1, 0)
schools.data$ods.sixth.grade <- ifelse (str_detect(schools.data$ods.grades, "6"), 1, 0)
schools.data$ods.fifth.or.sixth.grade <- ifelse (str_detect(schools.data$ods.grades, "5") | (str_detect(schools.data$ods.grades, "6")), schools.data$ods.fifth.grade + schools.data$ods.sixth.grade, 0)


# Count number of grades

schools.data$ods.number.of.grades <- str_count(schools.data$ods.grades, "\\,") + 1
schools.data$ods.number.of.grades <- ifelse(schools.data$ods.grades == "", 0, schools.data$ods.number.of.grades)

## Count approximate number of students in 5th or 6th grade doing ODS

schools.data$ods.students.per.grade <- ifelse(schools.data$ods.number.of.grades != 0, 
                                              round(as.numeric(schools.data$ods.total.students) / schools.data$ods.number.of.grades, digits = 0), NA)

schools.data$ods.students.in.fifth.or.sixth <- schools.data$ods.students.per.grade * schools.data$ods.fifth.or.sixth.grade

schools.data$ods.number.of.students.kindergarten <- if_else(str_detect(schools.data$ods.grades, "K"), round(schools.data$ods.total.students / schools.data$ods.number.of.grades, digits = 0), 0)
schools.data$ods.number.of.students.first <- if_else(str_detect(schools.data$ods.grades, "1"), round(schools.data$ods.total.students / schools.data$ods.number.of.grades, digits = 0), 0)
schools.data$ods.number.of.students.second <- if_else(str_detect(schools.data$ods.grades, "2"), round(schools.data$ods.total.students / schools.data$ods.number.of.grades, digits = 0), 0)
schools.data$ods.number.of.students.third <- if_else(str_detect(schools.data$ods.grades, "3"), round(schools.data$ods.total.students / schools.data$ods.number.of.grades, digits = 0), 0)
schools.data$ods.number.of.students.fourth <- if_else(str_detect(schools.data$ods.grades, "4"), round(schools.data$ods.total.students / schools.data$ods.number.of.grades, digits = 0), 0)
schools.data$ods.number.of.students.fifth <- if_else(str_detect(schools.data$ods.grades, "5"), round(schools.data$ods.total.students / schools.data$ods.number.of.grades, digits = 0), 0)
schools.data$ods.number.of.students.sixth <- if_else(str_detect(schools.data$ods.grades, "6"), round(schools.data$ods.total.students / schools.data$ods.number.of.grades, digits = 0), 0)
schools.data$ods.number.of.students.seventh <- if_else(str_detect(schools.data$ods.grades, "7"), round(schools.data$ods.total.students / schools.data$ods.number.of.grades, digits = 0), 0)
schools.data$ods.number.of.students.eighth <- if_else(str_detect(schools.data$ods.grades, "8"), round(schools.data$ods.total.students / schools.data$ods.number.of.grades, digits = 0), 0)
schools.data$ods.number.of.students.ninth <- if_else(str_detect(schools.data$ods.grades, "9"), round(schools.data$ods.total.students / schools.data$ods.number.of.grades, digits = 0), 0)
schools.data$ods.number.of.students.tenth <- if_else(str_detect(schools.data$ods.grades, "10"), round(schools.data$ods.total.students / schools.data$ods.number.of.grades, digits = 0), 0)
schools.data$ods.number.of.students.eleventh <- if_else(str_detect(schools.data$ods.grades, "11"), round(schools.data$ods.total.students / schools.data$ods.number.of.grades, digits = 0), 0)
schools.data$ods.number.of.students.twelfth <- if_else(str_detect(schools.data$ods.grades, "12"), round(schools.data$ods.total.students / schools.data$ods.number.of.grades, digits = 0), 0)


## Create staffing dichotomous variables ##

ods.staffing <- data.frame(as.character(unique(schools.data$In.addition.to.classroom.teachers..who.do.you.use.to.staff.your.Outdoor.School.program.)))
ods.staffing <- ods.staffing %>%
  set_names("original") %>%
  filter(original != "") %>%
  filter(original != " N/A")

ods.staffing$staffing.provider.staff <- str_detect(ods.staffing$original, fixed("Program provider staff", ignore_case = TRUE)) | str_detect(ods.staffing$original, fixed("MESD staff", ignore_case = TRUE))  
ods.staffing$staffing.hs.mentors <- str_detect(ods.staffing$original, fixed("High school", ignore_case = TRUE))
ods.staffing$staffing.parents.volunteers <- str_detect(ods.staffing$original, fixed("Parent", ignore_case = TRUE)) | str_detect(ods.staffing$original, fixed("Volunteer", ignore_case = TRUE))
ods.staffing$staffing.community.experts <- str_detect(ods.staffing$original, fixed("State agency", ignore_case = TRUE))
ods.staffing$staffing.other <- FALSE
ods.staffing$staffing.other[c(8)] <- TRUE

temp <- filter(ods.staffing, original %in% "Program provider staff")


# Join it back up

schools.data <- left_join(schools.data, ods.staffing, by = c("In.addition.to.classroom.teachers..who.do.you.use.to.staff.your.Outdoor.School.program." = "original"))





## Create curriculum dichotomous variables ##

ods.curriculum <- data.frame(as.character(unique(schools.data$Who.develops.the.curriculum.that.you.use.for.Outdoor.School.)))

ods.curriculum <- ods.curriculum %>%
  set_names("original") %>%
  filter(original != "") %>%
  filter(original != " N/A")

ods.curriculum$curriculum.outside.provider[c(1:10, 12:13, 15, 20:22, 25:42, 44:53)] <- TRUE
ods.curriculum$curriculum.outside.provider[is.na(ods.curriculum$curriculum.outside.provider)] <- FALSE

ods.curriculum$curriculum.school.district.staff[c(2, 5, 7, 11, 16, 17, 19, 23, 28, 31, 32, 34, 36, 37, 38, 40, 43, 46, 50, 53)] <- TRUE
ods.curriculum$curriculum.school.district.staff[is.na(ods.curriculum$curriculum.school.district.staff)] <- FALSE

ods.curriculum$curriculum.volunteers <- FALSE
ods.curriculum$curriculum.volunteers[c(6, 24)] <- TRUE
# ods.curriculum$curriculum.volunteers[is.na(ods.curriculum$curriculum.volunteers)] <- FALSE

## Join it back up

schools.data <- left_join(schools.data, ods.curriculum, by = c("Who.develops.the.curriculum.that.you.use.for.Outdoor.School." = "original"))




## State ID formatting

schools.data$State.ID[schools.data$State.ID == ""] <- NA
schools.data$State.ID <- as.integer(as.character(schools.data$State.ID))












## Add state test data ##

# ELA

test.data.ela <- read.csv("data/state data/ELA.csv")

test.data.ela <- test.data.ela %>%
  filter(Student.Group == "Total Population (All Students)")

test.data.ela <- test.data.ela[c(4, 5, 10)]
colnames(test.data.ela) <- c("State.ID", "School Name", "ELA percent proficient")

# Remove non-numeric items
test.data.ela$ela.numeric <- test.data.ela$`ELA percent proficient` 
test.data.ela$ela.numeric <- str_replace(test.data.ela$ela.numeric, "\\*", "")
test.data.ela$ela.numeric <- str_replace(test.data.ela$ela.numeric, "--", "")
test.data.ela$ela.numeric <- str_replace(test.data.ela$ela.numeric, "> 95.0%", "95.0")
test.data.ela$ela.numeric <- as.numeric(test.data.ela$ela.numeric)

test.data.ela$ela.quartile <- cut(test.data.ela$ela.numeric, breaks = seq(0,100, by = 25),
                                  labels = c("0 - 25 percent", 
                                             "25 - 50 percent",
                                             "50 - 75 percent",
                                             "75 - 100 percent"))


# Math

test.data.math <- read.csv("data/state data/math.csv")

test.data.math <- test.data.math %>%
  filter(Student.Group == "Total Population (All Students)")

test.data.math <- test.data.math[c(4, 5, 10)]

colnames(test.data.math) <- c("State.ID", "School Name", "Math percent proficient")


# Remove non-numeric items
test.data.math$math.numeric <- test.data.math$`Math percent proficient` 
test.data.math$math.numeric <- str_replace(test.data.math$math.numeric, "\\*", "")
test.data.math$math.numeric <- str_replace(test.data.math$math.numeric, "--", "")
test.data.math$math.numeric <- str_replace(test.data.math$math.numeric, "> 95.0%", "95.0")
test.data.math$math.numeric <- as.numeric(test.data.math$math.numeric)

test.data.math$math.quartile <- cut(test.data.math$math.numeric, breaks = seq(0,100, by = 25),
                                    labels = c("0 - 25 percent", 
                                               "25 - 50 percent",
                                               "50 - 75 percent",
                                               "75 - 100 percent"))


test.scores.totals <- schools.data %>%
  select(one_of(c("ela.quartile", "math.quartile", "science.quartile"))) %>%
  gather() %>%
  group_by(key) %>%
  count(value) %>%
  filter(!is.na(value))


# Science

test.data.science <- read.csv("data/state data/science.csv")

test.data.science <- test.data.science %>%
  filter(Student.Group == "Total Population (All Students)") %>%
  filter(Grade.Level == "All Grades")

test.data.science <- test.data.science[c(4, 5, 10)]

colnames(test.data.science) <- c("State.ID", "School Name", "Science percent proficient")


# Remove non-numeric items
test.data.science$science.numeric <- test.data.science$`Science percent proficient` 
test.data.science$science.numeric <- str_replace(test.data.science$science.numeric, "\\*", "")
test.data.science$science.numeric <- str_replace(test.data.science$science.numeric, "--", "")
test.data.science$science.numeric <- str_replace(test.data.science$science.numeric, "> 95.0%", "95.0")
test.data.science$science.numeric <- as.numeric(test.data.science$science.numeric)

test.data.science$science.quartile <- cut(test.data.science$science.numeric, breaks = seq(0,100, by = 25),
                                          labels = c("0 - 25 percent", 
                                                     "25 - 50 percent",
                                                     "50 - 75 percent",
                                                     "75 - 100 percent"))



## Merge into master database

schools.data <- left_join(schools.data, test.data.ela, by = "State.ID")
schools.data <- left_join(schools.data, test.data.math, by = "State.ID")
schools.data <- left_join(schools.data, test.data.science, by = "State.ID")



## Add free and reduced lunch ##

frl <- read.csv("data/state data/FRL.csv")
frl <- frl[c(1, 17)]
colnames(frl) <- c("State.ID", "FRL")
# frl$State.ID <- as.factor(frl$State.ID)

# Remove non-numeric items

frl$frl.quartile <- cut(frl$FRL, breaks = seq(0,100, by = 25),
                        labels = c("0 - 25 percent", 
                                   "25 - 50 percent",
                                   "50 - 75 percent",
                                   "75 - 100 percent"))

schools.data <- left_join(schools.data, frl, by = "State.ID")

## Make items numeric ##

schools.data$X2016.17....White. <- str_replace(schools.data$X2016.17....White., "\\%", "")
schools.data$X2016.17....White. <- as.numeric(schools.data$X2016.17....White.) / 100

schools.data$white.quartile <- cut(schools.data$X2016.17....White., breaks = seq(0,1, by = .25),
                                   labels = c("0 - 25 percent", 
                                              "25 - 50 percent",
                                              "50 - 75 percent",
                                              "75 - 100 percent"))

schools.data$ela.numeric <- schools.data$ela.numeric / 100
schools.data$math.numeric <- schools.data$math.numeric / 100
schools.data$science.numeric <- schools.data$science.numeric / 100
schools.data$FRL <- schools.data$FRL / 100

## Create first year doing ODS variable ##

schools.data$How.many.years.has.your.school.sent.students.to.Outdoor.School. <- str_replace(schools.data$How.many.years.has.your.school.sent.students.to.Outdoor.School., "A long time", "NA")
schools.data$How.many.years.has.your.school.sent.students.to.Outdoor.School. <- str_replace_na(schools.data$How.many.years.has.your.school.sent.students.to.Outdoor.School.)
schools.data$How.many.years.has.your.school.sent.students.to.Outdoor.School. <- as.numeric(schools.data$How.many.years.has.your.school.sent.students.to.Outdoor.School.)

schools.data$first.year.of.ODS <- 2017 - schools.data$How.many.years.has.your.school.sent.students.to.Outdoor.School.

###

## Make schools dataset concise ##

schools.data.concise <- schools.data %>%
  select(-School.Name...City) %>%
  select(-(Street.Address:Zip)) %>%
  select(-(Phone)) %>%
  select(-(Contact.person.role:Email)) %>%
  select(-If.your.school.is.not.listed.above..please.enter.its.name.below.) %>%
  select(-X2015.16.Total.Enrollment) %>%
  select(-(X2016.17..Male:X2016.17.White)) %>%  
  select(-(X2016.17.Multiracial:X2016.17.Grade.Four)) %>%
  select(-(X2016.17.Grade.Seven:Sister.school..opposite.direction.)) %>%
  select(-(MS:Good.interview.candidate)) %>%
  select(-(ods.fifth.or.sixth.grade:ods.students.per.grade)) %>%
  select(-`School Name.x`) %>%
  select(-`School Name.y`) %>%
  select(-`School Name`) 
# set_names(c("name", 
#             "district",
#             "esd", 
#             "sister.school", 
#             "lowest.grade", 
#             "highest.grade",
#             "grade.level",
#             "governance",
#             "type.of.school",
#             "state.id",
#             "ods.notes",
#             
#             ))



## Make lon and lat data numeric

schools.data$lon <- as.numeric(as.character(schools.data$lon))
schools.data$lat <- as.numeric(as.character(schools.data$lat))


#### QUAL DATA ####

qual.data <- schools.data %>%
  select(one_of(c("School.Name...City",  "Do.you.have.any.other.comments.about.Outdoor.School."))) %>%
  set_names(c("school", "survey_comments")) %>%
  filter(!is.na(survey_comments)) %>%
  filter(survey_comments != "NA")

write.csv(qual.data, "qualdata.csv")




## Remove stuff

rm(temp, grades.responses, ods.curriculum, ods.staffing, test.data.ela, test.data.math, test.data.science, frl)

temp <- schools.data %>%
  filter(!is.na(`Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year.`))


#### FUNCTIONS ####

ods.participation.graph <- function(dataset, plotname, maincolor) {
  participation.plot <- ggplot(dataset, aes(x = grouping, y = pct)) +
    geom_bar(stat = "identity",
             fill = maincolor) +
    geom_text(label = percent(dataset$pct),
              hjust = -.2,
              color = maincolor,
              size = ods.text.size * .35) + 
    scale_y_continuous(limits = c(0, max(dataset$pct + .1)), labels = percent) +
    coord_flip() +
    ods.bar.theme
  
  ggsave(participation.plot, 
         file = paste("plots/current programs/ods participation by ", plotname, ".png", sep = ""), 
         dpi = 300,
         height = nrow(dataset))
}




#### CURRENT PROGRAMS ####

#### ODS participation by number of students/grades ####

number.of.students <- schools.data %>%
  select(contains("ods.number.of.students")) %>%
  gather() %>%
  filter(value != 0) %>%
  group_by(key) %>%
  mutate(total = sum(value, na.rm = TRUE)) %>%
  distinct(key, .keep_all = TRUE) %>%
  set_names(c("grade", "unsure","number_of_students")) %>%
  ungroup() %>%
  select(one_of("grade", "number_of_students"))

number.of.students$grade <- str_replace(number.of.students$grade, "ods.number.of.students.", "")
number.of.students$grade <- str_title_case(number.of.students$grade)

non.fifth.sixth.ods.students <- data.frame(sum(number.of.students$number_of_students[number.of.students$grade != "Fifth" & number.of.students$grade != "Sixth"]))
temp <- data.frame(c("Other", non.fifth.sixth.ods.students))
temp <- temp %>%
  set_names(colnames(number.of.students))
number.of.students <- bind_rows(number.of.students, temp)

number.of.students <- number.of.students %>%
  filter(grade == "Fifth" | grade == "Sixth" | grade == "Other")

number.of.students$grade <- factor(number.of.students$grade, levels = rev(c("Fifth", "Sixth", "Other")))

ggplot(number.of.students, aes(x = grade, y = number_of_students, 
                               fill = factor(grade),
                               color = factor(grade))) +
  geom_bar(stat = "identity") +
  geom_text(label = comma(number.of.students$number_of_students),
            hjust = -.2, 
            size = ods.text.size * .3) +
  scale_fill_manual(values = c(ods.dark.gray, ods.blue, ods.dark.gray)) +
  scale_color_manual(values = c(ods.dark.gray, ods.blue, ods.dark.gray)) +
  scale_y_continuous(limits = c(0, 25000)) +
  coord_flip() +
  ods.bar.theme

ggsave("plots/current programs/number of students by grade.png", dpi = 300, 
       height = nrow(number.of.students))


#### ODS participation by general ####

ods.participation.by.general <- schools.data %>%
  select(one_of(c("School.Name", "Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year."))) %>%
  set_names(c("school", "participation")) %>%
  count(participation) %>%
  filter(participation == "No" | participation == "Yes") %>%
  set_names(c("grouping", "number")) %>%
  mutate(pct = round(prop.table(number), digits = 2)) %>%  
  filter(grouping == "Yes")


# ods.participation.graph(ods.participation.by.general, "general", ods.dark.gray)

#### ODS participation by general - waffle ####

ods.participation.by.general.waffle <- schools.data %>%
  select(one_of(c("School.Name", "Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year."))) %>%
  set_names(c("school", "participation")) %>%
  count(participation) %>%
  filter(participation == "No" | participation == "Yes") %>%
  set_names(c("response", "number"))

ods.participation.by.general.waffle <- c(`Participated` = ods.participation.by.general.waffle$number[2],
                                         `DID NOT participate` = ods.participation.by.general.waffle$number[1])
  

waffle(ods.participation.by.general.waffle,
       rows = 15,
       color = c(ods.blue, ods.red),
       legend_pos = "none") 
  
ggsave(file = "plots/current programs/ods participation by general - waffle.png", 
       dpi = 300,
       height = 3)

#### ODS participation by school size ####

ods.participation.by.size <- schools.data %>%
  select(one_of(c("School.Name", "X2016.17.Total.Enrollment", "Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year."))) %>%
  set_names(c("school", "enrollment", "participation")) %>%
  filter(!is.na(enrollment)) %>%
  filter(participation == "Yes" | participation == "No") %>%
  mutate(enrollment = as.numeric(enrollment)) %>%
  mutate(size = cut(enrollment,
                    breaks = c(0, 100, 500, 10000),
                    labels = c("Small (100 or fewer students)",
                               "Medium",
                               "Large (500 or more students)"))) %>%
  select(-c(enrollment, school)) %>%
  filter(size != "Medium") %>%
  group_by(size, participation) %>%
  count() %>%
  ungroup() %>%
  group_by(size) %>%
  mutate(pct = round(prop.table(n), digits = 2)) %>%
  filter(participation == "Yes") %>%
  select(-c(participation, n)) %>%
  set_names(c("grouping", "pct"))
  
ods.participation.graph(ods.participation.by.size, "size", c(ods.dark.gray, ods.blue)) 

# summary(schools.data$X2016.17.Total.Enrollment)
# temp <- schools.data %>%
#   filter(X2016.17.Total.Enrollment > 1000)

#### ODS participation by governance ####

ods.participation.by.governance <- schools.data %>%
  select(one_of(c("School.Name", "Governance", "Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year."))) %>%
  set_names(c("school", "grouping", "participation")) %>%
  group_by(grouping) %>%
  count(participation) %>%
  filter(participation == "No" | participation == "Yes") %>%
  set_names(c("grouping", "response", "number")) %>%
  mutate(pct = round(prop.table(number), digits = 2)) %>%
  filter(response == "Yes") 

ods.participation.graph(ods.participation.by.governance, "governance", c(ods.blue, ods.dark.gray))

#### ODS participation by FRL ####

ods.participation.by.frl <- schools.data %>%
  select(one_of("frl.quartile", "Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year.")) %>%
  set_names(c("frl", "participation")) %>%
  group_by(frl) %>%
  count(participation) %>%
  filter(participation == "Yes" | participation == "No") %>%
  filter(!is.na(frl)) %>%
  mutate(pct = round(prop.table(n), digits = 2)) %>%
  filter(participation == "Yes") %>%
  set_names(c("grouping", "participation", "number", "pct")) %>%
  mutate(grouping = str_replace(grouping, "percent", "percent free and reduced lunch")) %>%
  ungroup %>%
  mutate(grouping = c("0 - 25 percent free and reduced lunch", 
                      "Between 25 and 50 percent",
                      "Between 50 and 75 percent",
                      "75 percent or more"))

ods.participation.by.frl$grouping <- factor(ods.participation.by.frl$grouping,
                                            levels = rev(ods.participation.by.frl$grouping))

# ods.participation.graph(ods.participation.by.frl, "frl", ods.dark.gray) 

## Manual version to highlight items

ods.participation.by.frl$color <- c(ods.dark.gray,
                                    ods.dark.gray,
                                    ods.dark.gray,
                                    ods.blue)

ods.participation.by.frl$fill <- rev(ods.participation.by.frl$color)

ggplot(ods.participation.by.frl, aes(x = grouping, y = pct)) +
    geom_bar(stat = "identity",
             fill = ods.participation.by.frl$fill) +
    geom_text(label = percent(ods.participation.by.frl$pct),
              hjust = -.2,
              color = ods.participation.by.frl$color,
              size = ods.text.size * .35) + 
    scale_y_continuous(limits = c(0, max(ods.participation.by.frl$pct + .1)), labels = percent) +
    coord_flip() +
    ods.bar.theme
  
  ggsave(file = "plots/current programs/ods participation by frl.png", 
         dpi = 300,
         height = nrow(ods.participation.by.frl))


#### ODS participation by ESD ####

write.csv(ods.participation.by.esd, "temp.csv", quote = FALSE, row.names = FALSE)  
  
ods.participation.by.esd <- schools.data %>%
  select(one_of("School.Name", "ESD", "Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year.")) %>%
  set_names(c("school", "grouping", "participation")) %>%
  filter(participation != "Sister school did") %>%
  mutate(participation = str_replace(participation, "No, but have other outdoor ed programming", "No")) %>%
  filter(grouping != "Private") %>%
  filter(grouping != "Oregon Department of Education") %>%
  filter(!is.na(participation)) %>%
  group_by(grouping, participation) %>%
  count() %>%
  ungroup() %>%
  group_by(grouping) %>%
  mutate(pct = round(prop.table(n), digits = 2)) %>%
  set_names(c("grouping", "participation", "number", "pct")) %>%
  filter(grouping != "Region 18 ESD") %>%
  filter(participation == "Yes") %>%
  mutate(highlight_color = ifelse(pct >= .5, ods.blue, ods.dark.gray))

# ods.participation.by.esd$grouping <- factor(ods.participation.by.esd$grouping, 
                                            # levels = rev(ods.participation.by.esd$grouping))

ggplot(ods.participation.by.esd, aes(x = reorder(grouping, pct), y = pct)) +
  geom_bar(stat = "identity", 
           fill = ods.dark.gray) +
  geom_text(label = percent(ods.participation.by.esd$pct),
            hjust = -.2,
            size = ods.text.size * .35,
            color = ods.dark.gray) + 
  scale_y_continuous(limits = c(0, 1), labels = percent) +
  coord_flip() +
  ods.bar.theme

ggsave("plots/current programs/ods participation by esd.png",
       dpi = 300,
       height = nrow(ods.participation.by.esd) / 2.5)

# ods.participation.graph(ods.participation.by.esd, "esd", ods.dark.gray)

#### ODS participation by race/ethnicity ####

ods.participation.by.whiteness <- schools.data %>%
  group_by(white.quartile) %>%
  count(Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year.) %>%
  set_names(c("white.quartile", "response", "number")) %>%
  filter(response == "No" | response == "Yes") %>%
  mutate(pct = round(prop.table(number), digits = 2)) %>%
  filter(!is.na(white.quartile)) %>%
  ungroup() %>%
  mutate(white.quartile = str_replace(white.quartile, "percent", "percent white")) %>%
  mutate(response = str_replace(response, "Yes", "PARTICIPATED")) %>%
  mutate(response = str_replace(response, "No", "DID NOT participate")) %>%
  set_names(c("grouping", "response", "number", "pct")) %>%
  filter(response == "PARTICIPATED") %>%
  mutate(grouping = c("0 - 25 percent white", 
                            "Between 25 and 50 percent",
                            "Between 50 and 75 percent",
                            "75 percent or more"))


ods.participation.by.whiteness$grouping <- factor(ods.participation.by.whiteness$grouping,
                                               levels = rev(ods.participation.by.whiteness$grouping))

ods.participation.graph(ods.participation.by.whiteness, "whiteness", ods.dark.gray)


#### ODS participation by test scores ####

ods.participation.by.test.scores <- schools.data %>%
  select(one_of(c("ela.quartile", "math.quartile", "science.quartile", "Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year."))) %>%
  gather(Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year.) %>%
  set_names(c("ods_participation", "test_type", "school_test_scores")) %>%
  group_by(ods_participation, test_type) %>%
  count(school_test_scores) %>%
  filter(ods_participation == "Yes") %>%
  filter(!is.na(school_test_scores)) %>%
  bind_cols(test.scores.totals) %>%
  select(-(key:value)) %>%
  mutate(pct = round(n / n1, digits = 2)) %>%
  ungroup() %>%
  mutate(test_type = str_replace(test_type, "ela.quartile", "Reading")) %>%
  mutate(test_type = str_replace(test_type, "math.quartile", "Math")) %>%
  mutate(test_type = str_replace(test_type, "science.quartile", "Science")) %>%
  set_names(c("ods_participation", "test_type", "grouping", "number_who_did_ods", "number_total", "pct")) 


ods.participation.by.test.scores$grouping <- str_replace(ods.participation.by.test.scores$grouping, 
                                                         "percent",
                                                         "percent proficient")


ods.participation.by.test.scores$grouping[1:4] <- factor(ods.participation.by.test.scores$grouping[1:4],
                                                         levels = rev(ods.participation.by.test.scores$grouping[1:4]))

ods.participation.by.test.scores$grouping[5:8] <- factor(ods.participation.by.test.scores$grouping[5:8],
                                                         levels = rev(ods.participation.by.test.scores$grouping[5:8]))

ods.participation.by.test.scores$grouping[9:12] <- factor(ods.participation.by.test.scores$grouping[9:12],
                                                         levels = rev(ods.participation.by.test.scores$grouping[9:12]))


ods.participation.by.test.scores.labels <- c("0 - 25 percent proficient", 
                                            "Between 25 and 50 percent",
                                            "Between 50 and 75 percent",
                                            "75 percent or more")

ggplot(ods.participation.by.test.scores, aes(x = grouping, y = pct,
                                             fill = factor(test_type),
                                             color = factor(test_type))) +
    geom_bar(stat = "identity") +
    geom_text(label = percent(ods.participation.by.test.scores$pct),
              hjust = -.2,
              size = ods.text.size * .35) + 
    scale_fill_manual(values = c(ods.dark.gray, ods.dark.gray, ods.blue)) +
    scale_color_manual(values = c(ods.dark.gray, ods.dark.gray, ods.blue)) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_discrete(labels = rev(ods.participation.by.test.scores.labels)) +
    facet_wrap(~test_type) +
    coord_flip() +
    ods.bar.theme.faceted 
  
ggsave("plots/current programs/ods participation by test scores.png",
       dpi = 300,
       height = 4)


ods.participation.by.ela.scores <- ods.participation.by.test.scores %>%
  filter(test_type == "Reading")

ods.participation.by.ela.scores$grouping <- factor(ods.participation.by.ela.scores$grouping,
                                                    levels = rev(ods.participation.by.ela.scores$grouping))

ods.participation.by.science.scores <- ods.participation.by.test.scores %>%
  filter(test_type == "Science")

ods.participation.by.science.scores$grouping <- factor(ods.participation.by.science.scores$grouping,
                                                       levels = rev(ods.participation.by.science.scores$grouping))

ods.participation.by.math.scores <- ods.participation.by.test.scores %>%
  filter(test_type == "Math")

ods.participation.by.math.scores$grouping <- factor(ods.participation.by.math.scores$grouping,
                                                    levels = rev(ods.participation.by.math.scores$grouping))


## For individual subjects

ods.participation.graph(ods.participation.by.science.scores, "science scores", ods.blue)
ods.participation.graph(ods.participation.by.ela.scores, "ela scores", ods.dark.gray)
ods.participation.graph(ods.participation.by.math.scores, "math scores", ods.dark.gray)







#### PROGRAM CHARACTERISTICS ####

#### Length of programs (days/nights) ####

## Days and nights

program.length.days.nights <- schools.data %>%
  select(How.many.DAYS.does.your.Outdoor.School.program.last.:How.many.NIGHTS.does.your.Outdoor.School.program.last.) %>%
  gather() %>%
  group_by(key) %>%
  count(value) %>%
  filter(!is.na(value)) %>%
  set_names(c("days_nights", "length", "number")) %>%
  ungroup()
  
program.length.days.nights$length <- as.character(program.length.days.nights$length)
program.length.days.nights$length <- c("One", "Two", "Three", "Four", "Five", "Six", "Seven",
                                       "Zero", "One", "Two", "Three", "Four", "Five", "Six")


program.length.days.nights$days_nights <- str_replace(program.length.days.nights$days_nights, "How.many.DAYS.does.your.Outdoor.School.program.last.", "Days")
program.length.days.nights$days_nights <- str_replace(program.length.days.nights$days_nights, "How.many.NIGHTS.does.your.Outdoor.School.program.last.", "Nights")


# program.length.days.nights <- program.length.days.nights %>%
#   group_by(days_nights) %>%
#   mutate(fillcolor = top)


ggplot(program.length.days.nights, aes(x = length, y = number, 
                                       fill = factor(length), color = factor(length))) +
  geom_bar(stat = "identity") +
  geom_text(label = program.length.days.nights$number,
            hjust = -.7,
            size = ods.text.size * .3) +
  coord_flip() +
  # scale_color_manual(values = (c(ods.dark.gray, ods.dark.gray, ods.dark.gray,
  #                                ods.blue, ods.blue,
  #                                ods.dark.gray, ods.dark.gray))) +
  # scale_fill_manual(values = (c(ods.dark.gray, ods.dark.gray, ods.dark.gray,
  #                               ods.blue, ods.blue,
  #                               ods.dark.gray, ods.dark.gray))) +
  # scale_y_continuous(limits = c(0, 240)) +
  # facet_wrap(~days_nights) +
  ods.bar.theme


## Days

program.length.days <- program.length.days.nights %>%
  filter(days_nights == "Days")

program.length.days$length <- factor(program.length.days$length,
                                     levels = rev(program.length.days$length))


ggplot(program.length.days, aes(x = length, y = number, 
                                fill = factor(length), color = factor(length))) +
  geom_bar(stat = "identity") +
  geom_text(label = program.length.days$number,
            hjust = -.7,
            size = ods.text.size * .3) +
  coord_flip() +
  scale_color_manual(values = (c(ods.dark.gray, ods.dark.gray, ods.dark.gray,
                                ods.blue, ods.blue,
                                ods.dark.gray, ods.dark.gray))) +
  scale_fill_manual(values = (c(ods.dark.gray, ods.dark.gray, ods.dark.gray,
                                ods.blue, ods.blue,
                                ods.dark.gray, ods.dark.gray))) +
  scale_y_continuous(limits = c(0, 240)) +
  # labs(title = "Days") +
  ods.bar.theme

ggsave("plots/program characteristics/program length days.png", dpi = 300, 
       height = nrow(program.length.days) / 2)

## Nights


program.length.nights <- program.length.days.nights %>%
  filter(days_nights == "Nights")

program.length.nights$length <- factor(program.length.nights$length,
                                       levels = rev(program.length.nights$length))


ggplot(program.length.nights, aes(x = length, y = number, 
                                fill = factor(length), color = factor(length))) +
  geom_bar(stat = "identity") +
  geom_text(label = program.length.nights$number,
            hjust = -.7,
            size = ods.text.size * .3) +
  coord_flip() +
  scale_color_manual(values = (c(ods.dark.gray, ods.dark.gray, ods.dark.gray,
                                 ods.blue, ods.blue,
                                 ods.dark.gray, ods.dark.gray))) +
  scale_fill_manual(values = (c(ods.dark.gray, ods.dark.gray, ods.dark.gray,
                                ods.blue, ods.blue,
                                ods.dark.gray, ods.dark.gray))) +
  scale_y_continuous(limits = c(0, 240)) +
  # labs(title = "Nights") +
  ods.bar.theme 

ggsave("plots/program characteristics/program length nights.png", dpi = 300, 
       height = nrow(program.length.nights) / 2)



#### Curriculum development ####

curriculum.developer <- schools.data %>%
  select(curriculum.outside.provider:curriculum.volunteers) %>%
  gather() %>%
  group_by(key) %>%
  count(value) %>%
  filter(!is.na(value)) %>%
  mutate(pct = round(prop.table(n), digits = 2)) %>%
  filter(value != FALSE) %>%
  # filter(key != "staffing.other") %>%
  select(-value) %>%
  set_names(c("grouping", "number", "pct")) %>%
  mutate(grouping = str_replace(grouping, "curriculum.outside.provider", "Outside provider")) %>%
  mutate(grouping = str_replace(grouping, "curriculum.school.district.staff", "School or district staff")) %>%
  mutate(grouping = str_replace(grouping, "curriculum.volunteers", "Volunteers"))

ggplot(curriculum.developer, aes(x = reorder(grouping, number), y = number, 
                                 fill = factor(grouping),
                                 color = factor(grouping))) +
  geom_bar(stat = "identity") +
  geom_text(label = curriculum.developer$number,
            hjust = -.7,
            size = ods.text.size * .3) +
  coord_flip() +
  scale_fill_manual(values = c(ods.blue, ods.dark.gray, ods.dark.gray)) +
  scale_color_manual(values = c(ods.blue, ods.dark.gray, ods.dark.gray)) +
  scale_y_continuous(limits = c(0, 335)) +
  ods.bar.theme


ggsave("plots/program characteristics/curriculum developer.png", dpi = 300, 
       height = nrow(curriculum.developer))

#### Staffing ####

staffing <- schools.data %>%
  select(contains("staffing")) %>%
  gather() %>%
  group_by(key) %>%
  count(value) %>%
  filter(value == TRUE) %>%
  ungroup () %>%
  select(-value) %>%
  set_names(c("staffer", "number")) %>%
  mutate(staffer = str_replace(staffer, staffer[1], "Community experts")) %>%
  mutate(staffer = str_replace(staffer, staffer[2], "High school mentors")) %>%
  mutate(staffer = str_replace(staffer, staffer[3], "Other")) %>%
  mutate(staffer = str_replace(staffer, staffer[4], "Parents and/or volunteers")) %>%
  mutate(staffer = str_replace(staffer, staffer[5], "Camp/provider staff")) %>%
  filter(staffer != "Other")

ggplot(staffing, aes(x = reorder(staffer, number), y = number, 
                     fill = factor(staffer),
                     color = factor(staffer))) +
  geom_bar(stat = "identity") +
  geom_text(data = staffing,
            label = staffing$number,
            hjust = -.5,
            size = ods.text.size * .3) +
  coord_flip() +
  scale_fill_manual(values = rev(c(ods.dark.gray, ods.dark.gray, ods.dark.gray, ods.blue))) +
  scale_color_manual(values = rev(c(ods.dark.gray, ods.dark.gray, ods.dark.gray, ods.blue))) +
  scale_y_continuous(limits = c(0, 270)) +
  ods.bar.theme

ggsave("plots/program characteristics/staffing.png", dpi = 300, 
       height = nrow(staffing))

#### Academic component ####

academic.component <- schools.data %>%
  select("Does.your.Outdoor.School.program.have.an.academic.component.") %>%
  gather() %>%
  group_by(key) %>%
  count(value) %>%
  ungroup %>%
  filter(!is.na(value)) %>%
  select(one_of("value", "n")) %>%
  set_names(c("response", "number")) %>%
  filter(response != "Yes") %>%
  mutate(pct = round(prop.table(number), digits = 2)) 

ggplot(academic.component, aes(x = reorder(response, pct), y = pct, 
                     fill = factor(response),
                     color = factor(response))) +
  geom_bar(stat = "identity") +
  geom_text(data = academic.component,
            label = percent(academic.component$pct),
            hjust = -.5,
            size = ods.text.size * .3) +
  coord_flip() +
  scale_fill_manual(values = c(ods.dark.gray, ods.blue, ods.dark.gray)) +
  scale_color_manual(values = c(ods.dark.gray, ods.blue, ods.dark.gray)) +
  scale_x_discrete(labels = function(response) str_wrap(response, width = 35)) +
  scale_y_continuous(limits = c(0, 1.1)) +
  ods.bar.theme

ggsave("plots/program characteristics/academic component.png", dpi = 300, 
       height = nrow(academic.component))


#### Accommodations ####

accommodations <- schools.data %>%
  select("To.what.degree.are.you.able.to.make.accommodations.to.ensure.that.the.Outdoor.School.program.is.accessible.for.all.students.") %>%
  gather() %>%
  group_by(key) %>%
  count(value) %>%
  ungroup %>%
  filter(!is.na(value)) %>%
  select(one_of("value", "n")) %>%
  set_names(c("response", "number")) %>%
  mutate(pct = round(prop.table(number), digits = 2)) %>%
  mutate(response = str_replace(response, response[1], "All accommodations")) %>%
  mutate(response = str_replace(response, response[2], "Some accommodations")) %>%
  mutate(response = str_replace(response, response[3], "No accommodations")) 

ggplot(accommodations, aes(x = reorder(response, number), y = number, 
                           fill = factor(response),
                           color = factor(response))) +
  geom_bar(stat = "identity") +
  geom_text(label = accommodations$number,
            hjust = -.5,
            size = ods.text.size * .3) +
  coord_flip() +
  scale_fill_manual(values = c(ods.blue, ods.dark.gray, ods.dark.gray)) +
  scale_color_manual(values = c(ods.blue, ods.dark.gray, ods.dark.gray)) +
  scale_y_continuous(limits = c(0, 210)) +
  ods.bar.theme

ggsave("plots/program characteristics/accommodations.png", dpi = 300, 
       height = nrow(accommodations))

#### By length of programs (years in existence) ####

ods.program.length <- schools.data %>%
  select(contains("How.many.years.has.your.school.sent.students.to.Outdoor.School.")) %>%
  set_names("years") %>%
  count(years) %>%
  set_names(c("years", "number")) %>%
  filter(!is.na(years)) 

ods.program.length$grouped <- cut(ods.program.length$years,
                                   breaks = seq(0, 60, by = 10),
                                   labels = c("0 - 10 years",
                                              "11 - 20 years",
                                              "21 - 30 years",
                                              "31 - 40 years",
                                              "41 - 50 years",
                                              "50+ years"))

ods.program.length <- ods.program.length %>%
  ungroup() %>%
  group_by(grouped) %>%
  select(-years) %>%
  mutate(total_per_group = sum(number)) %>%
  distinct(grouped, .keep_all = TRUE) %>%
  select(-number) %>%
  set_names(c("length", "number"))

ods.program.length$length <- factor(ods.program.length$length, 
                                    levels = rev(ods.program.length$length))

ggplot (ods.program.length, aes(x = length, y = number,
                                fill = factor(length),
                                color = factor(length))) +
  geom_bar(stat = "identity") +
  geom_text(label = ods.program.length$number,
            hjust = -.5,
            size = ods.text.size * .3) +
  scale_fill_manual(values = c(rep(ods.dark.gray, times = 5), ods.blue)) +
  scale_color_manual(values = c(rep(ods.dark.gray, times = 5), ods.blue)) +
  coord_flip() +
  ods.bar.theme

ggsave("plots/program characteristics/program length years in existence.png", dpi = 300, 
       height = nrow(ods.program.length) / 1.5)

## Animated version

ods.program.length.animated <- schools.data %>%
  select(one_of(c("School.Name", 
                  "City", 
                  "Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year.", 
                  "How.many.years.has.your.school.sent.students.to.Outdoor.School.",
                  "lon", 
                  "lat"))) %>%
  filter(!is.na(How.many.years.has.your.school.sent.students.to.Outdoor.School.)) %>%
  mutate(ods.first.year = 2017 - How.many.years.has.your.school.sent.students.to.Outdoor.School.)


ods.program.length.animated.plot <- ggplot() +
  geom_polygon(data = oregon.map,
               aes(x=long, y=lat, group=group),
               color = ods.gray,
               fill = ods.light.gray) +
  geom_point(data = ods.program.length.animated,
             color = ods.blue,
             aes(x = lon, y = lat,
                 frame = ods.first.year, 
                 cumulative = TRUE)) +
 labs(title = "Outdoor School programs in") +
    coord_map() +
    ods.map.theme +
  theme(title = element_text(color = ods.blue,
                             face = "bold",
                             size = ods.text.size))


gganimate(ods.program.length.animated.plot, "plots/animated/ods-programs-first-year-animated.gif")


#### PARTICIPATION MAPS ####

## Get maps

oregon.map <- map_data("state") %>%
  filter(region == "oregon")

ods.states.map <- map_data("state", region = c("oregon", "washington", "idaho"))

oregon.map.by.county <- map_data("county") %>%
  filter(region == "oregon")

schools.data$lon <- as.numeric(schools.data$lon)
schools.data$lat <- as.numeric(schools.data$lat)

#### Overall participation map ####

ggplot() +
  geom_polygon(data = oregon.map, 
               aes(x=long, y=lat, group=group),
               color = ods.gray,
               fill = ods.light.gray) +
  geom_point(data = filter(schools.data, Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year. == "Yes"),
             color = ods.blue,
             aes(x = lon, y = lat)) +
  ods.map.theme

ggsave("plots/current programs/ods participation map - participated.png", dpi = 300)

ggplot() +
  geom_polygon(data = oregon.map, 
               aes(x=long, y=lat, group=group),
               color = ods.gray,
               fill = ods.light.gray) +
  geom_point(data = filter(schools.data, Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year. == "No"),
             color = ods.red,
             aes(x = lon, y = lat)) +
  ods.map.theme

ggsave("plots/current programs/ods participation map - did not participate.png", dpi = 300)

## Not all interested schools

not.at.all.interested <- schools.data %>%
  filter(Is.your.school.interested.in.starting.an.Outdoor.School.program.in.the.future. == "NOT AT ALL interested") %>%
  filter(Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year. == "No")

ggplot() +
  geom_polygon(data = oregon.map, 
               aes(x=long, y=lat, group=group),
               color = ods.gray,
               fill = ods.light.gray) +
  geom_point(data = not.at.all.interested,
             color = ods.red,
             aes(x = lon, y = lat)) +
  ods.map.theme 

ggsave("plots/future programs/not at all interested schools.png", dpi = 300)



#### ODS participation by county ####

ods.participation.by.county <- schools.data %>%
  group_by(County) %>%
  count(Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year.) %>%
  set_names(c("County", "response", "number")) %>%
  filter(response == "No" | response == "Yes") %>%
  mutate(pct = round(prop.table(number), digits = 2)) %>%
  filter(!is.na(County)) %>%
  filter(response == "Yes")

ods.participation.by.county$pct.text <- cut(ods.participation.by.county$pct, breaks = seq(0,1, by = .25),
                                            labels = c("0 - 25", 
                                                       "25 - 50",
                                                       "50 - 75",
                                                       "75 - 100"))

ods.participation.by.county$County <- str_to_lower(ods.participation.by.county$County)

ods.participation.by.county <- left_join(ods.participation.by.county, oregon.map.by.county, by = c("County" = "subregion"))

ggplot(ods.participation.by.county, aes(x=long, y=lat, group=group, 
                                        fill = factor(pct.text))) +
  geom_polygon(color = "white") +
  scale_fill_manual(name = "Percent participation",
                    values = c("#EAEEF1", "#B6CADD", "#6894BF", ods.blue)) +
  # labs(title = "Outdoor School participation rates vary widely, \nbut in five counties fewer than 25 percent of schools participate") +
  ods.map.theme + theme(plot.title = element_text(size = 25, color = ods.blue, face = "bold"))

ggsave("plots/current programs/ods participation by county map.png", dpi = 300)

ggplot(ods.participation.by.county, aes(x=long, y=lat, group=group, 
                                        fill = factor(pct.text))) +
  geom_polygon(color = "white") +
  scale_fill_manual(name = "Percent participation",
                    values = c("#EAEEF1", "#B6CADD", "#6894BF", ods.blue)) +
  labs(title = "Outdoor School participation rates vary widely, \nbut in five counties fewer than 25 percent of schools participate") +
  coord_map() +
  ods.map.theme + theme(plot.title = element_text(size = 25, color = ods.blue, face = "bold"))

ggsave("plots/current programs/ods participation by county map with title.png", dpi = 300)

## Animated version

ggplot(oregon.map, aes(x=long, y=lat, group=group)) +
  geom_polygon()



oregon.map.bg1 <- oregon.map %>%
  mutate(pct.text = "0 - 25")

oregon.map.bg2 <- oregon.map %>%
  mutate(pct.text = "25 - 50")

oregon.map.bg3 <- oregon.map %>%
  mutate(pct.text = "50 - 75")

oregon.map.bg4 <- oregon.map %>%
  mutate(pct.text = "75 - 100")

oregon.map.bg <- bind_rows(oregon.map.bg1, 
                           oregon.map.bg2,
                           oregon.map.bg3,
                           oregon.map.bg4)

oregon.map.bg %>%
  mutate(pct.text = factor(pct.text, levels = c  ("0 - 25", 
                                                  "25 - 50",
                                                  "50 - 75",
                                                  "75 - 100")))
  

  

  


ods.participation.by.county.animated <-
  ggplot(ods.participation.by.county, 
         aes(x=long, y=lat, 
             group=group,
             fill = factor(pct.text))) +
  geom_polygon(fill = ods.light.gray) +
  geom_polygon(aes(frame = pct.text), 
               fill = ods.blue) +
  coord_map() +
  
  ods.map.theme + 
  theme(plot.title = element_text(size = 25, 
                                  color = ods.blue, 
                                  face = "bold"))

gganimate(ods.participation.by.county.animated,
          interval = 2.5,
          filename = "plots/animated/ods-participation-by-county.gif")




#### PAST PROGRAMS ####

#### Has school participated in past ####

participated.in.past <- schools.data %>%
  select(contains("Has.your.school.sent.students.to.Outdoor.School.prior.to.the.2016.2017.school.year.")) %>%
  gather() %>%
  count(value) %>%
  filter(!is.na(value)) %>%
  mutate(value = str_replace(value, "I'm not sure", "Not sure")) %>%
  set_names(c("response", "number")) %>%
  mutate(response = factor(response, levels = c("Not sure", "No", "Yes"))) %>%
  mutate(pct = round(prop.table(number), digits = 2)) 



ggplot(participated.in.past, aes(x = response, y = pct, 
                              fill = factor(response),
                              color = factor(response))) +
  geom_bar(stat = "identity") +
  geom_text(label = percent(participated.in.past$pct),
            hjust = -.7,
            size = ods.text.size * .3) +
  coord_flip() +
  scale_fill_manual(values = c("white", ods.red, ods.dark.gray)) +
  scale_color_manual(values = c(ods.dark.gray, ods.red, ods.dark.gray)) +
  scale_y_continuous(limits = c(0, .55)) +
  ods.bar.theme

ggsave("plots/past programs/participated in past.png", dpi = 300,
       height = nrow(participated.in.past) * .9)

#### Last year schools participated in ODS ####

last.participated <- schools.data %>%
  select(contains("When.was.the.last.time.that.students.participated.in.Outdoor.School.")) %>%
  gather() %>%
  count(value) %>%
  filter(value != "No" & value != "so long ago that I'm not sure" & value != "when we still had 6th graders") 

last.participated$value <- last.participated$value %>%
  str_replace("1998-2003", "2002-2003 school year") %>%
  str_replace("2000-201", "2000-2001 school year")

last.participated <- last.participated %>%
  arrange(value) %>%
  set_names("year", "number")

ggplot(last.participated, aes(x = year, y = number, 
                              fill = factor(year),
                              color = factor(year))) +
  geom_bar(stat = "identity") +
  geom_text(label = last.participated$number,
            hjust = -.7,
            size = ods.text.size * .3) +
  coord_flip() +
  scale_fill_manual(values = c(rep(ods.dark.gray, times = 9), ods.red)) +
  scale_color_manual(values = c(rep(ods.dark.gray, times = 9), ods.red)) +
  ods.bar.theme

ggsave("plots/past programs/last participated.png", dpi = 300,
       height = nrow(last.participated) / 2)

#### Camp last time attended ####

last.participated.camp <- schools.data %>%
  select(contains("Which.camp.or.camps.did.students.attend.the.last.time.your.school.participated.in.Outdoor.School.")) %>%
  set_names("camp") %>%
  filter(!is.na(camp))

last.participated.camp$camp <- last.participated.camp$camp %>%
  str_replace_all("Sandy, MT. Hood", "Other") %>%
  str_replace_all("Polk Soil & Water Commission Organized", "Other") %>%
  str_replace_all("We designed an outdoor school with BLM and Fish and Wildlife", "Other") %>%
  str_replace_all("Other an outdoor school", "Other") %>%
  str_replace_all("Catherine Creek Campground  ", "Other") 

last.participated.camp <- last.participated.camp %>%
  cSplit("camp", "Camp Howard, Camp Westwind", sep = ",") %>%
  count(camp) %>%
  set_names(c("camp", "number")) 

# last.participated.camp$camp <- factor(last.participated.camp$camp, levels = last.participated.camp$number)

ggplot(last.participated.camp, aes(x = reorder(camp, number), y = number, 
                                   fill = factor(camp),
                                   color = factor(camp))) +
  geom_bar(stat = "identity") +
  geom_text(label = last.participated.camp$number,
            hjust = -.7,
            size = ods.text.size * .3) +
  scale_fill_manual(values = c(rep(ods.dark.gray, times = 9), ods.red, ods.dark.gray, ods.red, rep(ods.dark.gray, times = 3))) +
  scale_color_manual(values = c(rep(ods.dark.gray, times = 9), ods.red, ods.dark.gray, ods.red, rep(ods.dark.gray, times = 3))) +
  coord_flip() +
  ods.bar.theme

ggsave("plots/past programs/last participated camp.png", dpi = 300,
       height = nrow(last.participated.camp) / 2)

#### Why stoped going ####

why.stopped.going <- schools.data %>%
  select(contains("Why.did.your.school.stop.sending.students.to.Outdoor.School.")) %>%
  set_names("reason") %>%
  filter(!is.na(reason))

why.stopped.going$financial <- ""
why.stopped.going$financial[c(1, 3, 4, 5, 6, 7, 10, 16)] <- "x"

why.stopped.going$staffing <- ""
why.stopped.going$staffing[c(1, 10, 19, 24)] <- "x"

why.stopped.going$no_students <- ""
why.stopped.going$no_students[c(8, 21)] <- "x"

why.stopped.going$program_not_available <- ""
why.stopped.going$program_not_available[c(17, 22)] <- "x"

why.stopped.going$changed_grades <- ""
why.stopped.going$changed_grades[c(9, 11, 12, 13, 14, 15, 18)] <- "x"

why.stopped.going$every_other_year <- ""
why.stopped.going$every_other_year[c(20, 23)] <- "x"

why.stopped.going <- why.stopped.going %>%
  select(-reason) %>%
  gather() %>%
  group_by(key) %>%
  count(value) %>%
  filter(value == "x") %>%
  select(-value) %>%
  set_names(c("reason", "number")) %>%
  ungroup()

why.stopped.going$reason <- c("Change in grades doing Outdoor School",
                              "Do program every other year",
                              "Cost",
                              "No students in 5th or 6th grade",
                              "Could not continue to work with Outdoor School provider",
                              "Lack of interest from teachers")


ggplot(why.stopped.going, aes(x = reorder(reason, number), y = number, 
                              fill = factor(reason),
                              color = factor(reason))) +
  geom_bar(stat = "identity") +
  geom_text(label = why.stopped.going$number,
            hjust = -.7,
            size = ods.text.size * .3) +
  scale_x_discrete(labels = function(reason) str_wrap(reason, width = 35)) +
  scale_fill_manual(values = c(ods.red, ods.red, rep(ods.dark.gray, times = 4))) +
  scale_color_manual(values = c(ods.red, ods.red, rep(ods.dark.gray, times = 4))) +
  coord_flip() +
  ods.bar.theme

ggsave("plots/past programs/why stopped going.png", dpi = 300, 
       height = nrow(why.stopped.going) * .65)

#### FUTURE PROGRAMS ####

#### Interest in starting ODS program ####

interest.in.ods <- schools.data %>%
  filter(`Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year.` != "Sister school did") %>%
  select(contains("Is.your.school.interested.in.starting.an.Outdoor.School.program.in.the.future.")) %>%
  set_names("interest") %>%
  group_by(interest) %>%
  count() %>%
  filter(interest != "NOT AT ALL interested, SOMEWHAT interested") %>%
  filter(interest != "Yes") %>%
  set_names(c("interest", "number")) %>%
  ungroup() %>%
  mutate(pct = round(prop.table(number), digits = 2))

interest.in.ods$interest <- c("A bit", "Not sure", "Not at all", "Somewhat", "Very")

interest.in.ods$interest <- factor(interest.in.ods$interest, 
                                   levels = c("Not sure",
                                               "Not at all",
                                               "A bit",
                                               "Somewhat",
                                               "Very"))



ggplot(interest.in.ods, aes(x = interest, y = pct, 
                            fill = factor(interest), 
                            color = factor(interest))) +
  geom_bar(stat = "identity") +
  geom_text(label = percent(interest.in.ods$pct),
            hjust = -.5,
            size = ods.text.size * .3) +
  scale_fill_manual(values = c("white", rep(ods.dark.gray, times = 2), ods.red, ods.red)) +
  scale_color_manual(values = c(ods.dark.gray, rep(ods.dark.gray, times = 2), ods.red, ods.red)) +
  scale_y_continuous(limits = c(0, .35)) +
  coord_flip() +
  ods.bar.theme

ggsave("plots/future programs/interest in starting ODS program.png", dpi = 300, 
       height = nrow(interest.in.ods) * .8)

#### When might be interested in starting program ####

when.start.program <- schools.data %>%
  filter(`Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year.` != "Sister school did") %>%
  select(contains("When.do.you.think.your.might.be.ready.to.start.an.Outdoor.School.program.")) %>%
  set_names("year") %>%
  group_by(year) %>%
  count() %>%
  filter(year == "2017/2018 school year" | year == "2018/2019 school year" |
           year == "2019/2020 school year" | year == "Not sure at this point") %>%
  set_names(c("year", "number")) %>%
  ungroup() %>%
  mutate(pct = round(prop.table(number), digits = 2))

when.start.program$year <- str_replace(when.start.program$year, "Not sure at this point", "Not sure")

when.start.program$year <- factor(when.start.program$year, levels = rev(c("2017/2018 school year",
                                                                      "2018/2019 school year",
                                                                      "2019/2020 school year",
                                                                      "Not sure")))

ggplot(when.start.program, aes(x = year, y = pct, 
                               fill = factor(year), 
                               color = factor(year))) +
  geom_bar(stat = "identity") +
  geom_text(label = percent(when.start.program$pct),
            hjust = -.5,
            size = ods.text.size * .3) +
  scale_fill_manual(values = c("white", rep(ods.dark.gray, times = 2), ods.red)) +
  scale_color_manual(values = c(rep(ods.dark.gray, times = 3), ods.red)) +
  scale_y_continuous(limits = c(0, .5)) +
  coord_flip() +
  ods.bar.theme

ggsave("plots/future programs/when want to start ODS program.png", dpi = 300, 
       height = nrow(when.start.program) * .8)



#### CAMPS ####

schools.geodata <- schools.data %>%
  select(one_of(c("School.Name...City", "lon", "lat")))


camps.with.associated.schools <- read.csv("data/camps.csv") %>%
  select(-(Camp.Owner:X.Notes.by.Friends.of.Outdoor.School..)) %>%
  select(-(Coordinates)) %>%
  filter(!is.na(lon)) %>%
  cSplit("Schools", sep =",") %>%
  gather(camp, school, Schools_01:Schools_50) %>%
  filter(!is.na(school)) %>%
  select(one_of("Camp.Name", "lon", "lat", "school")) %>%
  set_names(c("camp", "lon", "lat", "school")) %>%
  left_join(schools.geodata, by = c("school" = "School.Name...City")) %>%
  set_names(c("camp", "lon.camp", "lat.camp", "school", "lon.school", "lat.school")) %>%
  group_by(camp) %>%
  ungroup() %>%
  mutate(lon.school = as.numeric(lon.school)) %>%
  mutate(lat.school = as.numeric(lat.school))

camps.with.associated.schools.hancock <- camps.with.associated.schools %>%
  filter(camp == "Hancock Field Station")

camps.with.associated.schools.gray <- camps.with.associated.schools %>%
  filter(camp == "Coastal Discovery Center at Camp Gray")

camps.with.associated.schools.tamarack <- camps.with.associated.schools %>%
  filter(camp == "Camp Tamarack")

#### Plot camps and associated schools ####


## Camp Gray

ggplot() +
  geom_polygon(data=oregon.map, 
               aes(x=long, y=lat, group=group), 
               fill=ods.light.gray, 
               color = ods.gray,
               alpha=0.5) +
  geom_text(data = camps.with.associated.schools.gray[1,],
            aes(x = lon.camp, y = lat.camp),
            label = "Camp Gray",
            color = ods.green,
            vjust = 1.8,
            size = ods.text.size * .3) +
  geom_point(data = camps.with.associated.schools.gray[1,],
             color = ods.green,
             size = 5,
             aes(x = lon.camp, y = lat.camp)) +
  geom_point(data = camps.with.associated.schools.gray,
             color = ods.blue,
             aes(x = lon.school, y = lat.school)) +
  ods.map.theme

ggsave("plots/camps/schools per camp - gray.png", dpi = 300, height = 5, width = 5 * 1.5)

## Hancock
# 
# ggplot() +
#   geom_polygon(data=oregon.map, 
#                aes(x=long, y=lat, group=group), 
#                fill=ods.light.gray, 
#                color = ods.gray,
#                alpha=0.5) +
#   geom_text(data = camps.with.associated.schools.hancock[1,],
#             aes(x = lon.camp, y = lat.camp),
#             label = "Hancock Field Station",
#             color = ods.green,
#             hjust = -.1,
#             size = ods.text.size * .25) +
#   geom_point(data = camps.with.associated.schools.hancock[1,],
#              color = ods.green,
#              size = 5,
#              aes(x = lon.camp, y = lat.camp)) +
#   geom_point(data = camps.with.associated.schools.hancock,
#              color = ods.blue,
#              aes(x = lon.school, y = lat.school)) +
#   ods.map.theme
# 
# ggsave("plots/camps/schools per camp - hancock.png", dpi = 300)

## Tamarack

ggplot() +
  geom_polygon(data=oregon.map, 
               aes(x=long, y=lat, group=group), 
               fill=ods.light.gray, 
               color = ods.gray,
               alpha=0.5) +
  geom_text(data = camps.with.associated.schools.tamarack[1,],
            aes(x = lon.camp, y = lat.camp),
            label = "Camp Tamarack",
            color = ods.green,
            hjust = 1.1,
            size = ods.text.size * .3) +
  geom_point(data = camps.with.associated.schools.tamarack[1,],
             color = ods.green,
             size = 5,
             aes(x = lon.camp, y = lat.camp)) +
  geom_point(data = camps.with.associated.schools.tamarack,
             color = ods.blue,
             aes(x = lon.school, y = lat.school)) +
  ods.map.theme

ggsave("plots/camps/schools per camp - tamarack.png", dpi = 300)


## Animated

camps.with.associated.schoool.animated.data <- camps.with.associated.schools %>%
  mutate(camp = str_replace(camp, "Camp Lutherwood Oregon", "Camp Lutherwood"))


camps.with.associated.schoool.animated <- 
  ggplot() +
  geom_polygon(data=ods.states.map, 
               aes(x=long, y=lat, group=group), 
               fill=ods.light.gray, 
               color = ods.gray) +
  geom_point(data = camps.with.associated.schoool.animated.data,
             color = ods.green,
             size = 5,
             aes(x = lon.camp, y = lat.camp,
                 frame = camp)) +
  geom_point(data = camps.with.associated.schoool.animated.data,
             color = ods.blue,
             aes(x = lon.school, y = lat.school,
                 frame = camp)) +
  coord_map() +
  ods.map.theme + 
  labs(subtitle = "Schools that attend each site are shown as blue dots") +
  theme(title = element_text(color = ods.green,
                             face = "bold",
                             size = ods.text.size * .75),
        plot.subtitle = element_text(color = ods.blue,
                                     face = "plain",
                             size = ods.text.size * .5))

gganimate(camps.with.associated.schoool.animated,
          interval = 2,
          filename = "plots/animated/camps-with-associated-schools.gif")

#### Number of schools that each camp hosts ####

camps.number.of.schools <- read.csv("data/camps.csv") %>%
  select(one_of(c("Camp.Name", "Schools", "lon", "lat"))) %>%
  mutate(number_of_schools = str_count(Schools, ",")) %>%
  mutate(Camp.Name = str_replace(Camp.Name, "Coastal Discovery Center at Camp Gray", "Camp Gray")) %>%
  filter(number_of_schools > 0) 

camps.number.of.schools.graph <- camps.number.of.schools %>%
  filter(number_of_schools >= 5) 

#### Graph of number of schools hosted by each camp ####

ggplot(camps.number.of.schools.graph, aes(x = reorder(Camp.Name, number_of_schools), y = number_of_schools, 
                                    fill = factor(Camp.Name),
                                    color = factor(Camp.Name))) +
  geom_bar(stat = "identity") +
  geom_text(label = camps.number.of.schools.graph$number_of_schools,
            hjust = -.5, 
            size = ods.text.size * .25) +
  scale_fill_manual(values = c(rep(ods.dark.gray, times = 21), ods.blue,
                               rep(ods.dark.gray, times = 16))) +
  scale_color_manual(values = c(rep(ods.dark.gray, times = 21), ods.blue,
                               rep(ods.dark.gray, times = 16))) +
  scale_y_continuous(limits = c(0, 55)) +
  coord_flip() +
  ods.bar.theme

ggsave("plots/camps/number of schools each camp hosts - graph.png", dpi = 300)

#### Map of number of schools at each camp ####

ggplot(camps.number.of.schools, aes(group = Camp.Name)) +
  geom_polygon(data=ods.states.map, 
               aes(x=long, y=lat, group=group), 
               fill=ods.light.gray, 
               color = ods.gray,
               alpha=0.5) +
  geom_point(data = camps.number.of.schools,
             color = ods.green,
             aes(x = lon, y = lat),
             size = camps.number.of.schools$number_of_schools / 2,
             alpha = 0.75) +
  ods.map.theme

ggsave("plots/camps/number of schools each camp hosts - map.png", dpi = 300)



#### MEASURE 99 ####

m99.awareness.general <- schools.data %>%
  select(one_of(c("How.familiar..if.at.all..are.you.with.Measure.99..which.passed.in.November.2016.and.dedicates.funding.to.schools.to.send.students.to.Outdoor.School."))) %>%
  set_names("m99_awareness") %>%
  group_by (m99_awareness) %>%
  count() %>%
  filter(!is.na(m99_awareness)) %>%
  ungroup() %>%
  mutate(pct = round(prop.table(n), digits = 2)) %>%
  mutate(m99_awareness = str_replace(m99_awareness, " with Measure 99", ""))  %>%
  mutate()

m99.awareness.general$m99_awareness <- c("A bit", "Not at all", "Somewhat", "Very")
m99.awareness.general$m99_awareness <- factor(m99.awareness.general$m99_awareness,
                                              levels = c("Not at all", 
                                                         "A bit", 
                                                         "Somewhat", 
                                                         "Very"))

ggplot(m99.awareness.general, aes(x = m99_awareness, y = pct,
                                  fill = factor(m99_awareness),
                                  color = factor(m99_awareness))) +
  geom_bar(stat = "identity") +
  geom_text(label = percent(m99.awareness.general$pct),
            hjust = -.2,
            size = ods.text.size * .3) +
  coord_flip() +
  scale_fill_manual(values = c(ods.dark.gray, ods.dark.gray, ods.blue, ods.dark.gray)) +
  scale_color_manual(values = c(ods.dark.gray, ods.dark.gray, ods.blue, ods.dark.gray)) +
  scale_y_continuous(limits = c(0, .5)) +
  ods.bar.theme

ggsave("plots/measure 99/measure 99 awareness - general.png", dpi = 300,
       height = nrow(m99.awareness.general))


m99.awareness.by.ods.participation <- schools.data %>%
  select(one_of(c("Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year.",
                "How.familiar..if.at.all..are.you.with.Measure.99..which.passed.in.November.2016.and.dedicates.funding.to.schools.to.send.students.to.Outdoor.School."))) %>%
  set_names(c("participation", "m99_awareness")) %>%
  filter(!is.na(m99_awareness)) %>%
  group_by(participation, m99_awareness) %>%
  count() %>%
  ungroup() %>%
  group_by(participation) %>%
  mutate(pct = round(prop.table(n), digits = 2)) %>%
  mutate(m99_awareness = str_replace(m99_awareness, " with Measure 99", "")) %>%
  filter(participation != "Sister school did") %>%
  ungroup() %>%
  mutate(participation = str_replace(participation, "Yes", "DID participate")) %>%
  mutate(participation = str_replace(participation, "No", "DID NOT participate"))

m99.awareness.by.ods.participation$m99_awareness <- c("A bit", "Not at all", "Somewhat", "Very")
m99.awareness.by.ods.participation$m99_awareness <- factor(m99.awareness.by.ods.participation$m99_awareness,
                                              levels = c("Not at all", 
                                                         "A bit", 
                                                         "Somewhat", 
                                                         "Very"))


m99.awareness.by.ods.participation$fill <- c(ods.red,
                                             ods.dark.gray,
                                             ods.dark.gray,
                                             ods.dark.gray,
                                             ods.blue,
                                             ods.dark.gray,
                                             ods.dark.gray,
                                             ods.dark.gray)

m99.awareness.by.ods.participation$color <- c(ods.dark.gray,
                                             ods.red,
                                             ods.dark.gray,
                                             ods.dark.gray,
                                             ods.dark.gray,
                                             ods.blue,
                                             ods.dark.gray,
                                             ods.dark.gray)


ggplot(m99.awareness.by.ods.participation, aes(x = m99_awareness, y = pct)) +
  geom_bar(stat = "identity",
           fill = m99.awareness.by.ods.participation$fill) +
  geom_text(label = percent(m99.awareness.by.ods.participation$pct),
            hjust = -.2,
            size = ods.text.size * .3,
            color = m99.awareness.by.ods.participation$color) +
  coord_flip() +
  scale_y_continuous(limits = c(0, .5)) +
  facet_wrap(~participation) +
  ods.bar.theme.faceted

ggsave("plots/measure 99/measure 99 awareness - by ODS participation.png", dpi = 300,
       height = nrow(m99.awareness.general))

#### Familiarity with OSU Extension ####

## By general

familiarity.with.osu.general <- schools.data %>%
  select(contains("Oregon.State.Extension")) %>%
  set_names("familiarity") %>%
  group_by (familiarity) %>%
  count() %>%
  filter(!is.na(familiarity)) %>%
  ungroup() %>%
  mutate(pct = round(prop.table(n), digits = 2)) %>%
  mutate(familiarity = str_replace(familiarity, " with the role OSU Extension plays", ""))

familiarity.with.osu.general$familiarity <- c("A bit", "Not at all", "Somewhat", "Very")
familiarity.with.osu.general$familiarity <- factor(familiarity.with.osu.general$familiarity,
                                                           levels = c("Not at all", 
                                                                      "A bit", 
                                                                      "Somewhat", 
                                                                      "Very"))
  

ggplot(familiarity.with.osu.general, aes(x = familiarity, y = pct,
                                  fill = factor(familiarity),
                                  color = factor(familiarity))) +
  geom_bar(stat = "identity") +
  geom_text(label = percent(familiarity.with.osu.general$pct),
            hjust = -.2,
            size = ods.text.size * .3) +
  coord_flip() +
  scale_fill_manual(values = c(ods.blue, ods.dark.gray, ods.dark.gray, ods.dark.gray)) +
  scale_color_manual(values = c(ods.blue, ods.dark.gray, ods.dark.gray, ods.dark.gray)) +
  scale_y_continuous(limits = c(0, .55)) +
  ods.bar.theme

ggsave("plots/measure 99/familiarity with osu - general.png", dpi = 300,
       height = nrow(familiarity.with.osu.general))


## By participation
familiarity.with.osu.by.ods.participation <- schools.data %>%
  select(one_of(c("Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year.",
                  "How.familiar..if.at.all..are.you.with.the.role.that.Oregon.State.Extension.plays.in.funding.Outdoor.School.programs."))) %>%
  set_names(c("participation", "familiarity")) %>%
  filter(!is.na(familiarity)) %>%
  group_by(participation, familiarity) %>%
  count() %>%
  ungroup() %>%
  group_by(participation) %>%
  mutate(pct = round(prop.table(n), digits = 2)) %>%
  # mutate(m99_awareness = str_replace(m99_awareness, " with Measure 99", "")) %>%
  filter(participation != "Sister school did") %>%
  ungroup() %>%
  mutate(familiarity = str_replace(familiarity, " with the role OSU Extension plays", "")) %>%
  mutate(participation = str_replace(participation, "Yes", "DID Participate")) %>%
  mutate(participation = str_replace(participation, "No", "DID NOT participate"))

familiarity.with.osu.by.ods.participation$familiarity <- c("A bit", "Not at all", "Somewhat", "Very")
familiarity.with.osu.by.ods.participation$familiarity <- factor(familiarity.with.osu.by.ods.participation$familiarity,
                                                           levels = c("Not at all", 
                                                                      "A bit", 
                                                                      "Somewhat", 
                                                                      "Very"))
familiarity.with.osu.by.ods.participation$color <- c(ods.dark.gray,
                                                     ods.red,
                                                     ods.dark.gray,
                                                     ods.dark.gray,
                                                     ods.dark.gray,
                                                     ods.blue,
                                                     ods.dark.gray,
                                                     ods.dark.gray)

familiarity.with.osu.by.ods.participation$fill <- c(ods.red,
                                                     ods.dark.gray,
                                                     ods.dark.gray,
                                                     ods.dark.gray,
                                                     ods.blue,
                                                     ods.dark.gray,
                                                     ods.dark.gray,
                                                     ods.dark.gray)



ggplot(familiarity.with.osu.by.ods.participation, aes(x = familiarity, y = pct)) +
  geom_bar(stat = "identity", 
           fill = familiarity.with.osu.by.ods.participation$fill) +
  geom_text(label = percent(familiarity.with.osu.by.ods.participation$pct),
            hjust = -.2,
            size = ods.text.size * .3,
            color = familiarity.with.osu.by.ods.participation$color) +
  coord_flip() +
  # scale_fill_manual(values = c(ods.red, ods.blue)) +
  # scale_color_manual(values = c(ods.red, ods.blue)) +
  scale_y_continuous(limits = c(0, .65)) +
  facet_wrap(~participation) +
  ods.bar.theme.faceted


ggsave("plots/measure 99/familiarity with osu - by ODS participation.png", dpi = 300,
       height = nrow(familiarity.with.osu.general))

#### Save data for Tableau ####

temp <- data.frame((colnames(schools.data)))

schools.data.tableau <- schools.data %>%
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
                  "X2016.17....White.",
                  "lon",
                  "lat"))) %>%
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
              "pct.white",
              "lon",
              "lat")) %>%
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
  mutate(staffing.other = str_replace(staffing.other, "FALSE", "No")) %>%
  # mutate(ods.academic.component = str_replace(ods.academic.component, "Yes\b", "")) %>%
  # mutate(curriculum = paste(curriculum.outside.provider, curriculum.school.district.staff, curriculum.volunteers, sep = ", "))
  cSplit("ods.camp", sep = ",", drop = FALSE)

schools.data.tableau[ods.academic.component == "Yes"]$ods.academic.component <- NA
schools.data.tableau$ods.camp[51] <- "Camp Magruder, Coastal Discovery Center at Camp Gray"
schools.data.tableau$ods.camp[85] <- NA
schools.data.tableau$ods.camp[442] <- NA
schools.data.tableau$ods.camp[496] <- NA
schools.data.tableau$ods.camp[517] <- NA
schools.data.tableau$ods.camp[750] <- NA
                       


temp <- data.frame(schools.data.tableau$ods.camp)

write_csv(schools.data.tableau, "tableau/tableau - schools.csv", na = "")

# tableau.data.gsheet <- gs_title("ODS Data for Tableau")
# gs_edit_cells(tableau.data.gsheet,
#               ws = 1,
#               input = schools.data.tableau,
#               anchor = "A1",
#               trim = TRUE,
#               col_names = TRUE,
#               verbose = TRUE)
# 
# 


