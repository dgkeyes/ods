#################### INSTALL PACKAGES ####################

require('ggplot2')
require("dplyr")
require("tidyr")
require("maps")
require("plotly")

#################### GET DATA ####################

# Schools data

schoolsdata <- read.csv ("archive/schools.csv")
schoolsdata <- filter(schoolsdata, schoolsdata$School.Name...City != "")

schools.with.ods <- filter(schoolsdata, schoolsdata$Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year. != "")
schools.wo.ods <- filter(schoolsdata, schoolsdata$Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year. == "No")

# Camps data

camps <- read.csv("archive/camps.csv")
camps <- filter(camps, camps$Camp.Name != "")


#################### MAPPING ####################

# Get basemaps

oregon.map <- map_data("state", "oregon")
ODS.states <- map_data("state", region = c("oregon", "washington", "idaho"))

# Schools that do ODS

ggplot() +
  
  geom_polygon(data=oregon.map, 
               aes(x=long, y=lat, group=group), 
               fill="#a9a9a9", 
               color = "#a9a9a9",
               alpha=0.5) +
  
  geom_point(data = subset(schools.data, Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year. == "Yes"),
             color = "#5c9134",
             aes(x = lon, y = lat)) +
  
  theme_bw() + dkmaptheme


ggsave("plots/schools.with.ods.png", width = 10, units ="in", dpi = 300)


# Schools that don't do ODS

schools.wo.ods.map <- ggplot() +
  
  geom_polygon(data=oregon.map, 
               aes(x=long, y=lat, group=group), 
               fill="#a9a9a9", 
               color = "#a9a9a9",
               alpha=0.5) +
  
  geom_point(data = subset(schoolsdata, Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year. == "No"),
             color = "#c63320",
             aes(x = lon, y = lat)) +
  
  theme_bw() + dkmaptheme

schools.wo.ods.map

ggsave("plots/schools.without.ods.png", plot = schools.wo.ods.map, width = 10, units ="in", dpi = 300)

# Camps map

ODS.camps.map <- ggplot() +
  geom_polygon(data=ODSstates, 
               aes(x=long, y=lat, group=group), 
               fill="#a9a9a9", 
               color = "#a9a9a9", 
               alpha=0.5) +
   geom_point(data = camps, 
             color = "#2667a7",
             aes(x = lon, y = lat)) +
  
  theme_bw() + dkmaptheme

ODS.camps.map

ggsave(ODS.camps.map, file = "plots/ODS.camps.png", width = 10, units ="in", dpi = 300)






#################### OTHER GRAPHS ####################


# ODS participation rates

ODSparticipation <- data.frame(table(schoolsdata$Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year.))

# Remove sister schools
ODSparticipation <- subset(ODSparticipation, ODSparticipation$Var1 != "Sister school did")
ODSparticipation$largest <- 0

colnames(ODSparticipation) <- c("participation", "freq", "largest")

# Adjust number of no responses for sister schools issue
# ODSparticipation[1,2] <- (ODSparticipation[1,2] - 60)



ODSparticipation$largest[which.max(ODSnightstable$freq)] <- 1
ODSparticipation$participation <- c("Awaiting response", "No", "Yes")
ODSparticipation[1,3] <- 2


ODSparticipationgraph <- ggplot(data = ODSparticipation, aes(x = participation, y = freq, fill = factor(largest), color = factor(largest))) + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  scale_fill_manual(values = dkpallette) +
  scale_color_manual(values = dkpallette2) +
  geom_text (data = ODSparticipation, aes(x = participation, y = freq), label = ODSparticipation$freq, nudge_y = 30, family = "Montserrat") + 
  scale_x_discrete(limits=c("Awaiting response", "No", "Yes")) +
  dkbartheme

ODSparticipationgraph

ggsave("plots/ODSparticipation.png", plot = ODSparticipationgraph, width = 5, height = 2.5, units ="in", dpi = 300)


# Analysis of length of ODS programs

ODSdaystable <- data.frame(table(schoolsdata$How.many.DAYS.does.your.Outdoor.School.program.last.))
ODSdaystable <- ODSdaystable[2:8,]

colnames(ODSdaystable) <- c("Days", "freq")
ODSdaystable$largest <- 0
ODSdaystable$largest[which.max(ODSnightstable$freq)] <- 1


# Number of days bar chart

ODSdaysbarchart <- ggplot(data = ODSdaystable, aes(x = Days, y = freq, fill = factor(largest))) + 
  geom_col() + 
  labs (x = "Number of days", y = "Number of schools") +
  scale_fill_manual(values = dkpallette) +
  geom_text (data = ODSdaystable, aes(x = Days, y = freq), label = ODSdaystable$freq, nudge_y = 5, family = "Montserrat") + 
  dkcolumntheme + 
  theme (
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title = element_text()
  )

ODSdaysbarchart
ggsave("temp/dayschart.png", plot = ODSdaysbarchart, width = 20, height = 16, units ="in", dpi = 300)







# Average number of nights bar chart

ODSnightstable <- data.frame(table(schoolsdata$How.many.NIGHTS.does.your.Outdoor.School.program.last.))
colnames(ODSnightstable) <- c("Nights", "freq")

ODSnightstable <- ODSnightstable[2:8,]

ODSnightstable$largest <- 0
ODSnightstable$largest[which.max(ODSnightstable$freq)] <- 1
ODSnightstable

ODSnightsbarchart <- ggplot(data = ODSnightstable, aes(x = Nights, y = freq, fill = factor(largest))) + 
  geom_col() + 
  labs (x = "Number of nights", y = "Number of schools") +
  scale_fill_manual(values = dkpallette) +
  geom_text (data = ODSnightstable, aes(x = Nights, y = freq), label = ODSnightstable$freq, nudge_y = 5, family = "Montserrat") + 
  dkcolumntheme +
  theme (
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title = element_text()
  )

ODSnightsbarchart

ggsave("plots/nightschart.png", plot = ODSnightsbarchart, width = 5, height = 4, units ="in", dpi = 300)

# Interest in starting program

ODSinterest <- data.frame(table(schools.wo.ods$Is.your.school.interested.in.starting.an.Outdoor.School.program.in.the.future.))
ODSinterest <- ODSinterest[2:7,]
ODSinterest <- ODSinterest[-4,]


ODSinterest$largest <- 0
ODSinterest$largest[which.max(ODSinterest$Freq)] <- 1

ODSinterestgraph <- ggplot(data = ODSinterest, aes(x = Var1, y = Freq, fill = factor(largest))) + 
  geom_col () + 
  coord_flip() + 
  scale_fill_manual(values = dkpallette) +
  geom_text (data = ODSinterest, aes(x = Var1, y = Freq), label = ODSinterest$Freq, nudge_y = 2, family = "Montserrat") + 
  scale_x_discrete(limits=c("I'm not sure", "NOT AT ALL interested", "A BIT interested", "SOMEWHAT interested", "VERY interested")) +
  dkbartheme

ODSinterestgraph

ggsave (ODSinterestgraph, file = "plots/ODSinterest.png", width = 5, height = 2, units ="in", dpi = 300)



# Measure 99 familiarity

m99familiarity <- data.frame(table(schoolsdata$How.familiar..if.at.all..are.you.with.Measure.99..which.passed.in.November.2016.and.dedicates.funding.to.schools.to.send.students.to.Outdoor.School.))
m99familiarity <- m99familiarity[2:5,1:2]
colnames(m99familiarity) <- c("familiarity", "freq")
m99familiarity$familiarity <- c("A bit familiar", "Not at all familiar", "Somewhat familiar", "Very familiar")

m99familiarity$largest <- 0
m99familiarity$largest[which.max(m99familiarity$freq)] <- 1

m99graph <- ggplot(data = m99familiarity, aes(x = familiarity, y = freq, fill = factor(largest))) +
  geom_bar(stat="identity") + 
  coord_flip() +
  scale_fill_manual(values = dkpallette) +
  scale_x_discrete(limits=c("Not at all familiar", "A bit familiar", "Somewhat familiar", "Very familiar")) +
  geom_text (data = m99familiarity, aes(x = familiarity, y = freq), label = m99familiarity$freq, nudge_y = 5) + 
  dkbartheme 

m99graph

ggsave (m99graph, file = "plots/m99.png", width = 5, height = 2, units ="in", dpi = 300)


# When want to start program

whenstart <- data.frame(table(schools.wo.ods$When.do.you.think.your.might.be.ready.to.start.an.Outdoor.School.program.))
whenstart <- subset(whenstart, Freq > 1)
whenstart <- whenstart [-1,]

whenstart$largest <- 0
whenstart$largest[which.max(whenstart$Freq)] <- 1

whenstartgraph <- ggplot(data = whenstart, aes(x = Var1, y = Freq, fill = factor(largest))) + 
  geom_col () + 
  coord_flip() + 
  scale_fill_manual(values = dkpallette) +
  geom_text (data = whenstart, aes(x = Var1, y = Freq), label = whenstart$Freq, nudge_y = 2, family = "Montserrat") + 
  scale_x_discrete(limits=c("Not sure at this point", "2019/2020 school year","2018/2019 school year","2017/2018 school year")) +
  dkbartheme

whenstartgraph

ggsave (whenstartgraph, file = "plots/whenstart.png", width = 5, height = 2, units ="in", dpi = 300)



