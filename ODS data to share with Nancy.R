#### CURRENT PROGRAMS ####

#### ODS participation by number of students/grades ####

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
  labs(title = "Most students who participated in Outdoor School\nin 2016-2017 were in sixth grade") +
  ods.bar.theme

ggsave("gff/plots/current programs/number of students by grade.png", dpi = 300, 
       height = nrow(number.of.students))


#### ODS participation by general - waffle ####



ods.participation.waffle.graph <- waffle(ods.participation.by.general.waffle,
       rows = 25,
       color = c(ods.blue, ods.red),
       legend_pos = "none")
       

ods.participation.waffle.graph + 
  labs(title = "493 schools (64 percent of the total)\nparticipated in Outdoor School",
       subtitle = "Each dot represents one school.\nBlue dots are schools that participated.\nRed dots are those that did not.") +
  ods.base.theme
  

 ggsave(file = "gff/plots/current programs/ods participation by general - waffle.png", 
       dpi = 300,
       height = 6)

#### ODS participation by school size ####

ods.participation.graph(ods.participation.by.size, "size", c(ods.dark.gray, ods.blue)) 


#### ODS participation by governance ####

ods.participation.graph(ods.participation.by.governance, "governance", c(ods.blue, ods.dark.gray))

#### ODS participation by FRL ####

ggplot(ods.participation.by.frl, aes(x = grouping, y = pct)) +
  geom_bar(stat = "identity",
           fill = ods.participation.by.frl$fill) +
  geom_text(label = percent(ods.participation.by.frl$pct),
            hjust = -.2,
            color = ods.participation.by.frl$color,
            size = ods.text.size * .35) + 
  scale_y_continuous(limits = c(0, max(ods.participation.by.frl$pct + .1)), labels = percent) +
  coord_flip() +
  labs(title = "High-poverty schools are less likely\nto participate in Outdoor School") +
  ods.bar.theme

ggsave(file = "gff/plots/current programs/ods participation by frl.png", 
       dpi = 300,
       height = nrow(ods.participation.by.frl))


#### ODS participation by ESD ####

ggplot(ods.participation.by.esd, aes(x = reorder(grouping, pct), y = pct)) +
  geom_bar(stat = "identity", 
           fill = ods.dark.gray) +
  geom_text(label = percent(ods.participation.by.esd$pct),
            hjust = -.2,
            size = ods.text.size * .35,
            color = ods.dark.gray) + 
  scale_y_continuous(limits = c(0, 1), labels = percent) +
  labs(title = "There is large variation in Outdoor School\nparticipation rates between ESDs",
       subtitle = "Figure shows the percentage of schools in each ESD that participate") +
  coord_flip() +
  ods.bar.theme

ggsave("gff/plots/current programs/ods participation by esd.png",
       dpi = 300,
       height = nrow(ods.participation.by.esd) / 2)

#### ODS participation by race/ethnicity ####

ods.participation.graph(ods.participation.by.whiteness, "whiteness", ods.dark.gray)


#### ODS participation by test scores ####


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
  labs(title = "Schools with higher rates of proficiency\non statewide science tests are more likely\nto participate in Outdoor School") +
  ods.bar.theme.faceted 

ggsave("gff/plots/current programs/ods participation by test scores.png",
       dpi = 300,
       height = 8)



#### PROGRAM CHARACTERISTICS ####

#### Length of programs (days/nights) ####


## Days


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
  labs(title = "Most Outdoor School programs last three or four DAYS") +
  ods.bar.theme

ggsave("gff/plots/program characteristics/program length days.png", dpi = 300, 
       height = nrow(program.length.days))

## Nights

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
  labs(title = "Most Outdoor School programs last two or three NIGHTS") +
  ods.bar.theme 

ggsave("gff/plots/program characteristics/program length nights.png", dpi = 300, 
       height = nrow(program.length.nights))



#### Curriculum development ####

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
  labs(title = "The curriculum for most Outdoor School\nprograms is developed at least in part by\noutside providers") +
  ods.bar.theme

ggsave("gff/plots/program characteristics/curriculum developer.png", dpi = 300, 
       height = 6)

#### Staffing ####

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
  labs(title = "In addition to teachers, camps and\nproviders are the main source of staff\nfor Outdoor School programs") +
  ods.bar.theme

ggsave("gff/plots/program characteristics/staffing.png", dpi = 300, 
       height = 6)

#### Academic component ####

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
  labs(title = "Nearly all Outdoor School programs\nhave an academic component") +
  ods.bar.theme

ggsave("gff/plots/program characteristics/academic component.png", dpi = 300, 
       height = 4)


#### Accommodations ####

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
  labs(title = "The vast majority of schools make\nall accommodations necessary to ensure\nthat all students can participate\nin Outdoor School") +
  ods.bar.theme

ggsave("gff/plots/program characteristics/accommodations.png", dpi = 300, 
       height = 5)

#### By length of programs (years in existence) ####

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
  labs(title = "Most Outdoor School programs have existed for\nten years or less") +
  ods.bar.theme

ggsave("gff/plots/program characteristics/program length years in existence.png", dpi = 300, 
       height = 5)


#### PARTICIPATION MAPS ####



#### Overall participation map ####

ggplot() +
  geom_polygon(data = oregon.map, 
               aes(x=long, y=lat, group=group),
               color = ods.gray,
               fill = ods.light.gray) +
  geom_point(data = filter(schools.data, Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year. == "Yes"),
             color = ods.blue,
             aes(x = lon, y = lat)) +
  labs(title = "Schools that participated in Outdoor School in 2016-2017") +
  ods.map.theme

ggsave("gff/plots/current programs/ods participation map - participated.png", dpi = 300)

ggplot() +
  geom_polygon(data = oregon.map, 
               aes(x=long, y=lat, group=group),
               color = ods.gray,
               fill = ods.light.gray) +
  geom_point(data = filter(schools.data, Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year. == "No"),
             color = ods.red,
             aes(x = lon, y = lat)) +
  labs(title = "Schools that did not participate in Outdoor School in 2016-2017") +
  ods.map.theme + theme(plot.title = element_text(color = ods.red))

ggsave("gff/plots/current programs/ods participation map - did not participate.png", dpi = 300)




#### ODS participation by county ####

ggplot(ods.participation.by.county, aes(x=long, y=lat, group=group, 
                                        fill = factor(pct.text))) +
  geom_polygon(color = "white") +
  scale_fill_manual(name = "Percent participation",
                    values = c("#EAEEF1", "#B6CADD", "#6894BF", ods.blue)) +
  labs(title = "Outdoor School participation rates vary widely,\nbut in five counties fewer than 25 percent\n of schools participate") +
  ods.map.theme

ggsave("gff/plots/current programs/ods participation by county map.png", dpi = 300)



#### PAST PROGRAMS ####

#### Has school participated in past ####

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
  labs(title = "Of schools that did not participate in Outdoor School\nin 2016-2017, most say they have never participated") +
  ods.bar.theme + theme(plot.title = element_text(color = ods.red))

ggsave("gff/plots/past programs/participated in past.png", dpi = 300,
       height = 4)

#### Last year schools participated in ODS ####



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
  labs(title = "Among schools that did not participate in \nOutdoor School in 2016-2017, the most \ncommon previous year of involvement was \n2015-2016") +
  ods.bar.theme + theme(plot.title = element_text(color = ods.red))

ggsave("gff/plots/past programs/last year participated.png", dpi = 300,
       height = 6)

#### Camp last time attended ####


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
  labs(title = "Many schools without current\nOutdoor School programs last attended\nHancock Field Station and\nCamp Westwind") +
  ods.bar.theme + theme(plot.title = element_text(color = ods.red))

ggsave("gff/plots/past programs/last participated camp.png", dpi = 300,
       height = 8)

#### Why stoped going ####


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
  labs(title = "The most common reasons for dropping\nOutdoor School programs were\ncost and a change in the grades involved") +
  ods.bar.theme + theme(plot.title = element_text(color = ods.red))

ggsave("gff/plots/past programs/why stopped going.png", dpi = 300, 
       height = nrow(why.stopped.going))

#### FUTURE PROGRAMS ####

#### Interest in starting ODS program ####


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
  labs(title = "Over half of schools without Outdoor School\nprograms say they are very or somewhat\ninterested in starting one") +
  ods.bar.theme + theme(plot.title = element_text(color = ods.red))

ggsave("gff/plots/future programs/interest in starting ODS program.png", dpi = 300, 
       height = 6)

#### When might be interested in starting program ####

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
  labs(title = "Of schools with a date in mind to start\nan Outdoor School program,\nmost plan to start during 2017-2018,\nbut many are not sure") +
  ods.bar.theme + theme(plot.title = element_text(color = ods.red))

ggsave("gff/plots/future programs/when want to start ODS program.png", dpi = 300, 
       height = 6)



#### CAMPS ####


#### Plot camps and associated schools ####

library(gridExtra)

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
  labs(title = "Some camps, such as Camp Gray, host schools from\nacross Oregon",
       subtitle = "Each blue dot represents one school") +
  ods.map.theme + theme(plot.title = element_text(color = ods.green,
                                                  hjust = 0))

ggsave("gff/plots/camps/schools per camp - gray.png", dpi = 300)

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
  labs(title = "Some camps, such as Camp Tamarack, host schools from\nnearby Central Oregon",
       subtitle = "Each blue dot represents one school") +
  ods.map.theme + theme(plot.title = element_text(color = ods.green,
                                                  hjust = 0))

ggsave("gff/plots/camps/schools per camp - tamarack.png", dpi = 300)

#### Number of schools that each camp hosts ####

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
  labs(title = "Camps that hosted five or more schools\nfor Outdoor School in 2016-2017") +
  ods.bar.theme + theme(plot.title = element_text(color = ods.green,
                                                  hjust = 0))

ggsave("gff/plots/camps/number of schools each camp hosts - graph.png", dpi = 300)

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
  coord_map() +
  labs(title = "Camps that host the most schools are concentrated in\nNorthwestern and Central Oregon",
       subtitle = "The larger the dot, the more schools the camp hosted") +
  ods.map.theme + theme(plot.title = element_text(color = ods.green,
                                                  hjust = 0,
                                                  size = 20,
                                                  face = "bold"),
                        panel.background = element_rect(fill = "transparent",colour = NA),
                        plot.background = element_rect(fill = "transparent",colour = NA),
                        plot.subtitle = element_text(color = ods.dark.gray,
                                                     size = 13,
                                                     face = "italic"))

ggsave("gff/plots/camps/number of schools each camp hosts - map.png", dpi = 300)
ggsave("gff/plots/camps/number of schools each camp hosts - map.svg", 
       width = 10, height = 7,
       bg = "transparent")

?ggsave


#### MEASURE 99 ####



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
  labs(title = "Most schools say they are somewhat familiar\nwith Measure 99") +
  ods.bar.theme

ggsave("gff/plots/measure 99/measure 99 awareness - general.png", dpi = 300,
       height = 5)



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
  labs(title = "Schools that DID NOT participate in Outdoor School\nin 2016-2017 are more likely to be not at all familiar\nwith Measure 99 than are those that DID participate") +
  ods.bar.theme.faceted

ggsave("gff/plots/measure 99/measure 99 awareness - by ODS participation.png", dpi = 300,
       height = 6)

#### Familiarity with OSU Extension ####

## By general

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
  labs(title = "Half of schools are not at familiar with\nOregon State University Extension Service’s\nrole in funding Outdoor School programs") +
  ods.bar.theme

ggsave("gff/plots/measure 99/familiarity with osu - general.png", dpi = 300,
       height = 6)





