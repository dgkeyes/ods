



# Plot routes from camps to school -------------------------------------------------------------

library(ggmap)
library(tidyverse)
library(maps)

schools.to.camps.route <- camps 

temp <- route("Portland, OR 97211",
              "Yellow Springs, OH 45387",
              structure = "route")


us.map <- map_data("state") 

ggplot(temp, aes(x = lon, y = lat, group = 1)) +
  geom_polygon(data = us.map, 
               aes(x=long, y=lat, group=group),
               color = ods.gray,
               fill = ods.light.gray) +
  geom_line() +
  coord_map()


temp <- route(schools.to.camps.route$camp[1], 
              schools.to.camps.route$school[1], 
              structure = "route")
  
str(schools.to.camps.route$camp)

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
  geom_text(data = camps.with.associated.schools.tamarack[1,],
            aes(x = lon.camp, y = lat.camp),
            label = "Camp Tamarack",
            color = ods.green,
            hjust = -.1,
            size = ods.text.size * .3) +
  geom_point(data = camps.with.associated.schools.tamarack[1,],
             color = ods.green,
             size = 5,
             aes(x = lon.camp, y = lat.camp)) +
  geom_point(data = camps.with.associated.schools.tamarack,
             color = ods.red,
             aes(x = lon.school, y = lat.school)) +
  labs(title = "Some camps, such as Camp Tamarack, host schools from\nnearby Central Oregon",
       subtitle = "Each dot represents one school") +
  ods.map.theme

# ggsave("plots/camps/schools per camp - gray.png", dpi = 300)


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

# ggsave("plots/camps/schools per camp - tamarack.png", dpi = 300)

#### Map of number of schools at each camp ####

ggplot(camps.number.of.schools) +
  geom_polygon(data=ods.states.map, 
               aes(x=long, y=lat, group=group), 
               fill=ods.light.gray, 
               color = ods.gray,
               alpha=0.5) +
  geom_point(data = filter(schools.data, Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year. == "Yes"),
             color = ods.blue,
             aes(x = lon, y = lat)) +
  geom_point(data = filter(schools.data, Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year. == "No"),
             color = ods.red,
             aes(x = lon, y = lat)) +
  geom_point(data = camps.number.of.schools,
             color = ods.green,
             aes(x = lon, y = lat),
             size = camps.number.of.schools$number_of_schools / 2,
             alpha = 0.75) +
  labs(title = "Camps that host the most schools are concentrated in\nNorthwestern and Central Oregon",
       subtitle = "The larger the green dot, the more schools the camp hosted. \nBlue dots are schools that participated. Red dots are schools that did not.") +
  ods.map.theme + theme(plot.title = element_text(color = ods.green,
                                                  hjust = 0,
                                                  size = 20,
                                                  face = "bold"),
                        plot.subtitle = element_text(color = ods.dark.gray,
                                                     size = 13,
                                                     face = "italic"))
ggsave(file = "plots/misc/camps and associated schools.png")




