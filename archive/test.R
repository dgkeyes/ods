ODSschoolsmaptest2 <- ggplot(map.county) +
  # Schools that do ODS
  geom_point(data = subset(schoolswithdata, Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year. == "Yes"), 
             shape = 21,
             size = 0.05,
             fill = "#5c9134",
             color = "#5c9134",
             aes(x = lon, y = lat)) +
  
  # Schools that don't do ODS
  geom_point(data = subset(schoolswithdata, Did.your.school.participate.in.Outdoor.School.in.the.2016.2017.school.year. == "No"), 
             shape = 21,
             size = 0.05,
             fill = NA,
             color = "#5c9134",
             aes(x = lon, y = lat)) +
  
  # scale_colour_manual(values=c("No" = "#f4f9f0", "Yes" = "#5c9134", "No (sister school did)" = NA)) +
  
  # Camp points
  geom_point(data = camps, 
             aes(x = lon, y = lat), 
             size = 0.05,
             colour = "#2667a7",
             fill = "#2667a7",
             shape = 22) +
  dkmaptheme

ODSschoolsmaptest2