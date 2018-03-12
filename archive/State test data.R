#### Packages ####

library('tidyverse')
library('stringr')

#### State test data ####

# ELA

test.data.ela <- read.csv("data/state data/ELA.csv")

test.data.ela <- test.data.ela %>%
  filter(Student.Group == "Total Population (All Students)")

test.data.ela <- test.data.ela[c(4, 5, 10)]
colnames(test.data.ela) <- c("State.ID", "School Name", "ELA percent proficient")

# Math

test.data.math <- read.csv("data/state data/math.csv")

test.data.math <- test.data.math %>%
  filter(Student.Group == "Total Population (All Students)")

test.data.math <- test.data.math[c(4, 5, 10)]

colnames(test.data.math) <- c("State.ID", "School Name", "Math percent proficient")


# Science

test.data.science <- read.csv("data/state data/science.csv")

test.data.science <- test.data.science %>%
  filter(Student.Group == "Total Population (All Students)") %>%
  filter(Grade.Level == "All Grades")

test.data.science <- test.data.science[c(4, 5, 10)]

colnames(test.data.science) <- c("State.ID", "School Name", "Science percent proficient")

# Merge into master database

schools.data <- left_join(schools.data, test.data.ela, by = "State.ID")
schools.data <- left_join(schools.data, test.data.math, by = "State.ID")
schools.data <- left_join(schools.data, test.data.science, by = "State.ID")
