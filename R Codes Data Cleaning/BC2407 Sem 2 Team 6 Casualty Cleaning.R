# ====================================================================================
# Purpose:      Cleaning and Visualization for Casualty Data
# Author:       Joshua Khoo, Joel Mui, Ethan ong, Brandon Quek, He Ying
# DOC:          02-04-2022
# Topics:       Data Cleaning, Data Visualization
# Data Source:  dft-road-casualty-statistics-accident-last-5-years.csv, dft-road-casualty-statistics-casualty-last-5-years.csv
# Packages:     data.table, ggplot2, rcompanion
#=====================================================================================

#Automation to install packages that are necessary but not yet installed
list.of.packages <- c("data.table", "ggplot2","rcompanion")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#import respective libraries
library(data.table)
library(ggplot2)
library(rcompanion)

#set your working directory here..
setwd("C:/Users/joshua/Downloads/BC2407 Analytics II/Datasets Raw")

#import dataset as datatable
casualty.dt <- fread("dft-road-casualty-statistics-casualty-last-5-years.csv")

str(casualty.dt)

#We are only interested to study year 2017 to 2019
casualty.dt <- casualty.dt[accident_year == 2017 | accident_year == 2018 | accident_year == 2019]
#convert year to categorical
casualty.dt[, "accident_year" := lapply(.SD, as.factor), .SDcols = "accident_year"]

#accident_index is unique but accident_reference is only unique within the year, therefore we will remove it
casualty.dt[,accident_reference:=NULL]

#vehicle reference and casualty reference are not needed, common join can be done on accident_index
casualty.dt[,c("vehicle_reference","casualty_reference"):=NULL]

#remove casualty severity, since the information is provided in terms of accident_severity
casualty.dt[,casualty_severity:=NULL]

#remove accident year
casualty.dt[,accident_year:=NULL]
#===============================================================================

#Renaming casualty class and sex of casualty & converting to categorical

#===============================================================================

#convert casualty class to categorical and rename it for clarity
casualty_class = character()
casualty_class[casualty.dt$casualty_class == 1] <- "Driver/Rider"
casualty_class[casualty.dt$casualty_class == 2] <- "Passenger"
casualty_class[casualty.dt$casualty_class == 3] <- "Pedestrian"
casualty_class <- as.factor(casualty_class)
casualty.dt[,casualty_class:=NULL] #remove original casualty_class
casualty.dt <- cbind(casualty.dt,casualty_class) #add newly created casualty_class column

#convert sex of casualty to categorical and rename it for clarity
casualty.dt <- casualty.dt[!(sex_of_casualty == -1 | sex_of_casualty == 9)]
casualty_sex = character()
casualty_sex[casualty.dt$sex_of_casualty == 1] <- "Male"
casualty_sex[casualty.dt$sex_of_casualty == 2] <- "Female"
casualty_sex <- as.factor(casualty_sex)
casualty.dt[,sex_of_casualty:=NULL] #remove original sex_of_casualty columnn
casualty.dt <- cbind(casualty.dt,casualty_sex) #add newly created casualty_sex column

#===============================================================================

#Removing multiple age datas, since they provide the same information

#===============================================================================

#remove missing data from age
casualty.dt <- casualty.dt[!(age_of_casualty == -1)]
#remove duplicate age column which means the same thing
casualty.dt[,age_band_of_casualty:=NULL] #remove original age_band_of_casualty

#===============================================================================

#Regrouping casualty type as it has too high cardinality, which will affect
#machine learning model

#===============================================================================

casualty.dt <- casualty.dt[!(casualty_type == 99 | casualty_type == -1)]

casualty.dt[, "casualty_type" := lapply(.SD, as.factor), .SDcols = "casualty_type"]

#casualty_type has too many categories, explore regrouping
summary(casualty.dt$casualty_type)

#regroup based on domain knowledge and common categories.
levels(casualty.dt$casualty_type) = list("Pedestrian" = 0, "Cyclist" = 1, 
                                         "Motorcycle"= c(2:5,23,97), 
                                         "Car/Taxi" = c(8,9),
                                         "Buses" = c(10,11),
                                         "Vehicles(Goods)" = c(19:21,98), 
                                         "Others" = c(90,22,16:18))

#===============================================================================

#Regrouping of pedestrian location categories

#===============================================================================

#remove unclassified or missing data
casualty.dt <- casualty.dt[!(pedestrian_location == -1 | pedestrian_location == 10)]
pedestrian_location = numeric()
pedestrian_location[casualty.dt$pedestrian_location == 0] <- 0
pedestrian_location[casualty.dt$pedestrian_location == 1] <- 1
#group crossing in zig-zag approach/exit lines category together
pedestrian_location[casualty.dt$pedestrian_location == 2 | 
                    casualty.dt$pedestrian_location == 3] <- 2
pedestrian_location[casualty.dt$pedestrian_location == 4] <- 4
pedestrian_location[casualty.dt$pedestrian_location == 5] <- 5
pedestrian_location[casualty.dt$pedestrian_location == 6] <- 6
pedestrian_location[casualty.dt$pedestrian_location == 7] <- 7
pedestrian_location[casualty.dt$pedestrian_location == 8] <- 8
pedestrian_location[casualty.dt$pedestrian_location == 9] <- 9
pedestrian_location <- as.factor(pedestrian_location)
casualty.dt[,pedestrian_location:=NULL] #remove original pedestrian_location column
casualty.dt <- cbind(casualty.dt,pedestrian_location) #add newly created pedestrian_location column


#===============================================================================

#Chunk cleaning the remaining categorical columns

#===============================================================================

#remove rows with unclassified and data missing or out of range 

casualty.dt <- casualty.dt[!(pedestrian_movement == -1 | pedestrian_movement == 9)]
casualty.dt <- casualty.dt[!(car_passenger == -1 | car_passenger == 9)]
casualty.dt <- casualty.dt[!(bus_or_coach_passenger == -1 | bus_or_coach_passenger == 9)]
casualty.dt <- casualty.dt[!(pedestrian_road_maintenance_worker == -1)]
casualty.dt <- casualty.dt[!(casualty_imd_decile == -1)]
casualty.dt <- casualty.dt[!(casualty_home_area_type == -1)]

#convert int to categorical as necessary
categoricalcols = c("pedestrian_movement", "car_passenger", "bus_or_coach_passenger",
                    "pedestrian_road_maintenance_worker","casualty_imd_decile",
                    "casualty_home_area_type")
casualty.dt[, (categoricalcols) := lapply(.SD, as.factor), .SDcols = categoricalcols]

str(casualty.dt) #verify cleaning for casualty data

#===============================================================================

#Merging casualty data with accident data to explore relationship between
#casualty data and accident severity

#===============================================================================

#set working directory to import cleaned accident data
setwd("C:/Users/joshua/Downloads/BC2407 Analytics II/Datasets Cleaned")

#import accident data
accident.dt <- fread("accident_data_cleaned.csv")

#subset accident data to extract necessary columns
accident.dt <- subset(accident.dt, select = c("accident_index", "accident_severity"))

#perform merge on accident index
casualty.dt <- merge(casualty.dt, accident.dt, by = "accident_index")

#convert accident severity to categorical
casualty.dt[, "accident_severity" := lapply(.SD, as.factor), .SDcols = "accident_severity"]

#verify the join
str(casualty.dt)

#===================== END OF INITIAL CLEANING =================================

#===============================================================================

#No variable that perfectly determines accident severity
#Lets perform data exploration on casualty data

#===============================================================================

ggplot(data = casualty.dt, aes(x = accident_severity, y = age_of_casualty, color = accident_severity)) +
  geom_boxplot() +
  labs(x = "Severity", 
       y = "Age", 
       title = "Age vs Accident Severity") + 
  theme(legend.position = "none")
#Median age for Serious accidents is higher than that of non-serious accidents

#Lets try grouping the age for more meaningful comparisons
casualty_agegroup = character()
casualty_agegroup[casualty.dt$age_of_casualty < 16] <- "Children"
casualty_agegroup[casualty.dt$age_of_casualty >= 16 & casualty.dt$age_of_casualty <= 20] <- "Teenager"
casualty_agegroup[casualty.dt$age_of_casualty >= 21 & casualty.dt$age_of_casualty <= 64] <- "Adult"
casualty_agegroup[casualty.dt$age_of_casualty >= 65] <- "Senior"
casualty_agegroup <- factor(casualty_agegroup, levels = c("Children", "Teenager", "Adult", "Senior"))
casualty.dt[,age_of_casualty:=NULL] #remove original age of of casualty
casualty.dt <- cbind(casualty.dt,casualty_agegroup) #add newly created casualty_agegroup column

ggplot(data = casualty.dt, aes(x = casualty_agegroup,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "casualty_agegroup", 
       x = "Age Group of Casualty",
       y = "Percentage", 
       title = "Accident Severity for different casualty age groups") +
  facet_grid(.~accident_severity) + 
  theme(legend.position = "none")
#Higher proportion of serious accidents have seniors as casualties
#Lower proportion of serious accidents involve adults.

ggplot(data = casualty.dt, aes(x = pedestrian_movement,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop.., accuracy = 0.01),
                 y=..prop.. ), stat= "count", vjust = -.5, size = 2.5) +
  labs(fill = "pedestrian_movement", 
       x = "Pedestrian Movement",
       y = "Percentage", 
       title = "Accident Severity for different pedestrian movements") +
  facet_grid(.~accident_severity) + 
  theme(legend.position = "none")
#Higher proportion of serious accidents involve crossing from drivers nearside and offside with no masking

#Grouping for better distinction
levels(casualty.dt$pedestrian_movement) = list("Not a pedestrian" = 0, 
                                            "Crossing from driver's nearside" = 1,
                                            "Crossing from driver's offside" = 3,
                                            "Others" = c(2,4:8))

ggplot(data = casualty.dt, aes(x = car_passenger,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "car_passenger", 
       x = "Car passenger",
       y = "Percentage", 
       title = "Accident Severity depending on the type of car passenger") +
  facet_grid(.~accident_severity) + 
  theme(legend.position = "none")
#The type of car passenger does not really affect accident severity as the
#distribution is similar.

ggplot(data = casualty.dt, aes(x = bus_or_coach_passenger,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "bus_or_coach_passenger", 
       x = "Bus or Coach passenger",
       y = "Percentage", 
       title = "Accident Severity depending on the type of bus/coach passenger") +
  facet_grid(.~accident_severity) + 
  theme(legend.position = "none")
#Once again, the type of bus/coach  passenger does not really affect accident severity

ggplot(data = casualty.dt, aes(x = pedestrian_road_maintenance_worker,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "pedestrian_road_maintenance_worker", 
       x = "Maintenance Worker",
       y = "Percentage", 
       title = "Accident Severity depending on whether casualty is maintenance worker") +
  facet_grid(.~accident_severity) + 
  theme(legend.position = "none")
#Whether a casualty is a pedestrian_road_maintenance worker don't really affect accident severity

ggplot(data = casualty.dt, aes(x = casualty_type,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(fill = "casualty_type", 
       x = "Casualty Type",
       y = "Percentage", 
       title = "Accident Severity depending on casualty type") +
  facet_grid(.~accident_severity) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  theme(legend.position = "none")
#Serious accidents tend to involve motorcycle casualties
#Serious accidents typically involve less cars.

ggplot(data = casualty.dt, aes(x = casualty_home_area_type,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "casualty_home_area_type", 
       x = "Casualty Home Area Type",
       y = "Percentage", 
       title = "Accident Severity depending on casualty home area type") +
  facet_grid(.~accident_severity) + 
  scale_x_discrete(labels=c("1" = "Urban area", "2" = "Small town", "3" = "Rural")) + 
  theme(legend.position = "none")
#Similar distribution is seen, casualty's home area does not really affect accident severity

ggplot(data = casualty.dt, aes(x = casualty_imd_decile,fill=casualty_imd_decile)) +
  geom_bar() +
  labs(x = "Casualty IMD Decile", 
       y = "Count", 
       title = "Count of traffic accidents over casualty's IMD Decile") + 
  theme(legend.position = "none")
#As casualties become less deprived, traffic accident decreases
#There are many categories, potential to reduce categories by regrouping the categories

#regrouping casualty_imd_decile to reduce number of categories
levels(casualty.dt$casualty_imd_decile) = list("Most Deprived(0-20%)" = c(1,2), 
                                               "More Deprived(20-40%)" = c(3,4), 
                                               "Moderately Deprived(40-50%)"= c(5,6),
                                               "Less Deprived(20-40%)" = c(7,8), 
                                               "Least Deprived(0-20%)" = c(9,10))

ggplot(data = casualty.dt, aes(x = casualty_imd_decile,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(fill = "casualty_imd_decile", 
       x = "Casualty IMD Decile",
       y = "Percentage", 
       title = "Accident Severity depending on casualty IMD Decile") +
  facet_grid(.~accident_severity) + 
  theme(legend.position = "none")
#However, in terms of affecting accident severity, the casualty's IMD Decile does
#not really affect the severity of traffic accidents.

ggplot(data = casualty.dt, aes(x = casualty_class,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "casualty_class", 
       x = "Casualty Class",
       y = "Percentage", 
       title = "Accident Severity depending on casualty class") +
  facet_grid(.~accident_severity) + 
  theme(legend.position = "none")
#Serious accident tend to involve more pedestrians

ggplot(data = casualty.dt, aes(x = casualty_sex,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "casualty_sex", 
       x = "Casualty Gender",
       y = "Percentage", 
       title = "Accident Severity depending on casualty gender") +
  facet_grid(.~accident_severity) + 
  theme(legend.position = "none")
#Serious accident tend to involve more male casualties

ggplot(data = casualty.dt, aes(x = pedestrian_location,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  labs(fill = "pedestrian_location", 
       x = "Casualty Location",
       y = "Percentage", 
       title = "Accident Severity depending on pedestrian's location") +
  facet_grid(.~accident_severity) + 
  theme(legend.position = "none")
#Serious accident tend to involve more pedestrians crossing on pedestrian crossings,
#crossing elsewhere within 50m of crossings and crossing in carriageway elsewhere.

levels(casualty.dt$pedestrian_location) = list("Not a pedestrian" = 0,
                                               "Crossing on pedestrian crossing" = 1,
                                               "Crossing elsewhere, within 50m of pedestrian crossing" = 4,
                                               "In Carriageway, crossing elsewhere" = 5,
                                               "Others" = c(2,6:9))

#=================== END OF DATA EXPLORATION OF CASUALTY DATA ==================

#===============================================================================

#Export cleaned casualty data

#===============================================================================

str(casualty.dt)

#set working directory to export cleaned data to...
setwd("C:/Users/joshua/Downloads/BC2407 Analytics II/Datasets Cleaned")

#export as csv
write.csv(casualty.dt, "casualty_data_cleaned.csv",row.names = FALSE)

