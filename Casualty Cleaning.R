#NOTE: Lines 14, 150, 271 requires setting of working directory

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

#casualty severity, initially Fatal = 1, serious = 2, slight = 3
#we will regroup serious and fatal together, with slight as a different category
casualty_severity = character()
casualty_severity[casualty.dt$casualty_severity == 1 | casualty.dt$casualty_severity == 2] <- "Serious"
casualty_severity[casualty.dt$casualty_severity == 3] <- "Not Serious"
casualty_severity <- as.factor(casualty_severity)
casualty.dt[,casualty_severity:=NULL] #remove original severity column
casualty.dt <- cbind(casualty.dt,casualty_severity) #add newly created severity column

#===============================================================================

#Regrouping casualty type as it has too many categories which will affect
#machine learning model

#===============================================================================

casualty.dt <- casualty.dt[!(casualty_type == 99 | casualty_type == -1)]

casualty.dt[, "casualty_type" := lapply(.SD, as.factor), .SDcols = "casualty_type"]

#casualty_type has too many categories, explore regrouping
summary(casualty.dt$casualty_type)

#regroup based on domain knowledge
levels(casualty.dt$casualty_type) = list("Pedestrian" = 0, "Cyclist" = 1, 
                                         "Motorcycle"= c(2:5,23,97), "Car/Taxi" = c(8,9),
                                         "Heavy Vehicles" = c(10,11,19:21,98), "Others" = c(90,22,16:18))

#===============================================================================

#Regrouping of pedestrian location categories

#===============================================================================

#remove unclassified or missing data
casualty.dt <- casualty.dt[!(pedestrian_location == -1 | pedestrian_location == 10)]
pedestrian_location = numeric()
pedestrian_location[casualty.dt$pedestrian_location == 0] <- 0
pedestrian_location[casualty.dt$pedestrian_location == 1] <- 1
#group crossing in zig-zag approach/exit lines category together
pedestrian_location[casualty.dt$pedestrian_location == 2 | casualty.dt$pedestrian_location == 3] <- 2
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

#subset the data table 
accident.dt <- accident.dt[,c("accident_index", "accident_severity")]

#convert accident severity to categorical
accident.dt[, "accident_severity" := lapply(.SD, as.factor), .SDcols = "accident_severity"]

# set the merge clause as keys of the tables:
setkey(accident.dt,accident_index) #107,688 obs 2 variables
setkey(casualty.dt,accident_index) #395,283 obs 14 variables

# perform the join, eliminating not matched rows from Right
casualty.dt <- casualty.dt[accident.dt, nomatch=0]

#casualty_severity is similar to output variable accident_severity, hence, 
#we will remove it since the focus is on predicting accident severity
casualty.dt[,casualty_severity:=NULL]

#verify cleaning done
str(casualty.dt)

#===================== END OF INITIAL CLEANING =================================

#===============================================================================

#Perform Cramer's V test to check for association between categorical variables
#0 -> No association, #1 -> Perfect correlation, one variable completely determines the other

#===============================================================================

cramerV(casualty.dt$accident_severity,casualty.dt$pedestrian_location,bias.correct = TRUE) #0.09105
cramerV(casualty.dt$accident_severity,casualty.dt$pedestrian_movement,bias.correct = TRUE) #0.09055
cramerV(casualty.dt$accident_severity,casualty.dt$car_passenger,bias.correct = TRUE) #0.02747
cramerV(casualty.dt$accident_severity,casualty.dt$bus_or_coach_passenger,bias.correct = TRUE) #0.02745
cramerV(casualty.dt$accident_severity,casualty.dt$pedestrian_road_maintenance_worker,bias.correct = TRUE) #0.03248
cramerV(casualty.dt$accident_severity,casualty.dt$casualty_type,bias.correct = TRUE) #0.1875
cramerV(casualty.dt$accident_severity,casualty.dt$casualty_home_area_type,bias.correct = TRUE) #0.04623
cramerV(casualty.dt$accident_severity,casualty.dt$casualty_imd_decile,bias.correct = TRUE) #0.0278
cramerV(casualty.dt$accident_severity,casualty.dt$casualty_class,bias.correct = TRUE) #0.09154
cramerV(casualty.dt$accident_severity,casualty.dt$casualty_sex,bias.correct = TRUE) #0.081

#===============================================================================

#Based on the test results, we can perform data exploration on variables with stronger correlation

#===============================================================================

# Compare the salary of male and females across Rank
ggplot(data = casualty.dt, aes(x = accident_severity, y = age_of_casualty, color = accident_severity)) +
  geom_boxplot() +
  labs(x = "Severity", 
       y = "Age", 
       title = "Age vs Accident Severity") 
#Median age for Serious accidents is higher than that of non-serious accidents

ggplot(data = casualty.dt, aes(x = casualty_imd_decile,fill=casualty_imd_decile)) +
  geom_bar() +
  labs(x = "Casualty IMD Decile", 
       y = "Count", 
       title = "Count of traffic accidents over casualty's IMD Decile") 
#As casualties become less deprived, traffic accident decreases
#There are many categories, potential to reduce categories by regrouping the categories

#regrouping casualty_imd_decile to reduce number of categories
levels(casualty.dt$casualty_imd_decile) = list("Most Deprived(0-20%)" = c(1,2), "More Deprived(20-40%)" = c(3,4), 
                                               "Moderately Deprived(40-50%)"= c(5,6),
                                               "Less Deprived(20-40%)" = c(7,8), "Least Deprived(0-20%)" = c(9,10))

ggplot(data = casualty.dt, aes(x = pedestrian_location,fill=accident_severity)) +
  geom_bar(position="fill") +
  labs(x = "Pedestrian Location", 
       y = "Proportion", 
       title = "Proportion of Serious vs Not Serious accident depending on pedestrian location")
#Proportion of serious accidents are higher where pedestrian location is:
#Crossing in zig-zag approach lines/exist lines or on refuge, central island or central reservation

ggplot(data = casualty.dt, aes(x = pedestrian_movement,fill=accident_severity)) +
  geom_bar(position="fill") +
  labs(x = "Pedestrian Movement", 
       y = "Proportion", 
       title = "Proportion of Serious vs Not Serious accident depending on pedestrian movement")
#Proportion of serious accidents are higher where pedestrian movement is
#In carriageway, stationary - not crossing  (standing or playing) - masked by parked or stationary vehicle
# and Walking along in carriageway, back to traffic

ggplot(data = casualty.dt, aes(x = casualty_class,fill=accident_severity)) +
  geom_bar(position="fill") +
  labs(x = "Casualty Class", 
       y = "Proportion", 
       title = "Proportion of Serious vs Not Serious accident depending on casualty class")
#Higher proportion of severe accidents tend to have pedestrians as a casualty 

ggplot(data = casualty.dt, aes(x = casualty_sex,fill=accident_severity)) +
  geom_bar(position="fill") +
  labs(x = "Casualty Sex", 
       y = "Proportion", 
       title = "Proportion of Serious vs Not Serious accident depending on casualty sex")
#Higher proportion of severe accidents tend to involve male casualties

ggplot(data = casualty.dt, aes(x = casualty_home_area_type,fill=accident_severity)) +
  geom_bar(position="fill") +
  labs(x = "Home Area Type of Casualty", 
       y = "Proportion", 
       title = "Proportion of Serious vs Not Serious accident depending on casualty's home area") +
  scale_x_discrete(labels=c("1" = "Urban area", "2" = "Small town", "3" = "Rural"))
#Higher proportion of severe accidents tend to involve casualties living in rural areas

ggplot(data = casualty.dt, aes(x = casualty_type,fill=accident_severity)) +
  geom_bar(position="fill") +
  labs(x = "Casualty Type", 
       y = "Proportion", 
       title = "Proportion of Serious vs Not Serious accident depending on casualty type") 
#Higher proportion of severe accidents tend to involve motorcycles, pedestrians and other forms of transport

#=================== END OF DATA EXPLORATION OF CASUALTY DATA ==================

#set working directory to export cleaned data to...
setwd("C:/Users/joshua/Downloads/BC2407 Analytics II/Datasets Cleaned")

#export as csv
write.csv(casualty.dt, "casualty_data_cleaned.csv",row.names = FALSE)

