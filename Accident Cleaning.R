#NOTE: Lines 15, 487 requires setting of working directory

#Automation to install packages that are necessary but not yet installed
list.of.packages <- c("data.table", "stringr", "ggplot2", "rcompanion")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#import respective libraries
library(data.table)
library(stringr)
library(ggplot2)
library(rcompanion)

#set your working directory here..
setwd("C:/Users/joshua/Downloads/BC2407 Analytics II/Datasets Raw")

#import dataset as datatable
accident.dt <- fread("dft-road-casualty-statistics-accident-last-5-years.csv")

#We are only interested to study year 2017 to 2019
accident.dt <- accident.dt[accident_year == 2017 | accident_year == 2018 | accident_year == 2019]
accident.dt[,"accident_year" := lapply(.SD, as.factor), .SDcols = "accident_year"]

#accident_index is unique but accident_reference is only unique within the year, therefore we will remove it
accident.dt[,accident_reference:=NULL]

#===============================================================================

# Removing columns that provide the same location information

#===============================================================================

#remove all data points without a specified location
accident.dt <- accident.dt[!(location_easting_osgr == "NULL" | latitude == "NULL" 
                             | location_northing_osgr == "NULL" | longitude == "NULL")]

#convert locations, longtitude & latitude from char to numeric
locationtemp = c("location_easting_osgr", "location_northing_osgr", "longitude", "latitude")
accident.dt[, (locationtemp) := lapply(.SD, as.numeric), .SDcols = locationtemp]

#check to verify based on theory that (easting,longitude) & (northing,latitude) refer to the same thing
cor.test(accident.dt$location_easting_osgr, accident.dt$longitude, method = "pearson") #0.9994175 
cor.test(accident.dt$location_northing_osgr, accident.dt$latitude, method = "pearson") #0.9999705 

#Since the correlation (easting,longitude) & (northing,latitude) are indeed close to 1, we will remove one of them
accident.dt[,c("location_easting_osgr","location_northing_osgr"):=NULL]

#converting remaining geographic location information to categories
locationdatatocat = c("local_authority_district","local_authority_ons_district","local_authority_highway","police_force")
accident.dt[,(locationdatatocat) := lapply(.SD, as.factor), .SDcols = (locationdatatocat)]

#===============================================================================

#1. Create field weekend that indicates whether day is weekend or not.
#2. Converting raw day of week field to categorical and re-leveling it

#===============================================================================

#creating control variable, 1 for weekend, 0 for not weekend
weekend = numeric()
weekend[accident.dt$day_of_week == 1 | accident.dt$day_of_week == 7] <- 1
weekend[accident.dt$day_of_week != 1 & accident.dt$day_of_week != 7] <- 0
weekend <- as.factor(weekend)
accident.dt <- cbind(accident.dt,weekend) #add newly created weekend column

#convert day of week to categorical
day = character()
day[accident.dt$day_of_week == '1'] <- "Sunday"
day[accident.dt$day_of_week == '2'] <- "Monday"
day[accident.dt$day_of_week == '3'] <- "Tuesday"
day[accident.dt$day_of_week == '4'] <- "Wednesday"
day[accident.dt$day_of_week == '5'] <- "Thursday"
day[accident.dt$day_of_week == '6'] <- "Friday"
day[accident.dt$day_of_week == '7'] <- "Saturday"
day <- factor(day,levels = c("Monday","Tuesday","Wednesday","Thursday",
                    "Friday","Saturday","Sunday"))  
accident.dt <- cbind(accident.dt, day)
accident.dt[,day_of_week:=NULL] #remove the original day of week column
  
#===============================================================================

#Reclassifying accident severity into - 'Serious' vs 'Not Serious'

#===============================================================================

#accident severity, initially Fatal = 1, serious = 2, slight = 3
#we will regroup serious and fatal together, with slight as a different category
accident_severity = character()
accident_severity[accident.dt$accident_severity == 1 | accident.dt$accident_severity == 2] <- "Serious"
accident_severity[accident.dt$accident_severity == 3] <- "Not Serious"
accident_severity <- as.factor(accident_severity)
accident.dt[,accident_severity:=NULL] #remove original severity column
accident.dt <- cbind(accident.dt,accident_severity) #add newly created severity column

#===============================================================================

#Creating a month column based on an existing field date 

#===============================================================================

#extract month from date
accident.dt$substring_Date = str_sub(accident.dt$date,4,5)
month <- character()
month[accident.dt$substring_Date == "01"] <- "Jan"
month[accident.dt$substring_Date == "02"] <- "Feb"
month[accident.dt$substring_Date == "03"] <- "Mar"
month[accident.dt$substring_Date == "04"] <- "Apr"
month[accident.dt$substring_Date == "05"] <- "May"
month[accident.dt$substring_Date == "06"] <- "Jun"
month[accident.dt$substring_Date == "07"] <- "Jul"
month[accident.dt$substring_Date == "08"] <- "Aug"
month[accident.dt$substring_Date == "09"] <- "Sep"
month[accident.dt$substring_Date == "10"] <- "Oct"
month[accident.dt$substring_Date == "11"] <- "Nov"
month[accident.dt$substring_Date == "12"] <- "Dec"
month <- factor(month, levels = c("Jan","Feb","Mar","Apr","May","Jun",
                                  "Jul","Aug","Sep","Oct","Nov","Dec"))
accident.dt <- cbind(accident.dt, month)
accident.dt[,c("substring_Date","date"):=NULL] #remove the time column


#===============================================================================

#Deriving a column - Time of day which groups the different times of day into 
#Categories based on peak hours etc...

#===============================================================================

#extract the HH of time and convert to numeric
accident.dt$substring_Time = str_sub(accident.dt$time,1,2)
accident.dt <- accident.dt[,"substring_Time":=lapply(.SD,as.numeric),.SDcols = "substring_Time"]
#accident.dt <- accident.dt[,"substring_Time":=lapply(.SD,as.factor),.SDcols = "substring_Time"]

#grouping time of day
  #1: "Morning Rush (6-10)"
  #2: "Day (10-12)"
  #3: "Lunch Rush (12-14)"
  #4: "Afternoon (14-16)"
  #5: "After Work Rush (16-18)"
  #6: "Evening (18-22)"
  #7: "Night (22-6)"
timeofday <- character()  # Create empty data object
timeofday[accident.dt$substring_Time >= 6 & accident.dt$substring_Time < 10] <- "Morning Rush"                   
timeofday[accident.dt$substring_Time >= 10 & accident.dt$substring_Time < 12] <- "Day"
timeofday[accident.dt$substring_Time >= 12 & accident.dt$substring_Time < 14] <- "Lunch Rush"
timeofday[accident.dt$substring_Time >= 14 & accident.dt$substring_Time < 16] <- "Afternoon"
timeofday[accident.dt$substring_Time >= 16 & accident.dt$substring_Time < 19] <- "After Work Rush"
timeofday[accident.dt$substring_Time >= 19 & accident.dt$substring_Time < 22] <- "Evening"
timeofday[accident.dt$substring_Time >= 22] <- "Night"
timeofday[accident.dt$substring_Time < 6] <- "Night"
timeofday <- factor(timeofday,levels = c("Morning Rush","Day","Lunch Rush","Afternoon","After Work Rush",
                                         "Evening","Night"))
accident.dt <- cbind(accident.dt, timeofday)
accident.dt[,c("substring_Time","time"):=NULL] #remove the time and substring time column

#===============================================================================

#Removing columns that have too many missing data 

#===============================================================================

#For second_road_class, ~82% of data is either unclassified or missing/out of range
#Hence, we will remove this column to reduce the removal of data points due to NULL values.
length(grep(-1, accident.dt$second_road_class)) #155668
length(grep(6, accident.dt$second_road_class)) #147782
nrow(accident.dt) #370041
#correspondingly, second_road_number will be removed as well as they are correlated.
accident.dt[,c("second_road_number","second_road_class"):=NULL] #remove the time column

#===============================================================================

#Reclassifying whether police officer attended accident scene into either YES
#or NO.

#===============================================================================

#converting police_officer_present to categorical
#based on data documentation, police_officer_attend == 2/3 means NO, therefore, we will regroup them together
accident.dt <- accident.dt[!(did_police_officer_attend_scene_of_accident == -1)]
police_officer_present = numeric()
police_officer_present[accident.dt$did_police_officer_attend_scene_of_accident == 2 | 
                         accident.dt$did_police_officer_attend_scene_of_accident == 3] <- 0
police_officer_present[accident.dt$did_police_officer_attend_scene_of_accident == 1] <- 1
police_officer_present <- as.factor(police_officer_present)
accident.dt <- cbind(accident.dt,police_officer_present) #add newly created column
accident.dt[,did_police_officer_attend_scene_of_accident:=NULL] #remove existing column

#===============================================================================

#Reclassifying light conditions into daylight, darkness(lighting on) & darkness(no lighting)

#===============================================================================

accident.dt <- accident.dt[!(light_conditions == -1)]
light_conditions = character()
light_conditions[accident.dt$light_conditions == 1] <- "Daylight"
light_conditions[accident.dt$light_conditions == 4] <- "Darkness(Lights Lit)"
light_conditions[accident.dt$light_conditions == 5 | accident.dt$light_conditions == 6 |
                 accident.dt$light_conditions == 7] <- "Darkness(No Lights)"
light_conditions <- factor(light_conditions,levels = c("Daylight", "Darkness(Lights Lit)", "Darkness(No Lights)"))
accident.dt[,light_conditions:=NULL] #remove original light conditions column
accident.dt <- cbind(accident.dt,light_conditions) #add newly created column

#===============================================================================

#Converting speed from numeric to categorical

#===============================================================================

#convert speed to limit to numeric since there are only 6 permitted speeds
accident.dt <- accident.dt[!(speed_limit == -1)] #remove data missing or out of range
accident.dt[,"speed_limit" := lapply(.SD, as.factor), .SDcols = "speed_limit"]

#===============================================================================

#Chunk cleaning the remaining categorical columns

#===============================================================================

#remove all unspecified, null and data missing or out of range
accident.dt <- accident.dt[!(road_type == -1 | road_type == 9)]
accident.dt <- accident.dt[!(junction_detail == 99 | junction_detail == -1)]
accident.dt <- accident.dt[!(junction_control == -1 | junction_control == 9)]
accident.dt <- accident.dt[!(first_road_class == 6)]
accident.dt <- accident.dt[!(first_road_number == 0)]
accident.dt <- accident.dt[!(pedestrian_crossing_human_control == 9 | pedestrian_crossing_human_control== -1)]
accident.dt <- accident.dt[!(pedestrian_crossing_physical_facilities == 9 | pedestrian_crossing_physical_facilities == -1)]
accident.dt <- accident.dt[!(weather_conditions == -1 | weather_conditions == 9)]
accident.dt <- accident.dt[!(road_surface_conditions == -1 | road_surface_conditions == 9)]
accident.dt <- accident.dt[!(special_conditions_at_site == -1 | special_conditions_at_site == 9)]
accident.dt <- accident.dt[!(carriageway_hazards == -1 | carriageway_hazards == 9)]
accident.dt <- accident.dt[!(urban_or_rural_area == -1 | urban_or_rural_area == 3)]
accident.dt <- accident.dt[!(trunk_road_flag == -1)]

#convert int/character to categorical
remcategoricalcols = c("first_road_class","road_type","junction_detail","junction_control",
                    "pedestrian_crossing_human_control","pedestrian_crossing_physical_facilities",
                    "weather_conditions","road_surface_conditions","special_conditions_at_site",
                    "carriageway_hazards","urban_or_rural_area", "trunk_road_flag")
accident.dt[, (remcategoricalcols) := lapply(.SD, as.factor), .SDcols = remcategoricalcols]

#verify cleaning
summary(accident.dt)
str(accident.dt)

#===================== END OF INITIAL CLEANING =================================

#===============================================================================

#Data Exploration to explore grouping of variables/reclassification

#===============================================================================

ggplot(data = accident.dt, aes(x=accident_severity, y= number_of_vehicles, color = accident_severity)) +
  geom_violin() +
  labs(title = "Relationship between number of vehicles and accident severity") +
  scale_y_continuous(breaks=seq(0,10,1))
#From initial exploration, we see that it might be better to convert number of vehicles
#from continuous to categorical

#convert number_of_vehicles to categorical
accident.dt[,"number_of_vehicles" := lapply(.SD, as.factor), .SDcols = "number_of_vehicles"]

ggplot(data = accident.dt, aes(x = number_of_vehicles,fill=accident_severity)) +
  geom_bar() +
  labs(x = "number of vehicles", 
       y = "Count", 
       title = "Count of Serious vs Not Serious accident in terms of number of vehicles") 
#We can see that when number of vehicles >= 5, the number of accident cases are very little

#Therefore, we will regroup the categories into 1,2,3,4 and 5+
levels(accident.dt$number_of_vehicles) = list("1" = 1, "2" = 2, "3"= 3, "4" = 4, "5 or more" = c(5,6,7,8,9,10))

ggplot(data = accident.dt, aes(x = number_of_vehicles,fill=number_of_vehicles)) +
  geom_bar() +
  labs(x = "Number of vehicles", 
       y = "Count", 
       title = "Count of Serious vs Not Serious accident with different number of vehicles") +
  facet_grid(.~accident_severity)
#Majority of accidents involves 1-2 vehicles, regardless of severity.


ggplot(data = accident.dt, aes(x = number_of_vehicles,fill=accident_severity)) +
  geom_bar(position="fill") +
  labs(x = "Number of vehicles", 
       y = "Proportion", 
       title = "Proportion of Serious vs Not Serious accident over different number of vehicles") 
#Proportion of Severe accidents increases as number of vehicles increases, but is the highest when number of vehicle = 1.
#possible that it is vehicle - pedestrian accidents that are the most severe?

ggplot(data = accident.dt, aes(x=accident_severity, y= number_of_casualties, color = accident_severity)) +
  geom_violin() +
  labs(title = "Relationship between number of casualties and accident severity") +
  scale_y_continuous(breaks=seq(0,26,2))
#From initial exploration, we see that it might be better to convert number of casualties
#from continuous to categorical

accident.dt[,"number_of_casualties" := lapply(.SD, as.factor), .SDcols = "number_of_casualties"]

ggplot(data = accident.dt, aes(x = number_of_casualties,fill=accident_severity)) +
  geom_bar() +
  labs(x = "Number of Casualties", 
       y = "Count", 
       title = "Count of Serious vs Not Serious accident in terms of number of casualties") 
#We can see that when number of casualties >= 5, the number of accident cases are very little

#Once again, we will regroup the categories into 1,2,3,4 and 5+
levels(accident.dt$number_of_casualties) = list("1" = 1, "2" = 2, "3"= 3, "4" = 4, "5 or more" = c(5:26))

ggplot(data = accident.dt, aes(x = number_of_casualties,fill=number_of_casualties)) +
  geom_bar() +
  labs(x = "Number of Casualties", 
       y = "Count", 
       title = "Count of Serious vs Not Serious accident with different number of casualties") +
  facet_grid(.~accident_severity)
#Majority of accidents involves 1-2 casualties, regardless of severity.


ggplot(data = accident.dt, aes(x = number_of_casualties,fill=accident_severity)) +
  geom_bar(position="fill") +
  labs(x = "Number of Casualties", 
       y = "Proportion", 
       title = "Proportion of Serious vs Not Serious accident over different number of casualties") 
#Proportion of Severe accidents increases as number of casualties increases.

#===============================================================================

#Perform Cramer's V test to check for association between categorical variables
#0 -> No association, #1 -> Perfect correlation, one variable completely determines the other

#===============================================================================

cramerV(accident.dt$accident_severity,accident.dt$number_of_vehicles,bias.correct = TRUE) #0.1153
cramerV(accident.dt$accident_severity,accident.dt$number_of_casualties,bias.correct = TRUE) #0.0437
cramerV(accident.dt$accident_severity,accident.dt$first_road_class,bias.correct = TRUE) #0.01784
cramerV(accident.dt$accident_severity,accident.dt$road_type,bias.correct = TRUE) #0.05986
cramerV(accident.dt$accident_severity,accident.dt$speed_limit,bias.correct = TRUE) #0.05733
cramerV(accident.dt$accident_severity,accident.dt$junction_detail,bias.correct = TRUE) #0.06758
cramerV(accident.dt$accident_severity,accident.dt$junction_control,bias.correct = TRUE) #0.04205
cramerV(accident.dt$accident_severity,accident.dt$pedestrian_crossing_human_control,bias.correct = TRUE) #0.006557
cramerV(accident.dt$accident_severity,accident.dt$pedestrian_crossing_physical_facilities,bias.correct = TRUE) #0.0217
cramerV(accident.dt$accident_severity,accident.dt$light_conditions,bias.correct = TRUE) #0.03037
cramerV(accident.dt$accident_severity,accident.dt$weather_conditions,bias.correct = TRUE) #0.0219
cramerV(accident.dt$accident_severity,accident.dt$road_surface_conditions,bias.correct = TRUE) #0.01018
cramerV(accident.dt$accident_severity,accident.dt$special_conditions_at_site,bias.correct = TRUE) #0.02238
cramerV(accident.dt$accident_severity,accident.dt$carriageway_hazards,bias.correct = TRUE) #0.009145
cramerV(accident.dt$accident_severity,accident.dt$urban_or_rural_area,bias.correct = TRUE) #0.04477
cramerV(accident.dt$accident_severity,accident.dt$trunk_road_flag,bias.correct = TRUE) #0.009588
cramerV(accident.dt$accident_severity,accident.dt$weekend,bias.correct = TRUE) #0.01579
cramerV(accident.dt$accident_severity,accident.dt$day,bias.correct = TRUE) #0.01735
cramerV(accident.dt$accident_severity,accident.dt$month,bias.correct = TRUE) #0.01521
cramerV(accident.dt$accident_severity,accident.dt$timeofday,bias.correct = TRUE) #0.0395
cramerV(accident.dt$accident_severity,accident.dt$police_officer_present,bias.correct = TRUE) #0.1075

#===============================================================================

#Based on the test results, we can perform data exploration on variables with stronger correlation

#===============================================================================

ggplot(data = accident.dt, aes(x = police_officer_present,fill=accident_severity)) +
  geom_bar(position="fill") +
  labs(x = "Presence of police officer", 
       y = "Proportion", 
       title = "Proportion of Serious vs Not Serious accident depending on whether police officer was present")  +
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes"))
#Proportion of Serious accidents are higher when police officer was present at the scene.

ggplot(data = accident.dt, aes(x = day,fill=day)) +
  geom_bar() +
  labs(x = "day", 
       y = "Count", 
       title = "Count of Serious vs Not Serious accident over different days") +
  facet_grid(.~accident_severity)
#Majority of accidents occurs on Fridays and on weekdays.

ggplot(data = accident.dt, aes(x = day,fill=accident_severity)) +
  geom_bar(position="fill") +
  labs(x = "day", 
       y = "Proportion", 
       title = "Proportion of Serious vs Not Serious accident over different days") 
#Proportion of Serious accidents are slightly higher for weekends.

ggplot(data = accident.dt, aes(x = road_type,fill=accident_severity)) +
  geom_bar(position="fill") +
  labs(x = "Road Type", 
       y = "Proportion", 
       title = "Proportion of Serious vs Not Serious accident over different road type") +
  scale_x_discrete(labels=c("1" = "Roundabout", "2" = "One way street",
                            "3" = "Dual carriageway", "6" = "Single carriageway",
                            "7" = "Slip road", "12" = "One way street/Slip road"))
#Proportion of Serious accidents are higher on one way streets and single carriageways

ggplot(data = accident.dt, aes(x = speed_limit,fill=accident_severity)) +
  geom_bar(position="fill") +
  labs(x = "Speed Limit", 
       y = "Proportion", 
       title = "Proportion of Serious vs Not Serious accident over different speed limits") 
#Proportion of Serious accidents increases when speed limit increases. Outlier: speed limit = 70

ggplot(data = accident.dt, aes(x = junction_detail,fill=accident_severity)) +
  geom_bar(position="fill") +
  labs(x = "Junction Detail", 
       y = "Proportion", 
       title = "Proportion of Serious vs Not Serious accident over different junction details") +
  scale_x_discrete(labels=c("0" = "Not at junction", "1" = "Roundabout",
                            "2" = "Mini-roundabout", "3" = "T or staggered junction",
                            "5" = "Slip road", "6" = "Crossroads", "7" = ">4 arms",
                            "8" = "Private Drive/entrance", "9" = "Other junction"))
#Proportion of Serious accidents are higher at T/staggered junction and Private drivers/entrance

ggplot(data = accident.dt, aes(x = junction_control,fill=accident_severity)) +
  geom_bar(position="fill") +
  labs(x = "Junction Control", 
       y = "Proportion", 
       title = "Proportion of Serious vs Not Serious accident over different junction control") +
  scale_x_discrete(labels=c("0" = "Not at junction", "1" = "Authorised person",
                            "2" = "Auto traffic signal", "3" = "Stop sign",
                            "4" = "Give way or uncontrolled"))
#Proportion of Serious accidents are higher at give way or uncontrolled junctions
summary(accident.dt$junction_control)
#Here, not at junction only has 1 case - Serious, which is why Not at junction has Serious accidents taking up 100%

ggplot(data = accident.dt, aes(x = urban_or_rural_area,fill=accident_severity)) +
  geom_bar(position="fill") +
  labs(x = "Area Type", 
       y = "Proportion", 
       title = "Proportion of Serious vs Not Serious accident comparing urban vs rural areas") +
  scale_x_discrete(labels=c("1" = "Urban", "2"= "Rural"))
#Proportion of serious accidents are higher in rural areas.

ggplot(data = accident.dt, aes(x = timeofday,fill=timeofday)) +
  geom_bar() +
  labs(x = "Time of day", 
       y = "Count", 
       title = "Count of traffic accidents over different times of the day") 
#Majority of accidents occur during the morning rush and after work rush

ggplot(data = accident.dt, aes(x = timeofday,fill=accident_severity)) +
  geom_bar(position="fill") +
  labs(x = "Time of day", 
       y = "Proportion", 
       title = "Proportion of Serious vs Not Serious accident comparing different times of the day")
#However, Proportion of serious accidents are higher at night and in the evening.

ggplot(data = accident.dt, aes(x = light_conditions,fill=light_conditions)) +
  geom_bar() +
  labs(x = "Lighting", 
       y = "Count", 
       title = "Count of traffic accidents over different lighting conditions") 
#Majority of accidents occur during daylight

ggplot(data = accident.dt, aes(x = light_conditions,fill=accident_severity)) +
  geom_bar(position="fill") +
  labs(x = "Lighting", 
       y = "Proportion", 
       title = "Proportion of Serious vs Not Serious accident comparing different lighting conditions")
#However, Proportion of serious accidents are higher at night and when there are no lights

ggplot(data = accident.dt, aes(x = special_conditions_at_site,fill=accident_severity)) +
  geom_bar(position="fill") +
  labs(x = "Special on-site conditions", 
       y = "Proportion", 
       title = "Proportion of Serious vs Not Serious accident across different special on-site conditions") +
  scale_x_discrete(labels=c("0" = "None", "1" = "Auto traffic signal - out", "2" = "Auto signal part defective",
                            "3" = "Road sign or marking defective or obscured", "4" = "Roadwork", 
                            "5" = "Road surface defective", "6" = "Oil or diesel", "7" = "Mud"))
#Proportion of serious accidents are very high when the road surface is defective.

ggplot(data = accident.dt, aes(x = month,fill=month)) +
  geom_bar() +
  labs(x = "Month", 
       y = "Count", 
       title = "Count of traffic accidents over different months") 
#Seems that traffic accidents occur anytime of the year regardless of month

ggplot(data = accident.dt, aes(x = month,fill=accident_severity)) +
  geom_bar(position="fill") +
  labs(x = "Month", 
       y = "Proportion", 
       title = "Proportion of Serious vs Not Serious accident across different months") 
#Proportion of severe accidents are likewise seemingly independent of month

#=================== END OF DATA EXPLORATION OF ACCIDENT DATA ==================

#set working directory to export cleaned data to...
setwd("C:/Users/joshua/Downloads/BC2407 Analytics II/Datasets Cleaned")

#export as csv
write.csv(accident.dt, "accident_data_cleaned.csv",row.names = FALSE)
