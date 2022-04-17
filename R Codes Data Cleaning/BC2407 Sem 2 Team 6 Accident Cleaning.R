# ====================================================================================
# Purpose:      Cleaning and Visualization for Accident Data
# Author:       Joshua Khoo, Joel Mui, Ethan ong, Brandon Quek, He Ying
# DOC:          02-04-2022
# Topics:       Data Cleaning, Data Visualization
# Data Source:  dft-road-casualty-statistics-accident-last-5-years.csv
# Packages:     data.table, ggplot2, rcompanion, mapproj, ggmap, rstudioapi, cattonum
#=====================================================================================

#Automation to install packages that are necessary but not yet installed
list.of.packages <- c("data.table", "stringr", "ggplot2", "rcompanion",
                      "mapproj","ggmap","rstudioapi","cattonum")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#import respective libraries
library(data.table)
library(stringr)
library(ggplot2)
library(rcompanion)
library(mapproj)
library(ggmap)
library(rstudioapi)
library(cattonum)

#replace YOUR_API_KEY_HERE with your own API key.
#not the best practice I know....
register_google(key = "YOUR_API_KEY_HERE)


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
locationtemp = c("location_easting_osgr", "location_northing_osgr", 
                 "longitude", "latitude")
accident.dt[, (locationtemp) := lapply(.SD, as.numeric), .SDcols = locationtemp]

#check to verify based on theory that (easting,longitude) & (northing,latitude) refer to the same thing
cor.test(accident.dt$location_easting_osgr, accident.dt$longitude, method = "pearson") #0.9994175 
cor.test(accident.dt$location_northing_osgr, accident.dt$latitude, method = "pearson") #0.9999705 

#Since the correlation (easting,longitude) & (northing,latitude) are indeed close to 1, we will remove one of them
accident.dt[,c("location_easting_osgr","location_northing_osgr"):=NULL]

#converting remaining geographic location information to categories
locationdatatocat = c("local_authority_district","local_authority_ons_district",
                      "local_authority_highway","police_force","lsoa_of_accident_location")
accident.dt[,(locationdatatocat) := lapply(.SD, as.factor), .SDcols = (locationdatatocat)]

#===============================================================================

#Converting raw day of week field to categorical and re-leveling it

#===============================================================================

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

#extract month from string date
accident.dt$substring_Date = str_sub(accident.dt$date,4,5)
#make month a categorical variable
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
accident.dt[,c("substring_Date","date"):=NULL] #remove the original date field


#Convert string version of time into numeric.
accident.dt$time = paste(str_sub(accident.dt$time,1,2),'.', str_sub(accident.dt$time,4,5),sep="")
accident.dt <- accident.dt[,"time":=lapply(.SD,as.numeric),.SDcols = "time"]

#===============================================================================

#Removing columns that have too many missing data 

#===============================================================================

#For second_road_class, ~82% of data is either unclassified or missing/out of range
#Hence, we will remove this column to reduce the removal of data points due to NULL values.
length(grep(-1, accident.dt$second_road_class)) #155668
length(grep(6, accident.dt$second_road_class)) #147782
nrow(accident.dt) #370041
#correspondingly, second_road_number will be removed as well as they are correlated.
accident.dt[,c("second_road_number","second_road_class"):=NULL] #remove the second road class/number column

#===============================================================================

#Reclassifying whether police officer attended accident scene into either YES
#or NO.

#===============================================================================

#converting police_officer_present to categorical
#based on data documentation, police_officer_attend == 2/3 means NO, 
#therefore, we will regroup them together since they are essentially the same.

#remove data missing or out of range.
accident.dt <- accident.dt[!(did_police_officer_attend_scene_of_accident == -1)]

#reclassifying the category
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

#remove data missing or out of range or unknown
accident.dt <- accident.dt[!(light_conditions == -1 | light_conditions == 7)]

light_conditions = character()
light_conditions[accident.dt$light_conditions == 1] <- "Daylight"
light_conditions[accident.dt$light_conditions == 4] <- "Darkness(Lights Lit)"
#darkness no lights or light unlit are essentially the same
#hence, we group them together under darkness(No Lights)
light_conditions[accident.dt$light_conditions == 5 | accident.dt$light_conditions == 6] <- "Darkness(No Lights)"
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
accident.dt <- accident.dt[!(first_road_number == 0)]
accident.dt <- accident.dt[!(pedestrian_crossing_human_control == 9 | 
                             pedestrian_crossing_human_control== -1)]
accident.dt <- accident.dt[!(pedestrian_crossing_physical_facilities == 9 | 
                             pedestrian_crossing_physical_facilities == -1)]
accident.dt <- accident.dt[!(weather_conditions == -1 | weather_conditions == 9)]
accident.dt <- accident.dt[!(road_surface_conditions == -1 | 
                             road_surface_conditions == 9)]
accident.dt <- accident.dt[!(special_conditions_at_site == -1 | 
                             special_conditions_at_site == 9)]
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
       title = "Count of Serious vs Not Serious accident across different number of vehicles") 
#We can see that when number of vehicles >= 5, the number of accident cases are very little

#Therefore, we will regroup the categories into 1,2,3,4 and 5 or more
levels(accident.dt$number_of_vehicles) = list("1" = 1, "2" = 2, "3"= 3, "4" = 4, 
                                              "5 or more" = c(5,6,7,8,9,10))

ggplot(data = accident.dt, aes(x = number_of_vehicles,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "Number of \nvehicles", 
       x = "Number of vehicles",
       y = "Percentage", 
       title = "Count of Serious vs Not Serious accidents across different number of vehicles") +
  facet_grid(.~accident_severity) + 
  theme(legend.position = "none")
#Majority of accidents involves 1-2 vehicles, regardless of severity.


ggplot(data = accident.dt, aes(x = number_of_vehicles,fill=accident_severity)) +
  geom_bar(position="fill") +
  labs(x = "Number of vehicles", 
       y = "Proportion", 
       title = "Proportion of Serious vs Not Serious accident over different number of vehicles") 
#Proportion of Severe accidents increases as number of vehicles increases, 
#but is the highest when number of vehicle = 1.
#possible that it is vehicle - pedestrian accidents that are the most severe?

ggplot(data = accident.dt, aes(x=accident_severity, y= number_of_casualties, 
                               color = accident_severity)) +
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
levels(accident.dt$number_of_casualties) = list("1" = 1, "2" = 2, "3"= 3, "4" = 4, 
                                                "5 or more" = c(5:26))

ggplot(data = accident.dt, aes(x = number_of_casualties,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "Number of \ncasualties", 
       x = "Number of Casualties",
       y = "Percentage", 
       title = "Count of Serious vs Not Serious accident with different number of casualties") + 
  theme(legend.position = "none") + 
  facet_grid(.~accident_severity)
#Majority of accidents involves 1-2 casualties, regardless of severity.


ggplot(data = accident.dt, aes(x = number_of_casualties,fill=accident_severity)) +
  geom_bar(position="fill") +
  labs(x = "Number of Casualties", 
       y = "Proportion", 
       title = "Proportion of Serious vs Not Serious accident over different number of casualties") 
#Proportion of Severe accidents increases as number of casualties increases.

ggplot(data = accident.dt, aes(x = day,fill=accident_severity)) +
  geom_bar() +
  labs(x = "day", 
       y = "Count", 
       title = "Count of Serious vs Not Serious accident over different days")
# Accidents are lowest on the weekend, and climb steadily as the week progresses, and peaks on Friday.
#In terms of accident severity, the same trend can be seen for both not serious and serious accidents.
# No outstanding trends for day of week that would make it a good predictor of accident severity.
#however, it seems that proportion of serious accidents increase across the week
#Proportion of serious accidents are lower on weekends as well.

ggplot(data = accident.dt, aes(x = first_road_class,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "First Road Class", 
       x = "First Road Class",
       y = "Percentage", 
       title = "Accident Severity over different road classes") +
  facet_grid(.~accident_severity) +
  scale_x_discrete(labels = c("1" = "Motorway", "2" = "A(M)", "3" = "A",
                              "4" = "B")) + 
  theme(legend.position = "none")
#We can observe that majority of accidents occur at Road Class A, followed by B.
#However, there is no distinction of accident severity which both serious
#and not serious accidents following a similar distribution.

ggplot(data = accident.dt, aes(x=time, fill=accident_severity))+
  geom_density(alpha=0.4) +
  labs(x = "Time of day", 
       y = "Density of Accident Severity", 
       title = "Accident Severity distribution across the day") +
  scale_x_continuous(breaks=seq(0,24,2))
#The figure shows the KDE of accident severity as a function of time of the day. 
#Again, no trends that would lend time of day as a good predictor of severity, since severity
#tend to track one another with time of day.
#Some interesting trends,however, fatal accidents become more frequent than not serious accidents
#between 8pm and 7am. There are two peaks to all severity which occurs between morning rush
#hour 7am and 10am and the evening rush hour 4pm and 7pm.

#Perhaps we can explore grouping them according to time of day.
timeofday <- character()
timeofday[accident.dt$time >= 7 & accident.dt$time < 10] <- "Morning Rush (7-10)"  
timeofday[accident.dt$time >= 10 & accident.dt$time < 16] <- "Day - Non Peak (10-16)"  
timeofday[accident.dt$time >= 16 & accident.dt$time < 19] <- "Evening Rush (16-19)"  
timeofday[accident.dt$time >= 19 & accident.dt$time < 24] <- "Night - Non Peak (19-7)"
timeofday[accident.dt$time >= 0 & accident.dt$time < 7] <- "Night - Non Peak (19-7)"
timeofday <- factor(timeofday, levels = c("Morning Rush (7-10)","Day - Non Peak (10-16)",
                                  "Evening Rush (16-19)","Night - Non Peak (19-7)"))
accident.dt <- cbind(accident.dt, timeofday)
accident.dt[,"time":=NULL] #remove the time column

ggplot(data = accident.dt, aes(x = timeofday,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "Time of \nday", 
       x = "Period of day",
       y = "Percentage", 
       title = "Accident Severity over different periods of a day") +
  facet_grid(.~accident_severity) + 
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
#We can observe that a higher % of serious accidents occur during the non peak hours at night.

ggplot(data = accident.dt, aes(x = light_conditions,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "Light conditions", 
       x = "Light Conditions",
       y = "Percentage", 
       title = "Accident Severity over different light conditions") +
  facet_grid(.~accident_severity) + 
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
#We can see that, the % of serious accidents is higher during darkness compared to daylight

ggplot(data = accident.dt, aes(x = road_type,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "Road Type", 
       x = "Road Type",
       y = "Percentage", 
       title = "Accident Severity over different road types") +
  scale_x_discrete(labels=c("1" = "Roundabout", "2" = "One way street",
                            "3" = "Dual carriageway", "6" = "Single carriageway",
                            "7" = "Slip road", "12" = "One way street/Slip road")) +
  facet_grid(.~accident_severity) + 
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
#We can see that, Slight accidents are much more frequent on roundabout whereas
#serious accidents are more frequent on single carriageways

ggplot(data = accident.dt, aes(x = month,fill=month)) +
  geom_bar() +
  labs(x = "Month", 
       y = "Count", 
       title = "Count of traffic accidents over different months") + 
  theme(legend.position = "none")
#Seems that traffic accidents occur anytime of the year regardless of month
#The winter months tend to have lower accidents in general.

ggplot(data = accident.dt, aes(x = trunk_road_flag,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(x = "Trunk Road Flag",
       y = "Percentage", 
       title = "Accident Severity for different trunk road flags") +
  facet_grid(.~accident_severity) + 
  theme(legend.position = "none")
#The trunk road flags do not seem to affect accident severity at all.

ggplot(data = accident.dt, aes(x = urban_or_rural_area,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "Area type", 
       x = "Area type",
       y = "Percentage", 
       title = "Accident Severity over different areas") +
  scale_x_discrete(labels=c("1" = "Urban", "2"= "Rural")) +
  facet_grid(.~accident_severity) + 
  theme(legend.position = "none")
#Higher frequency of severe accidents tend to occur in urban areas
#probably due to less access or delays to emergency service

ggplot(data = accident.dt, aes(x = carriageway_hazards,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  labs(fill = "carriageway_hazards", 
       x = "Carriageway Hazards",
       y = "Percentage", 
       title = "Accident Severity over different carriageway hazards") +
  facet_grid(.~accident_severity) + 
  theme(legend.position = "none")

ggplot(data = accident.dt, aes(x = special_conditions_at_site,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  labs(fill = "special_conditions_at_site", 
       x = "Onsite condtions",
       y = "Percentage", 
       title = "Accident Severity for different onsite conditions") +
  facet_grid(.~accident_severity) + 
  theme(legend.position = "none")


ggplot(data = accident.dt, aes(x = weather_conditions,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5, size = 3) +
  labs(fill = "weather_conditions", 
       x = "Weather conditions",
       y = "Percentage", 
       title = "Accident Severity over different weather conditions") +
  facet_grid(.~accident_severity) + 
  theme(legend.position = "none")
#Contrary to what is commonly known, the % of serious accidents when the weather is dry
#is higher than that the % for not serious accidents. 
#This might be the case because drivers drive more carefully when the weather is bad.

ggplot(data = accident.dt, aes(x = road_surface_conditions,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5, size = 4) +
  labs(fill = "road_surface_conditions", 
       x = "Road surface conditions",
       y = "Percentage", 
       title = "Accident Severity over different road surface conditions") +
  facet_grid(.~accident_severity) + 
  theme(legend.position = "none")
#Once again, we will regroup into 2 categories, indicating the presence of any
#unsual road surface conditions.
levels(accident.dt$road_surface_conditions) = list("None" = 1, 
                                                   "Abnormal Road Surface Condition" = c(2:5))

ggplot(data = accident.dt, aes(x = pedestrian_crossing_physical_facilities,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "pedestrian_crossing_physical_facilities", 
       x = "Nearby Crossing Facilities",
       y = "Percentage", 
       title = "Accident Severity across nearby crossing facilities") +
  facet_grid(.~accident_severity) + 
  theme(legend.position = "none")
#Serious accidents have higher frequency when there is no nearby crossing facilities

ggplot(data = accident.dt, aes(x = junction_control,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "junction_control", 
       x = "Junction Control",
       y = "Percentage", 
       title = "Accident Severity for different junction controls") +
  facet_grid(.~accident_severity) + 
  theme(legend.position = "none")
#Serious accidents occur more frequently at give way/uncontrolled junctions
#Not Serious accidents have a higher proportion at auto traffic signals

ggplot(data = accident.dt, aes(x = junction_detail,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5, size = 2.5) +
  labs(fill = "junction_detail", 
       x = "Junction Detail",
       y = "Percentage", 
       title = "Accident Severity for different junction details") +
  facet_grid(.~accident_severity) + 
  theme(legend.position = "none")
#Serious accidents occur more frequently at T or staggered junctions
#Not serious accidents occur more frequently at roundabouts

ggplot(data = accident.dt, aes(x = speed_limit,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "speed_limit", 
       x = "Speed Limit",
       y = "Percentage", 
       title = "Accident Severity at different speed limits") +
  facet_grid(.~accident_severity) + 
  theme(legend.position = "none")
#For posted speed limit, data originally was split into eight speed categories.
#However, there was a clear dichotomy in accident severity we could preserve
#if we consolidated speeds to under 40km/s and Over 40km/s, corresponding to
#non-highway and highway speeds.

#Lets regroup speed limit into a binary categorical variable
levels(accident.dt$speed_limit) = list("Below 40km/s" = c(20,30,40), "Above 40km/s" = c(50,60,70))

ggplot(data = accident.dt, aes(x = speed_limit,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "speed_limit", 
       x = "Speed Limit",
       y = "Percentage", 
       title = "Accident Severity for different speed limits") +
  facet_grid(.~accident_severity) + 
  theme(legend.position = "none")
#This clear diverge is seen which shows that fatal accidents are much more frequent
#at speeds over 40km/s, while slight accidents are much more frequent at speeds under 40km/s.

#===============================================================================

#Determining the importance of geospatial data 
#Note: You need to input your own API key for this section to run
#For setting up API key: 
#https://www.youtube.com/watch?v=Of_M4kcE9yM&ab_channel=StatisticsGuideswithDrPaulChristiansen

#===============================================================================

#get the United Kingdom map from Google MAPS API
accidentmap <- get_map(location=c(-2.3374,53.76946),
                       zoom = 6, maptype = 'terrain', color = 'color')

ggmap(accidentmap) +
  geom_point(aes(x=longitude, y=latitude, colour=accident_severity), data=accident.dt) +  
  ggtitle('Traffic Accidents in United Kingdom')
#This is not a particularly useful map. 
#The colors all overlap and obscure one another. 
#No patterns are readily visible. 
#Lets see if mapping by category will be more revealing. 

ggmap(accidentmap) +
  geom_point(aes(x=longitude, y=latitude, colour=accident_severity), data=accident.dt) +  
  ggtitle('Traffic Accidents in United Kingdom by severity') +
  facet_grid(.~accident_severity)
#Even when broken out by category the maps aren't particularly revealing. 
#Maybe a different type of map would be more informative.

#Lets try contour plot to find out hotspots instead...

#contour plot settings using stats_density2d
contours <- stat_density2d(
  aes(x = longitude, y = latitude, fill = ..level..),
  size = 0.1, data = accident.dt, n=200,
  geom = "polygon")

ggmap(accidentmap) + 
  contours +
  scale_fill_distiller(palette= "Spectral", direction=1) +
  facet_grid(.~accident_severity) +
  ggtitle('Traffic Accident in United Kingdom')

#This map seems much more informative. 
#It reveals a giant hotspot in the London area, with lots of smaller hotspots 
#in Manchester and Birmingham. 
#This is normal as these are the major cities in the United Kingdom.

#setting the limits to narrow down map scope
lims <- coord_map(xlim=c(-2, 2), ylim=c(50, 53))  
ggmap(accidentmap) + 
  lims +
  contours +
  scale_fill_distiller(palette= "Spectral", direction=1) +
  facet_grid(.~accident_severity) +
  ggtitle('Zoom in On London for Traffic Accidents in United Kingdom')
#From the contour plots we can see that geospatial data is an important factor.
#Not serious accidents have greater occurence nearer the city centre.
#Serious accidents tend to be more evenly distributed throughout London
#with a large radius extending London.

#===============================================================================

#Exploring geospatial data with police force district, local authority district
#local authority highway district and first road class number...

#===============================================================================

cramerV(accident.dt$local_authority_district,accident.dt$local_authority_ons_district) #1
cramerV(accident.dt$local_authority_district,accident.dt$local_authority_highway) #1
#Local authority district and local authority ons district are the same. 
#Local authority district and local authority highway are also essentially the same.
#We shall only use one of them for our analysis.

#lsoa_of_accident_location is only for Wales and England, hence we will remove it

accident.dt[,c("lsoa_of_accident_location","local_authority_ons_district",
               "local_authority_highway"):=NULL]

#These geospatial data has too many categories, inputting them into our models will detrimental
#This is because high cardinality of categories makes it difficult for the model to identify 
#such patterns and hence the model doesn't generalise well to examples outside the training set.

#Geospatial data is however important, hence, we cannot just discard it.
#Hence, we shall use frequency encoding for the geospatial data we have
#to determine if any meaningful relationships can be seen before feeding it into
#our machine learning model.

#extract the count of each category 
geoSpatialData <- catto_freq(accident.dt,c("local_authority_district",
                                           "first_road_number","police_force"))


#================ local_authority_district =====================================

#getting frequency as a proportion of total observations
geoSpatialData$local_authority_district = (geoSpatialData$local_authority_district) / length(geoSpatialData$local_authority_district)

#min-max scaler to get the range of [0,1] for frequency.
geoSpatialData$local_authority_district = 
  (geoSpatialData$local_authority_district - min(geoSpatialData$local_authority_district)) / 
  (max(geoSpatialData$local_authority_district) - min(geoSpatialData$local_authority_district))

ggplot(data = geoSpatialData, aes(x=local_authority_district, fill=accident_severity))+
  geom_density(alpha=0.4) +
  labs(x = "Local Authority District Frequency", 
       y = "Density of Accident Severity", 
       title = "Accident Severity distribution across different local authority district")
#We can see that Serious accidents have higher likelihood at local authority districts
#with lower frequencies. Therefore, local authority district is an important
#geospatial data

#============================ police force =====================================

geoSpatialData$police_force = (geoSpatialData$police_force) / length(geoSpatialData$police_force)

#minmax scaler [0,1]
geoSpatialData$police_force = 
  (geoSpatialData$police_force - min(geoSpatialData$police_force)) / 
  (max(geoSpatialData$police_force) - min(geoSpatialData$police_force))

ggplot(data = geoSpatialData, aes(x=police_force, fill=accident_severity))+
  geom_density(alpha=0.4) +
  labs(x = "Police Force Frequency", 
       y = "Density of Accident Severity", 
       title = "Accident Severity distribution across different police force")
#police force district doesn't really affect accident severity as the severity
#follows a similar distribution

#===================== first road number ========================================

geoSpatialData$first_road_number = (geoSpatialData$first_road_number) / length(geoSpatialData$first_road_number)

#minmax scaler [0,1]
geoSpatialData$first_road_number = 
  (geoSpatialData$first_road_number - min(geoSpatialData$first_road_number)) / 
  (max(geoSpatialData$first_road_number) - min(geoSpatialData$first_road_number))

ggplot(data = geoSpatialData, aes(x=first_road_number, fill=accident_severity))+
  geom_density(alpha=0.4) +
  labs(x = "First Road Number Frequency", 
       y = "Density of Accident Severity", 
       title = "Accident Severity distribution across different first road numbers")
#The first_road_number does not really affect accident severity as the severity
#follows a similar distribution

#first_road_number and police_force were removed, as it does not really affect accident severity
#furthermore, it offers the same information as local_authority_district
#accident_year is excluded as we are not considering the year when making predictions
#number of vehicles, number of casualties and police officer present are merely descriptions 
#of accidents and they do not offer predictive value.
colsToRemove = c("accident_year", "police_force", "number_of_vehicles",
                 "number_of_casualties", "first_road_number", "police_officer_present")

accident.dt[,(colsToRemove):=NULL]

#=================== END OF DATA EXPLORATION OF ACCIDENT DATA ==================

#===============================================================================

#======================= Exporting cleaned data ================================

#set working directory to export cleaned data to...
setwd("C:/Users/joshua/Downloads/BC2407 Analytics II/Datasets Cleaned")

#export as csv
write.csv(accident.dt, "accident_data_cleaned.csv",row.names = FALSE)

#===============================================================================
