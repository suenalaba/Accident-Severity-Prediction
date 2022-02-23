#import respective libraries
library(data.table)
library(stringr)
library(ggplot2)

#set your working directory here..
setwd("C:/Users/joshua/Downloads/BC2407 Analytics II/Datasets Raw")

#import dataset as datatable
accident.dt <- fread("dft-road-casualty-statistics-accident-last-5-years.csv")


View(accident.dt)
str(accident.dt)

#We are only interested to study year 2017 to 2019
accident.dt <- accident.dt[accident_year == 2017 | accident_year == 2018 | accident_year == 2019]

#accident_index is unique but accident_reference is only unique within the year, therefore we will remove it
accident.dt[,accident_reference:=NULL]

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
  
#accident severity, initially Fatal = 1, serious = 2, slight = 3
#we will regroup serious and fatal together, with slight as a different category
accident_severity = character()
accident_severity[accident.dt$accident_severity == 1 | accident.dt$accident_severity == 2] <- "Serious"
accident_severity[accident.dt$accident_severity == 3] <- "Not Serious"
accident_severity <- as.factor(accident_severity)
accident.dt[,accident_severity:=NULL] #remove original severity column
accident.dt <- cbind(accident.dt,accident_severity) #add newly created severity column

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
accident.dt[,c("substring_Time","time"):=NULL] #remove the time column

#For second_road_class, ~82% of data is either unclassified or missing/out of range
#Hence, we will remove this column to reduce the removal of data points due to NULL values.
length(grep(-1, accident.dt$second_road_class)) #155668
length(grep(6, accident.dt$second_road_class)) #147782
nrow(accident.dt) #370041

#remove all unspecified, null and data missing or out of range
accident.dt <- accident.dt[!(road_type == -1 | road_type == 9)]
accident.dt <- accident.dt[!(junction_detail == 99 | junction_detail == -1)]
accident.dt <- accident.dt[!(junction_control == -1 | road_type == 9)]
accident.dt <- accident.dt[!(first_road_class == 6)]
accident.dt <- accident.dt[!(pedestrian_crossing_human_control == 9 | pedestrian_crossing_human_control== -1)]
accident.dt <- accident.dt[!(pedestrian_crossing_physical_facilities == 9 | pedestrian_crossing_physical_facilities == -1)]
accident.dt <- accident.dt[!(light_conditions == -1)]
accident.dt <- accident.dt[!(weather_conditions == -1 | weather_conditions == 9)]
accident.dt <- accident.dt[!(road_surface_conditions == -1 | road_surface_conditions == 9)]
accident.dt <- accident.dt[!(special_conditions_at_site == -1 | special_conditions_at_site == 9)]
accident.dt <- accident.dt[!(carriageway_hazards == -1 | carriageway_hazards == 9)]
accident.dt <- accident.dt[!(urban_or_rural_area == -1 | urban_or_rural_area == 3)]
accident.dt <- accident.dt[!(did_police_officer_attend_scene_of_accident == -1)]
accident.dt <- accident.dt[!(trunk_road_flag == -1)]

#converting police_officer_present to categorical
#based on data documentation, police_officer_attend == 2/3 means NO, therefore, we will regroup them together
police_officer_present = numeric()
police_officer_present[accident.dt$did_police_officer_attend_scene_of_accident == 2 | 
          accident.dt$did_police_officer_attend_scene_of_accident == 3] <- 0
police_officer_present[accident.dt$did_police_officer_attend_scene_of_accident == 1] <- 1
police_officer_present <- as.factor(police_officer_present)
accident.dt <- cbind(accident.dt,police_officer_present) #add newly created column
accident.dt[,did_police_officer_attend_scene_of_accident:=NULL] #remove existing column


#convert int/chr to categorical
categoricalcols = c("first_road_class","road_type","junction_detail","junction_control",
                    "pedestrian_crossing_human_control","pedestrian_crossing_physical_facilities",
                    "light_conditions","weather_conditions","road_surface_conditions","special_conditions_at_site",
                    "carriageway_hazards","urban_or_rural_area", "trunk_road_flag")
accident.dt[, (categoricalcols) := lapply(.SD, as.factor), .SDcols = categoricalcols]

#convert speed to limit to numeric
accident.dt[,"speed_limit" := lapply(.SD, as.factor), .SDcols = "speed_limit"]


test = c("local_authority_district","local_authority_ons_district","local_authority_highway","police_force")
accident.dt[,(test) := lapply(.SD, as.factor), .SDcols = (test)]

# scotland

# wales

#london 

# manchester

#birmingham

#leeds

#others -> 


