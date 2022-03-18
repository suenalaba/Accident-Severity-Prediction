#Automation to install packages that are necessary but not yet installed
list.of.packages <- c("data.table", "stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#import respective libraries
library(data.table)
library(stringr)

#set your working directory here..
setwd("C:/Users/joshua/Downloads/BC2407 Analytics II/Datasets Raw")

#import dataset as datatable
accident.dt <- fread("dft-road-casualty-statistics-accident-last-5-years.csv")

#We are only interested to study year 2017 to 2019
accident.dt <- accident.dt[accident_year == 2017 | accident_year == 2018 | accident_year == 2019]
accident.dt[,"accident_year" := lapply(.SD, as.factor), .SDcols = "accident_year"]

#accident_index is unique but accident_reference is only unique within the year, therefore we will remove it
accident.dt[,accident_reference:=NULL]

#remove all data points without a specified location
accident.dt <- accident.dt[!(location_easting_osgr == "NULL" | latitude == "NULL" 
                             | location_northing_osgr == "NULL" | longitude == "NULL")]

#convert locations, longtitude & latitude from char to numeric
locationtemp = c("location_easting_osgr", "location_northing_osgr", 
                 "longitude", "latitude")
accident.dt[, (locationtemp) := lapply(.SD, as.numeric), .SDcols = locationtemp]

#Since the correlation (easting,longitude) & (northing,latitude) are indeed close to 1, we will remove one of them
accident.dt[,c("location_easting_osgr","location_northing_osgr"):=NULL]

#converting remaining geographic location information to categories
locationdatatocat = c("local_authority_district","local_authority_ons_district",
                      "local_authority_highway","police_force","lsoa_of_accident_location")
accident.dt[,(locationdatatocat) := lapply(.SD, as.factor), .SDcols = (locationdatatocat)]

#converting speed limit from char to numeric
accident.dt[,"speed_limit" := lapply(.SD, as.numeric), .SDcols = "speed_limit"]

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

#Perhaps we can explore grouping them according to time of day.
timeofday <- character()
timeofday[accident.dt$time >= 7 & accident.dt$time < 10] <- "Morning Rush (7-10)"  
timeofday[accident.dt$time >= 10 & accident.dt$time < 16] <- "Day - Non Peak (10-16)"  
timeofday[accident.dt$time >= 16 & accident.dt$time < 19] <- "Evening Rush (16-19)"  
timeofday[accident.dt$time >= 19 & accident.dt$time < 24] <- "Night - Non Peak (19-7)"
timeofday[accident.dt$time >= 0 & accident.dt$time < 7] <- "Night - Non Peak (19-7)"
month <- factor(month, levels = c("Morning Rush (7-10)","Day - Non Peak (10-16)",
                                  "Evening Rush (16-19)","Night - Non Peak (19-7)"))
accident.dt <- cbind(accident.dt, timeofday)
accident.dt[,"time":=NULL] #remove the time column

accident.dt[,c("second_road_number","second_road_class"):=NULL] #remove the second road class/number column

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
levels(accident.dt$police_officer_present) = list("No" = 0, "Yes" = 1)

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

#===============================================================================

#Chunk renaming for accident.dt

#===============================================================================

levels(accident.dt$trunk_road_flag) = list("Trunk" = 1, "Non-trunk" = 2)
levels(accident.dt$urban_or_rural_area) = list("Urban" = 1, "Rural" = 2)
levels(accident.dt$carriageway_hazards) = list("None" = 0, "Vehicle load on road" = 1,
                                               "Other object on road" = 2,
                                               "Previous accident" = 3,
                                               "Dog on road" = 4,
                                               "Other animal on road" = 5,
                                               "Pedestrian in carriageway - not injured" = 6,
                                               "Any animal in carriageway (except ridden horse)" = 7)
levels(accident.dt$special_conditions_at_site) = list("None" = 0,
                                                      "Auto traffic signal - out" = 1,
                                                      "Auto signal part defective" = 2,
                                                      "Road sign or marking defective or obscured" = 3,
                                                      "Roadworks" = 4,
                                                      "Road surface defective" = 5,
                                                      "Oil or diesel" = 6,
                                                      "Mud" = 7)
levels(accident.dt$road_surface_conditions) = list("Dry" = 1,
                                           "Wet or damp" = 2,
                                           "Snow" = 3,
                                           "Frost or ice" = 4,
                                           "Flood over 3cm. deep" = 5,
                                           "Oil or diesel" = 6,
                                           "Mud" = 7)
levels(accident.dt$weather_conditions) = list("None" = 0,
                                              "Fine no high winds" = 1,
                                              "Raining no high winds" = 2,
                                              "Snowing no high winds" = 3,
                                              "Fine + high winds" = 4,
                                              "Raining + high winds" = 5,
                                              "Snowing + high winds" = 6,
                                              "Fog or mist" = 7,
                                              "Other" = 8)
levels(accident.dt$pedestrian_crossing_physical_facilities) = list("No physical crossing facilities within 50 metres" = 0,
                                                                   "Zebra" = 1,
                                                                   "Pelican, puffin, toucan or similar non-junction pedestrian light crossing" = 4,
                                                                   "Pedestrian phase at traffic signal junction" = 5,
                                                                   "Footbridge or subway" = 7,
                                                                   "Central refuge" = 8)
levels(accident.dt$pedestrian_crossing_human_control) = list("None within 50 metres" = 0,
                                                             "Control by school crossing patrol" = 1,
                                                             "Control by other authorised person" = 2)

levels(accident.dt$junction_control) = list("Not at junction or within 20 metres" = 0,
                                            "Authorised person" = 1,
                                            "Auto traffic signal" = 2,
                                            "Stop sign" = 3,
                                            "Give way or uncontrolled" = 4)
levels(accident.dt$junction_detail) = list("Not at junction or within 20 metres" = 0,
                                           "Roundabout" = 1,
                                           "Mini-roundabout" = 2,
                                           "T or staggered junction" = 3,
                                           "Slip road" = 5,
                                           "Crossroads" = 6,
                                           "More than 4 arms (not roundabout)" = 7,
                                           "Private drive or entrance" = 8,
                                           "Other junction" = 9)
levels(accident.dt$road_type) = list("Roundabout" = 1,
                                     "One way street" = 2,
                                     "Dual carriageway" = 3,
                                     "Single carriageway" = 6,
                                     "Slip road" = 7)
levels(accident.dt$first_road_class) = list("Motorway" = 1,
                                            "A(M)" = 2,
                                            "A" = 3,
                                            "B" = 4,
                                            "C" = 5)
levels(accident.dt$pedestrian_crossing_human_control) = list("None within 50 metres" = 0,
                                                             "Control by school crossing patrol" = 1,
                                                             "Control by other authorised person" = 2)

#===============================================================================

#Cleaning: vehicle data

#===============================================================================

#import dataset into data.table
vehicle.dt <- fread("dft-road-casualty-statistics-vehicle-last-5-years.csv")

#View(vehicle.dt)
str(vehicle.dt)
#data dimensions: 1,101,591 * 27

#subset dataset for years 2017-2019 only
vehicle.dt <- subset(vehicle.dt, accident_year==2017|accident_year==2018|accident_year==2019)

#remove accident reference and vehicle reference since they are redundant
#common join can be done on accident_index
vehicle.dt[,c("vehicle_reference","accident_reference"):=NULL]

#===============================================================================

#Mass cleaning data by removing irrelevant rows

#===============================================================================

#remove all unspecified, null and data missing or out of range
vehicle.dt <- vehicle.dt[!(vehicle_type == -1|vehicle_type == 99)]
vehicle.dt <- vehicle.dt[!(towing_and_articulation == -1|towing_and_articulation == 9)]
vehicle.dt <- vehicle.dt[!(vehicle_manoeuvre == -1|vehicle_manoeuvre == 99)]
vehicle.dt <- vehicle.dt[!(vehicle_direction_from == -1|vehicle_direction_from == 9)]
vehicle.dt <- vehicle.dt[!(vehicle_direction_to == -1|vehicle_direction_to == 9)]
vehicle.dt <- vehicle.dt[!(vehicle_location_restricted_lane == -1|vehicle_location_restricted_lane == 99)]
vehicle.dt <- vehicle.dt[!(junction_location == -1|junction_location == 9)]
vehicle.dt <- vehicle.dt[!(skidding_and_overturning == -1|skidding_and_overturning == 9)]
vehicle.dt <- vehicle.dt[!(hit_object_in_carriageway == -1|hit_object_in_carriageway == 99)]
vehicle.dt <- vehicle.dt[!(vehicle_leaving_carriageway == -1|vehicle_leaving_carriageway == 9)]
vehicle.dt <- vehicle.dt[!(hit_object_off_carriageway == -1|hit_object_off_carriageway == 99)]
vehicle.dt <- vehicle.dt[!(first_point_of_impact == -1|first_point_of_impact == 9)]
vehicle.dt <- vehicle.dt[!(vehicle_left_hand_drive == -1|vehicle_left_hand_drive == 9)]
vehicle.dt <- vehicle.dt[!(journey_purpose_of_driver == -1|journey_purpose_of_driver == 15|journey_purpose_of_driver == 6)]
vehicle.dt <- vehicle.dt[!(sex_of_driver == -1|sex_of_driver == 3)]
vehicle.dt <- vehicle.dt[!(age_of_driver == -1)]
vehicle.dt <- vehicle.dt[!(age_band_of_driver == -1)]
vehicle.dt <- vehicle.dt[!(engine_capacity_cc == -1)]
vehicle.dt <- vehicle.dt[!(propulsion_code == -1)]
vehicle.dt <- vehicle.dt[!(age_of_vehicle == -1)]
vehicle.dt <- vehicle.dt[!(driver_imd_decile == -1)]
vehicle.dt <- vehicle.dt[!(driver_home_area_type == -1)]

#As 100% of data in generic_make_model is missing/out of range, we will remove it.
length(grep(-1, vehicle.dt$generic_make_model)) #407,725
vehicle.dt[,generic_make_model:=NULL]

#===============================================================================

#Converting variables from numeric to categorical

#===============================================================================

#converting towing_and_articulation to categorical
table(vehicle.dt$towing_and_articulation)
towing_and_articulation = character()
towing_and_articulation[vehicle.dt$towing_and_articulation == 0] <- "No tow/articulation"
towing_and_articulation[vehicle.dt$towing_and_articulation == 1] <- "Articulated vehicle"
towing_and_articulation[vehicle.dt$towing_and_articulation == 2] <- "Double or multiple trailer"
towing_and_articulation[vehicle.dt$towing_and_articulation == 3] <- "Caravan"
towing_and_articulation[vehicle.dt$towing_and_articulation == 4] <- "Single trailer"
towing_and_articulation[vehicle.dt$towing_and_articulation == 5] <- "Other tow"
towing_and_articulation <- factor(towing_and_articulation, levels = c("No tow/articulation", 
                                                                      "Articulated vehicle", 
                                                                      "Double or multiple trailer",
                                                                      "Caravan",
                                                                      "Single trailer",
                                                                      "Other tow"))
vehicle.dt[,towing_and_articulation:=NULL]
vehicle.dt <- cbind(vehicle.dt, towing_and_articulation)
rm(towing_and_articulation)
#verifying that distribution is intact
table(vehicle.dt$towing_and_articulation)

#converting vehicle_manoeuvre to categorical
table(vehicle.dt$vehicle_manoeuvre)
vehicle_manoeuvre = character()
vehicle_manoeuvre[vehicle.dt$vehicle_manoeuvre == 1] <- "Reversing"
vehicle_manoeuvre[vehicle.dt$vehicle_manoeuvre == 2] <- "Parked"
vehicle_manoeuvre[vehicle.dt$vehicle_manoeuvre == 3] <- "Waiting to go - held up"
vehicle_manoeuvre[vehicle.dt$vehicle_manoeuvre == 4] <- "Slowing or stopping"
vehicle_manoeuvre[vehicle.dt$vehicle_manoeuvre == 5] <- "Moving off"
vehicle_manoeuvre[vehicle.dt$vehicle_manoeuvre == 6] <- "U-turn"
vehicle_manoeuvre[vehicle.dt$vehicle_manoeuvre == 7] <- "Turning left"
vehicle_manoeuvre[vehicle.dt$vehicle_manoeuvre == 8] <- "Waiting to turn left"
vehicle_manoeuvre[vehicle.dt$vehicle_manoeuvre == 9] <- "Turning right"
vehicle_manoeuvre[vehicle.dt$vehicle_manoeuvre == 10] <- "Waiting to turn right"
vehicle_manoeuvre[vehicle.dt$vehicle_manoeuvre == 11] <- "Changing lane to left"
vehicle_manoeuvre[vehicle.dt$vehicle_manoeuvre == 12] <- "Changing lane to right"
vehicle_manoeuvre[vehicle.dt$vehicle_manoeuvre == 13] <- "Overtaking moving vehicle - offside"
vehicle_manoeuvre[vehicle.dt$vehicle_manoeuvre == 14] <- "Overtaking static vehicle - offside"
vehicle_manoeuvre[vehicle.dt$vehicle_manoeuvre == 15] <- "Overtaking - nearside"
vehicle_manoeuvre[vehicle.dt$vehicle_manoeuvre == 16] <- "Going ahead left-hand bend"
vehicle_manoeuvre[vehicle.dt$vehicle_manoeuvre == 17] <- "Going ahead right-hand bend"
vehicle_manoeuvre[vehicle.dt$vehicle_manoeuvre == 18] <- "Going ahead other"
vehicle_manoeuvre <- factor(vehicle_manoeuvre, 
                            levels = c("Reversing", "Parked", 
                                       "Waiting to go - held up", 
                                       "Slowing or stopping", "Moving off", 
                                       "U-turn", "Turning left", "Waiting to turn left", 
                                       "Turning right", "Waiting to turn right", 
                                       "Changing lane to left", "Changing lane to right", 
                                       "Overtaking moving vehicle - offside", 
                                       "Overtaking static vehicle - offside", 
                                       "Overtaking - nearside", 
                                       "Going ahead left-hand bend", 
                                       "Going ahead right-hand bend", 
                                       "Going ahead other"))
vehicle.dt[,vehicle_manoeuvre:=NULL]
vehicle.dt <- cbind(vehicle.dt, vehicle_manoeuvre)
rm(vehicle_manoeuvre)
table(vehicle.dt$vehicle_manoeuvre)

#converting vehicle_direction_from to categorical
table(vehicle.dt$vehicle_direction_from)
vehicle_direction_from = character()
vehicle_direction_from[vehicle.dt$vehicle_direction_from == 0] <- "Parked"
vehicle_direction_from[vehicle.dt$vehicle_direction_from == 1] <- "N"
vehicle_direction_from[vehicle.dt$vehicle_direction_from == 2] <- "NE"
vehicle_direction_from[vehicle.dt$vehicle_direction_from == 3] <- "E"
#Data guide erroneously labelled 2 columns as South East. The second one should be South.
vehicle_direction_from[vehicle.dt$vehicle_direction_from == 4] <- "SE"
vehicle_direction_from[vehicle.dt$vehicle_direction_from == 5] <- "S"
vehicle_direction_from[vehicle.dt$vehicle_direction_from == 6] <- "SW"
vehicle_direction_from[vehicle.dt$vehicle_direction_from == 7] <- "W"
vehicle_direction_from[vehicle.dt$vehicle_direction_from == 8] <- "NW"
vehicle_direction_from <- factor(vehicle_direction_from, levels = c("Parked", "N", 
                                                                    "NE", "E", "SE", 
                                                                    "S", "SW", "W", "NW"))
vehicle.dt[,vehicle_direction_from:=NULL]
vehicle.dt <- cbind(vehicle.dt, vehicle_direction_from)
rm(vehicle_direction_from)
table(vehicle.dt$vehicle_direction_from)

#converting vehicle_direction_to to categorical
table(vehicle.dt$vehicle_direction_to)
vehicle_direction_to = character()
vehicle_direction_to[vehicle.dt$vehicle_direction_to == 0] <- "Parked"
vehicle_direction_to[vehicle.dt$vehicle_direction_to == 1] <- "N"
vehicle_direction_to[vehicle.dt$vehicle_direction_to == 2] <- "NE"
vehicle_direction_to[vehicle.dt$vehicle_direction_to == 3] <- "E"
vehicle_direction_to[vehicle.dt$vehicle_direction_to == 4] <- "SE"
vehicle_direction_to[vehicle.dt$vehicle_direction_to == 5] <- "S"
vehicle_direction_to[vehicle.dt$vehicle_direction_to == 6] <- "SW"
vehicle_direction_to[vehicle.dt$vehicle_direction_to == 7] <- "W"
vehicle_direction_to[vehicle.dt$vehicle_direction_to == 8] <- "NW"
vehicle_direction_to <- factor(vehicle_direction_to, levels = c("Parked", "N", 
                                                                "NE", "E", "SE", 
                                                                "S", "SW", "W", "NW"))
vehicle.dt[,vehicle_direction_to:=NULL]
vehicle.dt <- cbind(vehicle.dt, vehicle_direction_to)
rm(vehicle_direction_to)
table(vehicle.dt$vehicle_direction_to)

#converting vehicle_location_restricted_lane to categorical
table(vehicle.dt$vehicle_location_restricted_lane)
vehicle_location_restricted_lane = character()
vehicle_location_restricted_lane[vehicle.dt$vehicle_location_restricted_lane == 0] <- "On main c'way - not in restricted lane"
vehicle_location_restricted_lane[vehicle.dt$vehicle_location_restricted_lane == 1] <- "Tram/Light rail track"
vehicle_location_restricted_lane[vehicle.dt$vehicle_location_restricted_lane == 2] <- "Bus lane"
vehicle_location_restricted_lane[vehicle.dt$vehicle_location_restricted_lane == 3] <- "Busway (including guided busway)"
vehicle_location_restricted_lane[vehicle.dt$vehicle_location_restricted_lane == 4] <- "Cycle lane (on main carriageway)"
vehicle_location_restricted_lane[vehicle.dt$vehicle_location_restricted_lane == 5] <- "Cycleway or shared use footway (not part of  main carriageway)"
vehicle_location_restricted_lane[vehicle.dt$vehicle_location_restricted_lane == 6] <- "On lay-by or hard shoulder"
vehicle_location_restricted_lane[vehicle.dt$vehicle_location_restricted_lane == 7] <- "Entering lay-by or hard shoulder"
vehicle_location_restricted_lane[vehicle.dt$vehicle_location_restricted_lane == 8] <- "Leaving lay-by or hard shoulder"
vehicle_location_restricted_lane[vehicle.dt$vehicle_location_restricted_lane == 9] <- "Footway (pavement)"
vehicle_location_restricted_lane <- factor(vehicle_location_restricted_lane, 
                                           levels = c("On main c'way - not in restricted lane", 
                                                      "Tram/Light rail track", "Bus lane", 
                                                      "Busway (including guided busway)", 
                                                      "Cycle lane (on main carriageway)", 
                                                      "Cycleway or shared use footway (not part of  main carriageway)", 
                                                      "On lay-by or hard shoulder", 
                                                      "Entering lay-by or hard shoulder", 
                                                      "Leaving lay-by or hard shoulder", 
                                                      "Footway (pavement)"))
vehicle.dt[,vehicle_location_restricted_lane:=NULL]
vehicle.dt <- cbind(vehicle.dt, vehicle_location_restricted_lane)
rm(vehicle_location_restricted_lane)
table(vehicle.dt$vehicle_location_restricted_lane)

#converting junction_location to categorical
table(vehicle.dt$junction_location)
junction_location = character()
junction_location[vehicle.dt$junction_location == 0] <- "Not at or within 20 metres of junction"
junction_location[vehicle.dt$junction_location == 1] <- "Approaching junction or waiting/parked at junction approach"
junction_location[vehicle.dt$junction_location == 2] <- "Cleared junction or waiting/parked at junction exit"
junction_location[vehicle.dt$junction_location == 3] <- "Leaving roundabout"
junction_location[vehicle.dt$junction_location == 4] <- "Entering roundabout"
junction_location[vehicle.dt$junction_location == 5] <- "Leaving main road"
junction_location[vehicle.dt$junction_location == 6] <- "Entering main road"
junction_location[vehicle.dt$junction_location == 7] <- "Entering from slip road"
junction_location[vehicle.dt$junction_location == 8] <- "Mid Junction - on roundabout or on main road"
junction_location <- factor(junction_location, levels = c("Not at or within 20 metres of junction",
                                                          "Approaching junction or waiting/parked at junction approach",
                                                          "Cleared junction or waiting/parked at junction exit",
                                                          "Leaving roundabout",
                                                          "Entering roundabout",
                                                          "Leaving main road",
                                                          "Entering main road",
                                                          "Entering from slip road",
                                                          "Mid Junction - on roundabout or on main road"))
vehicle.dt[,junction_location:=NULL]
vehicle.dt <- cbind(vehicle.dt, junction_location)
rm(junction_location)
table(vehicle.dt$junction_location)

#converting skidding_and_overturning to categorical
table(vehicle.dt$skidding_and_overturning)
skidding_and_overturning = character()
skidding_and_overturning[vehicle.dt$skidding_and_overturning == 0] <- "None"
skidding_and_overturning[vehicle.dt$skidding_and_overturning == 1] <- "Skidded"
skidding_and_overturning[vehicle.dt$skidding_and_overturning == 2] <- "Skidded and overturned"
skidding_and_overturning[vehicle.dt$skidding_and_overturning == 3] <- "Jackknifed"
skidding_and_overturning[vehicle.dt$skidding_and_overturning == 4] <- "Jackknifed and overturned"
skidding_and_overturning[vehicle.dt$skidding_and_overturning == 5] <- "Overturned"
skidding_and_overturning <- factor(skidding_and_overturning, levels = c("None", "Skidded", 
                                                                        "Skidded and overturned", 
                                                                        "Jackknifed", 
                                                                        "Jackknifed and overturned", 
                                                                        "Overturned"))
vehicle.dt[,skidding_and_overturning:=NULL]
vehicle.dt <- cbind(vehicle.dt, skidding_and_overturning)
rm(skidding_and_overturning)
table(vehicle.dt$skidding_and_overturning)

#converting hit_object_in_carriageway to categorical
table(vehicle.dt$hit_object_in_carriageway)
hit_object_in_carriageway = character()
hit_object_in_carriageway[vehicle.dt$hit_object_in_carriageway == 0] <- "None"
hit_object_in_carriageway[vehicle.dt$hit_object_in_carriageway == 1] <- "Previous accident"
hit_object_in_carriageway[vehicle.dt$hit_object_in_carriageway == 2] <- "Road works"
hit_object_in_carriageway[vehicle.dt$hit_object_in_carriageway == 4] <- "Parked vehicle"
hit_object_in_carriageway[vehicle.dt$hit_object_in_carriageway == 5] <- "Bridge (roof)"
hit_object_in_carriageway[vehicle.dt$hit_object_in_carriageway == 6] <- "Bridge (side)"
hit_object_in_carriageway[vehicle.dt$hit_object_in_carriageway == 7] <- "Bollard or refuge"
hit_object_in_carriageway[vehicle.dt$hit_object_in_carriageway == 8] <- "Open door of vehicle"
hit_object_in_carriageway[vehicle.dt$hit_object_in_carriageway == 9] <- "Central island of roundabout"
hit_object_in_carriageway[vehicle.dt$hit_object_in_carriageway == 10] <- "Kerb"
hit_object_in_carriageway[vehicle.dt$hit_object_in_carriageway == 11] <- "Other object"
hit_object_in_carriageway[vehicle.dt$hit_object_in_carriageway == 12] <- "Any animal (except ridden horse)"
hit_object_in_carriageway <- factor(hit_object_in_carriageway, levels = c("None", "Previous accident",
                                                                          "Road works", "Parked vehicle",
                                                                          "Bridge (roof)", "Bridge (side)",
                                                                          "Bollard or refuge", 
                                                                          "Open door of vehicle", 
                                                                          "Central island of roundabout", 
                                                                          "Kerb", "Other object", 
                                                                          "Any animal (except ridden horse)"))
vehicle.dt[,hit_object_in_carriageway:=NULL]
vehicle.dt <- cbind(vehicle.dt, hit_object_in_carriageway)
rm(hit_object_in_carriageway)
table(vehicle.dt$hit_object_in_carriageway)

#converting vehicle_leaving_carriageway to categorical
table(vehicle.dt$vehicle_leaving_carriageway)
vehicle_leaving_carriageway = character()
vehicle_leaving_carriageway[vehicle.dt$vehicle_leaving_carriageway == 0] <- "Did not leave carriageway"
vehicle_leaving_carriageway[vehicle.dt$vehicle_leaving_carriageway == 1] <- "Nearside"
vehicle_leaving_carriageway[vehicle.dt$vehicle_leaving_carriageway == 2] <- "Nearside and rebounded"
vehicle_leaving_carriageway[vehicle.dt$vehicle_leaving_carriageway == 3] <- "Straight ahead at junction"
vehicle_leaving_carriageway[vehicle.dt$vehicle_leaving_carriageway == 4] <- "Offside on to central reservation"
vehicle_leaving_carriageway[vehicle.dt$vehicle_leaving_carriageway == 5] <- "Offside on to central res + rebounded"
vehicle_leaving_carriageway[vehicle.dt$vehicle_leaving_carriageway == 6] <- "Offside - crossed central reservation"
vehicle_leaving_carriageway[vehicle.dt$vehicle_leaving_carriageway == 7] <- "Offside"
vehicle_leaving_carriageway[vehicle.dt$vehicle_leaving_carriageway == 8] <- "Offside and rebounded"
vehicle_leaving_carriageway <- factor(vehicle_leaving_carriageway, levels = c("Did not leave carriageway",
                                                                              "Nearside",
                                                                              "Nearside and rebounded",
                                                                              "Straight ahead at junction",
                                                                              "Offside on to central reservation",
                                                                              "Offside on to central res + rebounded",
                                                                              "Offside - crossed central reservation",
                                                                              "Offside", "Offside and rebounded"))
vehicle.dt[,vehicle_leaving_carriageway:=NULL]
vehicle.dt <- cbind(vehicle.dt, vehicle_leaving_carriageway)
rm(vehicle_leaving_carriageway)
table(vehicle.dt$vehicle_leaving_carriageway)

#converting hit_object_off_carriageway to categorical
table(vehicle.dt$hit_object_off_carriageway)
hit_object_off_carriageway = character()
hit_object_off_carriageway[vehicle.dt$hit_object_off_carriageway == 0] <- "None"
hit_object_off_carriageway[vehicle.dt$hit_object_off_carriageway == 1] <- "Road sign or traffic signal"
hit_object_off_carriageway[vehicle.dt$hit_object_off_carriageway == 2] <- "Lamp post"
hit_object_off_carriageway[vehicle.dt$hit_object_off_carriageway == 3] <- "Telegraph or electricity pole"
hit_object_off_carriageway[vehicle.dt$hit_object_off_carriageway == 4] <- "Tree"
hit_object_off_carriageway[vehicle.dt$hit_object_off_carriageway == 5] <- "Bus stop or bus shelter"
hit_object_off_carriageway[vehicle.dt$hit_object_off_carriageway == 6] <- "Central crash barrier"
hit_object_off_carriageway[vehicle.dt$hit_object_off_carriageway == 7] <- "Near/Offside crash barrier"
hit_object_off_carriageway[vehicle.dt$hit_object_off_carriageway == 8] <- "Submerged in water"
hit_object_off_carriageway[vehicle.dt$hit_object_off_carriageway == 9] <- "Entered ditch"
hit_object_off_carriageway[vehicle.dt$hit_object_off_carriageway == 10] <- "Other permanent object"
hit_object_off_carriageway[vehicle.dt$hit_object_off_carriageway == 11] <- "Wall or fence"
hit_object_off_carriageway <- factor(hit_object_off_carriageway, levels = c("None", "Road sign or traffic signal",
                                                                            "Lamp post", "Telegraph or electricity pole",
                                                                            "Tree", "Bus stop or bus shelter", 
                                                                            "Central crash barrier", "Near/Offside crash barrier",
                                                                            "Submerged in water", "Entered ditch", 
                                                                            "Other permanent object", "Wall or fence"))
vehicle.dt[,hit_object_off_carriageway:=NULL]
vehicle.dt <- cbind(vehicle.dt, hit_object_off_carriageway)
rm(hit_object_off_carriageway)
table(vehicle.dt$hit_object_off_carriageway)

#converting first_point_of_impact to categorical
table(vehicle.dt$first_point_of_impact)
first_point_of_impact = character()
first_point_of_impact[vehicle.dt$first_point_of_impact == 0] <- "No impact"
first_point_of_impact[vehicle.dt$first_point_of_impact == 1] <- "Front"
first_point_of_impact[vehicle.dt$first_point_of_impact == 2] <- "Back"
first_point_of_impact[vehicle.dt$first_point_of_impact == 3] <- "Offside"
first_point_of_impact[vehicle.dt$first_point_of_impact == 4] <- "Nearside"
first_point_of_impact <- factor(first_point_of_impact, levels = c("No impact", 
                                                                  "Front", "Back", 
                                                                  "Offside", "Nearside"))
vehicle.dt[,first_point_of_impact:=NULL]
vehicle.dt <- cbind(vehicle.dt, first_point_of_impact)
rm(first_point_of_impact)
table(vehicle.dt$first_point_of_impact)

#converting vehicle_left_hand_drive to categorical
table(vehicle.dt$vehicle_left_hand_drive)
lhd = character()
lhd[vehicle.dt$vehicle_left_hand_drive == 1] <- "No"
lhd[vehicle.dt$vehicle_left_hand_drive == 2] <- "Yes"
lhd <- factor(lhd, levels = c("No", "Yes"))
vehicle.dt[,vehicle_left_hand_drive:=NULL]
vehicle.dt <- cbind(vehicle.dt, lhd)
rm(lhd)
table(vehicle.dt$lhd)

#converting journey_purpose_of_driver to categorical
table(vehicle.dt$journey_purpose_of_driver)
journey_purpose = character()
journey_purpose[vehicle.dt$journey_purpose_of_driver == 1] <- "Journey as part of work"
journey_purpose[vehicle.dt$journey_purpose_of_driver == 2] <- "Commuting to/from work"
journey_purpose[vehicle.dt$journey_purpose_of_driver == 3] <- "Taking pupil to/from school"
journey_purpose[vehicle.dt$journey_purpose_of_driver == 4] <- "Pupil riding to/from school"
journey_purpose[vehicle.dt$journey_purpose_of_driver == 5] <- "Other"
journey_purpose <- factor(journey_purpose, levels = c("Journey as part of work", 
                                                      "Commuting to/from work", 
                                                      "Taking pupil to/from school", 
                                                      "Pupil riding to/from school", 
                                                      "Other"))
vehicle.dt[,journey_purpose_of_driver:=NULL]
vehicle.dt <- cbind(vehicle.dt, journey_purpose)
rm(journey_purpose)
table(vehicle.dt$journey_purpose)

#converting sex_of_driver to categorical
table(vehicle.dt$sex_of_driver)
sex_of_driver = character()
sex_of_driver[vehicle.dt$sex_of_driver == 1] <- "M"
sex_of_driver[vehicle.dt$sex_of_driver == 2] <- "F"
sex_of_driver <- factor(sex_of_driver, levels = c("M", "F"))
vehicle.dt[,sex_of_driver:=NULL]
vehicle.dt <- cbind(vehicle.dt, sex_of_driver)
rm(sex_of_driver)
table(vehicle.dt$sex_of_driver)

#converting propulsion_code to categorical
table(vehicle.dt$propulsion_code)
propulsion_code = character()
propulsion_code[vehicle.dt$propulsion_code == 1] <- "Petrol"
propulsion_code[vehicle.dt$propulsion_code == 2] <- "Heavy oil"
propulsion_code[vehicle.dt$propulsion_code == 5] <- "Gas"
propulsion_code[vehicle.dt$propulsion_code == 6] <- "LPG"
propulsion_code[vehicle.dt$propulsion_code == 7] <- "Gas/Bi-fuel"
propulsion_code[vehicle.dt$propulsion_code == 8] <- "Hybrid electric"
propulsion_code[vehicle.dt$propulsion_code == 10] <- "New fuel technology"
propulsion_code[vehicle.dt$propulsion_code == 12] <- "Electric diesel"
propulsion_code <- factor(propulsion_code, levels = c("Petrol", "Heavy oil", "Gas", 
                                                      "LPG", "Gas/Bi-fuel", "Hybrid electric", 
                                                      "New fuel technology", "Electric diesel"))
vehicle.dt[,propulsion_code:=NULL]
vehicle.dt <- cbind(vehicle.dt, propulsion_code)
rm(propulsion_code)
table(vehicle.dt$propulsion_code)

#converting driver_imd_decile to categorical
#IMD is used to identify how deprived an area is. This variable roughly reflects the socio-economic status of the driver.
#1 reflects the bottom 10% while 10 reflects the top 10%.

table(vehicle.dt$driver_imd_decile)
driver_imd_decile = character()
driver_imd_decile[vehicle.dt$driver_imd_decile == 1] <- "Most deprived 10%"
driver_imd_decile[vehicle.dt$driver_imd_decile == 2] <- "More deprived 10-20%"
driver_imd_decile[vehicle.dt$driver_imd_decile == 3] <- "More deprived 20-30%"
driver_imd_decile[vehicle.dt$driver_imd_decile == 4] <- "More deprived 30-40%"
driver_imd_decile[vehicle.dt$driver_imd_decile == 5] <- "More deprived 40-50%"
driver_imd_decile[vehicle.dt$driver_imd_decile == 6] <- "Less deprived 40-50%"
driver_imd_decile[vehicle.dt$driver_imd_decile == 7] <- "Less deprived 30-40%"
driver_imd_decile[vehicle.dt$driver_imd_decile == 8] <- "Less deprived 20-30%"
driver_imd_decile[vehicle.dt$driver_imd_decile == 9] <- "Less deprived 10-20%"
driver_imd_decile[vehicle.dt$driver_imd_decile == 10] <- "Least deprived 10%"
driver_imd_decile <- factor(driver_imd_decile, levels = c("Most deprived 10%", 
                                                          "More deprived 10-20%", 
                                                          "More deprived 20-30%", 
                                                          "More deprived 30-40%", 
                                                          "More deprived 40-50%", 
                                                          "Less deprived 40-50%", 
                                                          "Less deprived 30-40%", 
                                                          "Less deprived 20-30%", 
                                                          "Less deprived 10-20%", 
                                                          "Least deprived 10%"))
vehicle.dt[,driver_imd_decile:=NULL]
vehicle.dt <- cbind(vehicle.dt, driver_imd_decile)
rm(driver_imd_decile)
table(vehicle.dt$driver_imd_decile)


#converting driver_home_area_type to categorical
table(vehicle.dt$driver_home_area_type)
driver_home_area = character()
driver_home_area[vehicle.dt$driver_home_area_type == 1] <- "Urban"
driver_home_area[vehicle.dt$driver_home_area_type == 2] <- "Small town"
driver_home_area[vehicle.dt$driver_home_area_type == 3] <- "Rural"
driver_home_area <- factor(driver_home_area, levels = c("Urban", "Small town", "Rural"))
vehicle.dt[,driver_home_area_type:=NULL]
vehicle.dt <- cbind(vehicle.dt, driver_home_area)
rm(driver_home_area)
table(vehicle.dt$driver_home_area)

#Driver age groups
#1: "Underage" (<17)
#2: "Young driver" (17-24)
#3: "Adult driver" (24-59)
#4: "Older driver" (>60)

summary(vehicle.dt$age_of_driver)
driver_age_group = character()
driver_age_group[vehicle.dt$age_of_driver < 17] <- "Underage(<17)"
driver_age_group[vehicle.dt$age_of_driver >= 17 & vehicle.dt$age_of_driver <= 24] <- "Young driver(17-24)"
driver_age_group[vehicle.dt$age_of_driver > 24 & vehicle.dt$age_of_driver <= 59] <- "Adult driver(24-59)"
driver_age_group[vehicle.dt$age_of_driver >= 60] <- "Older driver(>=60)"
driver_age_group <- factor(driver_age_group, levels = c("Underage(<17)", 
                                                        "Young driver(17-24)", 
                                                        "Adult driver(24-59)", 
                                                        "Older driver(>=60)"))
vehicle.dt[,age_band_of_driver:=NULL]
vehicle.dt <- cbind(vehicle.dt, driver_age_group)
rm(driver_age_group)

vehicle.dt[,accident_year:=NULL] #duplicate column

#===============================================================================

#Cleaning casualty data

#===============================================================================

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
                                         "Vehicles(Goods)" = c(10,11,19:21,98), 
                                         "Others" = c(90,22,16:18))

#===============================================================================

#Regrouping of pedestrian location categories

#===============================================================================

#remove unclassified or missing data
casualty.dt <- casualty.dt[!(pedestrian_location == -1 | pedestrian_location == 10)]
pedestrian_location = character()
pedestrian_location[casualty.dt$pedestrian_location == 0] <- "Not a Pedestrian"
pedestrian_location[casualty.dt$pedestrian_location == 1] <- "Crossing on pedestrian crossing facility"
#group crossing in zig-zag approach/exit lines category together
pedestrian_location[casualty.dt$pedestrian_location == 2 | 
                      casualty.dt$pedestrian_location == 3] <- "Crossing in zig-zag lines"
pedestrian_location[casualty.dt$pedestrian_location == 4] <- "Crossing elsewhere within 50m. of pedestrian crossing"
pedestrian_location[casualty.dt$pedestrian_location == 5] <- "In carriageway, crossing elsewhere"
pedestrian_location[casualty.dt$pedestrian_location == 6] <- "On footway or verge"
pedestrian_location[casualty.dt$pedestrian_location == 7] <- "On refuge, central island or central reservation"
pedestrian_location[casualty.dt$pedestrian_location == 8] <- "In centre of carriageway - not on refuge, island or central reservation"
pedestrian_location[casualty.dt$pedestrian_location == 9] <- "In carriageway, not crossing"
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

#Lets try grouping the age for more meaningful comparisons
casualty_agegroup = character()
casualty_agegroup[casualty.dt$age_of_casualty < 16] <- "Children(<16)"
casualty_agegroup[casualty.dt$age_of_casualty >= 16 & casualty.dt$age_of_casualty <= 20] <- "Teenager(16-20)"
casualty_agegroup[casualty.dt$age_of_casualty >= 21 & casualty.dt$age_of_casualty <= 64] <- "Adult(21-64)"
casualty_agegroup[casualty.dt$age_of_casualty >= 65] <- "Senior(>=65)"
casualty_agegroup <- factor(casualty_agegroup, levels = c("Children(<16)", "Teenager(16-20)", 
                                                          "Adult(21-64)", "Senior(>=65)"))
casualty.dt[,age_of_casualty:=NULL] #remove original age of of casualty
casualty.dt <- cbind(casualty.dt,casualty_agegroup) #add newly created casualty_agegroup column

#===============================================================================

#Chunk renaming of categories for casualty data

#===============================================================================

levels(casualty.dt$pedestrian_movement) = list("Not a Pedestrian" = 0,
                                               "Crossing from driver's nearside" = 1,
                                               "Crossing from nearside - masked by parked or stationary vehicle" = 2,
                                               "Crossing from driver's offside" = 3,
                                               "Crossing from offside - masked by  parked or stationary vehicle" = 4,
                                               "In carriageway, stationary - not crossing  (standing or playing)" = 5,
                                               "In carriageway, stationary - not crossing  (standing or playing) - masked by parked or stationary vehicle" = 6,
                                               "Walking along in carriageway, facing traffic" = 7,
                                               "Walking along in carriageway, back to traffic" = 8)
levels(casualty.dt$car_passenger) = list("Not car passenger" = 0,
                                         "Front seat passenger" = 1,
                                         "Rear seat passenger" = 2)
levels(casualty.dt$bus_or_coach_passenger) = list("Not a bus or coach passenger" = 0,
                                         "Boarding" = 1,
                                         "Alighting" = 2,
                                         "Standing passenger" = 3,
                                         "Seated passenger" = 4)
levels(casualty.dt$pedestrian_road_maintenance_worker) = list("No" = 0,
                                                              "Yes" = 1,
                                                              "Not Known" = 2,
                                                              "Probable" = 3,
                                                              "Seated passenger" = 4)
levels(casualty.dt$casualty_home_area_type) = list("Urban area" = 1,
                                                   "Small town" = 2,
                                                   "Rural" = 3)
levels(casualty.dt$casualty_imd_decile) = list("Most deprived 10%" = 1,
                                               "More deprived 10-20%" = 2,
                                               "More deprived 20-30%" = 3,
                                               "More deprived 30-40%" = 4,
                                               "More deprived 40-50%" = 5,
                                               "Less deprived 40-50%" = 6,
                                               "Less deprived 30-40%" = 7,
                                               "Less deprived 20-30%" = 8,
                                               "Less deprived 10-20%" = 9,
                                               "Least deprived 10%" = 10)

casualty.dt[,accident_year:=NULL] #remove duplicate column

#===============================================================================

#Merging accident, vehicle and casualty data

#===============================================================================

# merging the 3 data tables based on accident index.
accident_casualty.dt <- merge(accident.dt, casualty.dt) #118732 obs 40 variables
accidents_combined.dt <- merge(accident_casualty.dt,vehicle.dt) #73,337 obs 62 variables

setwd("C:/Users/joshua/Downloads/BC2407 Analytics II/Datasets Cleaned")

#export cleaned data for tableau visualisation as csv.
write.csv(vehicle.dt, "accident_data_tableau.csv",row.names = FALSE)

