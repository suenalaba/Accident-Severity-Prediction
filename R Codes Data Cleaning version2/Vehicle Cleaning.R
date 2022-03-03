#Automation to install packages that are necessary but not yet installed
list.of.packages <- c("data.table", "ggplot2","rcompanion")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#import respective libraries
library(data.table)
library(ggplot2)
library(rcompanion)

#set working directory
#setwd('C:/Users/j2504/OneDrive - Nanyang Technological University/Desktop/Y2S2/BC2407 - Analytics 2/Project')
setwd("C:/Users/joshua/Downloads/BC2407 Analytics II/Datasets Raw")
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
driver_imd_decile[vehicle.dt$driver_imd_decile == 1] <- "1"
driver_imd_decile[vehicle.dt$driver_imd_decile == 2] <- "2"
driver_imd_decile[vehicle.dt$driver_imd_decile == 3] <- "3"
driver_imd_decile[vehicle.dt$driver_imd_decile == 4] <- "4"
driver_imd_decile[vehicle.dt$driver_imd_decile == 5] <- "5"
driver_imd_decile[vehicle.dt$driver_imd_decile == 6] <- "6"
driver_imd_decile[vehicle.dt$driver_imd_decile == 7] <- "7"
driver_imd_decile[vehicle.dt$driver_imd_decile == 8] <- "8"
driver_imd_decile[vehicle.dt$driver_imd_decile == 9] <- "9"
driver_imd_decile[vehicle.dt$driver_imd_decile == 10] <- "10"
driver_imd_decile <- factor(driver_imd_decile, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
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

#===============================================================================

#Reclassifying driver age bands into 4 categories

#===============================================================================

# Compare the salary of male and females across Rank
ggplot(data = vehicle.dt, aes(x = accident_severity, y = age_of_driver, color = accident_severity)) +
  geom_boxplot() +
  labs(x = "Severity", 
       y = "Age of Driver", 
       title = "Age of Driver vs Accident Severity") 
#No meaningful conclusion drawn based on age as a continuous variable, explore grouping them instead.

#converting age_band_of_driver to categorical
table(vehicle.dt$age_band_of_driver)

#Reclassify age groups to underage drivers, young drivers, adults and older drivers.
#https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/448039/young-car-drivers-2013-data.pdf

#Driver age groups
#1: "Underage" (<17)
#2: "Young driver" (17-24)
#3: "Adult driver" (24-59)
#4: "Older driver" (>60)

summary(vehicle.dt$age_of_driver)
driver_age_group = character()
driver_age_group[vehicle.dt$age_of_driver < 17] <- "Underage"
driver_age_group[vehicle.dt$age_of_driver >= 17 & vehicle.dt$age_of_driver <= 24] <- "Young driver"
driver_age_group[vehicle.dt$age_of_driver > 24 & vehicle.dt$age_of_driver <= 59] <- "Adult driver"
driver_age_group[vehicle.dt$age_of_driver >= 60] <- "Older driver"
driver_age_group <- factor(driver_age_group, levels = c("Underage", "Young driver", "Adult driver", "Older driver"))
vehicle.dt[,age_band_of_driver:=NULL]
vehicle.dt <- cbind(vehicle.dt, driver_age_group)
rm(driver_age_group)
table(vehicle.dt$driver_age_group)

#===============================================================================

#Reclassifying vehicle types into broad categories to reduce number of categories

#===============================================================================

#converting vehicle_type to categorical
table(vehicle.dt$vehicle_type)
vehicle_type = character()
vehicle_type[vehicle.dt$vehicle_type == 2|vehicle.dt$vehicle_type == 3|
               vehicle.dt$vehicle_type == 4| vehicle.dt$vehicle_type == 5 | 
               vehicle.dt$vehicle_type == 23] <- "Motorcycle"
vehicle_type[vehicle.dt$vehicle_type == 8 | vehicle.dt$vehicle_type == 9 ] <- "Car/Taxi"
vehicle_type[vehicle.dt$vehicle_type == 10 | vehicle.dt$vehicle_type == 11] <- "Bus/Coach"
vehicle_type[vehicle.dt$vehicle_type == 19 | vehicle.dt$vehicle_type == 20 | 
               vehicle.dt$vehicle_type == 21 | vehicle.dt$vehicle_type == 98] <- "Vehicles(Goods)"
vehicle_type[vehicle.dt$vehicle_type == 17 | vehicle.dt$vehicle_type == 90] <- "Other vehicle"
vehicle_type <- factor(vehicle_type, levels = c("Motorcycle", "Car/Taxi", 
                                                "Bus/Coach", "Vehicles(Goods)", 
                                                "Other vehicle"))
vehicle.dt[,vehicle_type:=NULL]
vehicle.dt <- cbind(vehicle.dt, vehicle_type)
rm(vehicle_type)
table(vehicle.dt$vehicle_type)

#===============================================================================

#Merging vehicle data with accident severity to explore relationship between
#vehicle data and accident severity

#===============================================================================
setwd('C:/Users/joshua/Downloads/BC2407 Analytics II/Datasets Cleaned')
#Merge cleaned accident dataset to obtain accident severity
accident.dt <- fread("accident_data_cleaned.csv")
accident.dt <- subset(accident.dt, select = c("accident_index", "accident_severity"))
vehicle.dt <- merge(vehicle.dt, accident.dt, by = "accident_index")
vehicle.dt[, "accident_severity" := lapply(.SD, as.factor), .SDcols = "accident_severity"]
str(vehicle.dt)

#===================== END OF INITIAL CLEANING =================================

#===============================================================================

#Perform Cramer's V test to check for association between categorical variables
#0 -> No association, #1 -> Perfect correlation, one variable completely determines the other

#===============================================================================

cramerV(vehicle.dt$accident_severity, vehicle.dt$vehicle_type, bias.correct = T) #0.1378
cramerV(vehicle.dt$accident_severity, vehicle.dt$driver_age_group, bias.correct = T) #0.03838
cramerV(vehicle.dt$accident_severity, vehicle.dt$driver_home_area, bias.correct = T) #0.03851
cramerV(vehicle.dt$accident_severity, vehicle.dt$driver_imd_decile, bias.correct = T) #0.02049
cramerV(vehicle.dt$accident_severity, vehicle.dt$propulsion_code, bias.correct = T) #0.03555
cramerV(vehicle.dt$accident_severity, vehicle.dt$sex_of_driver, bias.correct = T) #0.06614
cramerV(vehicle.dt$accident_severity, vehicle.dt$journey_purpose, bias.correct = T) #0.0512
cramerV(vehicle.dt$accident_severity, vehicle.dt$lhd, bias.correct = T) #0.002333
cramerV(vehicle.dt$accident_severity, vehicle.dt$first_point_of_impact, bias.correct = T) #0.1071
cramerV(vehicle.dt$accident_severity, vehicle.dt$hit_object_off_carriageway, bias.correct = T) #0.04821
cramerV(vehicle.dt$accident_severity, vehicle.dt$vehicle_leaving_carriageway, bias.correct = T) #0.06735
cramerV(vehicle.dt$accident_severity, vehicle.dt$hit_object_in_carriageway, bias.correct = T) #0.02695
cramerV(vehicle.dt$accident_severity, vehicle.dt$skidding_and_overturning, bias.correct = T) #0.07617
cramerV(vehicle.dt$accident_severity, vehicle.dt$junction_location, bias.correct = T) #0.06487
cramerV(vehicle.dt$accident_severity, vehicle.dt$vehicle_location_restricted_lane, bias.correct = T) #0.0126
cramerV(vehicle.dt$accident_severity, vehicle.dt$vehicle_direction_to, bias.correct = T) #0.0159
cramerV(vehicle.dt$accident_severity, vehicle.dt$vehicle_direction_from, bias.correct = T) #0.01811
cramerV(vehicle.dt$accident_severity, vehicle.dt$vehicle_manoeuvre, bias.correct = T) #0.1238
cramerV(vehicle.dt$accident_severity, vehicle.dt$towing_and_articulation, bias.correct = T) #0.02362

#Categorical variables to explore further:
# 1. Vehicle type (0.1378)
# 2. Vehicle manoeuvre (0.1238)
# 3. First point of impact (0.1071)
# 4. Skidding and overturning (0.07617)
# 5. Vehicle leaving carriageway (0.06735)

#===============================================================================

#Data Exploration of vehicle data

#===============================================================================

ggplot(data = vehicle.dt, aes(x = vehicle_type,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "vehicle_type", 
       x = "Driver Vehicle Type",
       y = "Percentage", 
       title = "Accident severity by Vehicle Type") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_grid(.~accident_severity)
#Higher proportion of Serious accidents tend to involve motorcycles compared to not serious.

ggplot(data = vehicle.dt, aes(x = driver_age_group,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "driver_age_group", 
       x = "Driver Age Group",
       y = "Percentage", 
       title = "Accident Severity for different driver age group") +
  facet_grid(.~accident_severity)
#Driver age group doesn't really affect accident severity.
#Both follow a similar distribution

ggplot(data = vehicle.dt, aes(x = driver_home_area,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "driver_home_area", 
       x = "Driver Home Area",
       y = "Percentage", 
       title = "Accident Severity for different driver's home area") +
  facet_grid(.~accident_severity)
#Home Area type of the driver does not really affect the severity of accidents

ggplot(data = vehicle.dt, aes(x = propulsion_code,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "propulsion_code", 
       x = "Propulsion Code",
       y = "Percentage", 
       title = "Accident Severity for different vehicle propulsion code") +
  facet_grid(.~accident_severity)
#It seems that for serious accidents, a higher proportion of vehicles uses heavy
#oil as compared to not serious accidents.
#Propulsion code has too many categories, lets try regrouping them
#Regroup propulsion code to reduce number of categories
levels(vehicle.dt$propulsion_code) = list("Petrol" = "Petrol", "Heavy oil" = "Heavy oil", 
                                          "Others" = c("Gas","LPG","Gas/Bi-fuel","Hybrid electric",
                                                       "New fuel technology","Electric diesel"))

ggplot(data = vehicle.dt, aes(x = driver_imd_decile,fill = driver_imd_decile)) +
  geom_bar() +
  labs(x = "Driver IMD Decile", 
       y = "Count", 
       title = "Count of traffic accidents by Driver's IMD Decile") 
#With exception of the bottom decile, there is a consistent decreasing trend for 
#accident count as drivers become less deprived. This is likely due to the fact
#that they are unable to afford driving cars.

#Regroup deciles to reduce number of categories
levels(vehicle.dt$driver_imd_decile) = list("Most Deprived(0-20%)" = c(1,2), "More Deprived(20-40%)" = c(3,4), 
                                            "Moderately Deprived(40-50%)"= c(5,6), 
                                            "Less Deprived(20-40%)" = c(7,8), "Least Deprived(0-20%)" = c(9,10))

ggplot(data = vehicle.dt, aes(x = driver_imd_decile,group=accident_severity)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "driver_imd_decile", 
       x = "Driver IMD Decile",
       y = "Percentage", 
       title = "Accident Severity for different driver's IMD Decile") +
  facet_grid(.~accident_severity)
#A driver's IMD Decile does not really affect accident severity.

ggplot(data = vehicle.dt, aes(x = sex_of_driver,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "sex_of_driver", 
       x = "Sex of Driver",
       y = "Percentage", 
       title = "Accident Severity for different genders of driver") +
  facet_grid(.~accident_severity)
#Higher proportion of serious accidents tend to involve male drivers.

ggplot(data = vehicle.dt, aes(x = journey_purpose,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(fill = "journey_purpose", 
       x = "Journey Purpose",
       y = "Percentage", 
       title = "Accident Severity for different journey purposes") +
  facet_grid(.~accident_severity)
#If purpose of driver is other, higher proportion of serious accident.

ggplot(data = vehicle.dt, aes(x = lhd,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "lhd", 
       x = "Left Hand Drive",
       y = "Percentage", 
       title = "Accident Severity depending if vehicle was left hand drive") +
  facet_grid(.~accident_severity)
#Whether vehicle was left hand drive anot does not really affect accident severity

ggplot(data = vehicle.dt, aes(x = first_point_of_impact,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "first_point_of_impact", 
       x = "First point of impact",
       y = "Percentage", 
       title = "Accident Severity depending on the first point of impact") +
  facet_grid(.~accident_severity)
#Higher proportion of serious accidents involve front as the first point of impact.
#If regroup, clearer distinction so lets do so.
levels(vehicle.dt$first_point_of_impact) = list("Front" = "Front", 
                                          "Others" = c("No impact","Back","Offside","Nearside"))

ggplot(data = vehicle.dt, aes(x = hit_object_off_carriageway,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(fill = "hit_object_off_carriageway", 
       x = "Hit object off carriageway",
       y = "Percentage", 
       title = "Accident Severity depending on whether vehicle hit objects off carriageway") +
  facet_grid(.~accident_severity)
#There are too many categories on whether the vehicle hit an object off carriageway.
#Lets regroup the variable into a binary categorical variable
levels(vehicle.dt$hit_object_off_carriageway) = list("1" = c("Road sign or traffic signal","Lamp post",
                                                             "Telegraph or electricity pole","Tree",
                                                             "Bus stop or bus shelter","Central crash barrier",
                                                             "Near/Offside crash barrier","Submerged in water",
                                                             "Entered ditch","Other permanent object",
                                                             "Wall or fence"),
                                                     "0" = c("None"))


ggplot(data = vehicle.dt, aes(x = vehicle_leaving_carriageway,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(fill = "vehicle_leaving_carriageway", 
       x = "Vehicle Leaving Carriageway",
       y = "Percentage", 
       title = "Accident Severity depending on whether vehicle is leaving carriageway") +
  facet_grid(.~accident_severity)
#Vehicle leaving carriageway has too many categories
#Once again, regrouping the variables.
levels(vehicle.dt$vehicle_leaving_carriageway) = list("1" = c("Nearside","Nearside and rebounded",
                                                              "Straight ahead at junction","Offside on to central reservation",
                                                              "Offside on to central res + rebounded",
                                                              "Offside - crossed central reservation","Offside",
                                                              "Offside and rebounded"),
                                                      "0" = c("Did not leave carriageway"))


ggplot(data = vehicle.dt, aes(x = hit_object_in_carriageway,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(fill = "hit_object_in_carriageway", 
       x = "Hit objecting in Carriageway",
       y = "Percentage", 
       title = "Accident Severity depending on whether vehicle hit object in carriageway") +
  facet_grid(.~accident_severity)
#Once again, there are too many categories
#Lets explore regrouping into a binary categorical variable.
levels(vehicle.dt$hit_object_in_carriageway) = list("1" = c("Previous accident","Road works",
                                                            "Parked vehicle","Bridge (roof)",
                                                            "Bridge (side)","Bollard or refuge",
                                                            "Open door of vehicle","Central island of roundabout",
                                                            "Kerb","Other object",
                                                            "Any animal (except ridden horse)"),
                                                    "0" = c("None"))


ggplot(data = vehicle.dt, aes(x = skidding_and_overturning,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "skidding_and_overturning", 
       x = "Skidding and overturning",
       y = "Percentage", 
       title = "Accident Severity depending on whether vehicle skidded and overturned") +
  facet_grid(.~accident_severity)
#Serious accidents tend to involve skidding and overturning.
#Reclassifying whether a vehicle skidded or overturn into a binary categorical variable
levels(vehicle.dt$skidding_and_overturning) = list("1" = c("Skidded","Skidded and overturned","Jackknifed",
                                                            "Jackknifed and overturned","Overturned"),
                                                    "0" = c("None"))

ggplot(data = vehicle.dt, aes(x = junction_location,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(fill = "junction_location", 
       x = "Junction Location",
       y = "Percentage", 
       title = "Accident Severity depending on Junction Location") +
  facet_grid(.~accident_severity)
#Higher proportion of severe accident is at mid junction - on roundabout or on main road

ggplot(data = vehicle.dt, aes(x = vehicle_location_restricted_lane,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "vehicle_location_restricted_lane", 
       x = "Vehicle Location Restricted Lane",
       y = "Percentage", 
       title = "Accident Severity depending on Vehicle Location Restricted Lane") +
  facet_grid(.~accident_severity)
#Vehicle Location Restricted Lane does not really affect accident severity

ggplot(data = vehicle.dt, aes(x = vehicle_direction_to,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "vehicle_direction_to", 
       x = "Vehicle Direction To",
       y = "Percentage", 
       title = "Accident Severity depending on Vehicle direction To") +
  facet_grid(.~accident_severity)
#vehicle direction to does not really affect accident severity
#A similar distribution can be seen

ggplot(data = vehicle.dt, aes(x = vehicle_direction_from,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(fill = "vehicle_direction_from", 
       x = "Vehicle Direction From",
       y = "Percentage", 
       title = "Accident Severity depending on Vehicle direction From") +
  facet_grid(.~accident_severity)
#vehicle direction from does not really affect accident severity
#Once again, a similar distribution can be seen regardless of accident severity

ggplot(data = vehicle.dt, aes(x = vehicle_manoeuvre,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(fill = "vehicle_manoeuvre", 
       x = "Vehicle Manoeuvre",
       y = "Percentage", 
       title = "Accident Severity depending on Vehicle Manoeuvre") +
  facet_grid(.~accident_severity)
#Higher proportion of serious accident is due to vehicle going ahead other and turning right.

#Regroup top 5 categories and group the rest as others to reduce number of categories
levels(vehicle.dt$vehicle_manoeuvre) = list("Waiting to go - held up" = "Waiting to go - held up",
                                            "Slowing or stopping" = "Slowing or stopping",
                                            "Moving off" = "Moving off",
                                            "Turning right" = "Turning right",
                                            "Going ahead other" = "Going ahead other",
                                            "Others" = c("Reversing","Parked","U-turn","Turning left",
                                                         "Waiting to turn left","Waiting to turn right",
                                                         "Changing lane to left","Changing lane to right",
                                                         "Overtaking moving vehicle - offside",
                                                         "Overtaking static vehicle - offside",
                                                         "Overtaking - nearside","Going ahead left-hand bend",
                                                         "Going ahead right-hand bend"))
ggplot(data = vehicle.dt, aes(x = vehicle_manoeuvre,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(fill = "vehicle_manoeuvre", 
       x = "Vehicle Manoeuvre",
       y = "Percentage", 
       title = "Accident Severity depending on Vehicle Manoeuvre") +
  facet_grid(.~accident_severity)

ggplot(data = vehicle.dt, aes(x = towing_and_articulation,group=accident_severity)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(fill = "towing_and_articulation", 
       x = "Towing and articulation",
       y = "Percentage", 
       title = "Accident Severity depending on presence of towing and articulation") +
  facet_grid(.~accident_severity)
#Presence of towing and articulation does not really affect accident severity

# Compare the salary of male and females across Rank
ggplot(data = vehicle.dt, aes(x=age_of_vehicle, fill=accident_severity))+
  geom_density(alpha=0.3) +
  labs(x = "Age of vehicle", 
       y = "Density of Accident Severity", 
       title = "Accident Severity distribution across age of vehicle")
#age of vehicle does not really affect accident severity.

ggplot(data = vehicle.dt, aes(x = accident_severity, y = engine_capacity_cc, color = accident_severity))+
  geom_boxplot() +
  labs(x = "Accident Severity", 
       y = "Engine Capacity(cc)", 
       title = "Accident Severity distribution against engine capacity")
#engine capacity does not really affect accident severity.

#=================== END OF DATA EXPLORATION OF VEHICLE DATA ===================

#set working directory to export cleaned data to...
#setwd("C:/Users/j2504/OneDrive - Nanyang Technological University/Desktop/Y2S2/BC2407 - Analytics 2/Project")
setwd("C:/Users/joshua/Downloads/BC2407 Analytics II/Datasets Cleaned")

#export as csv
write.csv(vehicle.dt, "vehicle_data_cleaned.csv",row.names = FALSE)
