#import libraries
pacman::p_load(data.table, stringr, ggplot2)

#set working directory
setwd('C:/Users/j2504/OneDrive - Nanyang Technological University/Desktop/Y2S2/BC2407 - Analytics 2/Project')


#import dataset into data.table
vehicle.dt <- fread("dft-road-casualty-statistics-vehicle-last-5-years.csv")


#View(vehicle.dt)
str(vehicle.dt)
#data dimensions: 1,101,591 * 27
length(unique(vehicle.dt$accident_index))


#subset dataset for years 2017-2019 only
vehicle.dt <- subset(vehicle.dt, accident_year==2017|accident_year==2018|accident_year==2019)


#remove accident reference as it is redundant
vehicle.dt[,accident_reference:=NULL]


#remove all unspecified, null and data missing or out of range
vehicle.dt <- vehicle.dt[!(vehicle_type == -1|vehicle_type == 97|vehicle_type==98|vehicle_type == 99)]
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
vehicle.dt <- vehicle.dt[!(journey_purpose_of_driver == -1|journey_purpose_of_driver == 15)]
vehicle.dt <- vehicle.dt[!(sex_of_driver == -1|sex_of_driver == 3)]
vehicle.dt <- vehicle.dt[!(age_of_driver == -1)]
vehicle.dt <- vehicle.dt[!(age_band_of_driver == -1)]
vehicle.dt <- vehicle.dt[!(engine_capacity_cc == -1)]
vehicle.dt <- vehicle.dt[!(propulsion_code == -1)]
vehicle.dt <- vehicle.dt[!(age_of_vehicle == -1)]
vehicle.dt <- vehicle.dt[!(driver_imd_decile == -1)]
vehicle.dt <- vehicle.dt[!(driver_home_area_type == -1)]


#As 100% of data in generic_make_model is missing/out of range, we will remove it.
length(grep(-1, vehicle.dt$generic_make_model)) #409,436
vehicle.dt[,generic_make_model:=NULL]


#converting vehicle_type to categorical
table(vehicle.dt$vehicle_type)
vehicle_type = character()
vehicle_type[vehicle.dt$vehicle_type == 1] <- "Pedal cycle"
vehicle_type[vehicle.dt$vehicle_type == 2] <- "Motorcycle <=50cc"
vehicle_type[vehicle.dt$vehicle_type == 3] <- "Motorcycle <=125cc"
vehicle_type[vehicle.dt$vehicle_type == 4] <- "Motorcycle >125cc & <=500cc"
vehicle_type[vehicle.dt$vehicle_type == 5] <- "Motorcycle >500cc"
vehicle_type[vehicle.dt$vehicle_type == 8] <- "Taxi/Private hire car"
vehicle_type[vehicle.dt$vehicle_type == 9] <- "Car"
vehicle_type[vehicle.dt$vehicle_type == 10] <- "Minibus (8-16 passenger seats)"
vehicle_type[vehicle.dt$vehicle_type == 11] <- "Bus or coach (>17 passenger seats)"
vehicle_type[vehicle.dt$vehicle_type == 16] <- "Ridden horse"
vehicle_type[vehicle.dt$vehicle_type == 17] <- "Agricultural vehicle"
vehicle_type[vehicle.dt$vehicle_type == 18] <- "Tram"
vehicle_type[vehicle.dt$vehicle_type == 19] <- "Van/Goods <=3.5t mgw"
vehicle_type[vehicle.dt$vehicle_type == 20] <- "Goods >3.5t & <7.5t"
vehicle_type[vehicle.dt$vehicle_type == 21] <- "Goods >=7.5t"
vehicle_type[vehicle.dt$vehicle_type == 22] <- "Mobility scooter"
vehicle_type[vehicle.dt$vehicle_type == 23] <- "Electric motorcycle"
vehicle_type[vehicle.dt$vehicle_type == 90] <- "Other vehicle"
vehicle_type[vehicle.dt$vehicle_type == 103] <- "Motorcycle - Scooter (1979-1998)"
vehicle_type[vehicle.dt$vehicle_type == 104] <- "Motorcycle (1979-1998)"
vehicle_type[vehicle.dt$vehicle_type == 105] <- "Motorcycle - Combination (1979-1998)"
vehicle_type[vehicle.dt$vehicle_type == 106] <- "Motorcycle >125cc (1999-2004)"
vehicle_type[vehicle.dt$vehicle_type == 108] <- "Taxi (excluding private hire cars) (1979-2004)"
vehicle_type[vehicle.dt$vehicle_type == 109] <- "Car (including private hire cars) (1979-2004)"
vehicle_type[vehicle.dt$vehicle_type == 110] <- "Minibus/Motor caravan (1979-1998)"
vehicle_type[vehicle.dt$vehicle_type == 113] <- "Goods over 3.5t (1979-1998)"
vehicle_type <- factor(vehicle_type, levels = c("Pedal cycle", "Motorcycle <=50cc", 
                                              "Motorcycle <=125cc", "Motorcycle >125cc & <=500cc",
                                              "Motorcycle >500cc", "Taxi/Private hire car",
                                              "Car", "Minibus (8-16 passenger seats)",
                                              "Bus or coach (>17 passenger seats)",
                                              "Ridden horse", "Agricultural vehicle",
                                              "Tram", "Van/Goods <=3.5t mgw",
                                              "Goods >3.5t & <7.5t", "Goods >=7.5t",
                                              "Mobility scooter", "Electric motorcycle",
                                              "Other vehicle", "Motorcycle - Scooter (1979-1998)",
                                              "Motorcycle (1979-1998)", 
                                              "Motorcycle - Combination (1979-1998)",
                                              "Motorcycle >125cc (1999-2004)",
                                              "Taxi (excluding private hire cars) (1979-2004)",
                                              "Car (including private hire cars) (1979-2004)",
                                              "Minibus/Motor caravan (1979-1998)",
                                              "Goods over 3.5t (1979-1998)"))
vehicle.dt[,vehicle_type:=NULL]
vehicle.dt <- cbind(vehicle.dt, vehicle_type)
rm(vehicle_type)
#verifying that distribution is intact
table(vehicle.dt$vehicle_type)


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
journey_purpose[vehicle.dt$journey_purpose_of_driver == 6] <- "Not known"
journey_purpose <- factor(journey_purpose, levels = c("Journey as part of work", 
                                                      "Commuting to/from work", 
                                                      "Taking pupil to/from school", 
                                                      "Pupil riding to/from school", 
                                                      "Other", "Not known"))
vehicle.dt[,journey_purpose_of_driver:=NULL]
vehicle.dt <- cbind(vehicle.dt, journey_purpose)
rm(journey_purpose)
table(vehicle.dt$journey_purpose)


#converting sex_of_driver to categorical
table(vehicle.dt$sex_of_driver)
sex = character()
sex[vehicle.dt$sex_of_driver == 1] <- "M"
sex[vehicle.dt$sex_of_driver == 2] <- "F"
sex <- factor(sex, levels = c("M", "F"))
vehicle.dt[,sex_of_driver:=NULL]
vehicle.dt <- cbind(vehicle.dt, sex)
rm(sex)
table(vehicle.dt$sex)


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
age_group = character()
age_group[vehicle.dt$age_of_driver < 17] <- "Underage"
age_group[vehicle.dt$age_of_driver >= 17 & vehicle.dt$age_of_driver <= 24] <- "Young driver"
age_group[vehicle.dt$age_of_driver > 24 & vehicle.dt$age_of_driver <= 59] <- "Adult driver"
age_group[vehicle.dt$age_of_driver >= 60] <- "Older driver"
age_group <- factor(age_group, levels = c("Underage", "Young driver", "Adult driver", "Older driver"))
vehicle.dt[,age_band_of_driver:=NULL]
vehicle.dt <- cbind(vehicle.dt, age_group)
rm(age_group)
table(vehicle.dt$age_group)


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
imd_decile = character()
imd_decile[vehicle.dt$driver_imd_decile == 1] <- "1"
imd_decile[vehicle.dt$driver_imd_decile == 2] <- "2"
imd_decile[vehicle.dt$driver_imd_decile == 3] <- "3"
imd_decile[vehicle.dt$driver_imd_decile == 4] <- "4"
imd_decile[vehicle.dt$driver_imd_decile == 5] <- "5"
imd_decile[vehicle.dt$driver_imd_decile == 6] <- "6"
imd_decile[vehicle.dt$driver_imd_decile == 7] <- "7"
imd_decile[vehicle.dt$driver_imd_decile == 8] <- "8"
imd_decile[vehicle.dt$driver_imd_decile == 9] <- "9"
imd_decile[vehicle.dt$driver_imd_decile == 10] <- "10"
imd_decile <- factor(imd_decile, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
vehicle.dt[,driver_imd_decile:=NULL]
vehicle.dt <- cbind(vehicle.dt, imd_decile)
rm(imd_decile)
table(vehicle.dt$imd_decile)


#converting driver_home_area_type to categorical
table(vehicle.dt$driver_home_area_type)
home_area = character()
home_area[vehicle.dt$driver_home_area_type == 1] <- "Urban"
home_area[vehicle.dt$driver_home_area_type == 2] <- "Small town"
home_area[vehicle.dt$driver_home_area_type == 3] <- "Rural"
home_area <- factor(home_area, levels = c("Urban", "Small town", "Rural"))
vehicle.dt[,driver_home_area_type:=NULL]
vehicle.dt <- cbind(vehicle.dt, home_area)
rm(home_are)
table(vehicle.dt$home_area)


View(vehicle.dt)



