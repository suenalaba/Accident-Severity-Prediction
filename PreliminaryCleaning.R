library(data.table)
library(stringr)
library(caTools)
library(nnet)
library(randomForest)
library(caret)
library(DMwR2)
#set your working directory here..
setwd("C:/Users/joshua/Downloads")

#import dataset as datatable
vehicle.dt <- fread("dft-road-casualty-statistics-vehicle-last-5-years.csv")
accident.dt <- fread("dft-road-casualty-statistics-accident-last-5-years.csv")

vehicle.dt <- vehicle.dt[!(vehicle.dt$accident_year == 2016 | vehicle.dt$accident_year== 2020),]
accident.dt <- accident.dt[!(accident.dt$accident_year == 2016 | accident.dt$accident_year== 2020),]
accident.dt<-accident.dt[!(accident.dt$first_road_number==-1 | accident.dt$road_type==-1 | accident.dt$speed_limit=="-1"
                           | accident.dt$junction_detail==-1 | accident.dt$junction_control==-1 | accident.dt$second_road_number==-1
                           | accident.dt$pedestrian_crossing_human_control==-1 | accident.dt$pedestrian_crossing_physical_facilities==-1 | accident.dt$light_conditions==-1
                           | accident.dt$weather_conditions==-1 | accident.dt$road_surface_conditions==-1 | accident.dt$special_conditions_at_site==-1
                           | accident.dt$carriageway_hazards==-1 | accident.dt$urban_or_rural_area==-1 | accident.dt$did_police_officer_attend_scene_of_accident==-1
                           | accident.dt$trunk_road_flag==-1 | accident.dt$local_authority_district==-1
                           | accident.dt$junction_detail==99 | accident.dt$junction_control==9
                           | accident.dt$pedestrian_crossing_human_control==9 | accident.dt$pedestrian_crossing_physical_facilities==9
                           | accident.dt$weather_conditions==9 | accident.dt$road_surface_conditions==9 | accident.dt$special_conditions_at_site==9
                           | accident.dt$carriageway_hazards==9),]



categoricalcols <- c("accident_year","accident_severity","day_of_week",
          "local_authority_district","first_road_class","road_type","junction_detail","junction_control","second_road_class",
          "pedestrian_crossing_human_control","pedestrian_crossing_physical_facilities",
          "light_conditions","weather_conditions","road_surface_conditions",
          "urban_or_rural_area","did_police_officer_attend_scene_of_accident","trunk_road_flag",
          "Month")

dropcols <- c("accident_reference","location_easting_osgr","location_northing_osgr",
              "local_authority_ons_district","local_authority_highway","police_force")

numericcols<- c("longitude","latitude","speed_limit")


datecols <- c("date")


accident.dt <- accident.dt[,(numericcols):=lapply(.SD,as.numeric),.SDcols = numericcols]
setDT(accident.dt)[, Month := format(as.Date(date), "%m")]
accident.dt <- accident.dt[,(categoricalcols):=lapply(.SD,as.factor),.SDcols = categoricalcols]



#accident.dt <- accident.dt[, -dropcols] # delete columns 5 through 7
setDT(accident.dt)[, c("accident_year","accident_reference","location_easting_osgr","location_northing_osgr",
                       "local_authority_ons_district","local_authority_highway","lsoa_of_accident_location","local_authority_district") := NULL]


accident.dt$substring_Time = str_sub(accident.dt$time,1,2)
timecols <- c("substring_Time")
accident.dt <- accident.dt[,(timecols):=lapply(.SD,as.numeric),.SDcols = timecols]




numericcat <- numeric()                       # Create empty data object
numericcat[accident.dt$substring_Time >= 6 & accident.dt$substring_Time < 10] <- 1                   # Assign categories based on numeric range
numericcat[accident.dt$substring_Time >= 10 & accident.dt$substring_Time < 12] <- 2
numericcat[accident.dt$substring_Time >= 12 & accident.dt$substring_Time < 14] <- 3
numericcat[accident.dt$substring_Time >= 14 & accident.dt$substring_Time < 16] <- 4
numericcat[accident.dt$substring_Time >= 16 & accident.dt$substring_Time < 18] <- 5
numericcat[accident.dt$substring_Time >= 18 & accident.dt$substring_Time < 22] <- 6
numericcat[accident.dt$substring_Time >= 22] <- 7
numericcat[accident.dt$substring_Time < 6] <- 7
numericcat <- as.factor(numericcat)                 # Convert numeric to factor
accident.dt <- cbind(accident.dt, numericcat)

number_of_vehicles_cat = numeric()
number_of_vehicles_cat[accident.dt$number_of_vehicles == 1] <- 1                   # Assign categories based on numeric range
number_of_vehicles_cat[accident.dt$number_of_vehicles == 2] <- 2
number_of_vehicles_cat[accident.dt$number_of_vehicles == 3] <- 3
number_of_vehicles_cat[accident.dt$number_of_vehicles >= 4] <- 4
number_of_vehicles_cat <- as.factor(number_of_vehicles_cat)
accident.dt <- cbind(accident.dt,number_of_vehicles_cat)

number_of_casualties_cat = numeric()
number_of_casualties_cat[accident.dt$number_of_casualties == 1] <- 1                   # Assign categories based on numeric range
number_of_casualties_cat[accident.dt$number_of_casualties == 2] <- 2
number_of_casualties_cat[accident.dt$number_of_casualties == 3] <- 3
number_of_casualties_cat[accident.dt$number_of_casualties >= 4] <- 4
number_of_casualties_cat <- as.factor(number_of_casualties_cat)
accident.dt <- cbind(accident.dt,number_of_casualties_cat)

special_conditions_cat = numeric()
special_conditions_cat[accident.dt$special_conditions_at_site == 0] <- 0                   # Assign categories based on numeric range
special_conditions_cat[accident.dt$special_conditions_at_site >= 1] <- 1
special_conditions_cat <- as.factor(special_conditions_cat)
accident.dt <- cbind(accident.dt,special_conditions_cat)

carriageway_hazards_cat = numeric()
carriageway_hazards_cat[accident.dt$carriageway_hazards == 0] <- 0                   # Assign categories based on numeric range
carriageway_hazards_cat[accident.dt$carriageway_hazards >= 1] <- 1
carriageway_hazards_cat <- as.factor(carriageway_hazards_cat)
accident.dt <- cbind(accident.dt,carriageway_hazards_cat)

accident_severity_cat = numeric()
accident_severity_cat[accident.dt$accident_severity == 1 | accident.dt$accident_severity == 2] <- 1                   # Assign categories based on numeric range
accident_severity_cat[accident.dt$accident_severity == 3] <- 0
accident_severity_cat <- as.factor(accident_severity_cat)
accident.dt <- cbind(accident.dt,accident_severity_cat)

setDT(accident.dt)[, c("date","time","substring_Time") := NULL]

accident.dt$accident_severity_cat <- relevel(accident.dt$accident_severity_cat,ref="2")

accident.dt <- accident.dt[complete.cases(accident.dt), ]

str(accident.dt)

setDT(accident.dt)[, c("accident_index","police_force","number_of_vehicles","number_of_casualties",
                       "accident_severity","carriageway_hazards","special_conditions_at_site",
                       "did_police_officer_attend_scene_of_accident","number_of_vehicles_cat","number_of_casualties_cat") := NULL]


str(accident.dt)





categoricalcols <- c("vehicle_type","towing_and_articulation","vehicle_manoeuvre","vehicle_location_restricted_lane","junction_location","skidding_and_overturning",
                     "first_point_of_impact","vehicle_left_hand_drive","journey_purpose_of_driver","sex_of_driver",
                     "driver_home_area_type","accident_year")


vehicle.dt<-vehicle.dt[!(vehicle.dt$vehicle_type==-1 | vehicle.dt$towing_and_articulation==-1 | vehicle.dt$vehicle_manoeuvre=="-1"
                           | vehicle.dt$vehicle_location_restricted_lane==-1 | vehicle.dt$junction_location==-1 | vehicle.dt$skidding_and_overturning==-1
                           | vehicle.dt$hit_object_in_carriageway==-1 | vehicle.dt$vehicle_leaving_carriageway==-1 | vehicle.dt$hit_object_off_carriageway==-1
                           | vehicle.dt$first_point_of_impact==-1 | vehicle.dt$vehicle_left_hand_drive==-1 | vehicle.dt$journey_purpose_of_driver==-1
                           | vehicle.dt$sex_of_driver==-1 | vehicle.dt$age_band_of_driver==-1 | vehicle.dt$engine_capacity_cc==-1
                           | vehicle.dt$propulsion_code==-1 | vehicle.dt$age_of_vehicle==-1 
                           | vehicle.dt$driver_imd_decile==-1 | vehicle.dt$driver_home_area_type==-1
                           | vehicle.dt$skidding_and_overturning==9 | vehicle.dt$junction_location==9 | vehicle.dt$towing_and_articulation==9
                         | vehicle.dt$vehicle_manoeuvre==9 | vehicle.dt$vehicle_type==99 | vehicle.dt$vehicle_location_restricted_lane==99
                         | vehicle.dt$hit_object_in_carriageway==99),]
#str(final.dt)
driver_imd_decile_cat = numeric()
driver_imd_decile_cat[vehicle.dt$driver_imd_decile == 1] <- 1                   # Assign categories based on numeric range
driver_imd_decile_cat[vehicle.dt$driver_imd_decile >= 2 & vehicle.dt$driver_imd_decile <=5] <- 2
driver_imd_decile_cat[vehicle.dt$driver_imd_decile >= 6 & vehicle.dt$driver_imd_decile <=9] <- 3
driver_imd_decile_cat[vehicle.dt$driver_imd_decile == 10] <- 4
driver_imd_decile_cat <- as.factor(driver_imd_decile_cat)
vehicle.dt <- cbind(vehicle.dt,driver_imd_decile_cat)

propulsion_code_cat = numeric()
propulsion_code_cat[vehicle.dt$propulsion_code == 1] <- 1                   # Assign categories based on numeric range
propulsion_code_cat[vehicle.dt$propulsion_code == 2] <- 2
propulsion_code_cat[vehicle.dt$propulsion_code >= 3] <- 3
propulsion_code_cat <- as.factor(propulsion_code_cat)
vehicle.dt <- cbind(vehicle.dt,propulsion_code_cat)

age_band_of_driver_cat = numeric()
age_band_of_driver_cat[vehicle.dt$age_band_of_driver <= 5] <- 1                   # Assign categories based on numeric range
age_band_of_driver_cat[vehicle.dt$age_band_of_driver == 6] <- 2
age_band_of_driver_cat[vehicle.dt$age_band_of_driver == 7] <- 3
age_band_of_driver_cat[vehicle.dt$age_band_of_driver == 8] <- 4
age_band_of_driver_cat[vehicle.dt$age_band_of_driver >= 9] <- 5
age_band_of_driver_cat <- as.factor(age_band_of_driver_cat)
vehicle.dt <- cbind(vehicle.dt,age_band_of_driver_cat)

hit_object_off_carriageway_cat = numeric()
hit_object_off_carriageway_cat[vehicle.dt$hit_object_off_carriageway == 0] <- 0                   # Assign categories based on numeric range
hit_object_off_carriageway_cat[vehicle.dt$hit_object_off_carriageway >= 1 & vehicle.dt$hit_object_off_carriageway <= 12] <- 1
hit_object_off_carriageway_cat <- as.factor(hit_object_off_carriageway_cat)
vehicle.dt <- cbind(vehicle.dt,hit_object_off_carriageway_cat)

hit_object_in_carriageway_cat = numeric()
hit_object_in_carriageway_cat[vehicle.dt$hit_object_in_carriageway == 0] <- 0                   # Assign categories based on numeric range
hit_object_in_carriageway_cat[vehicle.dt$hit_object_in_carriageway >= 1 & vehicle.dt$hit_object_in_carriageway <= 12] <- 1
hit_object_in_carriageway_cat <- as.factor(hit_object_in_carriageway_cat)
vehicle.dt <- cbind(vehicle.dt,hit_object_in_carriageway_cat)

vehicle_leaving_carriageway_cat = numeric()
vehicle_leaving_carriageway_cat[vehicle.dt$vehicle_leaving_carriageway == 0] <- 0                   # Assign categories based on numeric range
vehicle_leaving_carriageway_cat[vehicle.dt$vehicle_leaving_carriageway >= 1 & vehicle.dt$vehicle_leaving_carriageway <= 8] <- 1
vehicle_leaving_carriageway_cat <- as.factor(vehicle_leaving_carriageway_cat)
vehicle.dt <- cbind(vehicle.dt,vehicle_leaving_carriageway_cat)


vehicle_location_restricted_lane_cat = numeric()
vehicle_location_restricted_lane_cat[vehicle.dt$vehicle_location_restricted_lane == 0] <- 0                   # Assign categories based on numeric range
vehicle_location_restricted_lane_cat[vehicle.dt$vehicle_location_restricted_lane >= 1 & vehicle.dt$vehicle_location_restricted_lane <= 9] <- 1
vehicle_location_restricted_lane_cat <- as.factor(vehicle_location_restricted_lane_cat)
vehicle.dt <- cbind(vehicle.dt,vehicle_location_restricted_lane_cat)

vehicle_manoeuvre_cat = numeric()
vehicle_manoeuvre_cat[vehicle.dt$vehicle_manoeuvre == 3] <- 1   
vehicle_manoeuvre_cat[vehicle.dt$vehicle_manoeuvre == 4] <- 2  
vehicle_manoeuvre_cat[vehicle.dt$vehicle_manoeuvre == 5] <- 3  
vehicle_manoeuvre_cat[vehicle.dt$vehicle_manoeuvre == 7] <- 4  # Assign categories based on numeric range
vehicle_manoeuvre_cat[vehicle.dt$vehicle_manoeuvre == 9] <- 5  # Assign categories based on numeric range
vehicle_manoeuvre_cat[vehicle.dt$vehicle_manoeuvre == 18] <- 6  # Assign categories based on numeric range
vehicle_manoeuvre_cat[vehicle.dt$vehicle_manoeuvre <= 2 |
                        vehicle.dt$vehicle_manoeuvre ==6 | vehicle.dt$vehicle_manoeuvre ==8 | 
                        (vehicle.dt$vehicle_manoeuvre >=10 & vehicle.dt$vehicle_manoeuvre <=17)] <- 7
vehicle_manoeuvre_cat <- as.factor(vehicle_manoeuvre_cat)
vehicle.dt <- cbind(vehicle.dt,vehicle_manoeuvre_cat)

vehicle_type_cat = numeric()
vehicle_type_cat[vehicle.dt$vehicle_type == 9] <- 1                   # Assign categories based on numeric range
vehicle_type_cat[vehicle.dt$vehicle_type >= 2 & vehicle.dt$vehicle_type <= 5] <- 2
vehicle_type_cat[vehicle.dt$vehicle_type == 8] <- 3
vehicle_type_cat[vehicle.dt$vehicle_type == 19] <- 4
vehicle_type_cat[vehicle.dt$vehicle_type == 10 | vehicle.dt$vehicle_type == 11 | vehicle.dt$vehicle_type == 17
                 | vehicle.dt$vehicle_type >= 20] <- 4
vehicle_type_cat <- as.factor(vehicle_type_cat)
vehicle.dt <- cbind(vehicle.dt,vehicle_type_cat)


vehicle.dt <- vehicle.dt[,(categoricalcols):=lapply(.SD,as.factor),.SDcols = categoricalcols]


setDT(vehicle.dt)[, c("accident_reference","vehicle_reference","generic_make_model","age_of_driver") := NULL]

# set the ON clause as keys of the tables:
setkey(accident.dt,accident_index) #317,798 rows #29 cols
setkey(vehicle.dt,accident_index) #692,307 rows #23 cols

# perform the join, eliminating not matched rows from Right
final.dt <- accident.dt[vehicle.dt, nomatch=0]
str(final.dt)#369317 51 vars
full.dt <- merge(accident.dt,vehicle.dt, all=TRUE)
str(full.dt) #759896 rows, #51 vars.
full.dt[complete.cases(full.dt), ]
str(full.dt)

setDT(final.dt)[, c("driver_imd_decile",
                    "propulsion_code","age_band_of_driver","hit_object_off_carriageway","hit_object_in_carriageway",
                    "vehicle_leaving_carriageway","vehicle_direction_from",
                    "vehicle_direction_to","vehicle_location_restricted_lane","vehicle_manoeuvre","vehicle_type",
                    "local_authority_district","police_force","first_road_number","second_road_number","Month"
                    ,"accident_index","longitude","latitude","special_conditions_at_site","carriageway_hazards",
                    "number_of_vehicles","number_of_casualties", "accident_severity") := NULL]

final.dt<-final.dt[complete.cases(final.dt), ]

str(accident.dt)




str(final.dt)
#final.dt<-final.dt[,-c("engine_capacity_cc")]
#Use final.dt
set.seed(2407)
#train-test split
# Do a train test split, with 75% of the data as train set and the rest as test set
train_test_split <- sample.split(Y = accident.dt$accident_severity_cat, SplitRatio = 0.70)
accident_train_set <- subset(x = accident.dt, subset = train_test_split == TRUE)
accident_test_set <- subset(x = accident.dt, subset = train_test_split == FALSE)

#accident_train_set_smote <- ROSE(accident_severity_cat ~ ., data = accident_train_set, seed = 2407)$data
library(ROSE)

summary(accident_train_set$accident_severity_cat)
n_serious = 21323
new_ratio <- 0.5
new_n_total <- n_serious/new_ratio
undersampling_result <- ovun.sample(accident_severity_cat~.,
                                    data = accident.dt,
                                    method = 'under',
                                    p = 0.5,
                                    seed = 2407)
accident_train_set_smote <- undersampling_result$data
summary(accident_train_set_smote$accident_severity_cat)
balanced_sample = NULL


for (c in unique(accident_train_set$accident_severity_cat)) {
  tmp_df = accident_train_set%>%filter(accident_severity_cat=='1')
  tmp<-ovun.sample(accident_severity_cat ~ ., data = tmp_df, method = "under", p = 0.5, seed = 5)$data
  balanced_sample<-rbind(balanced_sample, tmp)
}

# Do SMOTE to over-sample the minority data on the train set.
#accident_train_set_smote <- SMOTE(form = accident_severity ~ ., data = accident_train_set, perc.over = 100, k = 5, perc.under = 200)
#accident_train_set_smote <- SLS(accident_train_set[,-1],accident_train_set[,1],K=7)
#gc()
#memory.limit(9999999999)

# Random forest using default settings to check variable importance
rf_analysis <- randomForest(formula = accident_severity_cat ~., data = final.dt, na.action = na.omit,importance = TRUE)

# Check the importance of the variables
importance(x = rf_analysis)

# Plot the variable importance, which uses mean decreasing accuracy
varImpPlot(x = rf_analysis, type = 1)

#-------------------------------------------------------------------------------
# End of Random Forest Variable Importance Analysis
#-------------------------------------------------------------------------------

# Random forest using default settings to generate model
rf1 <- randomForest(formula = accident_severity_cat ~., na.action = na.omit, mtry=5, data = accident_train_set_smote)

# Show the model and its confusion matrix
rf1

# Check variable importance
importance(x = rf1)
varImpPlot(x = rf1)

plot(x = rf1)

#-------------------------------------------------------------------------------
# Random Forest Model Accuracy
#-------------------------------------------------------------------------------

# Predict the model and show the confusion matrix along with the accuracy and other coefficients
rf_trainset_predicted <- predict(object = rf1, type='prob', newdata = accident_train_set_smote)
rf_trainset_predicted <- as.data.frame(rf_trainset_predicted)
rf_trainset_predicted <- ifelse(test = rf_trainset_predicted$`1` > 0.7, 1, 0)
confusionMatrix(data = table(accident_train_set_smote$accident_severity_cat, rf_trainset_predicted, deparse.level = 2), mode = "everything")

rf_testset_predicted <- predict(object = rf1, type='prob', newdata = accident_test_set)
rf_testset_predicted <- as.data.frame(rf_testset_predicted)
rf_testset_predicted <- ifelse(test = rf_testset_predicted$`1` > 0.6, 1, 0)
confusionMatrix(data = table(rf_testset_predicted,accident_test_set$accident_severity_cat
                             ,  deparse.level = 2), mode = "everything")
# Predict the model and show the confusion matrix along with the accuracy and other coefficients
rf_trainset_predicted <- predict(object = rf1, newdata = accident_train_set_smote)
confusionMatrix(data = table(accident_train_set_smote$accident_severity_cat, rf_trainset_predicted, deparse.level = 2), mode = "everything")

rf_testset_predicted <- predict(object = rf1, newdata = accident_test_set)
confusionMatrix(data = table(rf_testset_predicted,accident_test_set$accident_severity_cat, deparse.level = 2), mode = "everything")

#===============================================================================

#model buildings

#===============================================================================

#===============================================================================
# Start of Logistic Regression
#===============================================================================

# Train the model
glm1 <- glm(formula = accident_severity_cat ~ ., family = binomial, data = accident_train_set_smote)

# Significant variables from initial logistic regression model:
summary(object = glm1)

# Check the Odds Ratio of variables to find trends
options(scipen = 1)
glm1.OR <- exp(x = coef(object = glm1))
glm1.OR

# Check for multicollinearity problem in our model.
vif(mod = glm1)
# For VIF > 5 or VIF > 10, we may conclude that there is multicollinearity.
# For our case, there is no multicollinearity problem between variables.

#-------------------------------------------------------------------------------
# Logistic Regression Model Accuracy
#-------------------------------------------------------------------------------
threshold = 0.5
# Calculate the train set probabilities, and use it to predict the data
# Plot the confusion matrix as well, along with the accuracy and other coefficients
glm1_trainset_probabilities <- predict(object = glm1, type = "response", newdata = accident_train_set_smote)
glm1_trainset_predicted <- ifelse(test = glm1_trainset_probabilities > threshold, 1, 0)
glm1_trainset_predicted <-as.factor(glm1_trainset_predicted)
#glm1_trainset_predicted <- relevel(glm1_trainset_predicted,ref=)

confusionMatrix(data = table(accident_train_set_smote$accident_severity_cat, glm1_trainset_predicted, deparse.level = 2), mode = "everything")

# Calculate the test set probabilities, and use it to predict the data
# Plot the confusion matrix as well, along with the accuracy and other coefficients
glm1_testset_probabilities <- predict(object = glm1, type = "response", newdata = accident_test_set)
glm1_testset_predicted <- ifelse(test = glm1_testset_probabilities > threshold, 1,0)
glm1_testset_predicted <-as.factor(glm1_testset_predicted)
confusionMatrix(data = table(accident_test_set$accident_severity_cat, glm1_testset_predicted, deparse.level = 2), mode = "everything")

#===============================================================================
# End of Logistic Regression
#===============================================================================


#===============================================================================
# Start of CART
#===============================================================================

#-------------------------------------------------------------------------------
# CART - Variable Importance Analysis
#-------------------------------------------------------------------------------

# Use CART to find important variables, and display the results
#cart_analysis <- rpart(formula = accident_severity_cat~., method = "class", data = accident_train_set_smote, control = rpart.control(minsplit = 2, cp = 0))
#cart_analysis$variable.importance
#round(x = cart_analysis$variable.importance / sum(cart_analysis$variable.importance) * 100, digits = 4)

#-------------------------------------------------------------------------------
# CART - End of Variable Importance Analysis
#-------------------------------------------------------------------------------

# Use CART to create a predictive model
cart1 <- rpart(formula = accident_severity_cat~., method = "class", data = accident_train_set_smote, control = rpart.control(minsplit = 2, cp = 0))

# Plotting the maximal tree
# rpart.plot(cart1, nn = T, main = "Maximal Tree")
printcp(x = cart1)
plotcp(x = cart1, main = "Subtrees")

# Compute min CVerror + 1SE in maximal tree cart
CVerror.cap <- cart1$cptable[which.min(cart1$cptable[, "xerror"]), "xerror"] + cart1$cptable[which.min(cart1$cptable[, "xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j <- 4
while (cart1$cptable[i, j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if 
# optimal tree has at least one split. Where i = 3 in this case.
cp.opt <- ifelse(test = i > 1, yes = sqrt(x = cart1$cptable[i, 1] * cart1$cptable[i - 1, 1]), no = 1)

# Prune the max tree using the optimal cp = cp.opt
cart2 <- prune(tree = cart1, cp = cp.opt)
rpart.plot(x = cart2, nn = T, main = "Optimal Tree")

# Check the variable importance of all variables
round(x = cart2$variable.importance / sum(cart2$variable.importance) * 100)

# Show the splits of the tree after pruning
summary(object = cart2)

#-------------------------------------------------------------------------------
# CART Model Accuracy
#-------------------------------------------------------------------------------

# Predict the train set and show the confusion matrix of the data, along with the accuracy and other coefficients
cart2_trainset_predicted <- predict(object = cart2, type = "class", newdata = accident_train_set_smote)
confusionMatrix(data = table(accident_train_set_smote$accident_severity_cat, cart2_trainset_predicted, deparse.level = 2), mode = "everything")

# Predict the test set and show the confusion matrix of the data, along with the accuracy and other coefficients
cart2_testset_predicted <- predict(object = cart2, type = "class", newdata = accident_test_set)
confusionMatrix(data = table(accident_test_set$accident_severity_cat, cart2_testset_predicted, deparse.level = 2), mode = "everything")

#===============================================================================
# End of CART
#===============================================================================


#===============================================================================
# Start of MARS
#===============================================================================

# Create a MARS prediction model, using glm with family binomial as we have categorical Y
mars1 <- earth(formula = accident_severity_cat ~., degree = 1, data = accident_train_set_smote, glm = list(family = binomial))

# Show the hinges of the MARS model
summary(object = mars1)

#-------------------------------------------------------------------------------
# MARS Model Accuracy
#-------------------------------------------------------------------------------

# Predict and create confusion matrix for both train and test set along with the accuracy and other coefficients
mars_trainset_predicted <- predict(object = mars1, type = "class", newdata = accident_train_set_smote)
confusionMatrix(data = table(accident_train_set_smote$accident_severity_cat, mars_trainset_predicted, deparse.level = 2), mode = "everything")

mars_testset_predicted <- predict(object = mars1, type = "class", newdata = accident_test_set)
confusionMatrix(data = table(accident_test_set$accident_severity_cat, mars_testset_predicted, deparse.level = 2), mode = "everything")

#===============================================================================
# End of MARS
#===============================================================================

