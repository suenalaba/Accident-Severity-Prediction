# ===============================================================================================================
# Purpose:      Predicting accident severity with machine learning models and identifying key factors 
# Author:       Joshua Khoo, Joel Mui, Ethan ong, Brandon Quek, He Ying
# DOC:          02-04-2022
# Topics:       Logistic Regression, MARS, CART & Random Forest
# Data Source:  accident_data_cleaned.csv, vehicle_data_cleaned.csv, casualty_data_cleaned.csv
# Packages:     car, ROSE, caTools, caret, earth, rpart, rpart.plot, randomForest, data.table,
               #cattonum, plyr, ROCR, alookr
#================================================================================================================

#Automation to install packages that are necessary but not yet installed
list.of.packages <- c("car", "ROSE", "caTools", "caret", "earth", "rpart", "rpart.plot", 
                      "randomForest","data.table", "cattonum", "plyr", "ROCR", "alookr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(car)
library(ROSE)
library(caTools)
library(caret)
library(earth)
library(rpart)
library(rpart.plot)
library(randomForest)
library(data.table)
library(cattonum)
library(plyr)
library(ROCR)
library(alookr)

#set your working directory here....
setwd("C:/Users/joshua/Downloads/BC2407 Analytics II/Datasets Cleaned")

#========================= merging the datasets ================================
accident.dt <- fread("accident_data_cleaned.csv") #106121 obs 22 var
vehicle.dt <- fread("vehicle_data_cleaned.csv") #56850 obs 21 var
casualty.dt <- fread("casualty_data_cleaned.csv") #118732 13 var

# merging the 3 data tables based on accident index.
accident_casualty.dt <- merge(accident.dt, casualty.dt) #118732 obs 33 variables
accidents_combined.dt <- merge(accident_casualty.dt,vehicle.dt) #73,337 obs 52 variables

#remove accident index once after joining
accidents_combined.dt[,accident_index:=NULL]


#extracting columns to be converted to categorical
categoricalCols = c("vehicle_manoeuvre", "hit_object_off_carriageway",
                    "skidding_and_overturning","hit_object_in_carriageway", 
                    "first_point_of_impact", "junction_location",
                    "journey_purpose","sex_of_driver","propulsion_code",
                    "vehicle_type","casualty_agegroup","casualty_type", 
                    "casualty_class","casualty_sex",
                    "accident_severity", "road_type", "speed_limit",
                    "junction_detail", "junction_control","urban_or_rural_area", 
                    "light_conditions", "timeofday","special_conditions_at_site",
                    "pedestrian_crossing_physical_facilities", "month",
                    "pedestrian_crossing_human_control",  "weather_conditions",
                    "carriageway_hazards","day", "road_surface_conditions",
                    "pedestrian_movement", "pedestrian_location","first_road_class",
                    "trunk_road_flag","car_passenger", "bus_or_coach_passenger",
                    "pedestrian_road_maintenance_worker", "casualty_home_area_type",
                    "casualty_imd_decile", "towing_and_articulation","vehicle_location_restricted_lane",
                    "vehicle_leaving_carriageway", "lhd","driver_imd_decile",
                    "driver_home_area", "driver_age_group","local_authority_district")

#convert data to categorical accordingly
accidents_combined.dt[, (categoricalCols) := lapply(.SD, as.factor), .SDcols = categoricalCols]

#Kruskal test for categorical Y - continuous X
#p-value are all below 0.05, therefore, we can reject the null hypothesis that 
#accident severity are independent to these variables.
#Hence, all these variables will be kept.
kruskal.test(accident_severity ~ age_of_vehicle, data = accidents_combined.dt)
kruskal.test(accident_severity ~ engine_capacity_cc, data = accidents_combined.dt)
kruskal.test(accident_severity ~ longitude, data = accidents_combined.dt)
kruskal.test(accident_severity ~ latitude, data = accidents_combined.dt)

#Filter method for feature selection
#run chi square test to check if predictors are independent with accident_severity
#This test is for categorical X and categorical Y
chisqresult <- lapply(accidents_combined.dt[,-c("accident_severity","longitude",
                                                "latitude","engine_capacity_cc",
                                                "age_of_vehicle")], 
                      function(x) chisq.test(accidents_combined.dt[,c("accident_severity")], x))

#check result
do.call(rbind, chisqresult)[,c(1,3)]

#The p-value for these variables are >0.05, therefore, we cannot reject the null hypothesis
#that accident severity are independent to these variables
#Hence, they will be removed.

extraColsToRemove = c("lhd", "car_passenger","trunk_road_flag", "carriageway_hazards",
                      "road_surface_conditions")

accidents_combined.dt[,(extraColsToRemove):=NULL]

#no highly correlated variables to remove
accidents_combined.dt <- treatment_corr(accidents_combined.dt)

accidentml.dt <- copy(accidents_combined.dt)

#re level some factors

accidentml.dt$day <- factor(accidentml.dt$day, levels = c("Monday","Tuesday",
                                                           "Wednesday","Thursday",
                                                           "Friday","Saturday",
                                                           "Sunday"))

accidentml.dt$month <- factor(accidentml.dt$month, levels = c("Jan","Feb","Mar",
                                                              "Apr","May", "Jun",
                                                              "Jul","Aug","Sep",
                                                              "Oct","Nov","Dec"))
accidentml.dt$casualty_agegroup <- factor(accidentml.dt$casualty_agegroup, 
                                          levels = c("Children","Teenager",
                                                     "Adult","Senior"))

accidentml.dt$driver_age_group <- factor(accidentml.dt$driver_age_group,
                                         levels = c("Underage", "Young driver",
                                                    "Adult driver", "Older driver"))

#========================== Train-test split ===================================

set.seed(2407) #to ensure reproducibility of result

#70-30 train test split to have a constant evaluation point across models
traintest_split <- sample.split(Y = accidentml.dt$accident_severity, SplitRatio = 0.7)
accident_trainset <- subset(x = accidentml.dt, subset = traintest_split == TRUE)
accident_testset <- subset(x = accidentml.dt, subset = traintest_split == FALSE)

#===============================================================================

#Frequency encoding on train set

#Goal: To prevent data leakage
#Frequency encoding is done ONLY on train set. 
#Any frequency distribution used to predict the test set will be the distribution
#used in the train set.
#That is, ANY frequency distribution used to predict the test set, is 
#independent of test set distribution

#===============================================================================

#get count
countLocationData <- catto_freq(accident_trainset, c("local_authority_district"))

#get frequency
accident_trainset$local_authority_district_freq = countLocationData$local_authority_district / length(countLocationData$local_authority_district)


#min-max scaler
accident_trainset$local_authority_district_freq = 
  (accident_trainset$local_authority_district_freq - min(accident_trainset$local_authority_district_freq)) / 
  (max(accident_trainset$local_authority_district_freq) - min(accident_trainset$local_authority_district_freq))


#extract relevant column 
districtFreqEncoding <- subset(accident_trainset, select = c("local_authority_district", "local_authority_district_freq"))

#convert to numeric for matching
districtFreqEncoding$local_authority_district = as.numeric(districtFreqEncoding$local_authority_district)
accident_testset$local_authority_district = as.numeric(accident_testset$local_authority_district)

#joining the frequencies based on frequency encoding of TRAINSET
accident_testset = join(x=accident_testset,y=districtFreqEncoding, by="local_authority_district",
                        type = "left", match = "first")

#remove local authority district as its no longer of use.
accident_trainset[,c("local_authority_district"):=NULL]
accident_testset[,c("local_authority_district"):=NULL]

#=====generate different data sets based on different sampling techniques========

#over sampling of minority class
accident_trainset_over <- ovun.sample(accident_severity ~ ., 
                                      data = accident_trainset,
                                      method = "over",p = 0.5, seed = 2407)$data
#standard under sampling
accident_trainset_under <- ovun.sample(accident_severity ~ ., 
                                       data = accident_trainset, 
                                       method = "under",p = 0.5, seed = 2407)$data
#hybrid of oversampling and under sampling
accident_trainset_both <- ovun.sample(accident_severity ~ ., 
                                      data = accident_trainset,
                                      method = "both",p = 0.5, seed = 2407)$data
#check that the sampled data makes sense
#sampling techniques can sometimes generate negative values that don't make sense
summary(accident_trainset_over)
summary(accident_trainset_under)
summary(accident_trainset_both)

#================== Start of Machine Learning ==================================

#define the threshold to be the standard = 0.5
threshold = 0.5

#the cost of FNR is 21% greater than FPR.
#Therefore, total cost = FNR * amount + 0.79 * FPR * amount
#cost multiplier refers to % cost of amount difference
costmultiplier = 0.79

#===============================================================================

#1. Identifying of possible root causes, our interest here is studying associations
#and coefficients rather than model building, hence, the original data set
#excluding train-test split will be used.
#The label encoded local authority district will be removed, for interpretability.

#===============================================================================

#========= Logistic Regression with BE for identifying important factors =======

glmaccident_rc <- glm(formula = accident_severity ~ .-local_authority_district, family = binomial, data = accidentml.dt)


glm_accident_be <- step(glmaccident_rc)

summary(glm_accident_be)

#we have some NAs we need to remove, this occurs as some categories can be
#perfectly represented by other columns
glm_accident_be_noNa <- glm(formula = accident_severity ~ longitude + latitude + road_type + 
                              speed_limit + junction_detail + junction_control + pedestrian_crossing_physical_facilities + 
                              weather_conditions + urban_or_rural_area + day + month + 
                              light_conditions + timeofday + pedestrian_road_maintenance_worker + 
                              casualty_type + casualty_home_area_type +  
                              casualty_sex + casualty_agegroup + 
                              engine_capacity_cc + age_of_vehicle + vehicle_manoeuvre + 
                              vehicle_location_restricted_lane + junction_location + skidding_and_overturning + 
                              hit_object_in_carriageway + vehicle_leaving_carriageway + 
                              first_point_of_impact + journey_purpose + driver_imd_decile + 
                              driver_age_group + vehicle_type, family = binomial, data = accidentml.dt)

#final model from logistic regression used to identify important predictors
summary(glm_accident_be_noNa)

#calculate the odds ratio
options(scipen = 1)
glmaccident_rc_be.OR <- exp(coef(glm_accident_be_noNa))
glmaccident_rc_be.OR

#determine the Odds ratio confidence interval
glmaccident_rc.OR.CI <- exp(confint(glm_accident_be_noNa))  
glmaccident_rc.OR.CI

#================= MARS for identifying important factors ======================

#we set degree = 2, as we are interested to study interaction effects
mars_accident_rc <- earth(formula = accident_severity ~ . -local_authority_district, 
                          degree = 2, data = accidentml.dt, 
                          glm = list(family = binomial))

summary(mars_accident_rc)

evimp(mars_accident_rc)

#===============================================================================

#Modelling with logistic regression 

#===============================================================================

#creating data frames to store results of logistic regression

#generate empty data frame to store test set results for balanced data
trainset_accuracy_lr <- data.frame('Model' = c("Logistic Regression with BE (Oversampling)",
                                               "Logistic Regression with BE (Both)",
                                               "Logistic Regression with BE (Undersampling)"),
                                   'FPR' = rep(0, 3), 'FNR' = rep(0, 3),
                                   'Acc' = rep(0, 3), 'AUC' = rep(0, 3), 
                                   'Cost' = rep(0, 3), 'F1 Score' = rep(0,3),
                                   'F2 Score' = rep(0,3))

#generate empty data frame to store train set results
testset_accuracy_lr <- data.frame('Model' = c("Logistic Regression with BE (Oversampling)",
                                              "Logistic Regression with BE (Both)",
                                              "Logistic Regression with BE (Undersampling)"),
                                  'FPR' = rep(0, 3), 'FNR' = rep(0, 3),
                                  'Acc' = rep(0, 3), 'AUC' = rep(0, 3), 
                                  'Cost' = rep(0, 3), 'F1 Score' = rep(0,3),
                                  'F2 Score' = rep(0,3))

#========================= Logistic Regression with oversampling =======================

glmaccident_over <- glm(formula = accident_severity ~., 
                        family = binomial, data = accident_trainset_over)

glmaccident_over_be <- step(glmaccident_over)

glmaccident_over_be_noNa <- glm(formula = accident_severity ~ longitude + latitude + road_type + 
                                  speed_limit + junction_detail + junction_control + pedestrian_crossing_human_control + 
                                  pedestrian_crossing_physical_facilities + weather_conditions + 
                                  special_conditions_at_site + urban_or_rural_area + day + 
                                  month + light_conditions + timeofday + bus_or_coach_passenger + 
                                  casualty_type + casualty_home_area_type + 
                                  casualty_imd_decile + casualty_sex +
                                  casualty_agegroup + engine_capacity_cc + age_of_vehicle + 
                                  towing_and_articulation + vehicle_manoeuvre + vehicle_location_restricted_lane + 
                                  junction_location + skidding_and_overturning + hit_object_in_carriageway + 
                                  vehicle_leaving_carriageway + hit_object_off_carriageway + 
                                  first_point_of_impact + journey_purpose + sex_of_driver + 
                                  propulsion_code + driver_imd_decile + driver_age_group + 
                                  vehicle_type + local_authority_district_freq, family = binomial, 
                                data = accident_trainset_over)

summary(glmaccident_over_be_noNa)

#calculate the odds ratio
options(scipen = 1)
glmaccident_rc_be.OR <- exp(coef(glmaccident_over_be_noNa))
glmaccident_rc_be.OR

#determine the Odds ratio confidence interval
glmaccident_rc.OR.CI <- exp(confint(glmaccident_over_be_noNa))  
glmaccident_rc.OR.CI

trainset_prob <- predict(glmaccident_over_be_noNa, type = "response")
trainset_pred <- ifelse(test = trainset_prob > threshold, "Serious", "Not Serious")
lr_over_train_cf <- confusionMatrix(data = table(accident_trainset_over$accident_severity, 
                                                 trainset_pred, deparse.level = 2))

#getting the area under curve
pred = prediction(trainset_prob, accident_trainset_over$accident_severity)
auc = performance(pred, measure = "auc")

#storing the metrics into a table
trainset_accuracy_lr[1,2] <- (lr_over_train_cf[["table"]][3] / (lr_over_train_cf[["table"]][3] + lr_over_train_cf[["table"]][1])) #fpr
trainset_accuracy_lr[1,3] <- (lr_over_train_cf[["table"]][2] / (lr_over_train_cf[["table"]][2] + lr_over_train_cf[["table"]][4])) #fnr
trainset_accuracy_lr[1,4] <- ((lr_over_train_cf[["overall"]][1])) #error
trainset_accuracy_lr[1,5] <- auc@y.values
trainset_accuracy_lr[1,6] <- paste(round(100*trainset_accuracy_lr[1,3] + costmultiplier * 100*trainset_accuracy_lr[1,2],3), '*Amount')
trainset_accuracy_lr[1,7] <- (lr_over_train_cf[["table"]][4] / (lr_over_train_cf[["table"]][4] + (0.5 * (lr_over_train_cf[["table"]][3] + lr_over_train_cf[["table"]][2])))) #f1 score
trainset_accuracy_lr[1,8] <- (lr_over_train_cf[["table"]][4] / (lr_over_train_cf[["table"]][4] + 0.2 * lr_over_train_cf[["table"]][3] + 0.8 * lr_over_train_cf[["table"]][2])) #f2 score


#testing the model on unseen data and plotting of confusion matrix
testset_prob <- predict(glmaccident_over_be_noNa, type = "response", newdata = accident_testset)
testset_pred <- ifelse(test = testset_prob > threshold, "Serious", "Not Serious")
lr_over_test_cf <- confusionMatrix(data = table(accident_testset$accident_severity, 
                                                testset_pred, deparse.level = 2), mode = "everything")

#getting the area under curve
pred = prediction(testset_prob, accident_testset$accident_severity)
auc = performance(pred, measure = "auc")

#storing the metrics into a table
testset_accuracy_lr[1,2] <- (lr_over_test_cf[["table"]][3] / (lr_over_test_cf[["table"]][3] + lr_over_test_cf[["table"]][1])) #fpr
testset_accuracy_lr[1,3] <- (lr_over_test_cf[["table"]][2] / (lr_over_test_cf[["table"]][2] + lr_over_test_cf[["table"]][4])) #fnr
testset_accuracy_lr[1,4] <- ((lr_over_test_cf[["overall"]][1])) #error
testset_accuracy_lr[1,5] <- auc@y.values
testset_accuracy_lr[1,6] <- paste(round(100*testset_accuracy_lr[1,3] + costmultiplier * 100*testset_accuracy_lr[1,2],3), '*Amount')
testset_accuracy_lr[1,7] <- (lr_over_test_cf[["table"]][4] / (lr_over_test_cf[["table"]][4] + (0.5 * (lr_over_test_cf[["table"]][3] + lr_over_test_cf[["table"]][2])))) #f1 score
testset_accuracy_lr[1,8] <- (lr_over_test_cf[["table"]][4] / (lr_over_test_cf[["table"]][4] + 0.2 * lr_over_test_cf[["table"]][3] + 0.8 * lr_over_test_cf[["table"]][2])) #f2 score


#=================== Logistic Regression with both==============================

glmaccident_both <- glm(formula = accident_severity ~ . -local_authority_district_freq, 
                        family = binomial, data = accident_trainset_both)

glmaccident_both_be <- step(glmaccident_both)

summary(glmaccident_both_be)

glmaccident_both_be_noNa <- glm(formula = accident_severity ~ longitude + latitude + road_type + 
                                  speed_limit + junction_detail + junction_control + pedestrian_crossing_human_control + 
                                  pedestrian_crossing_physical_facilities + weather_conditions + 
                                  special_conditions_at_site + urban_or_rural_area + day + 
                                  month + light_conditions + timeofday + bus_or_coach_passenger + 
                                  casualty_type + casualty_home_area_type + casualty_imd_decile + 
                                  casualty_sex + casualty_agegroup + engine_capacity_cc + 
                                  age_of_vehicle + towing_and_articulation + vehicle_manoeuvre + 
                                  vehicle_location_restricted_lane + junction_location + skidding_and_overturning + 
                                  hit_object_in_carriageway + vehicle_leaving_carriageway + 
                                  first_point_of_impact + journey_purpose + sex_of_driver + 
                                  propulsion_code + driver_imd_decile + driver_home_area + 
                                  vehicle_type, family = binomial, data = accident_trainset_both)

summary(glmaccident_both_be_noNa)

#calculate the odds ratio
options(scipen = 1)
glmaccident_rc_be.OR <- exp(coef(glmaccident_both_be_noNa))
glmaccident_rc_be.OR

#determine the Odds ratio confidence interval
glmaccident_rc.OR.CI <- exp(confint(glmaccident_both_be_noNa))  
glmaccident_rc.OR.CI

trainset_prob <- predict(glmaccident_both_be_noNa, type = "response")
trainset_pred <- ifelse(test = trainset_prob > threshold, "Serious", "Not Serious")
lr_both_train_cf <- confusionMatrix(data = table(accident_trainset_both$accident_severity, 
                                                 trainset_pred, deparse.level = 2))

#getting the area both curve
pred = prediction(trainset_prob, accident_trainset_both$accident_severity)
auc = performance(pred, measure = "auc")

#storing the metrics into a table
trainset_accuracy_lr[2,2] <- (lr_both_train_cf[["table"]][3] / (lr_both_train_cf[["table"]][3] + lr_both_train_cf[["table"]][1])) #fpr
trainset_accuracy_lr[2,3] <- (lr_both_train_cf[["table"]][2] / (lr_both_train_cf[["table"]][2] + lr_both_train_cf[["table"]][4])) #fnr
trainset_accuracy_lr[2,4] <- ((lr_both_train_cf[["overall"]][1])) #error
trainset_accuracy_lr[2,5] <- auc@y.values
trainset_accuracy_lr[2,6] <- paste(round(100*trainset_accuracy_lr[2,3] + costmultiplier * 100*trainset_accuracy_lr[2,2],3), '*Amount')
trainset_accuracy_lr[2,7] <- (lr_both_train_cf[["table"]][4] / (lr_both_train_cf[["table"]][4] + (0.5 * (lr_both_train_cf[["table"]][3] + lr_both_train_cf[["table"]][2])))) #f1 score
trainset_accuracy_lr[2,8] <- (lr_both_train_cf[["table"]][4] / (lr_both_train_cf[["table"]][4] + 0.2 * lr_both_train_cf[["table"]][3] + 0.8 * lr_both_train_cf[["table"]][2])) #f2 score


#testing the model on unseen data and plotting of confusion matrix
testset_prob <- predict(glmaccident_both_be_noNa, type = "response", newdata = accident_testset)
testset_pred <- ifelse(test = testset_prob > threshold, "Serious", "Not Serious")
lr_both_test_cf <- confusionMatrix(data = table(accident_testset$accident_severity, 
                                                testset_pred, deparse.level = 2), mode = "everything")

#getting the area both curve
pred = prediction(testset_prob, accident_testset$accident_severity)
auc = performance(pred, measure = "auc")

#storing the metrics into a table
testset_accuracy_lr[2,2] <- (lr_both_test_cf[["table"]][3] / (lr_both_test_cf[["table"]][3] + lr_both_test_cf[["table"]][1])) #fpr
testset_accuracy_lr[2,3] <- (lr_both_test_cf[["table"]][2] / (lr_both_test_cf[["table"]][2] + lr_both_test_cf[["table"]][4])) #fnr
testset_accuracy_lr[2,4] <- ((lr_both_test_cf[["overall"]][1])) #error
testset_accuracy_lr[2,5] <- auc@y.values
testset_accuracy_lr[2,6] <- paste(round(100*testset_accuracy_lr[2,3] + costmultiplier * 100*testset_accuracy_lr[2,2],3), '*Amount')
testset_accuracy_lr[2,7] <- (lr_both_test_cf[["table"]][4] / (lr_both_test_cf[["table"]][4] + (0.5 * (lr_both_test_cf[["table"]][3] + lr_both_test_cf[["table"]][2])))) #f1 score
testset_accuracy_lr[2,8] <- (lr_both_test_cf[["table"]][4] / (lr_both_test_cf[["table"]][4] + 0.2 * lr_both_test_cf[["table"]][3] + 0.8 * lr_both_test_cf[["table"]][2])) #f2 score


#=================== Logistic Regression with Undersampling ====================


glmaccident_under <- glm(formula = accident_severity ~ . -local_authority_district_freq, 
                        family = binomial, data = accident_trainset_under)


glmaccident_under_be <- step(glmaccident_under)

summary(glmaccident_under_be)

#no na formula here

summary(glmaccident_under_be_noNa)

#calculate the odds ratio
options(scipen = 1)
glmaccident_rc_be.OR <- exp(coef(glmaccident_under_be_noNa))
glmaccident_rc_be.OR

#determine the Odds ratio confidence interval
glmaccident_rc.OR.CI <- exp(confint(glmaccident_under_be_noNa))  
glmaccident_rc.OR.CI

trainset_prob <- predict(glmaccident_under_be_noNa, type = "response")
trainset_pred <- ifelse(test = trainset_prob > threshold, "Serious", "Not Serious")
lr_under_train_cf <- confusionMatrix(data = table(accident_trainset_under$accident_severity, 
                                                 trainset_pred, deparse.level = 2))

#getting the area under curve
pred = prediction(trainset_prob, accident_trainset_under$accident_severity)
auc = performance(pred, measure = "auc")

#storing the metrics into a table
trainset_accuracy_lr[3,2] <- (lr_under_train_cf[["table"]][3] / (lr_under_train_cf[["table"]][3] + lr_under_train_cf[["table"]][1])) #fpr
trainset_accuracy_lr[3,3] <- (lr_under_train_cf[["table"]][2] / (lr_under_train_cf[["table"]][2] + lr_under_train_cf[["table"]][4])) #fnr
trainset_accuracy_lr[3,4] <- ((lr_under_train_cf[["overall"]][1])) #error
trainset_accuracy_lr[3,5] <- auc@y.values
trainset_accuracy_lr[3,6] <- paste(round(100*trainset_accuracy_lr[3,3] + costmultiplier * 100*trainset_accuracy_lr[3,2],3), '*Amount')
trainset_accuracy_lr[3,7] <- (lr_under_train_cf[["table"]][4] / (lr_under_train_cf[["table"]][4] + (0.5 * (lr_under_train_cf[["table"]][3] + lr_under_train_cf[["table"]][2])))) #f1 score
trainset_accuracy_lr[3,8] <- (lr_under_train_cf[["table"]][4] / (lr_under_train_cf[["table"]][4] + 0.2 * lr_under_train_cf[["table"]][3] + 0.8 * lr_under_train_cf[["table"]][2])) #f2 score

#testing the model on unseen data and plotting of confusion matrix
testset_prob <- predict(glmaccident_under_be_noNa, type = "response", newdata = accident_testset)
testset_pred <- ifelse(test = testset_prob > threshold, "Serious", "Not Serious")
lr_under_test_cf <- confusionMatrix(data = table(accident_testset$accident_severity, 
                                                testset_pred, deparse.level = 2), mode = "everything")

#getting the area under curve
pred = prediction(testset_prob, accident_testset$accident_severity)
auc = performance(pred, measure = "auc")

#storing the metrics into a table
testset_accuracy_lr[3,2] <- (lr_under_test_cf[["table"]][3] / (lr_under_test_cf[["table"]][3] + lr_under_test_cf[["table"]][1])) #fpr
testset_accuracy_lr[3,3] <- (lr_under_test_cf[["table"]][2] / (lr_under_test_cf[["table"]][2] + lr_under_test_cf[["table"]][4])) #fnr
testset_accuracy_lr[3,4] <- ((lr_under_test_cf[["overall"]][1])) #error
testset_accuracy_lr[3,5] <- auc@y.values
testset_accuracy_lr[3,6] <- paste(round(100*testset_accuracy_lr[3,3] + costmultiplier * 100*testset_accuracy_lr[3,2],3), '*Amount')
testset_accuracy_lr[3,7] <- (lr_under_test_cf[["table"]][4] / (lr_under_test_cf[["table"]][4] + (0.5 * (lr_under_test_cf[["table"]][3] + lr_under_test_cf[["table"]][2])))) #f1 score
testset_accuracy_lr[3,8] <- (lr_under_test_cf[["table"]][4] / (lr_under_test_cf[["table"]][4] + 0.2 * lr_under_test_cf[["table"]][3] + 0.8 * lr_under_test_cf[["table"]][2])) #f2 score

#===============================================================================

#Modelling with Multi-variate Adaptive Regression Splines (MARS)

#===============================================================================

#creating dataframes to store results

#generate empty data frame to store test set results for balanced data
testset_accuracy_mars <- data.frame('Model' = c("MARS with Oversampling",
                                                "MARS with Undersampling",
                                                "MARS with Both"), 
                                    'FPR' = rep(0, 3), 'FNR' = rep(0, 3),
                                    'Acc' = rep(0, 3), 'AUC' = rep(0, 3), 
                                    'Cost' = rep(0, 3), 'F1 Score' = rep(0,3),
                                    'F2 Score' = rep(0,3))

#generate empty data frame to store train set results
trainset_accuracy_mars <- data.frame('Model' = c("MARS with Oversampling",
                                                 "MARS with Undersampling",
                                                 "MARS with Both"), 
                                     'FPR' = rep(0, 3), 'FNR' = rep(0, 3),
                                     'Acc' = rep(0, 3), 'AUC' = rep(0, 3), 
                                     'Cost' = rep(0, 3), 'F1 Score' = rep(0,3),
                                     'F2 Score' = rep(0,3))

#============================= MARS with Oversampling ==========================

mars_accident_over <- earth(formula = accident_severity ~ ., degree = 2, data = accident_trainset_over, 
                            glm = list(family = binomial))

summary(mars_accident_over)

#checking variable importance
evimp(mars_accident_over)

#plotting of confusion matrix for the train set
trainset_prob <- predict(mars_accident_over, type = "response")
trainset_pred <- ifelse(trainset_prob > threshold, "Serious", "Not Serious")
mars_over_train_cf <- confusionMatrix(data = table(accident_trainset_over$accident_severity, 
                                                   trainset_pred, deparse.level = 2))

#getting the area under curve
pred = prediction(trainset_prob, accident_trainset_over$accident_severity)
auc = performance(pred, measure = "auc")

#storing the metrics into a table
trainset_accuracy_mars[1,2] <- (mars_over_train_cf[["table"]][3] / (mars_over_train_cf[["table"]][3] + mars_over_train_cf[["table"]][1])) #fpr
trainset_accuracy_mars[1,3] <- (mars_over_train_cf[["table"]][2] / (mars_over_train_cf[["table"]][2] + mars_over_train_cf[["table"]][4])) #fnr
trainset_accuracy_mars[1,4] <- ((mars_over_train_cf[["overall"]][1])) #error
trainset_accuracy_mars[1,5] <- auc@y.values
trainset_accuracy_mars[1,6] <- paste(round(100*trainset_accuracy_mars[1,3] + costmultiplier * 100*trainset_accuracy_mars[1,2],3), '*Amount')
trainset_accuracy_mars[1,7] <- (mars_over_train_cf[["table"]][4] / (mars_over_train_cf[["table"]][4] + (0.5 * (mars_over_train_cf[["table"]][3] + mars_over_train_cf[["table"]][2])))) #f1 score
trainset_accuracy_mars[1,8] <- (mars_over_train_cf[["table"]][4] / (mars_over_train_cf[["table"]][4] + 0.2 * mars_over_train_cf[["table"]][3] + 0.8 * mars_over_train_cf[["table"]][2])) #f2 score


#testing the model on unseen data and plotting of confusion matrix
testset_prob <- predict(mars_accident_over, type = "response", newdata = accident_testset)
testset_pred <- ifelse(testset_prob > threshold, "Serious", "Not Serious")
mars_over_test_cf <- confusionMatrix(data = table(accident_testset$accident_severity, 
                                                  testset_pred, deparse.level = 2))

#getting the area under curve
pred = prediction(testset_prob, accident_testset$accident_severity)
auc = performance(pred, measure = "auc")

#storing the metrics into a table
testset_accuracy_mars[1,2] <- (mars_over_test_cf[["table"]][3] / (mars_over_test_cf[["table"]][3] + mars_over_test_cf[["table"]][1])) #fpr
testset_accuracy_mars[1,3] <- (mars_over_test_cf[["table"]][2] / (mars_over_test_cf[["table"]][2] + mars_over_test_cf[["table"]][4])) #fnr
testset_accuracy_mars[1,4] <- ((mars_over_test_cf[["overall"]][1])) #error
testset_accuracy_mars[1,5] <- auc@y.values
testset_accuracy_mars[1,6] <- paste(round(100*testset_accuracy_mars[1,3] + costmultiplier * 100*testset_accuracy_mars[1,2],3), '*Amount')
testset_accuracy_mars[1,7] <- (mars_over_test_cf[["table"]][4] / (mars_over_test_cf[["table"]][4] + (0.5 * (mars_over_test_cf[["table"]][3] + mars_over_test_cf[["table"]][2])))) #f1 score
testset_accuracy_mars[1,8] <- (mars_over_test_cf[["table"]][4] / (mars_over_test_cf[["table"]][4] + 0.2 * mars_over_test_cf[["table"]][3] + 0.8 * mars_over_test_cf[["table"]][2])) #f2 score

#=========================== MARS with under sampling ==========================

mars_accident_under <- earth(formula = accident_severity ~ ., degree = 2, data = accident_trainset_under, 
                             glm = list(family = binomial))

summary(mars_accident_under)

evimp(mars_accident_under)

#getting train set predictions and plotting of confusion matrix
trainset_prob <- predict(mars_accident_under, type = "response")
trainset_pred <- ifelse(trainset_prob > threshold, "Serious", "Not Serious")
mars_under_train_cf <- confusionMatrix(data = table(accident_trainset_under$accident_severity,
                                                    trainset_pred, deparse.level = 2))

#getting the area under curve
pred = prediction(trainset_prob, accident_trainset_under$accident_severity)
auc = performance(pred, measure = "auc")

#storing the metrics into a table
trainset_accuracy_mars[2,2] <- (mars_under_train_cf[["table"]][3] / (mars_under_train_cf[["table"]][3] + mars_under_train_cf[["table"]][1])) #fpr
trainset_accuracy_mars[2,3] <- (mars_under_train_cf[["table"]][2] / (mars_under_train_cf[["table"]][2] + mars_under_train_cf[["table"]][4])) #fnr
trainset_accuracy_mars[2,4] <- ((mars_under_train_cf[["overall"]][1])) #error
trainset_accuracy_mars[2,5] <- auc@y.values
trainset_accuracy_mars[2,6] <- paste(round(100*trainset_accuracy_mars[2,3] + costmultiplier * 100*trainset_accuracy_mars[2,2],3), '*Amount')
trainset_accuracy_mars[2,7] <- (mars_under_train_cf[["table"]][4] / (mars_under_train_cf[["table"]][4] + (0.5 * (mars_under_train_cf[["table"]][3] + mars_under_train_cf[["table"]][2])))) #f1 score
trainset_accuracy_mars[2,8] <- (mars_under_train_cf[["table"]][4] / (mars_under_train_cf[["table"]][4] + 0.2 * mars_under_train_cf[["table"]][3] + 0.8 * mars_under_train_cf[["table"]][2])) #f2 score

#testing the model on unseen data and plotting of confusion matrix
testset_prob <- predict(mars_accident_under, type = "response", newdata = accident_testset)
testset_pred <- ifelse(testset_prob > threshold, "Serious", "Not Serious")
mars_under_test_cf <- confusionMatrix(data = table(accident_testset$accident_severity, 
                                                   testset_pred, deparse.level = 2))

#getting the area under curve
pred = prediction(testset_prob, accident_testset$accident_severity)
auc = performance(pred, measure = "auc")

#storing the metrics into a table
testset_accuracy_mars[2,2] <- (mars_under_test_cf[["table"]][3] / (mars_under_test_cf[["table"]][3] + mars_under_test_cf[["table"]][1])) #fpr
testset_accuracy_mars[2,3] <- (mars_under_test_cf[["table"]][2] / (mars_under_test_cf[["table"]][2] + mars_under_test_cf[["table"]][4])) #fnr
testset_accuracy_mars[2,4] <- ((mars_under_test_cf[["overall"]][1])) #error
testset_accuracy_mars[2,5] <- auc@y.values
testset_accuracy_mars[2,6] <- paste(round(100*testset_accuracy_mars[2,3] + costmultiplier * 100*testset_accuracy_mars[2,2],3), '*Amount')
testset_accuracy_mars[2,7] <- (mars_under_test_cf[["table"]][4] / (mars_under_test_cf[["table"]][4] + (0.5 * (mars_under_test_cf[["table"]][3] + mars_under_test_cf[["table"]][2])))) #f1 score
testset_accuracy_mars[2,8] <- (mars_under_test_cf[["table"]][4] / (mars_under_test_cf[["table"]][4] + 0.2 * mars_under_test_cf[["table"]][3] + 0.8 * mars_under_test_cf[["table"]][2])) #f2 score

#===================== MARS with both over and under sampling===================

mars_accident_both <- earth(formula = accident_severity ~ ., degree = 2, data = accident_trainset_both, 
                            glm = list(family = binomial))

summary(mars_accident_both)

evimp(mars_accident_both)

#getting the train set predictions and confusion matrix
trainset_prob <- predict(mars_accident_both, type = "response")
trainset_pred <- ifelse(trainset_prob > threshold, "Serious", "Not Serious")
mars_both_train_cf <- confusionMatrix(data = table(accident_trainset_both$accident_severity, 
                                                   trainset_pred, deparse.level = 2))

#getting the area under curve
pred = prediction(trainset_prob, accident_trainset_both$accident_severity)
auc = performance(pred, measure = "auc")

#storing the metrics into a table
trainset_accuracy_mars[3,2] <- (mars_both_train_cf[["table"]][3] / (mars_both_train_cf[["table"]][3] + mars_both_train_cf[["table"]][1])) #fpr
trainset_accuracy_mars[3,3] <- (mars_both_train_cf[["table"]][2] / (mars_both_train_cf[["table"]][2] + mars_both_train_cf[["table"]][4])) #fnr
trainset_accuracy_mars[3,4] <- ((mars_both_train_cf[["overall"]][1])) #error
trainset_accuracy_mars[3,5] <- auc@y.values
trainset_accuracy_mars[3,6] <- paste(round(100*trainset_accuracy_mars[3,3] + costmultiplier * 100*trainset_accuracy_mars[3,2],3), '*Amount')
trainset_accuracy_mars[3,7] <- (mars_both_train_cf[["table"]][4] / (mars_both_train_cf[["table"]][4] + (0.5 * (mars_both_train_cf[["table"]][3] + mars_both_train_cf[["table"]][2])))) #f1 score
trainset_accuracy_mars[3,8] <- (mars_both_train_cf[["table"]][4] / (mars_both_train_cf[["table"]][4] + 0.2 * mars_both_train_cf[["table"]][3] + 0.8 * mars_both_train_cf[["table"]][2])) #f2 score

#testing the model on unseen data and plotting of confusion matrix
testset_prob <- predict(mars_accident_both, type = "response", newdata = accident_testset)
testset_pred <- ifelse(testset_prob > threshold, "Serious", "Not Serious")
mars_both_test_cf <- confusionMatrix(data = table(accident_testset$accident_severity, 
                                                  testset_pred, deparse.level = 2))

#getting the area under curve
pred = prediction(testset_prob, accident_testset$accident_severity)
auc = performance(pred, measure = "auc")

#storing the metrics into a table
testset_accuracy_mars[3,2] <- (mars_both_test_cf[["table"]][3] / (mars_both_test_cf[["table"]][3] + mars_both_test_cf[["table"]][1])) #fpr
testset_accuracy_mars[3,3] <- (mars_both_test_cf[["table"]][2] / (mars_both_test_cf[["table"]][2] + mars_both_test_cf[["table"]][4])) #fnr
testset_accuracy_mars[3,4] <- ((mars_both_test_cf[["overall"]][1])) #error
testset_accuracy_mars[3,5] <- auc@y.values
testset_accuracy_mars[3,6] <- paste(round(100*testset_accuracy_mars[3,3] + costmultiplier * 100*testset_accuracy_mars[3,2],3), '*Amount')
testset_accuracy_mars[3,7] <- (mars_both_test_cf[["table"]][4] / (mars_both_test_cf[["table"]][4] + (0.5 * (mars_both_test_cf[["table"]][3] + mars_both_test_cf[["table"]][2])))) #f1 score
testset_accuracy_mars[3,8] <- (mars_both_test_cf[["table"]][4] / (mars_both_test_cf[["table"]][4] + 0.2 * mars_both_test_cf[["table"]][3] + 0.8 * mars_both_test_cf[["table"]][2])) #f2 score

#=========================== end of mars =======================================

#===============================================================================

#Modelling with CART

#===============================================================================

#generate data frames to store CART result

#generate empty data frame to store test set results for balanced data
testset_accuracy_cart <- data.frame('Model' = c("CART with Oversampling",
                                                "CART with Undersampling",
                                                "CART with Both"), 
                                    'FPR' = rep(0, 3), 'FNR' = rep(0, 3),
                                    'Acc' = rep(0, 3), 'AUC' = rep(0, 3), 
                                    'Cost' = rep(0, 3), 'F1 Score' = rep(0,3),
                                    'F2 Score' = rep(0,3))

#generate empty data frame to store train set results
trainset_accuracy_cart <- data.frame('Model' = c("CART with Oversampling",
                                                 "CART with Undersampling",
                                                 "CART with Both"), 
                                     'FPR' = rep(0, 3), 'FNR' = rep(0, 3),
                                     'Acc' = rep(0, 3), 'AUC' = rep(0, 3), 
                                     'Cost' = rep(0, 3), 'F1 Score' = rep(0,3),
                                     'F2 Score' = rep(0,3))


#======================== CART with oversampling ===============================

#rpart uses random sampling, therefore, set seed to ensure reproducibility
set.seed(2407)

# Use CART to create a predictive model
cart_accident_over <- rpart(accident_severity~., method = "class", 
                            data = accident_trainset_over, control = rpart.control(minsplit = 20, cp = 0))

par(mfrow = c(1,1))

#The maximal tree is extremely huge, hence we need to prune it
rpart.plot(cart_accident_over, nn=T, tweak = 0.8, main = "Maximal Tree for Accident Severity\n(Oversampling)")

print(cart_accident_over)

printcp(cart_accident_over)

plotcp(cart_accident_over)

#Source: Prof Neumann Chew's textbook Chapter 8 on CART
#function to find the Optimal CP for pruning
#takes in CART object and returns optimal CP
findOptimalCP <- function(cartmodel) {
 
  # Compute min CVerror + 1SE in maximal tree cart1.
  CVerror.cap <- cartmodel$cptable[which.min(cartmodel$cptable[,"xerror"]), "xerror"] + cartmodel$cptable[which.min(cartmodel$cptable[,"xerror"]), "xstd"]
  
  # Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cartmodel.
  i <- 1; j<- 4
  while (cartmodel$cptable[i,j] > CVerror.cap) {
    i <- i + 1
  }
  
  # Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
  cp.opt = ifelse(i > 1, sqrt(cartmodel$cptable[i,1] * cartmodel$cptable[i-1,1]), 1) #calculate geometric mean here
  
  return(cp.opt)
}

#find the optimal CP 
cp.opt = findOptimalCP(cart_accident_over)

# Prune the max tree using the optimal cp = cp.opt
cartpruned_accident_over <- prune(cart_accident_over, cp = cp.opt)

rpart.plot(cartpruned_accident_over, nn=T, tweak = 1.2, box.palette = "RdYlGn", 
           main = "Optimal Tree for Accident Severity\n (Oversampled data)")

print(cartpruned_accident_over)

printcp(cartpruned_accident_over)

summary(cartpruned_accident_over)

#due to large number of variables, it might be better to look at the aboslute values for variable importance
scaledVarImpt <- round(100*cartpruned_accident_over$variable.importance/sum(cartpruned_accident_over$variable.importance))
cartpruned_accident_over$variable.importance
scaledVarImpt[scaledVarImpt > 0]

#getting train set predictions and confusion matrix
cart2_trainset_predicted <- predict(cartpruned_accident_over, type = "class")
cart_over_train_cf <- confusionMatrix(data = table(accident_trainset_over$accident_severity, 
                                                   cart2_trainset_predicted, deparse.level = 2))
 
#getting the area under curve
cart.pred = predict(cartpruned_accident_over, type = "prob")[,2]
pred = prediction(cart.pred, accident_trainset_over$accident_severity)
auc = performance(pred, measure = "auc")

#storing the metrics into a table
trainset_accuracy_cart[1,2] <- (cart_over_train_cf[["table"]][3] / (cart_over_train_cf[["table"]][3] + cart_over_train_cf[["table"]][1])) #fpr
trainset_accuracy_cart[1,3] <- (cart_over_train_cf[["table"]][2] / (cart_over_train_cf[["table"]][2] + cart_over_train_cf[["table"]][4])) #fnr
trainset_accuracy_cart[1,4] <- ((cart_over_train_cf[["overall"]][1])) #error
trainset_accuracy_cart[1,5] <- auc@y.values
trainset_accuracy_cart[1,6] <- paste(round(100*trainset_accuracy_cart[1,3] + costmultiplier * 100*trainset_accuracy_cart[1,2],3), '*Amount')
trainset_accuracy_cart[1,7] <- (cart_over_train_cf[["table"]][4] / (cart_over_train_cf[["table"]][4] + (0.5 * (cart_over_train_cf[["table"]][3] + cart_over_train_cf[["table"]][2])))) #f1 score
trainset_accuracy_cart[1,8] <- (cart_over_train_cf[["table"]][4] / (cart_over_train_cf[["table"]][4] + 0.2 * cart_over_train_cf[["table"]][3] + 0.8 * cart_over_train_cf[["table"]][2])) #f2 score

#testing the model on unseen data and getting the confusion matrix
cart2_testset_predicted <- predict(cartpruned_accident_over, type = "class", newdata = accident_testset)
cart_over_test_cf <- confusionMatrix(data = table(accident_testset$accident_severity, 
                                                  cart2_testset_predicted, deparse.level = 2))

#getting the area under curve
cart.pred.test = predict(cartpruned_accident_over, type = "prob",newdata = accident_testset)[,2]
pred = prediction(cart.pred.test, accident_testset$accident_severity)
auc = performance(pred, measure = "auc")

#storing the metrics into a table
testset_accuracy_cart[1,2] <- (cart_over_test_cf[["table"]][3] / (cart_over_test_cf[["table"]][3] + cart_over_test_cf[["table"]][1])) #fpr
testset_accuracy_cart[1,3] <- (cart_over_test_cf[["table"]][2] / (cart_over_test_cf[["table"]][2] + cart_over_test_cf[["table"]][4])) #fnr
testset_accuracy_cart[1,4] <- ((cart_over_test_cf[["overall"]][1])) #error
testset_accuracy_cart[1,5] <- auc@y.values
testset_accuracy_cart[1,6] <- paste(round(100*testset_accuracy_cart[1,3] + costmultiplier * 100*testset_accuracy_cart[1,2],3), '*Amount')
testset_accuracy_cart[1,7] <- (cart_over_test_cf[["table"]][4] / (cart_over_test_cf[["table"]][4] + (0.5 * (cart_over_test_cf[["table"]][3] + cart_over_test_cf[["table"]][2])))) #f1 score
testset_accuracy_cart[1,8] <- (cart_over_test_cf[["table"]][4] / (cart_over_test_cf[["table"]][4] + 0.2 * cart_over_test_cf[["table"]][3] + 0.8 * cart_over_test_cf[["table"]][2])) #f2 score

#====================== CART with under sampling ===============================

#rpart uses random sampling, therefore, set seed to ensure reproducibility
set.seed(2407)
# Use CART to create a predictive model
cart_accident_under <- rpart(accident_severity~., method = "class", 
                             data = accident_trainset_under, control = rpart.control(minsplit = 20, cp = 0))

par(mfrow = c(1,1))

#Maximal tree is too huge hence, the need to prune
rpart.plot(cart_accident_under, nn=T, tweak = 0.8, main = "Maximal Tree for Accident Severity\n (Undersampled Data)")

print(cart_accident_under)

printcp(cart_accident_under)

plotcp(cart_accident_under)


cp.opt = findOptimalCP(cart_accident_under)

# Prune the max tree using the optimal cp = cp.opt
cartpruned_accident_under <- prune(cart_accident_under, cp = cp.opt)

rpart.plot(cartpruned_accident_under, nn=T, tweak = 1.2, box.palette = "RdYlGn", 
           main = "Optimal Tree for Accident Severity\n(Undersampled data)")

print(cartpruned_accident_under)

printcp(cartpruned_accident_under)

summary(cartpruned_accident_under)

scaledVarImpt <- round(100*cartpruned_accident_under$variable.importance/sum(cartpruned_accident_under$variable.importance))
cartpruned_accident_under$variable.importance
scaledVarImpt[scaledVarImpt > 0]

#getting train set predictions and confusion matrix
cart2_trainset_predicted <- predict(cartpruned_accident_under, type = "class")
cart_under_train_cf <- confusionMatrix(data = table(accident_trainset_under$accident_severity, 
                                                    cart2_trainset_predicted, deparse.level = 2))

#getting the area under curve
cart.pred = predict(cartpruned_accident_under, type = "prob")[,2]
pred = prediction(cart.pred, accident_trainset_under$accident_severity)
auc = performance(pred, measure = "auc")

#storing the metrics into a table
trainset_accuracy_cart[2,2] <- (cart_under_train_cf[["table"]][3] / (cart_under_train_cf[["table"]][3] + cart_under_train_cf[["table"]][1])) #fpr
trainset_accuracy_cart[2,3] <- (cart_under_train_cf[["table"]][2] / (cart_under_train_cf[["table"]][2] + cart_under_train_cf[["table"]][4])) #fnr
trainset_accuracy_cart[2,4] <- ((cart_under_train_cf[["overall"]][1])) #error
trainset_accuracy_cart[2,5] <- auc@y.values
trainset_accuracy_cart[2,6] <- paste(round(100*trainset_accuracy_cart[2,3] + costmultiplier * 100*trainset_accuracy_cart[2,2],3), '*Amount')
trainset_accuracy_cart[2,7] <- (cart_under_train_cf[["table"]][4] / (cart_under_train_cf[["table"]][4] + (0.5 * (cart_under_train_cf[["table"]][3] + cart_under_train_cf[["table"]][2])))) #f1 score
trainset_accuracy_cart[2,8] <- (cart_under_train_cf[["table"]][4] / (cart_under_train_cf[["table"]][4] + 0.2 * cart_under_train_cf[["table"]][3] + 0.8 * cart_under_train_cf[["table"]][2])) #f2 score

#testing the model on unseen data and getting the confusion matrix
cart2_testset_predicted <- predict(cartpruned_accident_under, type = "class", newdata = accident_testset)
cart_under_test_cf <- confusionMatrix(data = table(accident_testset$accident_severity, 
                                                   cart2_testset_predicted, deparse.level = 2))

#getting the area under curve
cart.pred.test = predict(cartpruned_accident_under, type = "prob",newdata = accident_testset)[,2]
pred = prediction(cart.pred.test, accident_testset$accident_severity)
auc = performance(pred, measure = "auc")

#storing the metrics into a table
testset_accuracy_cart[2,2] <- (cart_under_test_cf[["table"]][3] / (cart_under_test_cf[["table"]][3] + cart_under_test_cf[["table"]][1])) #fpr
testset_accuracy_cart[2,3] <- (cart_under_test_cf[["table"]][2] / (cart_under_test_cf[["table"]][2] + cart_under_test_cf[["table"]][4])) #fnr
testset_accuracy_cart[2,4] <- ((cart_under_test_cf[["overall"]][1])) #error
testset_accuracy_cart[2,5] <- auc@y.values
testset_accuracy_cart[2,6] <- paste(round(100*testset_accuracy_cart[2,3] + costmultiplier * 100*testset_accuracy_cart[2,2],3), '*Amount')
testset_accuracy_cart[2,7] <- (cart_under_test_cf[["table"]][4] / (cart_under_test_cf[["table"]][4] + (0.5 * (cart_under_test_cf[["table"]][3] + cart_under_test_cf[["table"]][2])))) #f1 score
testset_accuracy_cart[2,8] <- (cart_under_test_cf[["table"]][4] / (cart_under_test_cf[["table"]][4] + 0.2 * cart_under_test_cf[["table"]][3] + 0.8 * cart_under_test_cf[["table"]][2])) #f2 score


#========================== CART with both =====================================

#rpart uses random sampling, therefore, set seed to ensure reproducibility
set.seed(2407)

# Use CART to create a predictive model
cart_accident_both <- rpart(accident_severity~., method = "class", 
                            data = accident_trainset_both, control = rpart.control(minsplit = 20, cp = 0))

par(mfrow = c(1,1))

#Maximal tree is huge hence, the need to prune.
rpart.plot(cart_accident_both, nn=T, tweak = 0.8, main = "Maximal Tree for Accident Severity\n (Both)")

print(cart_accident_both)

printcp(cart_accident_both)

plotcp(cart_accident_both)


cp.opt = findOptimalCP(cart_accident_both)

# Prune the max tree using the optimal cp = cp.opt
cartpruned_accident_both <- prune(cart_accident_both, cp = cp.opt)

rpart.plot(cartpruned_accident_both, nn=T, tweak = 1.2, box.palette = "RdYlGn",
           main = "Optimal Tree for Accident Severity\n(Both Over and Under Sampling)")

print(cartpruned_accident_both)

printcp(cartpruned_accident_both)

summary(cartpruned_accident_both)

scaledVarImpt <- round(100*cartpruned_accident_both$variable.importance/sum(cartpruned_accident_both$variable.importance))
cartpruned_accident_both$variable.importance
scaledVarImpt[scaledVarImpt > 0]



#getting train set predictions and confusion matrix
cart2_trainset_predicted <- predict(cartpruned_accident_both, type = "class")
cart_both_train_cf <- confusionMatrix(data = table(accident_trainset_both$accident_severity, 
                                                   cart2_trainset_predicted, deparse.level = 2))

#getting the area under curve
cart.pred = predict(cartpruned_accident_both, type = "prob")[,2]
pred = prediction(cart.pred, accident_trainset_both$accident_severity)
auc = performance(pred, measure = "auc")

#storing the metrics into a table
trainset_accuracy_cart[3,2] <- (cart_both_train_cf[["table"]][3] / (cart_both_train_cf[["table"]][3] + cart_both_train_cf[["table"]][1])) #fpr
trainset_accuracy_cart[3,3] <- (cart_both_train_cf[["table"]][2] / (cart_both_train_cf[["table"]][2] + cart_both_train_cf[["table"]][4])) #fnr
trainset_accuracy_cart[3,4] <- ((cart_both_train_cf[["overall"]][1])) #error
trainset_accuracy_cart[3,5] <- auc@y.values
trainset_accuracy_cart[3,6] <- paste(round(100*trainset_accuracy_cart[3,3] + costmultiplier * 100*trainset_accuracy_cart[3,2],3), '*Amount')
trainset_accuracy_cart[3,7] <- (cart_both_train_cf[["table"]][4] / (cart_both_train_cf[["table"]][4] + (0.5 * (cart_both_train_cf[["table"]][3] + cart_both_train_cf[["table"]][2])))) #f1 score
trainset_accuracy_cart[3,8] <- (cart_both_train_cf[["table"]][4] / (cart_both_train_cf[["table"]][4] + 0.2 * cart_both_train_cf[["table"]][3] + 0.8 * cart_both_train_cf[["table"]][2])) #f2 score


#testing the model on unseen data and getting the confusion matrix
cart2_testset_predicted <- predict(cartpruned_accident_both, type = "class", newdata = accident_testset)
cart_both_test_cf <- confusionMatrix(data = table(accident_testset$accident_severity, 
                                                  cart2_testset_predicted, deparse.level = 2))

#getting the area under curve
cart.pred.test = predict(cartpruned_accident_both, type = "prob",newdata = accident_testset)[,2]
pred = prediction(cart.pred.test, accident_testset$accident_severity)
auc = performance(pred, measure = "auc")

#storing the metrics into a table
testset_accuracy_cart[3,2] <- (cart_both_test_cf[["table"]][3] / (cart_both_test_cf[["table"]][3] + cart_both_test_cf[["table"]][1])) #fpr
testset_accuracy_cart[3,3] <- (cart_both_test_cf[["table"]][2] / (cart_both_test_cf[["table"]][2] + cart_both_test_cf[["table"]][4])) #fnr
testset_accuracy_cart[3,4] <- ((cart_both_test_cf[["overall"]][1])) #error
testset_accuracy_cart[3,5] <- auc@y.values
testset_accuracy_cart[3,6] <- paste(round(100*testset_accuracy_cart[3,3] + costmultiplier * 100*testset_accuracy_cart[3,2],3), '*Amount')
testset_accuracy_cart[3,7] <- (cart_both_test_cf[["table"]][4] / (cart_both_test_cf[["table"]][4] + (0.5 * (cart_both_test_cf[["table"]][3] + cart_both_test_cf[["table"]][2])))) #f1 score
testset_accuracy_cart[3,8] <- (cart_both_test_cf[["table"]][4] / (cart_both_test_cf[["table"]][4] + 0.2 * cart_both_test_cf[["table"]][3] + 0.8 * cart_both_test_cf[["table"]][2])) #f2 score


#===============================================================================

#Modelling with Random Forest

#===============================================================================

#generate dataframes to store random forest results

#generate empty data frame to store test set results for balanced data
testset_accuracy_rf <- data.frame('Model' = c("Random Forest with Oversampling",
                                                   "Random Forest with Undersampling",
                                                   "Random Forest with Both"), 
                                       'FPR' = rep(0, 3), 'FNR' = rep(0, 3),
                                       'Acc' = rep(0, 3), 'AUC' = rep(0, 3), 
                                       'Cost' = rep(0, 3), 'F1 Score' = rep(0,3),
                                       'F2 Score' = rep(0,3))

#generate empty data frame to store train set results
trainset_accuracy_rf <- data.frame('Model' = c("Random Forest with Oversampling",
                                                   "Random Forest with Undersampling",
                                                   "Random Forest with Both"), 
                                       'FPR' = rep(0, 3), 'FNR' = rep(0, 3),
                                       'Acc' = rep(0, 3), 'AUC' = rep(0, 3), 
                                       'Cost' = rep(0, 3), 'F1 Score' = rep(0,3),
                                       'F2 Score' = rep(0,3))

#====================== random forest with oversampling ========================

set.seed(2407)  #for reproducibility of Bootstrap sampling & RSF selection.

rf_accident_over <- randomForest(accident_severity ~ ., 
                                 data = accident_trainset_over,
                                 na.action = na.omit, mtry = 9,
                                 importance = T, do.trace = TRUE)

rf_accident_over 

plot(rf_accident_over)

var.impt.over <- importance(rf_accident_over)

varImpPlot(rf_accident_over, type = 1)

#Computing train set predictions and plotting of confusion matrix
trainset_prob <- predict(rf_accident_over, type='prob')
trainset_prob <- as.data.frame(trainset_prob)
trainset_pred <- ifelse(test = trainset_prob$Serious > threshold, "Serious", "Not Serious")
rf_over_train_cf <- confusionMatrix(data = table(accident_trainset_over$accident_severity, trainset_pred, deparse.level = 2))

#getting the area under curve
pred = prediction(trainset_prob[,2], accident_trainset_over$accident_severity)
auc = performance(pred, measure = "auc")

#storing the metrics into a table
trainset_accuracy_rf[1,2] <- (rf_over_train_cf[["table"]][3] / (rf_over_train_cf[["table"]][3] + rf_over_train_cf[["table"]][1])) #fpr
trainset_accuracy_rf[1,3] <- (rf_over_train_cf[["table"]][2] / (rf_over_train_cf[["table"]][2] + rf_over_train_cf[["table"]][4])) #fnr
trainset_accuracy_rf[1,4] <- ((rf_over_train_cf[["overall"]][1])) #error
trainset_accuracy_rf[1,5] <- auc@y.values
trainset_accuracy_rf[1,6] <- paste(round(100*trainset_accuracy_rf[1,3] + costmultiplier * 100*trainset_accuracy_rf[1,2],3), '*Amount')
trainset_accuracy_rf[1,7] <- (rf_over_train_cf[["table"]][4] / (rf_over_train_cf[["table"]][4] + (0.5 * (rf_over_train_cf[["table"]][3] + rf_over_train_cf[["table"]][2])))) #f1 score
trainset_accuracy_rf[1,8] <- (rf_over_train_cf[["table"]][4] / (rf_over_train_cf[["table"]][4] + 0.2 * rf_over_train_cf[["table"]][3] + 0.8 * rf_over_train_cf[["table"]][2])) #f2 score


#Testing our model on an unseen test set and plotting the confusion matrix
testset_prob <- predict(rf_accident_over, type='prob', newdata = accident_testset)
testset_prob <- as.data.frame(testset_prob)
testset_pred <- ifelse(test = testset_prob$Serious > threshold, "Serious", "Not Serious")
rf_over_test_cf <- confusionMatrix(data = table(accident_testset$accident_severity, testset_pred,
                             deparse.level = 2))

#getting the area under curve
pred = prediction(testset_prob[,2], accident_testset$accident_severity)
auc = performance(pred, measure = "auc")

#storing the metrics into a table
testset_accuracy_rf[1,2] <- (rf_over_test_cf[["table"]][3] / (rf_over_test_cf[["table"]][3] + rf_over_test_cf[["table"]][1])) #fpr
testset_accuracy_rf[1,3] <- (rf_over_test_cf[["table"]][2] / (rf_over_test_cf[["table"]][2] + rf_over_test_cf[["table"]][4])) #fnr
testset_accuracy_rf[1,4] <- ((rf_over_test_cf[["overall"]][1])) #error
testset_accuracy_rf[1,5] <- auc@y.values
testset_accuracy_rf[1,6] <- paste(round(100*testset_accuracy_rf[1,3] + costmultiplier * 100*testset_accuracy_rf[1,2],3), '*Amount')
testset_accuracy_rf[1,7] <- (rf_over_test_cf[["table"]][4] / (rf_over_test_cf[["table"]][4] + (0.5 * (rf_over_test_cf[["table"]][3] + rf_over_test_cf[["table"]][2])))) #f1 score
testset_accuracy_rf[1,8] <- (rf_over_test_cf[["table"]][4] / (rf_over_test_cf[["table"]][4] + 0.2 * rf_over_test_cf[["table"]][3] + 0.8 * rf_over_test_cf[["table"]][2])) #f2 score


#==================== random forest with under sampling ========================

set.seed(2407)  #for reproducibility of Bootstrap sampling & RSF selection.

rf_accident_under <- randomForest(accident_severity ~ .,
                                  data = accident_trainset_under, 
                                  na.action = na.omit, mtry = 9,
                                  importance = T, do.trace = TRUE)

rf_accident_under  

plot(rf_accident_under)

var.impt.under <- importance(rf_accident_under)

varImpPlot(rf_accident_under, type = 1)

#Computing train set predictions and plotting of confusion matrix
trainset_prob <- predict(rf_accident_under, type='prob')
trainset_prob <- as.data.frame(trainset_prob)
trainset_pred <- ifelse(test = trainset_prob$Serious > threshold, "Serious", "Not Serious")
rf_under_train_cf <- confusionMatrix(data = table(accident_trainset_under$accident_severity, 
                                            trainset_pred, deparse.level = 2))

#getting the area under curve
pred = prediction(trainset_prob[,2], accident_trainset_under$accident_severity)
auc = performance(pred, measure = "auc")

#storing the metrics into a table
trainset_accuracy_rf[2,2] <- (rf_under_train_cf[["table"]][3] / (rf_under_train_cf[["table"]][3] + rf_under_train_cf[["table"]][1])) #fpr
trainset_accuracy_rf[2,3] <- (rf_under_train_cf[["table"]][2] / (rf_under_train_cf[["table"]][2] + rf_under_train_cf[["table"]][4])) #fnr
trainset_accuracy_rf[2,4] <- ((rf_under_train_cf[["overall"]][1])) #error
trainset_accuracy_rf[2,5] <- auc@y.values
trainset_accuracy_rf[2,6] <- paste(round(100*trainset_accuracy_rf[2,3] + costmultiplier * 100*trainset_accuracy_rf[2,2],3), '*Amount')
trainset_accuracy_rf[2,7] <- (rf_under_train_cf[["table"]][4] / (rf_under_train_cf[["table"]][4] + (0.5 * (rf_under_train_cf[["table"]][3] + rf_under_train_cf[["table"]][2])))) #f1 score
trainset_accuracy_rf[2,8] <- (rf_under_train_cf[["table"]][4] / (rf_under_train_cf[["table"]][4] + 0.2 * rf_under_train_cf[["table"]][3] + 0.8 * rf_under_train_cf[["table"]][2])) #f2 score


#Testing our model on an unseen test set and plotting the confusion matrix
testset_prob <- predict(rf_accident_under, type='prob', newdata = accident_testset)
testset_prob <- as.data.frame(testset_prob)
testset_pred <- ifelse(test = testset_prob$Serious > threshold, "Serious", "Not Serious")
rf_under_test_cf <- confusionMatrix(data = table(accident_testset$accident_severity, testset_pred,
                                    deparse.level = 2))

#getting the area under curve
pred = prediction(testset_prob[,2], accident_testset$accident_severity)
auc = performance(pred, measure = "auc")

#storing the metrics into a table
testset_accuracy_rf[2,2] <- (rf_under_test_cf[["table"]][3] / (rf_under_test_cf[["table"]][3] + rf_under_test_cf[["table"]][1])) #fpr
testset_accuracy_rf[2,3] <- (rf_under_test_cf[["table"]][2] / (rf_under_test_cf[["table"]][2] + rf_under_test_cf[["table"]][4])) #fnr
testset_accuracy_rf[2,4] <- ((rf_under_test_cf[["overall"]][1])) #error
testset_accuracy_rf[2,5] <- auc@y.values
testset_accuracy_rf[2,6] <- paste(round(100*testset_accuracy_rf[2,3] + costmultiplier * 100*testset_accuracy_rf[2,2],3), '*Amount')
testset_accuracy_rf[2,7] <- (rf_under_test_cf[["table"]][4] / (rf_under_test_cf[["table"]][4] + (0.5 * (rf_under_test_cf[["table"]][3] + rf_under_test_cf[["table"]][2])))) #f1 score
testset_accuracy_rf[2,8] <- (rf_under_test_cf[["table"]][4] / (rf_under_test_cf[["table"]][4] + 0.2 * rf_under_test_cf[["table"]][3] + 0.8 * rf_under_test_cf[["table"]][2])) #f2 score


#========================= Random Forest with both =============================

#set seed to ensure reproducibility of results
set.seed(2407)  # for Bootstrap sampling & RSF selection.

rf_accident_both <- randomForest(accident_severity ~ . , data = accident_trainset_both, 
                         mtry = 9, importance = T, do.trace = TRUE)

rf_accident_both  ## shows defaults are B = 500, RSF size = int(sqrt(m)) = 6


plot(rf_accident_both)

var.impt.both <- importance(rf_accident_both)

varImpPlot(rf_accident_both, type = 1)

#Computing train set predictions and plotting of confusion matrix
trainset_prob <- predict(rf_accident_both, type='prob')
trainset_prob <- as.data.frame(trainset_prob)
trainset_pred <- ifelse(test = trainset_prob$Serious > threshold, "Serious", "Not Serious")
rf_both_train_cf <-confusionMatrix(data = table(accident_trainset_both$accident_severity, 
                                                trainset_pred, deparse.level = 2))

#getting the area under curve
pred = prediction(trainset_prob[,2], accident_trainset_both$accident_severity)
auc = performance(pred, measure = "auc")

#storing the metrics into a table
trainset_accuracy_rf[3,2] <- (rf_both_train_cf[["table"]][3] / (rf_both_train_cf[["table"]][3] + rf_both_train_cf[["table"]][1])) #fpr
trainset_accuracy_rf[3,3] <- (rf_both_train_cf[["table"]][2] / (rf_both_train_cf[["table"]][2] + rf_both_train_cf[["table"]][4])) #fnr
trainset_accuracy_rf[3,4] <- ((rf_both_train_cf[["overall"]][1])) #error
trainset_accuracy_rf[3,5] <- auc@y.values
trainset_accuracy_rf[3,6] <- paste(round(100*trainset_accuracy_rf[3,3] + costmultiplier * 100*trainset_accuracy_rf[3,2],3), '*Amount')
trainset_accuracy_rf[3,7] <- (rf_both_train_cf[["table"]][4] / (rf_both_train_cf[["table"]][4] + (0.5 * (rf_both_train_cf[["table"]][3] + rf_both_train_cf[["table"]][2])))) #f1 score
trainset_accuracy_rf[3,8] <- (rf_both_train_cf[["table"]][4] / (rf_both_train_cf[["table"]][4] + 0.2 * rf_both_train_cf[["table"]][3] + 0.8 * rf_both_train_cf[["table"]][2])) #f2 score


#Testing our model on an unseen test set and plotting the confusion matrix
testset_prob <- predict(rf_accident_both, type='prob', newdata = accident_testset)
testset_prob <- as.data.frame(testset_prob)
testset_pred <- ifelse(test = testset_prob$Serious > threshold, "Serious", "Not Serious")
rf_both_test_cf <- confusionMatrix(data = table(accident_testset$accident_severity, testset_pred,
                             deparse.level = 2))

#getting the area under curve
pred = prediction(testset_prob[,2], accident_testset$accident_severity)
auc = performance(pred, measure = "auc")

#storing the metrics into a table
testset_accuracy_rf[3,2] <- (rf_both_test_cf[["table"]][3] / (rf_both_test_cf[["table"]][3] + rf_both_test_cf[["table"]][1])) #fpr
testset_accuracy_rf[3,3] <- (rf_both_test_cf[["table"]][2] / (rf_both_test_cf[["table"]][2] + rf_both_test_cf[["table"]][4])) #fnr
testset_accuracy_rf[3,4] <- ((rf_both_test_cf[["overall"]][1])) #error
testset_accuracy_rf[3,5] <- auc@y.values
testset_accuracy_rf[3,6] <- paste(round(100*testset_accuracy_rf[3,3] + costmultiplier * 100*testset_accuracy_rf[3,2],3), '*Amount')
testset_accuracy_rf[3,7] <- (rf_both_test_cf[["table"]][4] / (rf_both_test_cf[["table"]][4] + (0.5 * (rf_both_test_cf[["table"]][3] + rf_both_test_cf[["table"]][2])))) #f1 score
testset_accuracy_rf[3,8] <- (rf_both_test_cf[["table"]][4] / (rf_both_test_cf[["table"]][4] + 0.2 * rf_both_test_cf[["table"]][3] + 0.8 * rf_both_test_cf[["table"]][2])) #f2 score

#===============================================================================

#Plot partial dependence plot for RF using under sampling

#===============================================================================

#extracting variables names for top 20 RF variables
impvar_under <- rownames(var.impt.under)[order(var.impt.under[,3], decreasing=TRUE)][1:20]

#plotting the top 20 variables partial dependence plots
par(mfrow=c(2, 2))
for (i in 1:4) {
  partialPlot(rf_accident_under, accident_trainset_under, impvar_under[i], xlab = "",
              main=paste("Partial Dependence on", impvar_under[i]), which.class = "Serious", las = 3)
}

par(mfrow=c(2, 2))
for (i in 5:8) {
  partialPlot(rf_accident_under, accident_trainset_under, impvar_under[i], xlab = "",
              main=paste("Partial Dependence on", impvar_under[i]), which.class = "Serious", las = 3)
}

par(mfrow=c(1, 2))
for (i in 9:10) {
  partialPlot(rf_accident_under, accident_trainset_under, impvar_under[i], xlab = "",
              main=paste("Partial Dependence on", impvar_under[i]), which.class = "Serious", las = 3)
}

par(mfrow=c(1, 2))
for (i in 11:12) {
  partialPlot(rf_accident_under, accident_trainset_under, impvar_under[i], xlab = "",
              main=paste("Partial Dependence on", impvar_under[i]), which.class = "Serious", las = 3)
}


par(mfrow=c(2, 2))
for (i in 13:16) {
  partialPlot(rf_accident_under, accident_trainset_under, impvar_under[i], xlab = "",
              main=paste("Partial Dependence on", impvar_under[i]), which.class = "Serious", las = 3)
}


par(mfrow=c(2, 2))
for (i in 17:20) {
  partialPlot(rf_accident_under, accident_trainset_under, impvar_under[i], xlab = "",
              main=paste("Partial Dependence on", impvar_under[i]), which.class = "Serious", las = 3)
}

#final plots for report
plotid = c(2,7,8,17,19,20)
par(mfrow=c(2, 3))
for (i in plotid) {
  partialPlot(rf_accident_under, accident_trainset_under, impvar_under[i], xlab = "",
              main=paste("Partial Dependence on", impvar_under[i]), which.class = "Serious", las = 3)
}

#reset plot settings
par(mfrow=c(1,1))

#=================== Summarizing the results ===================================

#summarize train set results
trainset_accuracy_lr
trainset_accuracy_mars
trainset_accuracy_cart
trainset_accuracy_rf

#summarize test set results
testset_accuracy_lr
testset_accuracy_mars
testset_accuracy_cart
testset_accuracy_rf