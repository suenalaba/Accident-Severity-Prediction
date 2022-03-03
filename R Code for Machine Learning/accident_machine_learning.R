#Automation to install packages that are necessary but not yet installed
list.of.packages <- c("car", "pROC", "ROSE", "caTools", "caret", "earth",
                      "rpart","rpart.plot", "randomForest", "data.table",
                      "cattonum", "plyr", "h2o", "PRROC")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#import relevant libraries here...
library(car)
library(pROC)
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
library(h2o)
library(PRROC)

#set your working directory here....
setwd("C:/Users/joshua/Downloads/BC2407 Analytics II/Datasets Cleaned")

accident.dt <- fread("accident_data_cleaned.csv") #106121 obs 29 var
vehicle.dt <- fread("vehicle_data_cleaned.csv") #56850 obs 26 var
casualty.dt <- fread("casualty_data_cleaned.csv") #118732 14 var

# merging the 3 data tables based on accident index.
accident_casualty.dt <- merge(accident.dt, casualty.dt) #118732 obs 40 variables
accidents_combined.dt <- merge(accident_casualty.dt,vehicle.dt) #73,337 obs 62 variables

#subset relevant columns based on data exploration

#73,337 obs 33 variables
accidentml.dt <- accidents_combined.dt[,c("vehicle_manoeuvre", "hit_object_off_carriageway",
                                          "skidding_and_overturning","hit_object_in_carriageway", 
                                          "first_point_of_impact", "junction_location",
                                          "journey_purpose","sex_of_driver","propulsion_code",
                                          "vehicle_type","casualty_agegroup","casualty_type", 
                                          "casualty_class","casualty_sex",
                                          "accident_severity", "road_type", "speed_limit",
                                          "junction_detail", "junction_control","urban_or_rural_area", 
                                          "light_conditions", "timeofday","longitude","latitude",
                                          "local_authority_district", "special_conditions_at_site",
                                          "pedestrian_crossing_physical_facilities", "month",
                                          "pedestrian_crossing_human_control",  "weather_conditions",
                                          "carriageway_hazards","day", "road_surface_conditions",
                                          "pedestrian_movement", "pedestrian_location")]

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
                    "pedestrian_movement", "pedestrian_location")

#convert data to categorical accordingly
accidentml.dt[, (categoricalCols) := lapply(.SD, as.factor), .SDcols = categoricalCols]

str(accidentml.dt)

#========================== Train-test split ===================================

set.seed(2407) #to ensure reproducibility of result

#70-30 train test split to have a constant evaluation point across models
traintest_split <- sample.split(Y = accidentml.dt$accident_severity, SplitRatio = 0.7)
accident_trainset <- subset(x = accidentml.dt, subset = traintest_split == TRUE)
accident_testset <- subset(x = accidentml.dt, subset = traintest_split == FALSE)

#===============================================================================

#Frequency encoding on trainset

#Goal: To prevent data leakage
#Frequency encoding is done ONLY on trainset. 
#Any frequency distribution used to predict the testset will be the distribution
#used in the trainset.
#That is, ANY frequency distribution used to predict the testset, is 
#independent of testset distribution

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

#conver to numeric for matching
districtFreqEncoding$local_authority_district = as.numeric(districtFreqEncoding$local_authority_district)
accident_testset$local_authority_district = as.numeric(accident_testset$local_authority_district)

#joining the frequencies based on frequency encoding of TRAINSET
accident_testset = join(x=accident_testset,y=districtFreqEncoding, by="local_authority_district",
                        type = "left", match = "first")

#remove local authority district as its no longer of use.
accident_trainset[,c("local_authority_district"):=NULL]
accident_testset[,c("local_authority_district"):=NULL]

#generate oversampled, undersampled, both and ROSE traindata

#rose generates synthetic balanced data through oversampling of minority class
accident_trainset_rose <- ROSE(accident_severity ~ ., 
                               data = accident_trainset, seed = 2407)$data
#standard oversampling
accident_trainset_over <- ovun.sample(accident_severity ~ ., 
                                      data = accident_trainset, method = "over",p = 0.5, seed = 2407)$data
#standard undersampling
accident_trainset_under <- ovun.sample(accident_severity ~ ., 
                                       data = accident_trainset, method = "under",p = 0.5, seed = 2407)$data
#hybrid of oversampling and undersampling
accident_trainset_both <- ovun.sample(accident_severity ~ ., 
                                      data = accident_trainset, method = "both",p = 0.5, seed = 2407)$data

#===============================================================================

#Modelling with Logistic Regression
#TODO:
#how to fix confidence interval
#how to interpret importance of variables?
#backward elimination?

#===============================================================================

#====================== Logistic Regression with weights =======================

weights <- ifelse(accident_trainset$accident_severity == "Serious", 5, 1)

glmaccident <- glm(formula = accident_severity ~ ., family = binomial, weights = weights,data = accident_trainset)

summary(glmaccident)

options(scipen = 1)
glmaccident.OR <- exp(coef(glmaccident))
glmaccident.OR

# Use OR CI to conclude on statistical significance of X variables.
#glmaccident.OR.CI <- exp(confint(glmaccident))  #
#glmaccident.OR.CI
#If confidence interval includes 1, it means the variable is not statistically significant.
#  not using default takes way too long to load, why? anyway.. is there a way to calculate this manually?...

vif(mod = glmaccident) #if GVIF > 5, multicollinearity exists, in our case it doesnt exist.

threshold = 0.5
# Calculate the train set probabilities, and use it to predict the data
# Plot the confusion matrix as well, along with the accuracy and other coefficients
trainset_prob <- predict(object = glmaccident, type = "response", newdata = accident_trainset)
trainset_pred <- ifelse(test = trainset_prob > threshold, "Serious", "Not Serious")
confusionMatrix(data = table(accident_trainset$accident_severity, trainset_pred, deparse.level = 2), mode = "everything")

# Calculate the test set probabilities, and use it to predict the data
# Plot the confusion matrix as well, along with the accuracy and other coefficients
testset_prob <- predict(object = glmaccident, type = "response", newdata = accident_testset)
testset_pred <- ifelse(test = testset_prob > threshold, "Serious", "Not Serious")
confusionMatrix(data = table(accident_testset$accident_severity, testset_pred, deparse.level = 2), mode = "everything")

#Trainset accuracy with class weight logreg 52.0%, testset: 52.2%
auc(accident_testset$accident_severity, testset_prob) #ROC is 64% not very good.
#Overall, not very good to use class weight.

#roc.curve(accident_testset$accident_severity, testset_prob, plotit = F)

#========================= Logistic Regression with ROSE =======================

glmaccident_rose <- glm(formula = accident_severity ~ .-pedestrian_location 
                        -pedestrian_movement -junction_detail -casualty_class, 
                        family = binomial, data = accident_trainset_rose)

summary(glmaccident_rose)

options(scipen = 1)
glmaccident_rose.OR <- exp(coef(glmaccident_rose))
glmaccident_rose.OR

# Use OR CI to conclude on statistical significance of X variables.
#glmaccident_rose.OR.CI <- exp(confint(glmaccident_rose))  #
#glmaccident_rose.OR.CI
#If confidence interval includes 1, it means the variable is not statistically significant.
#  not using default takes way too long to load, why? anyway.. is there a way to calculate this manually?...

vif(mod = glmaccident_rose) #if GVIF > 5, multicollinearity exists, in our case it doesnt exist.

threshold = 0.5
# Calculate the train set probabilities, and use it to predict the data
# Plot the confusion matrix as well, along with the accuracy and other coefficients
trainset_prob <- predict(object = glmaccident_rose, type = "response", newdata = accident_trainset_rose)
trainset_pred <- ifelse(test = trainset_prob > threshold, "Serious", "Not Serious")
confusionMatrix(data = table(accident_trainset_rose$accident_severity, trainset_pred, deparse.level = 2), mode = "everything")

# Calculate the test set probabilities, and use it to predict the data
# Plot the confusion matrix as well, along with the accuracy and other coefficients
testset_prob <- predict(object = glmaccident_rose, type = "response", newdata = accident_testset)
testset_pred <- ifelse(test = testset_prob > threshold, "Serious", "Not Serious")
confusionMatrix(data = table(accident_testset$accident_severity, testset_pred, deparse.level = 2), mode = "everything")

#Trainset accuracy 56.8%, testset: 56.3%
auc(accident_testset$accident_severity, testset_prob) #ROC is 58.7% not very good.
#Overall, it seems that ROSE does help improve accuracy but still not too good.

#========================= Logistic Regression with over sampling ==============

glmaccident_over <- glm(formula = accident_severity ~ . -pedestrian_location 
                        -pedestrian_movement -junction_detail -casualty_class, 
                        family = binomial, data = accident_trainset_over)

summary(glmaccident_over)

options(scipen = 1)
glmaccident_over.OR <- exp(coef(glmaccident_over))
glmaccident_over.OR

# Use OR CI to conclude on statistical significance of X variables.
#glmaccident_over.OR.CI <- exp(confint(glmaccident_over))  #
#glmaccident_over.OR.CI
#If confidence interval includes 1, it means the variable is not statistically significant.
#  not using default takes way too long to load, why? anyway.. is there a way to calculate this manually?...

vif(mod = glmaccident_over) #if GVIF > 5, multicollinearity exists, in our case it doesnt exist.

threshold = 0.5
# Calculate the train set probabilities, and use it to predict the data
# Plot the confusion matrix as well, along with the accuracy and other coefficients
trainset_prob <- predict(object = glmaccident_over, type = "response", newdata = accident_trainset_over)
trainset_pred <- ifelse(test = trainset_prob > threshold, "Serious", "Not Serious")
confusionMatrix(data = table(accident_trainset_over$accident_severity, trainset_pred, deparse.level = 2), mode = "everything")

# Calculate the test set probabilities, and use it to predict the data
# Plot the confusion matrix as well, along with the accuracy and other coefficients
testset_prob <- predict(object = glmaccident_over, type = "response", newdata = accident_testset)
testset_pred <- ifelse(test = testset_prob > threshold, "Serious", "Not Serious")
confusionMatrix(data = table(accident_testset$accident_severity, testset_pred, deparse.level = 2), mode = "everything")

#Trainset accuracy 56.3%, testset: 56.8%
auc(accident_testset$accident_severity, testset_prob) #ROC is 58.8% not very good.
#Overall, it seems that ROSE does help improve accuracy but still not too good.

#roc.curve(accident_testset$accident_severity, testset_prob, plotit = F)

#=================== Logistic Regression with under sampling ===================

glmaccident_under <- glm(formula = accident_severity ~ . -pedestrian_location 
                         -pedestrian_movement -junction_detail -casualty_class , 
                         family = binomial, data = accident_trainset_under)

summary(glmaccident_under)

options(scipen = 1)
glmaccident_under.OR <- exp(coef(glmaccident_under))
glmaccident_under.OR

# Use OR CI to conclude on statistical significance of X variables.
#glmaccident_under.OR.CI <- exp(confint(glmaccident_under))  #
#glmaccident_under.OR.CI
#If confidence interval includes 1, it means the variable is not statistically significant.
#  not using default takes way too long to load, why? anyway.. is there a way to calculate this manually?...

vif(mod = glmaccident_under) #if GVIF > 5, multicollinearity exists, in our case it doesnt exist.
cramerV(accidentml.dt$pedestrian_location,accidentml.dt$pedestrian_movement)
cramerV(accidentml.dt$casualty_class,accidentml.dt$pedestrian_movement)
threshold = 0.5
# Calculate the train set probabilities, and use it to predict the data
# Plot the confusion matrix as well, along with the accuracy and other coefficients
trainset_prob <- predict(object = glmaccident_under, type = "response", newdata = accident_trainset_under)
trainset_pred <- ifelse(test = trainset_prob > threshold, "Serious", "Not Serious")
confusionMatrix(data = table(accident_trainset_under$accident_severity, trainset_pred, deparse.level = 2), mode = "everything")

# Calculate the test set probabilities, and use it to predict the data
# Plot the confusion matrix as well, along with the accuracy and other coefficients
testset_prob <- predict(object = glmaccident_under, type = "response", newdata = accident_testset)
testset_pred <- ifelse(test = testset_prob > threshold, "Serious", "Not Serious")
confusionMatrix(data = table(accident_testset$accident_severity, testset_pred, deparse.level = 2), mode = "everything")

#Trainset accuracy 56.3%, testset: 57.4%
auc(accident_testset$accident_severity, testset_prob) #ROC is 58.6% not very good.
#Overall, it seems that under does help improve accuracy but still not too good.

#=================== Logistic Regression with both==============================

glmaccident_both <- glm(formula = accident_severity ~ . -pedestrian_location 
                        -pedestrian_movement -junction_detail -casualty_class, 
                        family = binomial, data = accident_trainset_both)

summary(glmaccident_both)

options(scipen = 1)
glmaccident_both.OR <- exp(coef(glmaccident_both))
glmaccident_both.OR

# Use OR CI to conclude on statistical significance of X variables.
#glmaccident_both.OR.CI <- exp(confint(glmaccident_both))  #
#glmaccident_both.OR.CI
#If confidence interval includes 1, it means the variable is not statistically significant.
#  not using default takes way too long to load, why? anyway.. is there a way to calculate this manually?...

vif(mod = glmaccident_both) #if GVIF > 5, multicollinearity exists, in our case it doesnt exist.

threshold = 0.5
# Calculate the train set probabilities, and use it to predict the data
# Plot the confusion matrix as well, along with the accuracy and other coefficients
trainset_prob <- predict(object = glmaccident_both, type = "response", newdata = accident_trainset_both)
trainset_pred <- ifelse(test = trainset_prob > threshold, "Serious", "Not Serious")
confusionMatrix(data = table(accident_trainset_both$accident_severity, trainset_pred, deparse.level = 2), mode = "everything")

# Calculate the test set probabilities, and use it to predict the data
# Plot the confusion matrix as well, along with the accuracy and other coefficients
testset_prob <- predict(object = glmaccident_both, type = "response", newdata = accident_testset)
testset_pred <- ifelse(test = testset_prob > threshold, "Serious", "Not Serious")
confusionMatrix(data = table(accident_testset$accident_severity, testset_pred, deparse.level = 2), mode = "everything")

#Trainset accuracy 65.9%, testset: 64.2%
auc(accident_testset$accident_severity, testset_prob) #ROC is 64.1% not very good.
#Overall, it seems that under does help improve accuracy but still not too good.

#===============================================================================

#Modelling with Multi-variate Adaptive Regression Splines (MARS)
#TODO: 
#is hyperparameter tuning required?
#do we remove some variables?

#===============================================================================

#============================= MARS with ROSE ==================================

mars_accident_rose <- earth(formula = accident_severity ~ ., degree = 2, data = accident_trainset_rose, 
                            glm = list(family = binomial))

summary(mars_accident_rose)

evimp(mars_accident_rose)

# Calculate the train set probabilities, and use it to predict the data
# Plot the confusion matrix as well, along with the accuracy and other coefficients
trainset_prob <- predict(object = mars_accident_rose, type = "response", newdata = accident_trainset_rose)
trainset_pred <- ifelse(test = trainset_prob > threshold, "Serious", "Not Serious")
confusionMatrix(data = table(accident_trainset_rose$accident_severity, trainset_pred, deparse.level = 2), mode = "everything")

# Calculate the test set probabilities, and use it to predict the data
# Plot the confusion matrix as well, along with the accuracy and other coefficients
testset_prob <- predict(object = mars_accident_rose, type = "response", newdata = accident_testset)
testset_pred <- ifelse(test = testset_prob > threshold, "Serious", "Not Serious")
confusionMatrix(data = table(accident_testset$accident_severity, testset_pred, deparse.level = 2), mode = "everything")

#Trainset accuracy 54.9%, testset: 45.4%
auc(accident_testset$accident_severity, as.vector(testset_prob)) #ROC is 57.6% not very good.
#Overall, it seems that under does help improve accuracy but still not too good.

#========================= MARS with oversampling ==============================

mars_accident_over <- earth(formula = accident_severity ~ ., degree = 2, data = accident_trainset_over, 
                            glm = list(family = binomial))

summary(mars_accident_over)

evimp(mars_accident_over)

# Calculate the train set probabilities, and use it to predict the data
# Plot the confusion matrix as well, along with the accuracy and other coefficients
trainset_prob <- predict(object = mars_accident_over, type = "response", newdata = accident_trainset_over)
trainset_pred <- ifelse(test = trainset_prob > threshold, "Serious", "Not Serious")
confusionMatrix(data = table(accident_trainset_over$accident_severity, trainset_pred, deparse.level = 2), mode = "everything")

# Calculate the test set probabilities, and use it to predict the data
# Plot the confusion matrix as well, along with the accuracy and other coefficients
testset_prob <- predict(object = mars_accident_over, type = "response", newdata = accident_testset)
testset_pred <- ifelse(test = testset_prob > threshold, "Serious", "Not Serious")
confusionMatrix(data = table(accident_testset$accident_severity, testset_pred, deparse.level = 2), mode = "everything")

#Trainset accuracy 54.4%, testset: 44.2%
auc(accident_testset$accident_severity, as.vector(testset_prob)) #ROC is 57.7% not very good.
#Overall, it seems that under does help improve accuracy but still not too good.

#=========================== MARS with under sampling ==========================

mars_accident_under <- earth(formula = accident_severity ~ ., degree = 2, data = accident_trainset_under, 
                             glm = list(family = binomial))

summary(mars_accident_under)

evimp(mars_accident_under)

# Calculate the train set probabilities, and use it to predict the data
# Plot the confusion matrix as well, along with the accuracy and other coefficients
trainset_prob <- predict(object = mars_accident_under, type = "response", newdata = accident_trainset_under)
trainset_pred <- ifelse(test = trainset_prob > threshold, "Serious", "Not Serious")
confusionMatrix(data = table(accident_trainset_under$accident_severity, trainset_pred, deparse.level = 2), mode = "everything")

# Calculate the test set probabilities, and use it to predict the data
# Plot the confusion matrix as well, along with the accuracy and other coefficients
testset_prob <- predict(object = mars_accident_under, type = "response", newdata = accident_testset)
testset_pred <- ifelse(test = testset_prob > threshold, "Serious", "Not Serious")
confusionMatrix(data = table(accident_testset$accident_severity, testset_pred, deparse.level = 2), mode = "everything")

#Trainset accuracy 65.79%, testset: 65.21%
auc(accident_testset$accident_severity, as.vector(testset_prob)) #ROC is 57.7% not very good.
#Overall, it seems that under does help improve accuracy but still not too good.

#============================ MARS with both ===================================

mars_accident_both <- earth(formula = accident_severity ~ ., degree = 2, data = accident_trainset_both, 
                            glm = list(family = binomial))

summary(mars_accident_both)

evimp(mars_accident_both)

# Calculate the train set probabilities, and use it to predict the data
# Plot the confusion matrix as well, along with the accuracy and other coefficients
trainset_prob <- predict(object = mars_accident_both, type = "response", newdata = accident_trainset_both)
trainset_pred <- ifelse(test = trainset_prob > threshold, "Serious", "Not Serious")
confusionMatrix(data = table(accident_trainset_both$accident_severity, trainset_pred, deparse.level = 2), mode = "everything")

# Calculate the test set probabilities, and use it to predict the data
# Plot the confusion matrix as well, along with the accuracy and other coefficients
testset_prob <- predict(object = mars_accident_both, type = "response", newdata = accident_testset)
testset_pred <- ifelse(test = testset_prob > threshold, "Serious", "Not Serious")
confusionMatrix(data = table(accident_testset$accident_severity, testset_pred, deparse.level = 2), mode = "everything")

#Trainset accuracy 54.9%, testset: 45.4%
auc(accident_testset$accident_severity, as.vector(testset_prob)) #ROC is 57.6% not very good.
#Overall, it seems that under does help improve accuracy but still not too good.

#=========================== end of mars =======================================

#===============================================================================

#Modelling with CART

#===============================================================================

#================================== CART with ROSE =============================

#rpart uses random sampling, therefore, set seed to ensure reproducibility
set.seed(2407)
# Use CART to create a predictive model
cart_accident_rose <- rpart(accident_severity~., method = "class", 
                            data = accident_trainset_rose, control = rpart.control(minsplit = 20, cp = 0))

par(mfrow = c(1,1))

#takes too long to plot, anyway doesnt provide meaning cause the tree will just be too huge...
#rpart.plot(cart_accident_rose, nn=T, tweak = 0.8, main = "Maximal Tree for Accident Severity")

print(cart_accident_rose)

printcp(cart_accident_rose)

plotcp(cart_accident_rose)


#Extract the Optimal Tree , Source: Neumann Chew 
# Compute min CVerror + 1SE in maximal tree cart1.
CVerror.cap <- cart_accident_rose$cptable[which.min(cart_accident_rose$cptable[,"xerror"]), "xerror"] + cart_accident_rose$cptable[which.min(cart_accident_rose$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart_accident_rose.
i <- 1; j<- 4
while (cart_accident_rose$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cart_accident_rose$cptable[i,1] * cart_accident_rose$cptable[i-1,1]), 1) #calculate geometric mean here

# Prune the max tree using the optimal cp = cp.opt
cartpruned_accident_rose <- prune(cart_accident_rose, cp = cp.opt)

rpart.plot(cartpruned_accident_rose, nn=T, tweak = 1.2, box.palette = "RdYlGn", main = "Optimal Tree for Accident Severity")

print(cartpruned_accident_rose)

printcp(cartpruned_accident_rose)

summary(cartpruned_accident_rose)

scaledVarImpt <- round(100*cartpruned_accident_rose$variable.importance/sum(cartpruned_accident_rose$variable.importance))
cartpruned_accident_rose$variable.importance
scaledVarImpt[scaledVarImpt > 0]

# Predict the train set and show the confusion matrix of the data, along with the accuracy and other coefficients
cart2_trainset_predicted <- predict(object = cartpruned_accident_rose, type = "class", newdata = accident_trainset_rose)
confusionMatrix(data = table(accident_trainset_rose$accident_severity, cart2_trainset_predicted, deparse.level = 2), mode = "everything")

# Predict the test set and show the confusion matrix of the data, along with the accuracy and other coefficients
cart2_testset_predicted <- predict(object = cartpruned_accident_rose, type = "class", newdata = accident_testset)
confusionMatrix(data = table(accident_testset$accident_severity, cart2_testset_predicted, deparse.level = 2), mode = "everything")

#auc(accident_testset$accident_severity,as.vector(testset_pred))


#======================== CART with oversampling ===============================

#rpart uses random sampling, therefore, set seed to ensure reproducibility
set.seed(2407)
# Use CART to create a predictive model
cart_accident_over <- rpart(accident_severity~., method = "class", 
                            data = accident_trainset_over, control = rpart.control(minsplit = 20, cp = 0))

par(mfrow = c(1,1))

#takes too long to plot, anyway doesnt provide meaning cause the tree will just be too huge...
#rpart.plot(cart_accident_over, nn=T, tweak = 0.8, main = "Maximal Tree for Accident Severity")

print(cart_accident_over)

printcp(cart_accident_over)

plotcp(cart_accident_over)


#Extract the Optimal Tree , Source: Neumann Chew 
# Compute min CVerror + 1SE in maximal tree cart1.
CVerror.cap <- cart_accident_over$cptable[which.min(cart_accident_over$cptable[,"xerror"]), "xerror"] + cart_accident_over$cptable[which.min(cart_accident_over$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart_accident_over.
i <- 1; j<- 4
while (cart_accident_over$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cart_accident_over$cptable[i,1] * cart_accident_over$cptable[i-1,1]), 1) #calculate geometric mean here

# Prune the max tree using the optimal cp = cp.opt
cartpruned_accident_over <- prune(cart_accident_over, cp = cp.opt)

rpart.plot(cartpruned_accident_over, nn=T, tweak = 1.2, box.palette = "RdYlGn", main = "Optimal Tree for Accident Severity")

print(cartpruned_accident_over)

printcp(cartpruned_accident_over)

summary(cartpruned_accident_over)

scaledVarImpt <- round(100*cartpruned_accident_over$variable.importance/sum(cartpruned_accident_over$variable.importance))
cartpruned_accident_over$variable.importance
scaledVarImpt[scaledVarImpt > 0]

# Predict the train set and show the confusion matrix of the data, along with the accuracy and other coefficients
cart2_trainset_predicted <- predict(object = cartpruned_accident_over, type = "class", newdata = accident_trainset_over)
confusionMatrix(data = table(accident_trainset_over$accident_severity, cart2_trainset_predicted, deparse.level = 2), mode = "everything")

# Predict the test set and show the confusion matrix of the data, along with the accuracy and other coefficients
cart2_testset_predicted <- predict(object = cartpruned_accident_over, type = "class", newdata = accident_testset)
confusionMatrix(data = table(accident_testset$accident_severity, cart2_testset_predicted, deparse.level = 2), mode = "everything")

#train accuracy: 78.3%, test accuracy 59.6%.. overfitted

#auc(accident_testset$accident_severity,as.vector(testset_pred))

#====================== CART with under sampling ===============================

#rpart uses random sampling, therefore, set seed to ensure reproducibility
set.seed(2407)
# Use CART to create a predictive model
cart_accident_under <- rpart(accident_severity~., method = "class", 
                             data = accident_trainset_under, control = rpart.control(minsplit = 20, cp = 0))

par(mfrow = c(1,1))

#takes too long to plot, anyway doesnt provide meaning cause the tree will just be too huge...
#rpart.plot(cart_accident_under, nn=T, tweak = 0.8, main = "Maximal Tree for Accident Severity")

print(cart_accident_under)

printcp(cart_accident_under)

plotcp(cart_accident_under)


#Extract the Optimal Tree , Source: Neumann Chew 
# Compute min CVerror + 1SE in maximal tree cart1.
CVerror.cap <- cart_accident_under$cptable[which.min(cart_accident_under$cptable[,"xerror"]), "xerror"] + cart_accident_under$cptable[which.min(cart_accident_under$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart_accident_under.
i <- 1; j<- 4
while (cart_accident_under$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cart_accident_under$cptable[i,1] * cart_accident_under$cptable[i-1,1]), 1) #calculate geometric mean here

# Prune the max tree using the optimal cp = cp.opt
cartpruned_accident_under <- prune(cart_accident_under, cp = cp.opt)

rpart.plot(cartpruned_accident_under, nn=T, tweak = 1.2, box.palette = "RdYlGn", main = "Optimal Tree for Accident Severity")

print(cartpruned_accident_under)

printcp(cartpruned_accident_under)

summary(cartpruned_accident_under)

scaledVarImpt <- round(100*cartpruned_accident_under$variable.importance/sum(cartpruned_accident_under$variable.importance))
cartpruned_accident_under$variable.importance
scaledVarImpt[scaledVarImpt > 0]

# Predict the train set and show the confusion matrix of the data, along with the accuracy and other coefficients
cart2_trainset_predicted <- predict(object = cartpruned_accident_under, type = "class", newdata = accident_trainset_under)
confusionMatrix(data = table(accident_trainset_under$accident_severity, cart2_trainset_predicted, deparse.level = 2), mode = "everything")

# Predict the test set and show the confusion matrix of the data, along with the accuracy and other coefficients
cart2_testset_predicted <- predict(object = cartpruned_accident_under, type = "class", newdata = accident_testset)
confusionMatrix(data = table(accident_testset$accident_severity, cart2_testset_predicted, deparse.level = 2), mode = "everything")

#train accuracy: 74.8%, test accuracy 65.4%

#auc(accident_testset$accident_severity,as.vector(testset_pred))

#========================== CART with both =====================================

#rpart uses random sampling, therefore, set seed to ensure reproducibility
set.seed(2407)
# Use CART to create a predictive model
cart_accident_both <- rpart(accident_severity~., method = "class", 
                            data = accident_trainset_both, control = rpart.control(minsplit = 20, cp = 0))

par(mfrow = c(1,1))

#takes too long to plot, anyway doesnt provide meaning cause the tree will just be too huge...
#rpart.plot(cart_accident_both, nn=T, tweak = 0.8, main = "Maximal Tree for Accident Severity")

print(cart_accident_both)

printcp(cart_accident_both)

plotcp(cart_accident_both)


#Extract the Optimal Tree , Source: Neumann Chew 
# Compute min CVerror + 1SE in maximal tree cart1.
CVerror.cap <- cart_accident_both$cptable[which.min(cart_accident_both$cptable[,"xerror"]), "xerror"] + cart_accident_both$cptable[which.min(cart_accident_both$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart_accident_both.
i <- 1; j<- 4
while (cart_accident_both$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cart_accident_both$cptable[i,1] * cart_accident_both$cptable[i-1,1]), 1) #calculate geometric mean here

# Prune the max tree using the optimal cp = cp.opt
cartpruned_accident_both <- prune(cart_accident_both, cp = cp.opt)

rpart.plot(cartpruned_accident_both, nn=T, tweak = 1.2, box.palette = "RdYlGn", main = "Optimal Tree for Accident Severity")

print(cartpruned_accident_both)

printcp(cartpruned_accident_both)

summary(cartpruned_accident_both)

scaledVarImpt <- round(100*cartpruned_accident_both$variable.importance/sum(cartpruned_accident_both$variable.importance))
cartpruned_accident_both$variable.importance
scaledVarImpt[scaledVarImpt > 0]



# Predict the train set and show the confusion matrix of the data, along with the accuracy and other coefficients
cart2_trainset_predicted <- predict(object = cartpruned_accident_both, type = "class", newdata = accident_trainset_both)
confusionMatrix(data = table(accident_trainset_both$accident_severity, cart2_trainset_predicted, deparse.level = 2), mode = "everything")

# Predict the test set and show the confusion matrix of the data, along with the accuracy and other coefficients
cart2_testset_predicted <- predict(object = cartpruned_accident_both, type = "class", newdata = accident_testset)
confusionMatrix(data = table(accident_testset$accident_severity, cart2_testset_predicted, deparse.level = 2), mode = "everything")

#train accuracy: 90.2%, test accuracy 71.7%

#===============================================================================

#Use random grid search for random forest hyperparameter tuning 

#===============================================================================

#establish connection to h2o package
h2o.init()
h2o.cluster().shutdown()
h2o.shutdown(prompt = FALSE)
# create feature names
y <- "accident_severity"
x <- setdiff(names(accident_trainset_both), y)

# turn training set into h2o object
train.h2o <- as.h2o(accident_trainset_both)

# hyperparameter grid
hyper_grid.h2o <- list(
  ntrees      = 500,
  mtries      = seq(5,15,by=1)
)

#search criteria
search_criteria <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "AUC",
  stopping_tolerance = 0.005,
  stopping_rounds = 10,
  max_models = 20,
  max_runtime_secs = 15*60
)

# build grid search 
random_grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_grid1",
  x = x, 
  y = y, 
  seed = 2407,
  training_frame = train.h2o,
  hyper_params = hyper_grid.h2o,
  search_criteria = search_criteria
)

grid_perf <- h2o.getGrid(
  grid_id = "rf_grid1", 
  sort_by = "AUC", 
  decreasing = TRUE
)

print(grid_perf)

# Grab the model_id for the top model, chosen by validation error
best_model_id <- grid_perf@model_ids[[1]]
best_model <- h2o.getModel(best_model_id)

# Now let's evaluate the model performance on a test set
accident_testset.h2o <- as.h2o(accident_testset)
best_model_perf <- h2o.performance(model = best_model, newdata = accident_testset.h2o)
best_model_perf

#===============================================================================

#Modelling with Random Forest 
#TODO: AUC/PRC analysis to determine optimal threshold.

#===============================================================================

#============================ random forest with ROSE ==========================

set.seed(2407)  #for reproducibility of Bootstrap sampling & RSF selection.

rf_accident_rose <- randomForest(accident_severity ~ ., 
                                 data = accident_trainset_rose,
                                 na.action = na.omit, mtry = 7,
                                 importance = T, do.trace = TRUE)

rf_accident_rose 

plot(rf_accident_rose)

var.impt <- importance(rf_accident_rose)

varImpPlot(rf_accident_rose, type = 1)

threshold = 0.5

# Calculate the train set probabilities, and use it to predict the data
# Plot the confusion matrix as well, along with the accuracy and other coefficients
# Predict the model and show the confusion matrix along with the accuracy and other coefficients
trainset_prob <- predict(object = rf_accident_rose, type='prob', newdata = accident_trainset_rose)
trainset_prob <- as.data.frame(trainset_prob)
trainset_pred <- ifelse(test = trainset_prob$Serious > threshold, "Serious", "Not Serious")
confusionMatrix(data = table(accident_trainset_rose$accident_severity, trainset_pred, deparse.level = 2), mode = "everything")

testset_prob <- predict(object =  rf_accident_rose, type='prob', newdata = accident_testset)
testset_prob <- as.data.frame(testset_prob)
testset_pred <- ifelse(test = testset_prob$Serious > threshold, "Serious", "Not Serious")
confusionMatrix(data = table(accident_testset$accident_severity, testset_pred
                             ,  deparse.level = 2), mode = "everything")

#====================== random forest with oversampling ========================

set.seed(2407)  #for reproducibility of Bootstrap sampling & RSF selection.

rf_accident_over <- randomForest(accident_severity ~ ., 
                                 data = accident_trainset_over,
                                 na.action = na.omit, mtry = 7,
                                 importance = T, do.trace = TRUE)

rf_accident_over 

plot(rf_accident_over)

var.impt <- importance(rf_accident_over)

varImpPlot(rf_accident_over, type = 1)

threshold = 0.5

# Calculate the train set probabilities, and use it to predict the data
# Plot the confusion matrix as well, along with the accuracy and other coefficients
# Predict the model and show the confusion matrix along with the accuracy and other coefficients
trainset_prob <- predict(object = rf_accident_over, type='prob', newdata = accident_trainset_over)
trainset_prob <- as.data.frame(trainset_prob)
trainset_pred <- ifelse(test = trainset_prob$Serious > threshold, "Serious", "Not Serious")
confusionMatrix(data = table(accident_trainset_over$accident_severity, trainset_pred, deparse.level = 2), mode = "everything")

testset_prob <- predict(object =  rf_accident_over, type='prob', newdata = accident_testset)
testset_prob <- as.data.frame(testset_prob)
testset_pred <- ifelse(test = testset_prob$Serious > threshold, "Serious", "Not Serious")
confusionMatrix(data = table(accident_testset$accident_severity, testset_pred,
                             deparse.level = 2), mode = "everything")

#==================== random forest with under sampling ========================

set.seed(2407)  #for reproducibility of Bootstrap sampling & RSF selection.

rf_accident_under <- randomForest(accident_severity ~ .,
                                  data = accident_trainset_under, 
                                  na.action = na.omit, mtry = 7,
                                  importance = T, do.trace = TRUE)

rf_accident_under  

plot(rf_accident_under)

var.impt <- importance(rf_accident_under)

varImpPlot(rf_accident_under, type = 1)

threshold = 0.5

# Calculate the train set probabilities, and use it to predict the data
# Plot the confusion matrix as well, along with the accuracy and other coefficients
# Predict the model and show the confusion matrix along with the accuracy and other coefficients
trainset_prob <- predict(object = rf_accident_under, type='prob', newdata = accident_trainset_under)
trainset_prob <- as.data.frame(trainset_prob)
trainset_pred <- ifelse(test = trainset_prob$Serious > threshold, "Serious", "Not Serious")
confusionMatrix(data = table(accident_trainset_under$accident_severity, trainset_pred, deparse.level = 2), mode = "everything")

testset_prob <- predict(object =  rf_accident_under, type='prob', newdata = accident_testset)
testset_prob <- as.data.frame(testset_prob)
testset_pred <- ifelse(test = testset_prob$Serious > threshold, "Serious", "Not Serious")
confusionMatrix(data = table(accident_testset$accident_severity, testset_pred,
                             deparse.level = 2), mode = "everything")

#===================== random forest with both =================================

set.seed(2407)  #for reproducibility of Bootstrap sampling & RSF selection.

rf_accident_both <- randomForest(accident_severity ~ ., 
                                 data = accident_trainset_both, 
                                 na.action = na.omit, mtry = 7,
                                 importance = T, do.trace = TRUE)
rf_accident_both

plot(rf_accident_both)

var.impt <- importance(rf_accident_both)

varImpPlot(rf_accident_both, type = 1)

threshold = 0.5

# Calculate the train set probabilities, and use it to predict the data
# Plot the confusion matrix as well, along with the accuracy and other coefficients
# Predict the model and show the confusion matrix along with the accuracy and other coefficients
trainset_prob <- predict(object = rf_accident_both, type='prob', newdata = accident_trainset_both)
trainset_prob <- as.data.frame(trainset_prob)
trainset_pred <- ifelse(test = trainset_prob$Serious > threshold, "Serious", "Not Serious")
confusionMatrix(data = table(accident_trainset_both$accident_severity, trainset_pred, deparse.level = 2), mode = "everything")

testset_prob <- predict(object =  rf_accident_both, type='prob', newdata = accident_testset)
testset_prob <- as.data.frame(testset_prob)
testset_pred <- ifelse(test = testset_prob$Serious > threshold, "Serious", "Not Serious")
confusionMatrix(data = table(accident_testset$accident_severity, testset_pred,
                             deparse.level = 2), mode = "everything")

#Plot ROC/ PR curves...

fg <- trainset_prob$Serious[accident_trainset_under$accident_severity == "Serious"]
bg <- trainset_prob$Serious[accident_trainset_under$accident_severity == "Not Serious"]
# ROC Curve    
roc <- roc.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(roc)

# PR Curve
pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(pr)
