# libraries. Loading excess libraries will result in lost points.
library(tidyverse)
library(class)
library(rpart)
library(rpart.plot)
library(e1071)
library(scales)
library(fastDummies)
library(corrplot)
library(olsrr)
library(smotefamily)

# Set the working directory
setwd("C:/Users/DELL/RLabs")

# Read NPHA-doctor-visits.csv into a tibble called doctorVisits. 
# Assign the most appropriate data types for each of the features in the dataset
doctorVisits <- read_csv(file = "NPHA-doctor-visits1.csv",
                     col_types =  "nfffffffffffffff",
                     col_names = TRUE)

# Display the doctorVisits tibble on the console
print(doctorVisits)

# Display the structure of the doctorVisits tibble
str(doctorVisits)

# Display a summary of the doctorVisits tibble
summary(doctorVisits)

# since our target audience is above the age of 65
doctorVisits <- doctorVisits %>% select(-Age)

# Also, Sr No is not required for analysis
doctorVisits <- doctorVisits %>% select(-`Sr No`)

# Histogram on count of Employment
histogramEmploymentType <- ggplot(data = doctorVisits,
                                  aes(x = Employment)) +
  geom_bar(color = "red",
           fill = "pink",
           alpha = 0.8) +
  ggtitle("Employment Histogram")

histogramEmploymentType

# Histogram on count of Gender
histogramGenderType <- ggplot(data = doctorVisits,
                                  aes(x = Gender)) +
  geom_bar(color = "darkgreen",
           fill = "green",
           alpha = 0.8) +
  ggtitle("Gender Histogram")

histogramGenderType


# Histrogram on count of Physical Health
histogramPhysicalHealthType <- ggplot(data = doctorVisits,
                              aes(x = `Phyiscal Health`)) +
  geom_bar(color = "darkblue",
           fill = "skyblue",
           alpha = 0.8) +
  ggtitle("Physical Health Histogram")

histogramPhysicalHealthType

# Histogram on count of Mental Health
histogramMentalHealthType <- ggplot(data = doctorVisits,
                                    aes(x = `Mental Health`)) +
  geom_bar(color = "orange",
           fill = "yellow",
           alpha = 0.8) +
  ggtitle("Mental Health Histogram")

histogramMentalHealthType

# Histogram on count of Dental Health
histogramDentalHealthType <- ggplot(data = doctorVisits,
                                    aes(x = `Dental Health`)) +
  geom_bar(color = "lightblue",
           fill = "maroon",
           alpha = 0.8) +
  ggtitle("Dental Health Histogram")

histogramDentalHealthType

# Histogram on count of Race 
histogramRaceType <- ggplot(data = doctorVisits,
                                  aes(x = `Race`)) +
  geom_bar(color = "blue",
           fill = "skyblue",
           alpha = 0.8) +
  ggtitle("Race Histogram")

histogramRaceType


# Histogram on count of Trouble Sleeping
histogramTroubleSleepingType <- ggplot(data = doctorVisits,
                                  aes(x = `Trouble Sleeping`)) +
  geom_bar(color = "blue",
           fill = "lightblue",
           alpha = 0.8) +
  ggtitle("Trouble Sleeping Histogram")

histogramTroubleSleepingType


# Histogram on count of Prescription Sleep Medication
histogramPrescriptionSleepMedication <- 
  ggplot(data = doctorVisits,
         aes(x = `Prescription Sleep Medication`)) +
  geom_bar(color = "green",
           fill = "darkgreen",
           alpha = 0.8) +
  ggtitle("Prescription Sleep Medication")

histogramPrescriptionSleepMedication

# Total patients who visited doctor for 4 or more times
total_patients <- doctorVisits %>%
  filter(`Number of Doctors Visited` == "4 or more") %>%
  nrow()

# Percentage of patients who visited doctor for 4+ times by physical health 
doctorVisitsObs1 <- doctorVisits %>%
  filter(`Number of Doctors Visited` == "4 or more") %>%
  group_by(`Phyiscal Health`) %>%
  summarize(
    patients_trouble_sleeping = n(),
    percentage_trouble_sleeping = (patients_trouble_sleeping / 
                                     total_patients) * 100
  ) %>%
  arrange(desc(percentage_trouble_sleeping))

print(doctorVisitsObs1)

# Percentage of patients who visited doctor for 4+ times by mental health 
doctorVisitsObs2 <- doctorVisits %>%
  filter(`Number of Doctors Visited` == "4 or more") %>%
  group_by(`Mental Health`) %>%
  summarize(
    patients_trouble_sleeping = n()
  ) %>%
  mutate(
    percentage_trouble_sleeping = (patients_trouble_sleeping / total_patients) 
    * 100
  ) %>%
  arrange(desc(percentage_trouble_sleeping))

print(doctorVisitsObs2)

# correlation among different health types
health_correlation <- doctorVisits %>%
  mutate(
    Physical_Health_Score = 
      as.numeric(as.factor(`Phyiscal Health`)),
    Mental_Health_Score = 
      as.numeric(as.factor(`Mental Health`)),
    Dental_Health_Score = 
      as.numeric(as.factor(`Dental Health`)),
    Number_of_Doctors_Visited= 
      as.numeric(as.factor(`Number of Doctors Visited`))
  ) %>%
  select(Physical_Health_Score, 
         Mental_Health_Score, 
         Dental_Health_Score, 
         Number_of_Doctors_Visited)

colnames(health_correlation) <- c("Physical Health", 
                                  "Mental Health", 
                                  "Dental Health", 
                                  "Number of Doctors Visited")

# Compute correlation matrix again
cor_matrix <- cor(health_correlation)

print(cor_matrix)

corrplot(cor_matrix, 
         method = "color",          
         type = "upper",            
         tl.cex = 0.8,              
         tl.col = "black",          
         addCoef.col = "black",     
         col = colorRampPalette(c("red", "white", "blue"))(200))


# stress vs employment type
stress_vs_employment <- doctorVisits %>%
  group_by(Employment, `Stress Keeps Patient from Sleeping`) 

stress_vs_employment <- stress_vs_employment %>%
filter(`Stress Keeps Patient from Sleeping` == "Yes") %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / 714) * 100)
  
print(stress_vs_employment)


sleep_data <- doctorVisits %>%
  select(contains("Keeps Patient from Sleeping")) %>%
  mutate(across(everything(), ~ ifelse(. == "Yes", 1, 0)))

colnames(sleep_data) <- c("Stress", 
                          "Medication", 
                          "Pain", 
                          "Bathroom", 
                          "Unknown")

# Compute correlation matrix again
cor_matrix <- cor(sleep_data)


corrplot(cor_matrix, 
         method = "color",          
         type = "upper",            
         tl.cex = 0.8,              
         tl.col = "black",          
         addCoef.col = "black",     
         col = colorRampPalette(c("red", "white", "blue"))(200))

str(doctorVisits)
insight2 <- doctorVisits %>%
  filter(`Stress Keeps Patient from Sleeping` == "Yes" | 
          `Medication Keeps Patient from Sleeping` == "Yes" |
          `Pain Keeps Patient from Sleeping` == "Yes" |
          `Bathroom Needs Keeps Patient from Sleeping` == "Yes" |
          `Uknown Keeps Patient from Sleeping` == "Yes"
           ) %>%
  group_by(`Number of Doctors Visited`) %>%
  summarize(count = n(), .groups = "drop") %>%
  mutate(percentage = round(100 * count / sum(count), 2))

# View insight
print(insight2)
  
doctorVisits1 <- doctorVisits %>%
  rename(NumberOfDoctorsVisited = "Number of Doctors Visited",
          PhysicalHealth = "Phyiscal Health",
          MentalHealth = "Mental Health",
          DentalHealth = "Dental Health",
          StressKeepsPatientFromSleeping = 
            "Stress Keeps Patient from Sleeping",
          MedicationKeepsPatientFromSleeping = 
            "Medication Keeps Patient from Sleeping",
          PainKeepsPatientFromSleeping = 
            "Pain Keeps Patient from Sleeping",
          BathroomNeedsKeepsPatientFromSleeping =
            "Bathroom Needs Keeps Patient from Sleeping",
          UnknownKeepsPatientFromSleeping = 
            "Uknown Keeps Patient from Sleeping",
          TroubleSleeping = "Trouble Sleeping",
          PrescriptionSleepMedication = 
            "Prescription Sleep Medication")

doctorVisits1DataFrame <- data.frame(doctorVisits1)
  

# Dummy code categorical variables if your dataset contains any
# Create dummy variables

doctorVisits2 <- as_tibble
(dummy.data.frame(data = doctorVisits1DataFrame,
                  names = c("PhysicalHealth",
                             "MentalHealth",
                             "DentalHealth",
                             "TroubleSleeping",
                             "PrescriptionSleepMedication",
                             "Employment",
                             "Race",
                             "Gender")))

doctorVisits2 <- doctorVisits2 %>%
  mutate(NumberOfDoctorsVisited = 
           ifelse(NumberOfDoctorsVisited == "4 or more", 1, 0))

# Count of Patients Reporting Stress, Grouped by Employment
doctorVisits3 <- doctorVisits2 %>%
  filter(NumberOfDoctorsVisited == 1) %>%
  group_by(
    EmploymentRetired, 
    `EmploymentWorking full-time`,
    `EmploymentWorking part-time`, 
    `EmploymentNot working`
  ) %>%
  summarize(
    patients_by_employment_type = n(),
    stress_percentage = 
      round((patients_by_employment_type / total_patients) * 100,2)
  ) %>%
  arrange(desc(stress_percentage))

print(doctorVisits3)


doctorVisits2 <- doctorVisits2 %>%
mutate(StressKeepsPatientFromSleeping = 
           ifelse(StressKeepsPatientFromSleeping == "Yes", 1, 0)) 

doctorVisits2 <- doctorVisits2 %>%
  mutate(MedicationKeepsPatientFromSleeping = 
           ifelse(MedicationKeepsPatientFromSleeping == "Yes", 1, 0)) 

doctorVisits2 <- doctorVisits2 %>%
  mutate(PainKeepsPatientFromSleeping = 
           ifelse(PainKeepsPatientFromSleeping == "Yes", 1, 0)) 

doctorVisits2 <- doctorVisits2 %>%
  mutate(BathroomNeedsKeepsPatientFromSleeping = 
           ifelse(BathroomNeedsKeepsPatientFromSleeping == "Yes", 1, 0)) 

doctorVisits2 <- doctorVisits2 %>%
  mutate(UnknownKeepsPatientFromSleeping = 
           ifelse(UnknownKeepsPatientFromSleeping == "Yes", 1, 0))

str(doctorVisits2)                

# KNN
# Separate the tibble into two. One with just the label and one with the other
# variables.
doctorVisitsKNNLabels <- doctorVisits2 %>% select(NumberOfDoctorsVisited)

doctorVisitsKNN <- doctorVisits2 %>% select(-NumberOfDoctorsVisited) 


# Split data into 75% training and 25% testing using 554455 as the random seed.
set.seed(554455)
sampleSet <- sample(nrow(doctorVisitsKNN),
                    round(nrow(doctorVisitsKNN) * 0.75),
                    replace = FALSE)
doctorVisitsTrainingKNN <- doctorVisitsKNN[sampleSet, ]
doctorVisitsTrainingKNNLabels <- doctorVisitsKNNLabels[sampleSet, ]
doctorVisitsTestingKNN <- doctorVisitsKNN[-sampleSet, ]
doctorVisitsTestingKNNLabels <- doctorVisitsKNNLabels[-sampleSet, ]

# Generate the KNN model 
doctorVisitsPredictionKNN <- knn(train = doctorVisitsTrainingKNN,
                                 test = doctorVisitsTestingKNN,
                                 cl = doctorVisitsTrainingKNNLabels$
                                   NumberOfDoctorsVisited,
                                 k = 23)

# Display the predictions from the testing dataset on the console 
print(doctorVisitsPredictionKNN)

# Display summary of the predictions from the testing dataset
print(summary(doctorVisitsPredictionKNN))

# Evaluate the model by forming a confusion matrix
doctorVisitsConfusionMatrixKNN <- table(doctorVisitsTestingKNNLabels$
                                          NumberOfDoctorsVisited,
                                        doctorVisitsPredictionKNN)

# Display the confusion matrix on the console 
print(doctorVisitsConfusionMatrixKNN)

# Calculate the model predictive Accuracy  
predictiveAccuracyKNN <- sum(diag(doctorVisitsConfusionMatrixKNN))/
  nrow(doctorVisitsTestingKNN)

# Display the Predictive Accuracy on the console 
print(predictiveAccuracyKNN)

# Calculate and display the false positive rate
doctorVisitsConfusionMatrixKNN[1,2]/(doctorVisitsConfusionMatrixKNN[1,2] + 
                                       doctorVisitsConfusionMatrixKNN[1,1])

# Calculate and display the false negative rate
doctorVisitsConfusionMatrixKNN[2,1]/(doctorVisitsConfusionMatrixKNN[2,1] + 
                                       doctorVisitsConfusionMatrixKNN[2,2])


# Logistic Regression
# Set.seed() function
set.seed(554455)

doctorVisits2 <- doctorVisits2 %>%
  rename(PhysicalHealthVeryGood = "PhysicalHealthVery Good",
         MentalHealthVeryGood = "MentalHealthVery Good",
         DentalHealthVeryGood = "DentalHealthVery Good",
         EmploymentWorkingFullTime = "EmploymentWorking full-time",
         EmploymentWorkingPartTime = "EmploymentWorking part-time",
         EmploymentNotWorking  = "EmploymentNot working",
         PrescriptionSleepMedicationDoNotUse = 
           "PrescriptionSleepMedicationDo not use",
         PrescriptionSleepMedicationUseOcassionally = 
           "PrescriptionSleepMedicationUse ocassionally",
         PrescriptionSleepMedicationUseRegularly =
           "PrescriptionSleepMedicationUse regularly",
         Race2RacesNonHispanic = "Race2+ Races, Non-Hispanic",
         RaceWhiteNonHispanic = "RaceWhite, Non-Hispanic",
         RaceBlackNonHispanic = "RaceBlack, Non Hispanic",
         RaceOtherNonHispanic = "RaceOther, Non Hispanic")

# Create a vector of 75% randomly sampled rows from the original dataset
sampleSetLogistic <- sample(nrow(doctorVisits2),
                            round(nrow(doctorVisits2) * 0.75),
                            replace = FALSE)

# Put the records from the 75% sample into the Training
doctorVisitsTrainingLogistic <- doctorVisits2[sampleSetLogistic, ]

# Put all other records into the Testing
doctorVisitsTestingLogistic <- doctorVisits2[-sampleSetLogistic, ]



# Check class imbalance
summary(doctorVisitsTrainingLogistic$NumberOfDoctorsVisited)

# Store the magnitude of class imbalance into a variable
classImbalanceMagnitude <- 170/366

# Solve class imbalance in the training dataset
doctorVisitsTrainingLogistic <- doctorVisitsTrainingLogistic %>%
  mutate(across(where(is.factor), as.numeric))

doctorVisitsTrainingLogistic <- doctorVisitsTrainingLogistic %>%
  mutate(across(where(is.factor), as.numeric))

doctorVisitsTestingLogistic <- doctorVisitsTestingLogistic %>%
  mutate(across(where(is.factor), as.numeric))

doctorVisitsTrainingLogisticSmoted <- 
  tibble(SMOTE(X = data.frame(doctorVisitsTrainingLogistic),
               target = doctorVisitsTrainingLogistic$NumberOfDoctorsVisited,
               dup_size = 2)$data)

summary(doctorVisitsTrainingLogisticSmoted$NumberOfDoctorsVisited)


# Print the updated structure of the dataset
str(doctorVisitsTrainingLogisticSmoted)

# Get rid of class column in tibble
doctorVisitsTrainingLogisticSmoted <- doctorVisitsTrainingLogisticSmoted %>%
  select(-class)

# Generate logistic regression model
doctorVisitsLogisticModel <- glm(data = doctorVisitsTrainingLogisticSmoted,
                                 family = binomial,
                                 formula = NumberOfDoctorsVisited ~ .)

# Display the output of logistic regression model
summary(doctorVisitsLogisticModel)

# Use the model to predict outcomes in the testing dataset
doctorVisitsPredictionLogistic <- predict(doctorVisitsLogisticModel,
                                          doctorVisitsTestingLogistic,
                                          type = "response")


# Display DoctorVisitsPrediction on the console
print(doctorVisitsPredictionLogistic)

# Treat anything below or equal to 0.5 as a 0, anything above 0.5 as a 1
doctorVisitsPredictionLogistic <- 
  ifelse(doctorVisitsPredictionLogistic >= 0.5, 1, 0)

# Display DoctorVisitsPrediction on the console
print(doctorVisitsPredictionLogistic)

# Create a confusion matrix
doctorVisitsConfusionMatrixLogistic <- table(doctorVisitsTestingLogistic$
                                               NumberOfDoctorsVisited,
                                             doctorVisitsPredictionLogistic)

# Display the confusion matrix
print(doctorVisitsConfusionMatrixLogistic)

# Calculate the prediction accuracy by dividing the number of true positives and 
# true negatives by the total amount of predictions in the testing dataset
sum(diag(doctorVisitsConfusionMatrixLogistic)) / 
  nrow(doctorVisitsTestingLogistic)

# Calculate and display the false positive rate
doctorVisitsConfusionMatrixLogistic[1,2]/(
  doctorVisitsConfusionMatrixLogistic[1,2] + 
    doctorVisitsConfusionMatrixLogistic[1,1])

# Calculate and display the false negative rate
doctorVisitsConfusionMatrixLogistic[2,1]/(
  doctorVisitsConfusionMatrixLogistic[2,1] + 
    doctorVisitsConfusionMatrixLogistic[2,2])

# Naive Bayes
set.seed(1148)

# Create a vector of 75% randomly sampled rows from original dataset
sampleSetNB <- sample(nrow(doctorVisits2),
               round(nrow(doctorVisits2) * 0.75),
               replace = FALSE)

# Put the records from 75% sample into doctorVisitsTrainingNB
doctorVisitsTrainingNB <- doctorVisits2[sampleSetNB,]

# Put the records from remaining 25% sample into doctorVisitsTestingNB
doctorVisitsTestingNB <-  doctorVisits2[-sampleSetNB,]

#train the naive bayes model
doctorVisitsModelNB <- naiveBayes(formula = NumberOfDoctorsVisited ~ .,
                                data = doctorVisitsTrainingNB,
                                laplace = 1)

# Build Probabilities for each record in testing dataset
doctorVisitsProbability <- predict(doctorVisitsModelNB,
                                   doctorVisitsTestingNB,
                                   type = "raw")

# Display the probabilties  from doctorVisitsProbability on the coonsole
print(doctorVisitsProbability)

# Predict classes for each record in the testing dataset
doctorVisitsPredictionNB <- predict(doctorVisitsModelNB,
                                    doctorVisitsTestingNB,
                                    type = "class")

# Display the prediction from doctorVisitsPredictionNB in console
print(doctorVisitsPredictionNB)

# Evaluate the model by forming the confusion Matrix
doctorVisitsConfusionMatrixNB <- table(doctorVisitsTestingNB$
                                         NumberOfDoctorsVisited,
                                       doctorVisitsPredictionNB)

# Display the confusion matrix
print(doctorVisitsConfusionMatrixNB)

# Calculate the model predictive accuracy
predictiveAccruracyNB <- sum(diag(doctorVisitsConfusionMatrixNB)) / 
  nrow(doctorVisitsTestingNB)

# Display predictive accuracy
print(predictiveAccruracyNB)

# Decision Tree model
# Set random seet to 1248
set.seed(1248)

# Create a vector of 75% randomly sampled rows from original dataset
sampleSetDT <- sample(nrow(doctorVisits2),
                      round(nrow(doctorVisits2) * 0.75),
                      replace = FALSE)

# Put the records from 75% sample into doctorVisitsTrainingDT
doctorVisitsTrainingDT <- doctorVisits2[sampleSetDT,]

# Put the records from remaining 25% sample into doctorVisitsTestingDT
doctorVisitsTestingDT <-  doctorVisits2[-sampleSetDT,]

# Train the Decision tree model using  the training data set. 
doctorVisitsDTM <- rpart(formula = NumberOfDoctorsVisited ~ .,
                        method = "class",
                        cp = 0.007,
                        data = doctorVisitsTrainingDT)

# Display the decision tree plot
rpart.plot(doctorVisitsDTM)


# Predict classes for each record in the testing dataset
doctorVisitsPredictionDT <- predict(doctorVisitsDTM,
                                    doctorVisitsTestingDT,
                                    type = "class")

# Display the prediction from doctorVisitsPredictionNB in console
print(doctorVisitsPredictionDT)

# Evaluate the model by forming the confusion Matrix
doctorVisitsConfusionMatrixDT <- table(doctorVisitsTestingNB$
                                         NumberOfDoctorsVisited,
                                       doctorVisitsPredictionDT)

# Display the confusion matrix
print(doctorVisitsConfusionMatrixDT)

# Calculate the model predictive accuracy
predictiveAccruracyDT <- sum(diag(doctorVisitsConfusionMatrixDT)) / 
  nrow(doctorVisitsTestingDT)

# Display predictive accuracy
print(predictiveAccruracyDT)







