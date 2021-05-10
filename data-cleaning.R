data <- read.csv('/Users/rong/Desktop/NYU/21Spring/2338 Machine Learning/Project/healthcare-dataset-stroke-data.csv')
data$gender[data$gender == 'Male'] <- 1
data$gender[data$gender == 'Female'] <- 0

data$ever_married[data$ever_married == 'Yes'] <- 1
data$ever_married[data$ever_married == 'No'] <- 0

data$work_type[data$work_type == 'Private'] <- 0
data$work_type[data$work_type == 'children'] <- 1
data$work_type[data$work_type == 'Govt_job'] <- 2
data$work_type[data$work_type == 'Self-employed'] <- 3
data$work_type[data$work_type == 'Never_worked'] <- 9

data$Residence_type[data$Residence_type == 'Rural'] <- 0
data$Residence_type[data$Residence_type == 'Urban'] <- 1

data$smoking_status[data$smoking_status == "Unknown"] <- 9
data$smoking_status[data$smoking_status == "never smoked"] <- 0
data$smoking_status[data$smoking_status == "formerly smoked"] <- 1
data$smoking_status[data$smoking_status == "smokes"] <- 2

data$gender <- as.factor(data$gender)
data$ever_married <- as.factor(data$ever_married)
data$work_type <- as.factor(data$work_type)
data$Residence_type <- as.factor(data$Residence_type)
data$smoking_status <- as.factor(data$smoking_status)
data$bmi <- as.numeric(data$bmi)
data$hypertension <- as.factor(data$hypertension)
data$heart_disease <- as.factor(data$heart_disease)
data$stroke <- as.factor(data$stroke)
summary(data)

write.table(data, "data.csv", row.names = FALSE, col.names = TRUE, sep = ",")
