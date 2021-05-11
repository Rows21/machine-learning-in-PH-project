library(readxl)
data_1 <- read.csv("Desktop/2338 ML/project/data 1.csv")
View(data_1)

library(tidyverse)
library(dplyr)
colSums(is.na(data_1))
head(data_1)

#Filling missing values of smoking status for ages <= 5
data_1[data_1$age <= 5, "smoking_status"] <- "never smoked"

data_1 <- data_1 %>%
  #NA values insignificant portion of data, will remove them
  #Only one entry for Gender:Other, will remove
  filter(
    bmi != "N/A",
    gender != "Other"
  ) %>%
  mutate(
    gender = factor(gender),
    hypertension = factor(hypertension),
    heart_disease = factor(heart_disease),
    ever_married = factor(ever_married),
    work_type = factor(work_type),
    residence_type = factor(Residence_type),
    bmi = as.numeric(bmi),
    smoking_status = factor(smoking_status),
    stroke = factor(stroke),
  ) %>%
  select(
    -Residence_type
  )




#Setting seed for replication
set.seed(2001)
#Creating training and test sets
split <- sort(sample(nrow(data_1), nrow(data_1) * 0.7))
data_1_train <- data_1[split,]
data_1_test <- data_1[-split,]

glm_stroke <- glm(
  stroke ~ gender + age + hypertension + heart_disease + ever_married + 
    work_type + residence_type + avg_glucose_level + bmi + smoking_status,
  data = data_1_train,
  family = binomial
)

summary(glm_stroke)
anova(glm_stroke, test = 'Chisq')

#install.packages("pscl")
library(pscl)

pscl::pR2(glm_stroke)["McFadden"]
## fitting null model for pseudo-r2
##  McFadden 
## 0.2132497  
## The McFadden pseudo-R2 value of 0.2132497    indicate that the model fits the data
#well, but there are several non-significant terms in the model. BIC selection
#will be used to simplify the model.


######  BIC
glm_stroke_bic <- step(
  glm_stroke,
  direction = "both",
  trace = 0,
  k = log(nrow(data_1))
)
summary(glm_stroke_bic)

anova(glm_stroke_bic, test = 'Chisq')
pscl::pR2(glm_stroke_bic)["McFadden"]
## fitting null model for pseudo-r2
##  McFadden 
## 0.1941669 
#The BIC reduced model is much simpler. Each term is significant at a 0.001 level. 
#The McFadden pseudo-R2 of 0.2132497 is only a 0.02 decrease from the full model. This value indicates that the model fits the data well.

###
#install.packages("ROCR")
library(ROCR)

roc_pred <- prediction(
  predictions = glm_stroke_bic$fitted.values,
  labels = data_1_train$stroke
)

roc_perf <- performance(
  roc_pred,
  measure = "tpr",
  x.measure = "fpr"
)

roc_curve <- data.frame(
  Spec = 1 - unlist(roc_perf@x.values),
  Sens = unlist(roc_perf@y.values),
  thresh = unlist(roc_perf@alpha.values)
)

roc_curve$distance <- sqrt((1 - roc_curve$Spec)^2 + (1 - roc_curve$Sens)^2)

opt <- roc_curve %>%
  slice(
    distance %>% which.min()
  )

plot(
  roc_perf,
  main = "Logistic Regression for Strokes"
)
abline(0, 1, col = "grey80")
#Optimal Threshold
abline(v = 1 - opt$Spec, col = "gray80")
abline(h = opt$Sens, col = "gray80")



glm_predict <- predict.glm(glm_stroke_bic,
                           newdata =  data_1_test,
                           type = "response",
                           se.fit = FALSE) %>% as_tibble()
glm_predict$pred <- ifelse(glm_predict$value >= opt$thresh, 1, 0)
glm_predict <- glm_predict %>%
  mutate(
    pred = factor(pred)
  )


library(caret)
confusionMatrix(
  data = glm_predict$pred,
  reference = data_1_test$stroke,
  positive = "1"
)

confusionMatrix(
  data = glm_predict$pred,
  reference = dat_stroke_test$stroke,
  positive = "1"
)

#The model performs well with an accuracy of 74.2%. The sensitivity of 84.6% is good since we want to miss as little strokes as possible.

#Improvements can be made if more data were to be collected. They also might be made if additional factors were collected, specifically ones which are considered to be risk factors by those with domain knowledge.














