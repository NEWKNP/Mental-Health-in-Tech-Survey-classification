# Data preprocessing
mental_health <- read.csv("mental_health_survey.csv", stringsAsFactors = FALSE)
str(mental_health)

## Cleansing data

# table(mental_health$Gender)
# table(mental_health$Country)
# table(mental_health$state)
# table(mental_health$self_employed)
# table(is.na(mental_health$leave))
# table(mental_health$leave)
# table(mental_health$no_employees)
# table(mental_health$comments)
# table(mental_health$Timestamp)

### delete unimportant data
clean1 <- mental_health[ , !(names(mental_health) %in% "Timestamp")]
clean1 <- clean1[ , !(names(clean1) %in% "Country")]
clean1 <- clean1[ , !(names(clean1) %in% "state")]
clean1 <- clean1[ , !(names(clean1) %in% "comments")]
str(clean1)

### Gender unification.
# install.packages("stringr")
# install.packages("dplyr")
library(stringr)
library(dplyr)
clean1$Gender <- str_to_lower(clean1$Gender)
str(clean1$Gender)

male_str <- c("male", "m", "male-ish", "maile", "mal", "male (cis)", "make", "male ", "man","msle", "mail", "malr","cis man", "cis male")
trans_str <- c("trans-female", "something kinda male?", "queer/she/they", "non-binary","nah", "all", "enby", "fluid", "genderqueer", "androgyne", "agender", "male leaning androgynous", "guy (-ish) ^_^", "trans woman", "neuter", "female (trans)", "queer", "ostensibly male, unsure what that really means" )
female_str <- c("cis female", "f", "female", "woman",  "femake", "female ","cis-female/femme", "female (cis)", "femail")

clean1$Gender <- sapply(as.vector(clean1$Gender), function(x) if(x %in% male_str) "male" else x )
clean1$Gender <- sapply(as.vector(clean1$Gender), function(x) if(x %in% female_str) "female" else x )
clean1$Gender <- sapply(as.vector(clean1$Gender), function(x) if(x %in% trans_str) "trans" else x )
clean2 <- filter(clean1, Gender != "a little about you")
clean2 <- filter(clean2, Gender != "guy (-ish) ^_^")
clean2 <- filter(clean2, Gender != "p")
table(clean2$Gender)
str(clean2)

### Age unification.
table(clean2$Age)
clean3 <- filter(clean2, Age > 10 & Age < 100)
str(clean3$Age)

### NA values detection and deleting the row.
sapply(clean3, function(x) sum(is.na(x)))
clean4 <- clean3[!is.na(clean3$self_employed),]
clean_data <- clean4[!is.na(clean4$work_interfere),]
str(clean_data)

# train data
## Data training and testing split randomly
set.seed(112)
n <- nrow(clean_data)
clean_data.index <- sample(1:n , size=round(n*0.8))
train <- clean_data[clean_data.index,]
test <- clean_data[-clean_data.index,]

str(train)
str(test)

## train
library(C50)
### Executing model C5.0
train$treatment <- as.factor(train$treatment)
str(train[-5])
model1 <- C5.0(train[-5], train$treatment)
summary(model1)

# Evaluation
## Prediction
prediction1 <- predict(model1, newdata=test)

## Confussion matrix
library(gmodels)
CrossTable(test$treatment, prediction1, prop.chisq =
               FALSE, prop.c = FALSE, prop.r = FALSE, 
           dnn = c('actual default', 'predicted default'))
( mc <- table(prediction1, test$treatment) )
( accuracy1 <- sum(diag(mc)) / sum(mc) * 100 )
#### around 0.73

# Improve algorithm
model2 <- C5.0(train[-5], train$treatment, trials = 5)
summary(model2)
prediction2 <- predict(model2, newdata=test)
( mc2 <- table(prediction2, test$treatment) )
( accuracy2 <- sum(diag(mc2)) / sum(mc2) * 100 )
CrossTable(test$treatment, prediction2, prop.chisq =
               FALSE, prop.c = FALSE, prop.r = FALSE, 
           dnn = c('actual default', 'predicted default'))
#### around 0.78
