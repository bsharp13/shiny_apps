library(caret)

#-------------------------------------------------------------------------------

# Data Prep
#-------------------------------------------------------------------------------
# Read data
scores <- read_csv('~/Downloads/StudentsPerformance.csv')
colnames(scores) <- 
  c('Gender', 'Race', 'ParentEd', 'Lunch', 'Pre', 'Math', 'Read', 'Write')


# EDA
table(scores$Gender)
table(scores$Race)
table(scores$Lunch)
table(scores$Pre)

ggplot(scores, aes(x = Math)) + geom_histogram(binwidth = 5)
ggplot(scores, aes(x = Read)) + geom_histogram(binwidth = 5)
ggplot(scores, aes(x = Write)) + geom_histogram(binwidth = 5)

# Test/train split
trainrow <- createDataPartition(scores$Pre, p = 0.7, list = FALSE)
train <- scores[trainrow, ]
test <- scores[-trainrow, ]

X_train <- train %>% select(-Math, -Read, -Write)
X_test <- test %>% select(-Math, -Read, -Write)

ym_train <- train %>% pull(Math)
yr_train <- train %>% pull(Read)
yw_train <- train %>% pull(Write)

ym_test <- test %>% pull(Math)
yr_test <- test %>% pull(Read)
yw_test <- test %>% pull(Write)

#-------------------------------------------------------------------------------

# Math Scores
#-------------------------------------------------------------------------------
my_train <- function(y, method) {
  mod <- train(
    x = X_train,
    y = y,
    method = method,
    tuneLength = 3,
    trControl = trainControl(method = 'cv', number = 5, verboseIter = TRUE)
  )
}

# Linear Regression
lr_math <- my_train(ym_train, 'lm')

# Random Forest
rf_math <- my_train(ym_train, 'ranger')

# Assess model performance
lr_pred <- predict(lr_math, X_test)
rf_pred <- predict(rf_math, X_test)

RMSE(lr_pred, ym_test)
RMSE(rf_pred, ym_test)










