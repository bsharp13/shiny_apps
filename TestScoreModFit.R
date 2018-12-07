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
table(scores$ParentEd)
table(scores$Lunch)
table(scores$Pre)

ggplot(scores, aes(x = Math)) + geom_histogram(binwidth = 5)
ggplot(scores, aes(x = Read)) + geom_histogram(binwidth = 5)
ggplot(scores, aes(x = Write)) + geom_histogram(binwidth = 5)

# Test/train split
set.seed(314)
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
set.seed(314)
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

math_scores <- c('LR' = RMSE(lr_pred, ym_test), 'RF' = RMSE(rf_pred, ym_test))
if (math_scores['LR'] <= math_scores['RF']) {
  math_mod <- lr_math
} else {
  math_mod <- rf_math
} 

#-------------------------------------------------------------------------------

# Reading Scores
#-------------------------------------------------------------------------------
set.seed(314)

# Linear Regression
lr_read <- my_train(yr_train, 'lm')

# Random Forest
rf_read <- my_train(yr_train, 'ranger')

# Assess model performance
lr_pred <- predict(lr_read, X_test)
rf_pred <- predict(rf_read, X_test)

read_scores <- c('LR' = RMSE(lr_pred, yr_test), 'RF' = RMSE(rf_pred, yr_test))
if (read_scores['LR'] <= read_scores['RF']) {
  read_mod <- lr_math
} else {
  read_mod <- rf_math
} 
#-------------------------------------------------------------------------------

# Reading Scores
#-------------------------------------------------------------------------------
set.seed(314)

# Linear Regression
lr_write <- my_train(yw_train, 'lm')

# Random Forest
rf_write <- my_train(yw_train, 'ranger')

# Assess model performance
lr_pred <- predict(lr_write, X_test)
rf_pred <- predict(rf_write, X_test)

write_scores <- c('LR' = RMSE(lr_pred, yw_test), 'RF' = RMSE(rf_pred, yw_test))
if (write_scores['LR'] <= write_scores['RF']) {
  write_mod <- lr_math
} else {
  write_mod <- rf_math
} 
#-------------------------------------------------------------------------------

# Save model objects
#-------------------------------------------------------------------------------
save(math_mod, file = 'MathModel.rda')
save(read_mod, file = 'ReadModel.rda')
save(write_mod, file = 'WriteModel.rda')




