#Loading the file and testing the structure
credit <- read.csv("credit.csv")
str(credit)
table(credit$checking_balance)
table(credit$savings_balance)

#Checking the summary
summary(credit$months_loan_duration)
summary(credit$amount)

#Check whether the data has default applicants
table(credit$default)
set.seed(123)

#Now we will select 900 values
train_sample <- sample (1000,900)
str(train_sample)

# Creating the Training dataset and the Testing dataset
credit_train <- credit[train_sample,]
credit_test <- credit[-train_sample,]

# Checking for equal defaulted loans in each dataset
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

# Installing the C50 package
install.packages("C50")
library(C50)

#Now we will create the model and checking the tree using summary()
credit_model <- C5.0(credit_train[-17],factor(credit_train$default))
credit_model
summary(credit_model)

#Applying decision tree to test  dataset
credit_pred <- predict(credit_model,credit_test)
library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,
           dnn = c('actual default','predicted default'))

# Now the decision tree accuracy will be increased or boost with following command
credit_boost10 <- C5.0(credit_train[-17],factor(credit_train$default), trials = 10)
credit_boost10
summary(credit_boost10)

credit_boost_pred10 <- predict(credit_boost10,credit_test)

CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,
           dnn = c('actual default','predicted default'))

#Panelty for different errors
matrix_dimensions <- list(c("no","yes"),c("no","yes"))
names(matrix_dimensions) <- c("predicted","actual")

#Now we will assume a loan default will cost 4 times as compared to missed opportunity
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2)
error_cost

#Applying error cost to decision tree
credit_cost <- C5.0(credit_train[-17], factor(credit_train$default), costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, 
           dnn = c('actual default', 'predicted default'))