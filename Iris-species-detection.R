#Problem Description: Iris Species Detection
#Inputs: 
#Output file: Display values on the Console 
#========================================

#========================================
# Step 1: Collect data
# A biologist detects the three species of Iris including setosa, versicolor, virginica 
# based on its the dimensional measurement.
#========================================


#========================================
# Step 2: Explore and prepare data 
#========================================
# 1. import data
iris_train <- read.csv("~/Desktop/cs5310/a1/Homework-1/iris_training.csv")
iris_test <- read.csv("~/Desktop/cs5310/a1/Homework-1/iris_test.csv")

# 2. explore data structure
str(iris_train)
str(iris_test)

## data consists of 4 characteristics of iris species: spedal length in cm, 
## spedal width in cm, petal length in cm, petal width in cm. Data types of these features are numeric.


# 3. drop id column 
iris_train <- iris_train[,-1]
iris_test <- iris_test[,-1]

# 4. class variables diagnosis - class is balanced
table(iris_train$Species)

# 5. class variables in percent 
round(prop.table(table(iris_train$Species)) * 100, digits = 1)

# 6. decode the diagnosis variables 
iris_train$Species <-factor(iris_train$Species)

str(iris_train_n) # double check the structure

# 7. explore the data again to see if we can perform analysis 
summary(iris_train)
summary(iris_test) 

## we need to normalize the features to ensure that 
## the feature that has larger sales does not dominate other features.

# 8. transform and normalize features
normalize <- function(x) {
  return ((x-min(x)) / (min(x) - max(x)))
}

iris_train_n <- iris_train
iris_train_n[, 1:4] <- as.data.frame(lapply(iris_train[,1:4], normalize))

iris_test_n <- iris_test
iris_test_n <- as.data.frame(lapply(iris_test_n[,1:4], normalize))

summary(iris_train_n) # double check after normalizing
summary(iris_test_n)

# 9. Data preparation: splitting up training data into training set and validation set
iris_train_x <- iris_train_n[1:84, ]
iris_train_y <- iris_train_n[85:105,]


#=================================
# Step-3: Train a model on the data 
#=================================
library(class) # for knn 

train_labels_x <- iris_train_x[,5] # create class labels
train_labels_y <- iris_train_y[, 5]
iris_pred_y <- knn(iris_train_x[,-5], iris_train_y[,-5], cl = train_labels_x, k = 9) # predicting using k = 10 
iris_pred_y

#=======================================
#  Step-4: Evaluate the model performance 
#=======================================
library("gmodels")
CrossTable(x = train_labels_y, y = iris_pred_y, prop.chisq = FALSE)

#=======================================
#  Step-5: Improving the model performance
#=======================================
# approach 1: apply multiple k values 
k_values=c(1, 3, 5, 7, 9, 10, 12, 14)
for (k in k_values) {
  iris_pred_y <- knn(iris_train_x[,-5], iris_train_y[,-5], cl = train_labels_x, k = 9)
  message("++++++++++++ k=", k, " +++++++++++++++")
  CrossTable(x = train_labels_y, y = iris_pred_y, prop.chisq = FALSE)
}

## we pick k = 1 since k-values we are testing on are producing the same result. The accuracy rate with k=1 is 100%.

# approach 2: doing extra step: normalize data using z-score
iris_train_z <- as.data.frame(scale(iris_train[-5])) # normalizing using scale()

iris_train_x <- iris_train_z[1:84, ]
iris_train_y <- iris_train_z[85:105,]

train_labels_x <- iris_train_z[,5] # create class labels
train_labels_y <- iris_train_y[, 5]
iris_pred_y <- knn(iris_train_x[,-5], iris_train_y[,-5], cl = train_labels_x, k = 9) # predicting using k = 10 
iris_pred_y

CrossTable(x = train_labels_y, y = iris_pred_y, prop.chisq = FALSE)


## we can see that the results from the min max normalization approach is slightly better 
## than z-score approach. Therefore, we will use the approach 1 for deployment.


#=======================================
#  Step-5: Deployment: predicting Iris Species on the test data set. 
#=======================================
iris_pred <- knn(iris_train_x[,-5], iris_test_n[,-5], cl = train_labels_x, k = 1) # predicting using k = 10 
iris_pred


#=================
# Submission
#=================
iris_test_submission <- read.csv("~/Desktop/cs5310/a1/Homework-1/iris_test.csv")
iris_test_submission$Species <- iris_pred
iris_test_submission <- iris_test_submission[,-2:-5]
write.csv(iris_test_submission, "~/Desktop/cs5310/a1/Homework-1/iris_test_submissionFormat.csv", row.names = TRUE)
