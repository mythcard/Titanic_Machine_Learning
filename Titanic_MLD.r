#After reading data into train and test
train <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv")
test <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv")

#people in your training set survived the disaster with the Titanic
# absolute numbers
table(train$Survived)
# percentages
prop.table(table(train$Survived))

# Males & females that survived vs males & females that passed away

table(train$Sex,train$Survived)

prop.table(table(train$Sex,train$Survived),1)


# Create the column child, and indicate whether child or no child

train$Child <- NA

train$Child[train$Age < 18] <- 1

train$Child[train$Age >= 18] <- 0




# Two-way comparison (row wise comparison)
prop.table(table(train$Child,train$Survived), 1)

#While less obviously than gender, age does seems to have an impact on chance of survival.

#Loaded first submission into kaggle with test set, test set having a column called survived with data survival based on gender
# meaning female gender having more chances of survival

#Introduction to Deision trees
# Load in the R package  
#To create your first decision tree, you make use of R's rpart package

install.packages("rpart")

library(rpart)



# Build the decision tree

my_tree_two <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,data=train, method="class")



# Visualize the decision tree using plot() and text()

plot(my_tree_two)

text(my_tree_two)



# Load in the packages to create a fancified version of your tree

library(rattle)

library(rpart.plot)

library(RColorBrewer)



# Time to plot your fancified tree

fancyRpartPlot(my_tree_two)


# Make your prediction using the test set

my_prediction <- predict(my_tree_two, test, type = "class")



# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions

my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)



# Check that your data frame has 418 entries

nrow(my_solution)



# Write your solution to a csv file with the name my_solution.
csv
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)

#Maybe we can improve even more by making a complexer model? In rpart, the depth of our model is defined by two parameters:

#1. the cp parameter determines when the splitting up of the decision tree stops.
#2. the minsplit parameter monitors the amount of observations in a bucket. If a certain threshold is not reached (e.g minimum 10 passengers) no further splitting can be done.



#Create a new decision tree my_tree_three

my_tree_three <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,data=train, method="class", control = rpart.control(minsplit = 50, cp = 0))
  


# Visualize your new decision tree

fancyRpartPlot(my_tree_three)

#Re-engineering our titanic data set
#Data Science is an art that benefits from a human element. Enter feature engineering: creatively engineering your own features by combining the different existing variables.
#While feature engineering is a discipline in itself, too broad to be covered here in detail, you will have a look at a simple example by creating your own new predictive attribute: family_size.
#A valid assumption is that larger families need more time to get together on a sinking ship, and hence have less chance of surviving. Family size is determined by the variables SibSp and Parch, which indicate the number of family members a certain passenger is traveling with. So when doing feature engineering, you add a new variable family_size, which is the sum of SibSp and Parch plus one (the observation itself), to the test and train set.



# create a new train set with the new variable

train_two <- train

train_two$family_size <- train_two$SibSp + train_two$Parch + 1



# Create a new decision tree my_tree_three

my_tree_four <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size,data=train_two, method="class", control = rpart.control(minsplit = 50, cp = 0))
 
  


# Visualize your new decision tree

fancyRpartPlot(my_tree_four)


### Based on new column title

# Create a new model `my_tree_five`

my_tree_five <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title,data=train_new, method="class")



# Visualize your new decision tree

fancyRpartPlot(my_tree_five)



# cleaning test data (Don with a space)
test$title[415] <- " Don"

# Make your prediction using `my_tree_five` and `test_new`

my_prediction <- predict(my_tree_five, test_new, type = "class")



# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions

my_solution <- data.frame(PassengerId = test_new$PassengerId, Survived = my_prediction)



# Write your solution away to a csv file with the name my_solution.
csvwrite.csv(my_solution, file = "my_solution.csv", row.names = FALSE)



######################get str function  #############################

getstr = function(mystring, initial.character, final.character)
{
 
     # check that all 3 inputs are character variables
     
     if (!is.character(mystring))
     {
          stop('The parent string must be a character variable.')
     }
 
     if (!is.character(initial.character))
     {
          stop('The initial character must be a character variable.')
     }
 
 
     if (!is.character(final.character))
     {
          stop('The final character must be a character variable.')
     }

  
 
     # pre-allocate a vector to store the extracted strings
     snippet = rep(0, length(mystring))
 
 
 
     for (i in 1:length(mystring))
     {
          # extract the initial position
          initial.position = gregexpr(initial.character, mystring[i])[[1]][1] + 1
  
          # extract the final position
          final.position = gregexpr(final.character, mystring[i])[[1]][1] - 1
 
          # extract the substring between the initial and final positions, inclusively
          snippet[i] = substr(mystring[i], initial.position, final.position)
     }
 
     return(snippet)
}

##########################################################################

#Feature scaling: Here we extract the title from the column name and generate a new column called title for both train and test sets

tit <- as.vector(train_new$Name)
mytest2 <- getstr(tit, ',', '\\.')
train_title <- NA
train$title <- mytest2

tit_test <- as.vector(test_new$Name)
mytest3 <- getstr(tit_test, ',', '\\.')
test$title <- NA
test$title <- mytest3 

# implememnt the decision tree on new title column in train and test datasets as implemented above 

#final write
write.csv(my_solution, file = "C:/Users/Admi/Documents/Coursera_R/my_solution.csv", row.names = FALSE) 