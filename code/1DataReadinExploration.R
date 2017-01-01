#####################################
# Kaggle practice: Titanic data     #
#####################################
# Date: 01/01/17                    #
#####################################
# Author: Dan Kellett               #
#####################################

# Data has already been dowloaded from Kaggle to 'titanic/inputs'

# Set working directory
setwd("C:/Users/Oaklett/Desktop/Dan/Data Science/Kaggle/Titanic/data")

# Read-in data
train <- read.csv('C:/Users/Oaklett/Desktop/Dan/Data Science/Kaggle/Titanic/data/inputs/train.csv', stringsAsFactors = F)
test <- read.csv('C:/Users/Oaklett/Desktop/Dan/Data Science/Kaggle/Titanic/data/inputs/test.csv', stringsAsFactors = F)

summary(train)
str(train)
