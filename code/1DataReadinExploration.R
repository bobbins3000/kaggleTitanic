#####################################
# 1DataReadinExploration            #   
# Kaggle practice: Titanic data     #
#####################################
# Date: 01/01/17                    #
#####################################
# Author: Dan Kellett               #
#####################################

# Note this code draws heavily from the souptonuts repo from wehrley:
# https://github.com/wehrley/wehrley.github.io/blob/master/SOUPTONUTS.md

# Run libraries
library(Amelia)
library(corrgram)
library(Hmisc)
library(ggplot2)
library(plyr) 
library(stringr)
library(caret)

# Data has already been loaded from Kaggle to 'titanic/inputs' to my GitHub repo

# Create a generic .csv read-in function
readData <- function(pathName, fileName, columnTypes, missingTypes) {
  read.csv( url( paste(pathName, fileName, sep="") ), 
            colClasses=columnTypes,
            na.strings=missingTypes )
}

# Set working directory
setwd("C:/Users/Oaklett/Desktop/Dan/Data Science/Kaggle/Titanic/data")

# Read-in data
titanicPath <- "http://raw.github.com/bobbins3000/kaggleTitanic/master/data/inputs/"
trainName <- "train.csv"
testName <- "test.csv"
missing <- c("NA", "")
trainColumnTypes <- c('integer',   # PassengerId
                        'factor',    # Survived 
                        'factor',    # Pclass
                        'character', # Name
                        'factor',    # Sex
                        'numeric',   # Age
                        'integer',   # SibSp
                        'integer',   # Parch
                        'character', # Ticket
                        'numeric',   # Fare
                        'character', # Cabin
                        'factor'     # Embarked
)
testColumnTypes <- trainColumnTypes[-2] 

trainRaw <- readData(titanicPath, trainName, 
                      trainColumnTypes, missing)
train <- trainRaw

testRaw <- readData(titanicPath, testName, 
                     testColumnTypes, missing)
test <- testRaw

# Exploration of missing data using Amelia package
## map missing data by provided feature
missmap(train, main="Kaggle Titanic Data - Missings Map (train only)", 
        col=c("white", "blue"), legend=FALSE)

# Explore model inputs
barplot(table(train$Survived),
        names.arg = c("Perished", "Survived"),
        main="Survived (passenger fate)", col="black")
barplot(table(train$Pclass), 
        names.arg = c("first", "second", "third"),
        main="Pclass (passenger traveling class)", col="firebrick")
barplot(table(train$Sex), main="Sex (gender)", col="darkviolet")
hist(train$Age, main="Age", xlab = NULL, col="brown")
barplot(table(train$SibSp), main="SibSp (siblings + spouse aboard)", 
        col="darkblue")
barplot(table(train$Parch), main="Parch (parents + kids aboard)", 
        col="gray50")
hist(train$Fare, main="Fare (fee paid for ticket[s])", xlab = NULL, 
     col="darkgreen")
barplot(table(train$Embarked), 
        names.arg = c("Cherbourg", "Queenstown", "Southampton"),
        main="Embarked (port of embarkation)", col="chartreuse4")

# Mosaic plots for fate by class/gender

mosaicplot(train$Pclass ~ train$Survived, 
           main="Passenger Fate by Traveling Class", shade=FALSE, 
           color=TRUE, xlab="Pclass", ylab="Survived")

mosaicplot(train$Sex ~ train$Survived, 
           main="Passenger Fate by Gender", shade=FALSE, color=TRUE, 
           xlab="Sex", ylab="Survived")

mosaicplot(train$Embarked ~ train$Survived, 
           main="Passenger Fate by Port of Embarkation",
           shade=FALSE, color=TRUE, xlab="Embarked", ylab="Survived")

# Boxplots for age
boxplot(train$Age ~ train$Survived, 
        main="Passenger Fate by Age",
        xlab="Survived", ylab="Age")

# Create corregram for the data

corrgramData <- train
## change features of factor type to numeric type for inclusion on correlogram
corrgramData$Survived <- as.numeric(corrgramData$Survived)
corrgramData$Pclass <- as.numeric(corrgramData$Pclass)
corrgramData$Embarked <- revalue(corrgramData$Embarked, 
                                  c("C" = 1, "Q" = 2, "S" = 3))
## generate correlogram
corrgramVars <- c("Survived", "Pclass", "Sex", "Age", 
                   "SibSp", "Parch", "Fare", "Embarked")
corrgram(corrgramData[,corrgramVars], order=FALSE, 
         lower.panel=panel.ellipse, upper.panel=panel.pie, 
         text.panel=panel.txt, main="Titanic Training Data")
