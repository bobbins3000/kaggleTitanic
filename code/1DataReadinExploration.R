#####################################
# Kaggle practice: Titanic data     #
#####################################
# Date: 01/01/17                    #
#####################################
# Author: Dan Kellett               #
#####################################

# Note this code draws heavily from the souptonuts repo from wehrley:
# https://github.com/wehrley/wehrley.github.io/blob/master/SOUPTONUTS.md

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

testRaw <- readData(titanicPath, testName, 
                     testColumnTypes, missing)



