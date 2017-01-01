#####################################
# 2ImputationFeatureCreation        #
# Kaggle practice: Titanic data     #
#####################################
# Date: 01/01/17                    #
#####################################
# Author: Dan Kellett               #
#####################################

# Note this code draws heavily from the souptonuts repo from wehrley:
# https://github.com/wehrley/wehrley.github.io/blob/master/SOUPTONUTS.md

summary(train$Age)

head(train$Name, n=10L)

## function for extracting honorific (i.e. title) from the Name feature

getTitle <- function(data) {
  title.dot.start <- regexpr("\\,[A-Z ]{1,20}\\.", data$Name, TRUE)
  title.comma.end <- title.dot.start 
  + attr(title.dot.start, "match.length")-1
  data$Title <- substr(data$Name, title.dot.start+2, title.comma.end-1)
  return (data$Title)
}  


titleDotStart <- regexpr("\\,[A-Z ]{1,20}\\.", train$Name, TRUE)
titleCommaEnd <- titleDotStart + attr(titleDotStart, "match.length")-1
train$Title <- substr(train$Name, titleDotStart+2, titleCommaEnd-1)

unique(train$Title)

# Create a lookup for all the titles with missing volumne & means/medians

options(digits=2)
require(Hmisc)
bystats(train$Age, train$Title, 
        fun=function(x)c(Mean=mean(x),Median=median(x)))
titles.na.train <- c("Dr", "Master", "Mrs", "Miss", "Mr")

# Impute to meadian based on each title

imputeMedian <- function(impute.var, filter.var, var.levels) {
  for (v in var.levels) {
    impute.var[ which( filter.var == v)] <- impute(impute.var[ 
      which( filter.var == v)])
  }
  return (impute.var)
}

train$Age <- imputeMedian(train$Age, train$Title, titles.na.train)

# Impute missings in embarkment location with 'S' as most popular

train$Embarked[which(is.na(train$Embarked))] <- 'S'

# Impute missings on Fare feature with median fare by Pclass

train$Fare[ which( train$Fare == 0 )] <- NA
train$Fare <- imputeMedian(train$Fare, train$Pclass, as.numeric(levels(train$Pclass)))

## function for assigning a new title value to old title(s) 

changeTitles <- function(data, old.titles, new.title) {
  for (honorific in old.titles) {
    data$Title[ which( data$Title == honorific)] <- new.title
  }
  return (data$Title)
}

# Title consolidation

train$Title <- changeTitles(train, c("Capt", "Col", "Don", "Dr", "Jonkheer", "Lady", "Major", 
                                 "Rev", "Sir"), "Noble")
train$Title <- changeTitles(train, c("the Countess", "Ms"), "Mrs")
train$Title <- changeTitles(train, c("Mlle", "Mme"), "Miss")
train$Title <- as.factor(train$Title)

summary(train$Title)

## test a character as an EVEN single digit
isEven <- function(x) x %in% c("0","2","4","6","8") 
## test a character as an ODD single digit
isOdd <- function(x) x %in% c("1","3","5","7","9") 

## function to add features to training or test data frames
featureEngrg <- function(data) {
  ## Using Fate ILO Survived because term is shorter and just sounds good
  data$Fate <- data$Survived
  ## Revaluing Fate factor to ease assessment of confusion matrices later
  data$Fate <- revalue(data$Fate, c("1" = "Survived", "0" = "Perished"))
  ## Boat.dibs attempts to capture the "women and children first"
  ## policy in one feature.  Assuming all females plus males under 15
  ## got "dibs' on access to a lifeboat
  data$Boat.dibs <- "No"
  data$Boat.dibs[which(data$Sex == "female" | data$Age < 15)] <- "Yes"
  data$Boat.dibs <- as.factor(data$Boat.dibs)
  ## Family consolidates siblings and spouses (SibSp) plus
  ## parents and children (Parch) into one feature
  data$Family <- data$SibSp + data$Parch
  ## Fare.pp attempts to adjust group purchases by size of family
  data$Fare.pp <- data$Fare/(data$Family + 1)
  ## Giving the traveling class feature a new look
  data$Class <- data$Pclass
  data$Class <- revalue(data$Class, 
                        c("1"="First", "2"="Second", "3"="Third"))
  ## First character in Cabin number represents the Deck 
  data$Deck <- substring(data$Cabin, 1, 1)
  data$Deck[ which( is.na(data$Deck ))] <- "UNK"
  data$Deck <- as.factor(data$Deck)
  ## Odd-numbered cabins were reportedly on the port side of the ship
  ## Even-numbered cabins assigned Side="starboard"
  data$cabin.last.digit <- str_sub(data$Cabin, -1)
  data$Side <- "UNK"
  data$Side[which(isEven(data$cabin.last.digit))] <- "port"
  data$Side[which(isOdd(data$cabin.last.digit))] <- "starboard"
  data$Side <- as.factor(data$Side)
  data$cabin.last.digit <- NULL
  return (data)
}

## add remaining features to training data frame
train <- featureEngrg(train)

train.keeps <- c("Fate", "Sex", "Boat.dibs", "Age", "Title", 
                 "Class", "Deck", "Side", "Fare", "Fare.pp", 
                 "Embarked", "Family")
trainMunged <- train[train.keeps]
