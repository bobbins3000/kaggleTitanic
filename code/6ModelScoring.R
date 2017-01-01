#####################################
# 6ModelScoring                     #
# Kaggle practice: Titanic data     #
#####################################
# Date: 01/01/17                    #
#####################################
# Author: Dan Kellett               #
#####################################

# Note this code draws heavily from the souptonuts repo from wehrley:
# https://github.com/wehrley/wehrley.github.io/blob/master/SOUPTONUTS.md

# Score out logictis regression model 5

# get titles
titleDotStart <- regexpr("\\,[A-Z ]{1,20}\\.", test$Name, TRUE)
titleCommaEnd <- titleDotStart + attr(titleDotStart, "match.length")-1
test$Title <- substr(test$Name, titleDotStart+2, titleCommaEnd-1)

# impute missing Age values
test$Title <- changeTitles(test, c("Dona", "Ms"), "Mrs")
titles.na.test <- c("Master", "Mrs", "Miss", "Mr")
test$Age <- imputeMedian(test$Age, test$Title, titles.na.test)

# consolidate titles
test$Title <- changeTitles(test, c("Col", "Dr", "Rev"), "Noble")
test$Title <- changeTitles(test, c("Mlle", "Mme"), "Miss")
test$Title <- as.factor(test$Title)

# impute missing fares
test$Fare[ which( test$Fare == 0)] <- NA
test$Fare <- imputeMedian(test$Fare, test$Pclass, 
                              as.numeric(levels(test$Pclass)))
# add the other features
test <- featureEngrg(test)

# data prepped for casting predictions
test.keeps <- train.keeps[-1]
pred.these <- test[test.keeps]

# use the logistic regression model to generate predictions
Survived <- predict(glm.tune.5, newdata = pred.these)

# reformat predictions to 0 or 1 and link to PassengerId in a data frame
Survived <- revalue(Survived, c("Survived" = 1, "Perished" = 0))
predictions <- as.data.frame(Survived)
predictions$PassengerId <- test$PassengerId

# write predictions to csv file for submission to Kaggle
write.csv(predictions[,c("PassengerId", "Survived")], 
          file="Titanic_predictions.csv", row.names=FALSE, quote=FALSE)