#####################################
# 3ModelBuilding                    #
# Kaggle practice: Titanic data     #
#####################################
# Date: 01/01/17                    #
#####################################
# Author: Dan Kellett               #
#####################################

# Note this code draws heavily from the souptonuts repo from wehrley:
# https://github.com/wehrley/wehrley.github.io/blob/master/SOUPTONUTS.md

# Split the train data into build anf holdout

set.seed(23)
training.rows <- createDataPartition(trainMunged$Fate, p = 0.8, list = FALSE)
train.batch <- trainMunged[training.rows, ]
test.batch <- trainMunged[-training.rows, ]

# Build logistic regression

Titanic.logit.1 <- glm(Fate ~ Sex + Class + Age + Family + Embarked, 
                       data = train.batch, family=binomial("logit"))

Titanic.logit.1

anova(Titanic.logit.1, test="Chisq")

# Run 3x 10-fold cross-validation

## Define control function to handle optional arguments for train function
## Models to be assessed based on largest absolute area under ROC curve
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 3,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)

set.seed(35)
glm.tune.1 <- train(Fate ~ Sex + Class + Age + Family + Embarked,
                    data = train.batch,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl)

glm.tune.1
summary(glm.tune.1)

# Re-run with an embarkment 1/0 flag for Southampton to reduce dof

set.seed(35)
glm.tune.2 <- train(Fate ~ Sex + Class + Age + Family + I(Embarked=="S"),
                      data = train.batch, method = "glm",
                      metric = "ROC", trControl = cv.ctrl)
summary(glm.tune.2)

# Add title

glm.tune.3 <- train(Fate ~ Sex + Class + Title + Age 
                      + Family + I(Embarked=="S"), 
                      data = train.batch, method = "glm",
                      metric = "ROC", trControl = cv.ctrl)
summary(glm.tune.3)

# Drop age and fine class titles

glm.tune.4 <- train(Fate ~ Class + I(Title=="Mr") + I(Title=="Noble") 
                      + Age + Family + I(Embarked=="S"), 
                      data = train.batch, method = "glm",
                      metric = "ROC", trControl = cv.ctrl)
summary(glm.tune.4)

# Add flag for third class men

glm.tune.5 <- train(Fate ~ Class + I(Title=="Mr") + I(Title=="Noble") 
                    + Age + Family + I(Embarked=="S") 
                    + I(Title=="Mr"&Class=="Third"), 
                    data = train.batch, 
                    method = "glm", metric = "ROC", 
                    trControl = cv.ctrl)
summary(glm.tune.5)
