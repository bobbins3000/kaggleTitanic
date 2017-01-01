#####################################
# 5ModelEvaluation                  #
# Kaggle practice: Titanic data     #
#####################################
# Date: 01/01/17                    #
#####################################
# Author: Dan Kellett               #
#####################################

# Note this code draws heavily from the souptonuts repo from wehrley:
# https://github.com/wehrley/wehrley.github.io/blob/master/SOUPTONUTS.md

# Confusion matrices

glm.pred <- predict(glm.tune.5, test.batch)
confusionMatrix(glm.pred, test.batch$Fate)

ada.pred <- predict(ada.tune, test.batch)
confusionMatrix(ada.pred, test.batch$Fate)

rf.pred <- predict(rf.tune, test.batch)
confusionMatrix(rf.pred, test.batch$Fate)

svm.pred <- predict(svm.tune, test.batch)
confusionMatrix(svm.pred, test.batch$Fate)

# Plot perfromance

## Logistic regression model (BLACK curve)
glm.probs <- predict(glm.tune.5, test.batch, type = "prob")
glm.ROC <- roc(response = test.batch$Fate,
               predictor = glm.probs$Survived,
               levels = levels(test.batch$Fate))
plot(glm.ROC, type="S")   
## Area under the curve: 0.892 
## Boosted model (GREEN curve)
ada.probs <- predict(ada.tune, test.batch, type = "prob")
ada.ROC <- roc(response = test.batch$Fate,
               predictor = ada.probs$Survived,
               levels = levels(test.batch$Fate))
plot(ada.ROC, add=TRUE, col="green")    
## Area under the curve: 0.887
## Random Forest model (RED curve)
rf.probs <- predict(rf.tune, test.batch, type = "prob")
rf.ROC <- roc(response = test.batch$Fate,
              predictor = rf.probs$Survived,
              levels = levels(test.batch$Fate))
plot(rf.ROC, add=TRUE, col="red") 
## Area under the curve: 0.879
## SVM model (BLUE curve)
svm.probs <- predict(svm.tune, test.batch, type = "prob")
svm.ROC <- roc(response = test.batch$Fate,
               predictor = svm.probs$Survived,
               levels = levels(test.batch$Fate))
plot(svm.ROC, add=TRUE, col="blue",legend(0.4,0.4,c("Logistic","GBM","Random Forest","SVM"), 
                                          lty=c(1,1), lwd=c(2.5,2.5),col=c("black","green","red","blue")))
## Area under the curve: 0.894
