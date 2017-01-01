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

titleDotStart <- regexpr("\\,[A-Z ]{1,20}\\.", train$Name, TRUE)
titleCommaEnd <- titleDotStart + attr(titleDotStart, "match.length")-1
train$Title <- substr(train$Name, titleDotStart+2, titleCommaEnd-1)

unique(train$Title)

options(digits=2)
require(Hmisc)
bystats(df.train$Age, df.train$Title, 
        fun=function(x)c(Mean=mean(x),Median=median(x)))