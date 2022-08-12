# install.packages("leaps")
library(leaps)
# install.packages("faraway")
library(faraway)
# install.packages("dplyr")
library(dplyr)

# ----- Import Data ---------------------

loandata <- read.csv("loansData.csv")
head(loandata,n=20)
str(loandata) # look at which variables are numeric and factors
sum(is.na(loandata)) # number of blank values in the data

# ----- Data Cleaning -------------------

loandata <- na.omit(loandata) # remove NA's
unique(loandata$FICO.Range) # look at distinct FICO ranges and how many
loandata$FICO.Range <- as.numeric(loandata$FICO.Range) # change FICO ranges to numeric rankings
attach(loandata) # access variables without calling data frame

# ----- Build Stepwise Regression Models -------

# Use model.matrix function to create design matrix X
# This is necessary to convert factors into dummy variables
X <- model.matrix(lm(Interest.Rate ~., loandata))
head(X)
X <- X[,-1] # Remove intercept or column of 1's


######################
### FORWARD METHOD ###
######################

# Model selection using "forward" method
# Maximum number of variables to include = # of columns of design matrix X
regfwd <- regsubsets(Interest.Rate ~ X, loandata, method = "forward",nvmax=ncol(X))
regfwd.sum <- summary(regfwd)

regfwd.sum$which # matrix to show you the predictors in each model


# Examine adjusted R^2
regfwd.sum$adjr2 # A vector with adjusted r^2 values for models with 1, 2, ... variables
plot(regfwd.sum$adjr2,xlab="Number of variables",ylab="Adjusted RSq",type="l")
max_adjrsq = which.max(regfwd.sum$adjr2)   # Finds num. of variables that maximizes adj R^2
max_adjrsq # num. of variables that maximizes adj R^2
regfwd.sum$adjr2[max_adjrsq] # associated adj R^2
points(max_adjrsq,regfwd.sum$adjr2[max_adjrsq],col="red",cex=2,pch=20) # Add the maximum adj R^2 point to the plot
index1 <- regfwd.sum$which[max_rsq,][-1] # label the predictors in the model with max adj resq
index1
Xrsq <- X[,index1] # select the columns in X according to index
summary(lm(Interest.Rate ~ FICO.Range + Xrsq, data=loandata)) # summary of forward model which maximizes adj R^2


# Examine Mallow's Cp
regfwd.sum$cp # A vector with cp values for models with 1, 2, ... variables
plot(regfwd.sum$cp, xlab="Number of variables", ylab="Cross Validated Prediction", type="l")
min_cp = which.min(regfwd.sum$cp) # finds num. of variables that minimizes cp
min_cp # num. of variables that minimizes cp
regfwd.sum$cp[min_cp] # associated cp
points(min_cp,regfwd.sum$cp[min_cp],col="red",cex=2,pch=20,xlab="test")
index2 <- regfwd.sum$which[min_cp,][-1] # label the predictors in the model with min cp
index2
Xcp <- X[,index2] # select the columns in X according to index
summary(lm(Interest.Rate ~ FICO.Range + Xcp, data = loandata)) # summary of forward model which minimizes cp


######################
## BACKWARDS METHOD ##
######################

# Model selection using "backwards" method
# Maximum number of variables to include = # of columns of design matrix X
regbwd <- regsubsets(Interest.Rate ~ X, loandata, method = "backward",nvmax=ncol(X))
regbwd.sum <- summary(regbwd)

regbwd.sum$which # matrix to show you the predictors in each model


# Examine adjusted R^2
regbwd.sum$adjr2 # A vector with adjusted r^2 values for models with 1, 2, ... variables
plot(regbwd.sum$adjr2,xlab="Number of variables",ylab="Adjusted RSq",type="l")
max_adjrsq = which.max(regbwd.sum$adjr2)   # Finds num. of variables that maximizes adj R^2
max_adjrsq # num. of variables that maximizes adj R^2
regbwd.sum$adjr2[max_adjrsq] # associated adj R^2
points(max_adjrsq,regbwd.sum$adjr2[max_adjrsq],col="red",cex=2,pch=20) # Add the maximum adj R^2 point to the plot
index1 <- regbwd.sum$which[max_rsq,][-1] # label the predictors in the model with max adj resq
index1
Xrsq <- X[,index1] # select the columns in X according to index
summary(lm(Interest.Rate ~ FICO.Range + Xrsq, data=loandata)) # summary of backwards model which maximizes adj R^2


# Examine Mallow's Cp
regbwd.sum$cp # A vector with cp values for models with 1, 2, ... variables
plot(regbwd.sum$cp, xlab="Number of variables", ylab="Cross Validated Prediction", type="l")
min_cp = which.min(regbwd.sum$cp) # finds num. of variables that minimizes cp
min_cp # num. of variables that minimizes cp
regbwd.sum$cp[min_cp] # associated cp
points(min_cp,regbwd.sum$cp[min_cp],col="red",cex=2,pch=20,xlab="test")
index2 <- regbwd.sum$which[min_cp,][-1] # label the predictors in the model with min cp
index2
Xcp <- X[,index2] # select the columns in X according to index
summary(lm(Interest.Rate ~ FICO.Range + Xcp, data = loandata)) # summary of backwards model which minimizes cp


######################
## EXHAUSTIVE METHOD ##
######################

# Model selection using "exhaustive" method
# Maximum number of variables to include = # of columns of design matrix X
regexh <- regsubsets(Interest.Rate ~ X, loandata, method = "exhaustive",nvmax=ncol(X), really.big = TRUE)
regexh.sum <- summary(regexh)

regexh.sum$which # matrix to show you the predictors in each model


# Examine adjusted R^2
regexh.sum$adjr2 # A vector with adjusted r^2 values for models with 1, 2, ... variables
plot(regexh.sum$adjr2,xlab="Number of variables",ylab="Adjusted RSq",type="l")
max_adjrsq = which.max(regexh.sum$adjr2)   # Finds num. of variables that maximizes adj R^2
max_adjrsq # num. of variables that maximizes adj R^2
regexh.sum$adjr2[max_adjrsq] # associated adj R^2
points(max_adjrsq,regexh.sum$adjr2[max_adjrsq],col="red",cex=2,pch=20) # Add the maximum adj R^2 point to the plot
index1 <- regexh.sum$which[max_rsq,][-1] # label the predictors in the model with max adj resq
index1
Xrsq <- X[,index1] # select the columns in X according to index
summary(lm(Interest.Rate ~ FICO.Range + Xrsq, data=loandata)) # summary of exhaustive model which maximizes adj R^2


# Examine Mallow's Cp
regexh.sum$cp # A vector with cp values for models with 1, 2, ... variables
plot(regexh.sum$cp, xlab="Number of variables", ylab="Cross Validated Prediction", type="l")
min_cp = which.min(regexh.sum$cp) # finds num. of variables that minimizes cp
min_cp # num. of variables that minimizes cp
regexh.sum$cp[min_cp] # associated cp
points(min_cp,regexh.sum$cp[min_cp],col="red",cex=2,pch=20,xlab="test")
index2 <- regexh.sum$which[min_cp,][-1] # label the predictors in the model with min cp
index2
Xcp <- X[,index2] # select the columns in X according to index
summary(lm(Interest.Rate ~ FICO.Range + Xcp, data = loandata)) # summary of exhaustive model which minimizes cp


detach(loandata)


