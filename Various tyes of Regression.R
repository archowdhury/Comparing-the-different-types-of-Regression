# In this module we will evaluate the different kinds of Regression

# 1) OLS Regession
# 2) Ridge Regession
# 3) LASSO Regession
# 4) Elastic Nets

# The data for this will be "langley" which is one of the inbuilt datasets in R

library(glmnet)  # required for Ridge and Elastic Net
library(lars)    # required for LASSO


#  Function to calculate MSE
MSE = function(df1, df2)
{
  mse = mean((df1-df2)^2) * 100
  return (mse)
}


# Read in the data
data("longley")
train = longley



#---------------------------------------------------------#
#   OLS REGRESSION                                        #
#---------------------------------------------------------#

modOLS = lm(Employed~.,data=train)
summary(modOLS)

pred_OLS = predict(modOLS, data=train)
MSE(train$Employed, pred_OLS)


#---------------------------------------------------------#
#   RIDGE REGRESSION                                      #
#---------------------------------------------------------#

# Extract the target andpredictors as matrices
target = as.matrix(train[,7])
predictors = as.matrix(train[,1:6])

modRidge = glmnet(predictors, target, family="gaussian", alpha=0, lambda=0.001)
summary(modRidge)
modRidge

pred_Ridge = predict(modRidge, predictors)
MSE(target, pred_Ridge)


#---------------------------------------------------------#
# LASSO - LEAST ABSOLUTE SHRINKAGE AND SELECTION OPERATOR #
#---------------------------------------------------------#

# Extract the target andpredictors as matrices
target = as.matrix(train[,7])
predictors = as.matrix(train[,1:6])

modLASSO = lars(predictors, target, type="lasso")
summary(modLASSO)

best_step = modLASSO$df[which.min(modLASSO$RSS)]

pred_LASSO = predict(modLASSO, predictors, s=best_step, type="fit")$fit
MSE(target, pred_LASSO)


#---------------------------------------------------------#
# ELASTIC NET                                             #
#---------------------------------------------------------#

# Extract the target andpredictors as matrices
target = as.matrix(train[,7])
predictors = as.matrix(train[,1:6])

modElastic = glmnet(predictors, target, family="gaussian", alpha=0.5, lambda=0.001)
summary(modElastic)

pred_Elastic = predict(modElastic, predictors, type="link")
MSE(target, pred_Elastic)



#---------------------------------------------------------#
# SELECTING THE BEST MODEL                                #
#---------------------------------------------------------#

# Comparing the MSE values gives the following results

# OLS     - 5.22765
# Ridge   - 5.919831
# LASSO   - 6.400169
# Elastic - 5.90839

# So the best model of all these as per MSE is our simple OLS :)
