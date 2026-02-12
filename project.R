# Final Project
# Libraries needed
library(MASS) 
library(ISLR2)
library(corrplot)
library(e1071)
library(glmnet)
library(car)
library(boot) 
library(leaps)


head(Hitters) #peak into the data (var names, values, etc)
dim(Hitters) #dimensions of the og dataset

table(is.na(Hitters)) #number of na atributes
data<-na.omit(Hitters) #eliminate the na's
table(is.na(data)) # we check that there are not na's left
data<-data[1:(dim(data)[1]-15),-c(14,15,20)] #We remove the last 15 obs and the 3 class variables using their col index
head(data) # A peak of the final dataset
dim(data) # We check the final dim of the dataset
n<-dim(data)[1]

pairs(data) #Scatter plot (too big to see anything)
cor(data)[,17] #corr matrix so we can se the var with the response var
corrplot(cor(data), method = "circle") #Makes a heatmap matrix of the corr, easy to visualize

# We need to check other variables corr because it's very likely a problem of multicollinearity
boxplot(scale(data)) # we can see that there are still some outliers after we scaled the var in the boxplot

hist(data$Salary) # we can check the distrib of the response var. (doesn't look normal)
# looking at the skewness coef it's >1 which means it has a long right tail
sapply(data[,1:17],skewness) #  which means -> violates normality assumption => We need to do a transformation
# Identify the columns to transform (vars with skewness > 0.6)
cols_to_transform <- c(3, 7:17)
# Apply log transformation
data[ , cols_to_transform] <- lapply(data[ , cols_to_transform], function(x) log(x + 1))
# Rename the transformed columns with "log_" prefix so we know they are modified
names(data)[cols_to_transform] <- paste0("log_", names(data)[cols_to_transform])
# W check the new values for the skewness
sapply(data[,1:17],skewness) #PutOuts still bad because it has lots of outliers
plot(data$log_PutOuts) #As we can see here

#Before we start we need to scale the data because the pred var are measured in dif units
scaled_data<-data #So we can keep the original transf data
scaled_data[,1:16]<-scale(scaled_data[,1:16]) # The response var doesn't need to be scaled
scaled_data<-as.data.frame(scaled_data)

boxplot(scaled_data, las=2) #There are still some outliers but the variance of the vars looks better
hist(scaled_data$log_Salary) # Histogram of the transf response var
hist(data$log_PutOuts) # Outliers still mess with this pred var














# PART 1
# Variable selection method 1: Step wise mixed

## First we want to have a look to the nº of var we should select
reg.summary<-summary(regsubsets(log_Salary~.,data=scaled_data ,nvmax=16))

# I'll plot different measures and point their best values
par(mfrow=c(2,2))
plot(reg.summary$rss ,xlab="Number of Variables",ylab="RSS",type="l")

plot(reg.summary$adjr2 ,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)

plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(10,reg.summary$cp [10],col="red",cex=2,pch=20)

which.min(reg.summary$bic)
plot(reg.summary$bic ,xlab="Number of Variables",ylab="BIC",type='l')
points(6,reg.summary$bic [6],col="red",cex=2,pch=20)

# Full model
full_model <- lm(log_Salary ~ ., data = scaled_data)
# Stepwise selection using BIC penalty (more conservative than AIC)
step_bic <- stepAIC(full_model,
                    direction = "backward",
                    k = log(nrow(scaled_data)),  # <-- BIC penalty
                    trace = FALSE)
summary(step_bic)
vif(step_bic) # Big multicollinearity problems


# Variable selection method 2: Lasso Regresion
# Build model matrices
x <- model.matrix(log_Salary ~ ., data = scaled_data)[, -1]
y <- scaled_data$log_Salary
# Cross-validated LASSO
lasso_cv <- cv.glmnet(x, y, alpha = 1)  # alpha=1 for LASSO
lasso_cv$lambda.min #Best lambda choosen by CV
# Plot cross-validation curve
par(mfrow = c(1, 2))
plot(lasso_cv)
plot(glmnet(x,y, alpha=1), xvar="dev")
plot(glmnet(x,y, alpha=1), xvar="lambda")
plot(glmnet(x,y, alpha=1))
# Extract best lambda
lasso_cv$lambda.min
lasso_cv$lambda.1se
# Coefficients for optimal lambda
coef(lasso_cv, s = "lambda.min")
coef(lasso_cv, s = "lambda.1se")   # simpler model
# suggested models
lmin<-lm(log_Salary ~ Hits+log_HmRun+Runs+RBI+Walks+log_Years+log_CHmRun+log_PutOuts, data = scaled_data)
l1se<-lm(log_Salary ~ Hits+log_CRuns+log_CRBI, data = scaled_data)
summary(lmin)$adj.r.squared
summary(l1se)$adj.r.squared
vif(lmin)# We can make it simpler
vif(l1se) #Not good, multicollinearity problems



#Fitted Model: Taking into account both methods results
# Final model includes variables selected by both stepwise (BIC) and LASSO methods
part1Model<-lm(log_Salary ~ log_HmRun+Runs+log_Years+log_CHmRun+log_PutOuts, data = scaled_data)
summary(part1Model)

#Scatter plots
pairs(log_Salary ~ log_HmRun+Runs+log_Years+log_CHmRun+log_PutOuts, data = scaled_data)

#Parameters Estimation:
part1Model$coefficients #OLS estimates of the reg coef
confint(part1Model, level = 0.95) #Conf Intervals 95% for the OLS estim
# Goodness of fit:
summary(part1Model)$r.squared          # R²
summary(part1Model)$adj.r.squared      # Adjusted R²
summary(part1Model)$sigma              # Residual Standard Error
summary(part1Model)$fstatistic         # F-statistic
AIC(part1Model)                        # Akaike Information Criterion
BIC(part1Model)                        # Bayesian Information Criterion

# Robustness Analysis
par(mfrow = c(2, 2))
plot(part1Model)  # Residuals, leverage, normality
vif(part1Model) # Multicollinearity check
anova(part1Model)["Residuals", "Mean Sq"] # MSE of the model
# Plot dfbetas
B<-glm(log_Salary ~ log_HmRun+Runs+log_Years+log_CHmRun+log_PutOuts, data = scaled_data)
dfb<-(dfbetas(B)) # I use the dfbetas because it's the standarize version
boxplot(dfb, ylim=c(-1,1)) 
abline(h = c(-0.5,0.5), col='red')# We can see some values surpassing 0.5 
# pred error:
set.seed(123)
train_idx <- sample(1:nrow(scaled_data), size = 0.8 * n)
train <- scaled_data[train_idx, ]
test <- scaled_data[-train_idx, ]
model <- lm(log_Salary ~ log_HmRun+Runs+log_Years+log_CHmRun+log_PutOuts, data = train)
pred<-predict(model, newdata = test)
sqrt(mean((test$log_Salary - pred)^2)) # rmse


# CLEAN DATASET
# Detect extreme outliers (studentized residuals)
rstudent_vals <- rstudent(part1Model)
outliers <- which(abs(rstudent_vals) > 3)
# Detect high leverage points (hat values)
leverage_vals <- hatvalues(part1Model)
leverage_threshold <- 3 * mean(leverage_vals)
high_leverage <- which(leverage_vals > leverage_threshold)
# Detect influential points (Cook's Distance)
cooks_vals <- cooks.distance(part1Model)
cooks_threshold <- 1
influential <- which(cooks_vals > cooks_threshold)
# Combine flagged indices (only extreme cases)
flagged <- unique(c(outliers, high_leverage, influential))
# Create clean dataset
clean_data <- scaled_data[-flagged, ]

#Now I fit the model but with the clean data, and it has improved a lot
part1ModelC<-lm(log_Salary ~ log_HmRun+Runs+log_Years+log_CHmRun+log_PutOuts, data = clean_data)
summary(part1ModelC)
par(mfrow = c(2, 2))
plot(part1ModelC)  # Here we can compare Residuals, leverage, normality
# Plot dfbetas
B<-glm(log_Salary ~ log_HmRun+Runs+log_Years+log_CHmRun+log_PutOuts, data = clean_data)
dfb<-(dfbetas(B)) # I use the dfbetas because it's the standarize version
boxplot(dfb, ylim=c(-1,1)) 
abline(h = c(-0.5,0.5), col='red')# We can see some values surpassing 0.5 
anova(part1ModelC)["Residuals", "Mean Sq"] # MSE of the model with out extreme values
# pred err of the clean data
set.seed(123)
train_idx <- sample(1:nrow(clean_data), size = 0.8 * nrow(clean_data))
train <- clean_data[train_idx, ]
test <- clean_data[-train_idx, ]
model <- lm(log_Salary ~ log_HmRun+Runs+log_Years+log_CHmRun+log_PutOuts, data = train)
pred <- predict(model, newdata = test)
sqrt(mean((test$log_Salary - pred)^2))


# PART 2:
# Using the Ridge model as regulariz mth and the same model as part 1
x <- model.matrix(log_Salary ~ log_HmRun+Runs+log_Years+log_CHmRun+log_PutOuts, data = scaled_data)[, -1]
y <- scaled_data$log_Salary
# Cross-validated RIDGE
ridge_cv <- cv.glmnet(x, y, alpha = 0)  # alpha=0 for Ridge
ridge_cv$lambda.min# Lambda value obtained by cv
# Representations of the lambda selection
plot(ridge_cv)
plot(glmnet(x,y, alpha=0))
plot(glmnet(x,y, alpha=0), xvar="lambda")
plot(glmnet(x,y, alpha=0), xvar="dev")
#Parameters comparison
coef(ridge_cv, s = "lambda.min") # beta parameters estimated
part1Model$coefficients #OLS estimates of the reg coef
# Now for clean data
xc <- model.matrix(log_Salary ~ log_HmRun+Runs+log_Years+log_CHmRun+log_PutOuts, data = clean_data)[, -1]
yc <- clean_data$log_Salary
# Cross-validated RIDGE
ridge_cvC <- cv.glmnet(xc, yc, alpha = 0)  # alpha=0 for Ridge
coef(ridge_cvC, s = "lambda.min") # beta parameters estimated
part1ModelC$coefficients #OLS estimates of the reg coef
