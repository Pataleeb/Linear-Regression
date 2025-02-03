library(faraway)
fat <- read.table(file = "fat.csv", sep=",", header=TRUE);
###summary stats 
sapply(fat, mean, na.rm=T)
summary(fat)
dim(fat)
###first divide the data set into training and testing data (10% as testing data)
n=dim(fat)[1]
n
n1=round(n/10) ##1/10th of the total number of rows inthe fat data set assign to the variable n1
flag = c(1, 21, 22, 57, 70, 88, 91, 94, 121, 127, 149, 151, 159, 162,
         164, 177, 179, 194, 206, 214, 215, 221, 240, 241, 243); ##this line creates a vector "flag" containing of rows that will be used for testing
fat1train = fat[-flag,]; ##create train data set
fat1test = fat[flag,]; ##create test data set
##(b) data exploration with training data 
###scatter plot 
library(pastecs)
stat.desc(fat1train)
# Example: Creating a scatterplot matrix
pairs(~ brozek + age + weight+height, data = fat1train)
##hand thickness, waist circumference, hip and thigh circumference
##forearm, hip, thigh
library(psych)
library(corrplot)
correlation_matrix <- cor(fat1train[, c("neck", "hip", "thigh", "abdom","forearm","chest","biceps")])
# Create the correlation plot
corrplot(correlation_matrix, method = "number", type = "upper", tl.cex = 0.7) ##much better

head(fat1train,6)
MSEtrain <- NULL;
MSEtest  <- NULL; 


### (1) Linear regression with all predictors (Full Model)
model1 <- lm( brozek ~ ., data = fat1train); 

## Model 1: Training error
MSEmod1train <-   mean( (resid(model1) )^2);
MSEtrain <- c(MSEtrain, MSEmod1train);
# Model 1: testing error 
pred1a <- predict(model1, fat1test[,-1]);
ytrue    <- fat1test$brozek
MSEmod1test <-   mean((pred1a - ytrue)^2);
MSEmod1test;
MSEtest <- c(MSEtest, MSEmod1test); 
##Linear regression with the best subset of k=5 predictors 
### (2) Linear regression with the best subset model 
library(leaps);
fat.leaps <- regsubsets(brozek ~ ., data= fat1train, nbest= 100, really.big= TRUE); ##subset selection process.
## Record useful information from the output
fat.models <- summary(fat.leaps)$which;
fat.models.size <- as.numeric(attr(fat.models, "dimnames")[[1]]);
fat.models.rss <- summary(fat.leaps)$rss;

plot(fat.models.size, fat.models.rss); 
## find the smallest RSS values for each subset size 
fat.models.best.rss <- tapply(fat.models.rss, fat.models.size, min); 
## Also add the results for the only intercept model
fat.model0 <- lm( brozek ~ 1, data = fat1train); 
fat.models.best.rss <- c( sum(resid(fat.model0)^2), fat.models.best.rss); 
## plot all RSS for all subset models and highlight the smallest values 
plot( 0:8, fat.models.best.rss, type = "b", col= "red", xlab="Subset Size k", ylab="Residual Sum-of-Square")
points(fat.models.size, fat.models.rss)

# 2B: What is the best subset with k=5
op2 <- which(fat.models.size == 5); 
flag2 <- op2[which.min(fat.models.rss[op2])]; 
fat.models[flag2,]
mod2selectedmodel <- fat.models[flag2,]; 
mod2Xname <- paste(names(mod2selectedmodel)[mod2selectedmodel][-1], collapse="+"); 
mod2form <- paste ("brozek ~", mod2Xname);
## To auto-fit the best subset model with k=5 to the data
model2 <- lm( as.formula(mod2form), data= fat1train); 
# Model 2: training error 
MSEmod2train <- mean(resid(model2)^2);
## save this training error to the overall training error vector 
MSEtrain <- c(MSEtrain, MSEmod2train);
MSEtrain; 
##trainig error 
## Model 2:  testing error 
pred2 <- predict(model2, fat1test[,-1]);
MSEmod2test <-   mean((pred2 - ytrue)^2);
MSEtest <- c(MSEtest, MSEmod2test);
MSEtest;

###Manually set the model 
model2a <- lm( brozek ~ siri + density + thigh + knee + wrist, data = fat1train);
summary(model2a);
(3) Linear regression with the stepwise variable selection 

model1 <- lm( brozek ~ ., data = fat1train); 
model3  <- step(model1); 
## model coefficents of model3
round(coef(model3),3)
summary(model3)
## Model 3: training  and  testing errors 
MSEmod3train <- mean(resid(model3)^2);
pred3 <- predict(model3, fat1test[,-1]);
MSEmod3test <-   mean((pred3 - ytrue)^2);
MSEtrain <- c(MSEtrain, MSEmod3train);
MSEtrain; 
MSEtest <- c(MSEtest, MSEmod3test);
MSEtest;
(4) Ridge regression (MASS: lm.ridge, mda: gen.ridge)
library(MASS);

fat.ridge <- lm.ridge( brozek ~ ., data = fat1train, lambda= seq(0,100,0.001));

plot(fat.ridge) 
matplot(fat.ridge$lambda, t(fat.ridge$coef), type="l", lty=1, 
        xlab=expression(lambda), ylab=expression(hat(beta)))

###Optimal lambda value 
select(fat.ridge)
abline(v=0.003)

fat.ridge$coef[, which(fat.ridge$lambda == 0.003)]
fat.ridge$coef[, which(fat.ridge$lambda == 0)]
indexopt <-  which.min(fat.ridge$GCV);  
fat.ridge$coef[,indexopt]

ridge.coeffs = fat.ridge$coef[,indexopt]/ fat.ridge$scales;
intercept = -sum( ridge.coeffs  * colMeans(fat1train[,-1] )  )+ mean(fat1train[,18]);
## If you want to see the coefficients estimated from the Ridge Regression
##   on the original data scale
c(intercept, ridge.coeffs);

## Model 4 (Ridge): training errors 
yhat4train <- as.matrix( fat1train[,-1]) %*% as.vector(ridge.coeffs) + intercept;
MSEmod4train <- mean((yhat4train - fat1train$brozek)^2); 
MSEtrain <- c(MSEtrain, MSEmod4train); 
MSEtrain
## [1]  0.0293 0.03147 0.02946 0.5988
## Model 4 (Ridge):  testing errors in the subset "test" 
pred4test <- as.matrix( fat1test[,-1]) %*% as.vector(ridge.coeffs) + intercept;
MSEmod4test <-  mean((pred4test - ytrue)^2); 
MSEtest <- c(MSEtest, MSEmod4test);
MSEtest;

## Model (5): LASSO 
library(lars)
fat.lars <- lars( as.matrix(fat1train[,-1]), fat1train[,1], type= "lasso", trace= TRUE);

## 5A: some useful plots for LASSO for all penalty parameters \lambda 
plot(fat.lars)

## 5B: choose the optimal \lambda value that minimizes Mellon's Cp criterion 
Cp1  <- summary(fat.lars)$Cp;
index1 <- which.min(Cp1);
index1
## 5B(i) see the beta coefficient values (except the intercepts)
coef(fat.lars)[index1,]
fat.lars$beta[index1,]

##   the third way is to get the coefficients via prediction function 
lasso.lambda <- fat.lars$lambda[index1]
coef.lars1 <- predict(fat.lars, s=lasso.lambda, type="coef", mode="lambda")
coef.lars1$coef
## Can you get the intercept value? 
## 
##       for all linear models including LASSO
LASSOintercept = mean(fat1train[,1]) -sum( coef.lars1$coef  * colMeans(fat1train[,-1] ));
c(LASSOintercept, coef.lars1$coef)
## Model 5:  training error for lasso
## 
lasso.lambda <- fat.lars$lambda[index1]

coef.lars1 <- predict(fat.lars, s=lasso.lambda, type="coef", mode="lambda")
coef.lars1$coef

##       for all linear models including LASSO
LASSOintercept = mean(fat1train[,1]) -sum( coef.lars1$coef  * colMeans(fat1train[,-1] ));
c(LASSOintercept, coef.lars1$coef)
##Model 5:  training error for lasso
pred5train  <- predict(fat.lars, as.matrix(fat1train[,-1]), s=lasso.lambda, type="fit", mode="lambda");
yhat5train <- pred5train$fit; 
MSEmod5train <- mean((yhat5train - fat1train$brozek)^2); 
MSEmod5train
MSEtrain <- c(MSEtrain, MSEmod5train); 
MSEtrain

## Model 5:  testing error for lasso  
pred5test <- predict(fat.lars, as.matrix(fat1test[,-1]), s=lasso.lambda, type="fit", mode="lambda");
yhat5test <- pred5test$fit; 
MSEmod5test <- mean( (yhat5test - fat1test$brozek)^2); 
MSEmod5test
MSEtest <- c(MSEtest, MSEmod5test); 
MSEtest;

#### Model 7: Partial Least Squares (PLS) 
library(pls)
fat.pls <- plsr(brozek ~ ., data = fat1train, validation="CV");
7(i) auto-select the optimal # of components of PLS 
mod7ncompopt <- which.min(fat.pls$validation$adj);
mod7ncompopt

7(ii) Training Error with the optimal choice of "mod7ncompopt" 
ypred7train <- predict(fat.pls, ncomp = mod7ncompopt, newdata = fat1train[,-1]); 
MSEmod7train <- mean( (ypred7train - fat1train$brozek)^2); 
MSEtrain <- c(MSEtrain, MSEmod7train); 
MSEtrain;

## 7(iii) Testing Error with the optimal choice of "mod7ncompopt" 
ypred7test <- predict(fat.pls, ncomp = mod7ncompopt, newdata = fat1test[,-1]); 
MSEmod7test <- mean( (ypred7test - fat1test$brozek)^2); 
MSEtest <- c(MSEtest, MSEmod7test); 
MSEtest;

#### Model 6: Principal Component Regression (PCR) 

library(pls)
fat.pca <- pcr(brozek ~., data=fat1train, validation="CV");  
validationplot(fat.pca);
summary(fat.pca); 
ncompopt <- which.min(fat.pca$validation$adj);
ncompopt
ypred6train <- predict(fat.pca, ncomp = ncompopt, newdata = fat1train[,-1]); 
MSEmod6train <- mean( (ypred6train - fat1train$brozek)^2); 
MSEtrain <- c(MSEtrain, MSEmod6train); 
MSEtrain;

## 6B(v) Testing Error with the optimal choice of PCs
ypred6test <- predict(fat.pca, ncomp = ncompopt, newdata = fat1test[,-1]); 
MSEmod6test <- mean( (ypred6test - fat1test$brozek)^2); 
MSEtest <- c(MSEtest, MSEmod6test); 
MSEtest;

# Initialize the TE values for all models in all B=100 loops
B <- 100  
TEALL <- NULL  
set.seed(7406)
# Multiple round cross-validation
for (b in 1:B) {
  
  flag <- sort(sample(1:n, n1))
  fattrain <- fat[-flag,]  # Temp training set for CV
  fattest <- fat[flag,]  # Temp testing set for CV
  
  model1 <- lm( brozek ~ ., data = fattrain)
  pred1 <- predict(model1, newdata=fattest);
  te1 <- mean((pred1-fattest$brozek)^2)
  
  ####Model 2 
  model2 <- lm( as.formula(mod2form), data= fattrain); 
  ## Model 2:  testing error 
  pred2 <- predict(model2, fattest[,-1]);
  te2 <-   mean((pred2 - fattest$brozek)^2);
  
  ###Model 3
  model3  <- step(model1)
  pred3 <- predict(model3, fattest[,-1]);
  te3 <-   mean((pred3 - fattest$brozek)^2);
  
  ###Model 4
  yhat4train <- as.matrix( fattrain[,-1]) %*% as.vector(ridge.coeffs) + intercept;
  pred4test <- as.matrix( fattest[,-1]) %*% as.vector(ridge.coeffs) + intercept;
  te4 <-  mean((pred4test - fattest$brozek)^2); 
  
  ###Model 5
  fat.lars <- lars( as.matrix(fattrain[,-1]), fattrain[,18], type= "lasso", trace= TRUE);
  pred5test <- predict(fat.lars, as.matrix(fattest[,-1]), s=lasso.lambda, type="fit", mode="lambda");
  yhat5test <- pred5test$fit; 
  te5 <- mean( (yhat5test - fattest$brozek)^2); 
  
  ##Model 6
  fat.pca <- pcr(brozek ~., data=fat1train, validation="CV");  
  ncompopt <- which.min(fat.pca$validation$adj);
  ypred6test <- predict(fat.pca, ncomp = ncompopt, newdata = fattest[,-1]); 
  te6 <- mean( (ypred6test - fattest$brozek)^2); 
  
  ###Model 7
  fat.pls <- plsr(brozek ~ ., data = fattrain, validation="CV");
  mod7ncompopt <- which.min(fat.pls$validation$adj);
  ypred7test <- predict(fat.pls, ncomp = mod7ncompopt, newdata = fattest[,-1]); 
  te7 <- mean( (ypred7test - fattest$brozek)^2); 
  
  TEALL <- rbind(TEALL,c(te1,te2,te3,te4,te5,te6,te7))
} 
dim(TEALL); 
colnames(TEALL) <- c("mod1", "mod2", "mod3", "mod4", "mod5", "mod6", "mod7")
apply(TEALL, 2, mean);
apply(TEALL, 2, var);

