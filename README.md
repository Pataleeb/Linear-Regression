# Linear-Regression
Here we assume that the percentage of body fat using Brozek’s equation (brozek, the first column) as the response variable, and the other 17 variables as potential predictors. We will use several different statistical methods to fit this dataset in the problem of predicting brozek using the other 17 potential predictors. 

(a) First, we should split the original data set into disjoint training and testing data sets, so that we
can better evaluate and compare different models. One possible simple way is to random select a
proportion, say, 10% of observations from the data for use as a test sample, and use the remaining
data as a training sample building different models.

(b) Based on the training data “fat1train,” build the following models
(i) Linear regression with all predictors.
(ii) Linear regression with the best subset of k = 5 predictors variables;
(iii) Linear regression with variables (stepwise) selected using AIC;
(iv) Ridge regression;
(v) LASSO;
(vi) Principal component regression;
(vii) Partial least squares.

(c)for a relatively
small data, one may want to use Cross-Validation to further assess the robustness of each method.
Using Monte Carlo Cross-Validation algorithm that repeats the above computation B = 100
times, compute and compare the “average” performances of each model.
