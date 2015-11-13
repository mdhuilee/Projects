
# Predicting musical ability with LASSO
    * [Summary](#summary)
    * [Introduction](#introduction)
    * [Methods](#methods)
    * [Results](#results)
    * [Predictions](#predictions)


## Summary    
Objectives:To establish a proper model based on brain activity data and use it to predict unknown subjects' musical ability.    
Methods: Center and scale the predictors and center the response. Then fit models using LASSO and choose the best model based on MSE and BIC.     
Results: Ninety-nine voxels were chosen to be included in the best model using LASSO. MSE of this model is 2.78e-27, very close to zero. The number of the top ten subjects predicted to have the highest musical ability are 121, 126, 133, 146, 151, 153, 154, 164, 181 and 198.  

## Introduction  
Given the data with 200 subjects and 8000 voxels, we need to identify the most important voxels, which stand for the position of part of the brain, to fit a model with musical ability as the response, using the first 100 observations. Then we need to use this model to predict the musical ability of the last 100 subjects and determine which ones have the highest musical ability.

## Methods  
We use Least Absolute Shrinkage and Selection Operator(LASSO) to fit the model. LASSO performs shrinkage and selection simultaneously by modifying the ridge penalty so that some of the estimates are exactly zero.
In this task, we use R packege lars to do LASSO, using code lars(X,Y,use.Gram=FALSE) to fit models. Before fitting models, we center and scale the predictors and also center the response so that we could determine the importance of each predictor according to the correponding coefficient magnitude. To choose the best model, we based on BIC and MSE related to each model. The smaller BIC and MSE are, the better the model is.    
Finally, we use our best model to predict the response with given predictor values.

## Results


```r
library(knitr)
opts_chunk$set(tidy = TRUE, cache = TRUE, autodep = TRUE, message = FALSE)

load("/Users/hui/ST810/C1.RData")
```

```
## Error in readChar(con, 5L, useBytes = TRUE): cannot open the connection
```

```r
library(MASS)
library(knitr)

# Transform original X

X_dup <- X
```

```
## Error in eval(expr, envir, enclos): object 'X' not found
```

```r
dim(X_dup) <- c(200, 8000)
```

```
## Error in dim(X_dup) <- c(200, 8000): object 'X_dup' not found
```

```r
# divide data into two parts:one for training and one for predicting

X100 <- scale(X_dup[1:100, ])
```

```
## Error in scale(X_dup[1:100, ]): object 'X_dup' not found
```

```r
X100plus <- scale(X_dup[101:200, ])
```

```
## Error in scale(X_dup[101:200, ]): object 'X_dup' not found
```

```r
Y100 <- Y[1:100]
```

```
## Error in eval(expr, envir, enclos): object 'Y' not found
```

```r
Yc <- Y100 - mean(Y100)
```

```
## Error in eval(expr, envir, enclos): object 'Y100' not found
```

```r
n <- 100

# Fit the LASSO with LARS

library(lars)
lasso <- lars(X100, Yc, use.Gram = FALSE)
```

```
## Error in lars(X100, Yc, use.Gram = FALSE): object 'X100' not found
```

```r
plot(lasso)
```

```
## Error in plot(lasso): object 'lasso' not found
```

```r
# Selecting the best point on the path using BIC

betas <- lasso$beta
```

```
## Error in eval(expr, envir, enclos): object 'lasso' not found
```

```r
df <- lasso$df
```

```
## Error in eval(expr, envir, enclos): object 'lasso' not found
```

```r
MSE <- lasso$RSS/n
```

```
## Error in eval(expr, envir, enclos): object 'lasso' not found
```

```r
bic <- log(n) * df + n * log(MSE)
```

```
## Error in FUN(left, right): non-numeric argument to binary operator
```

```r
bestb <- which.min(bic)
```

```
## Error in which.min(bic): object 'bic' not found
```

```r
matplot(df, bic, type = "l", lty = 1, xlab = "Degrees of Freedom", ylab = "bic")
```

```
## Error in as.matrix(y): object 'bic' not found
```

```r
matplot(df, MSE, type = "l", lty = 1, xlab = "Degrees of Freedom", ylab = "MSE")
```

```
## Error in as.matrix(y): object 'MSE' not found
```

```r
beta_lasso <- betas[bestb, ]
```

```
## Error in eval(expr, envir, enclos): object 'betas' not found
```

```r
# Determine the most important predictors and how accurate

beta_num <- which(beta_lasso != 0)
```

```
## Error in which(beta_lasso != 0): object 'beta_lasso' not found
```

```r
betas <- cbind(beta_num, beta_lasso[beta_num])
```

```
## Error in cbind(beta_num, beta_lasso[beta_num]): object 'beta_num' not found
```

```r
topnv <- 20
topvar <- keep <- which(rank(-abs(betas[, 2])) <= topnv)
```

```
## Error in rank(-abs(betas[, 2])): object 'betas' not found
```
    

```r
MSE <- as.character(MSE[bestb])
BIC <- bic[bestb]
mb <- cbind(MSE, BIC)
colnames(mb) <- c("MSE", "BIC")
kable(mb, caption = "MSE and BIC of the best model")
```



|   |MSE                  |BIC               |
|:--|:--------------------|:-----------------|
|   |2.78313606084972e-27 |-5654.10489535098 |

The best model has MSE 2.783136e-27, very close to zero. The BIC of this model is -5654.10. Compared to other models obtained through LASSO, this one has the lowest MSE and BIC.


```r
# to get the voxels number
nv <- topnv
vox <- matrix(NA, nv, 3)
for (i in 1:nv) {
    
    if (keep[i]%%400 == 0) {
        vox[i, 3] <- keep[i]%/%400
        vox[i, 2] <- 20
        vox[i, 1] <- 20
    } else if (keep[i]%%20 == 0) {
        vox[i, 3] <- keep[i]%/%400 + 1
        vox[i, 2] <- (keep[i] - 400 * (vox[i, 3] - 1))%/%20
        vox[i, 1] <- 20
    } else {
        vox[i, 3] <- keep[i]%/%400 + 1
        vox[i, 2] <- (keep[i] - 400 * (vox[i, 3] - 1))%/%20 + 1
        vox[i, 1] <- keep[i] - 400 * (vox[i, 3] - 1) - 20 * (vox[i, 2] - 1)
    }
}
```

```
## Error: object 'keep' not found
```

```r
colnames(vox) <- c("u", "v", "w")

kable(as.data.frame(t(vox)), caption = "The Most Important Twenty Voxels")
```



|   |V1 |V2 |V3 |V4 |V5 |V6 |V7 |V8 |V9 |V10 |V11 |V12 |V13 |V14 |V15 |V16 |V17 |V18 |V19 |V20 |
|:--|:--|:--|:--|:--|:--|:--|:--|:--|:--|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
|u  |NA |NA |NA |NA |NA |NA |NA |NA |NA |NA  |NA  |NA  |NA  |NA  |NA  |NA  |NA  |NA  |NA  |NA  |
|v  |NA |NA |NA |NA |NA |NA |NA |NA |NA |NA  |NA  |NA  |NA  |NA  |NA  |NA  |NA  |NA  |NA  |NA  |
|w  |NA |NA |NA |NA |NA |NA |NA |NA |NA |NA  |NA  |NA  |NA  |NA  |NA  |NA  |NA  |NA  |NA  |NA  |

The best model includes ninety-nine voxels. We list the most important twenty voxels here according to the magnitude of corresponding coefficents. The order listed here is not related to their significance. 

## Predictions


```r
# To predict

Yhat <- X100plus %*% beta_lasso
top10 <- cbind(round(which(rank(-Yhat) <= 10) + 100, 0), round(Yhat[which(rank(-Yhat) <= 
    10)], 4))
colnames(top10) <- c("subject number", "score predicted")
kable(top10, caption = "Subjects with the highest musical ability")
```



| subject number| score predicted|
|--------------:|---------------:|
|            121|          2.3314|
|            126|          2.9178|
|            133|          2.3898|
|            146|          3.0912|
|            151|          2.4919|
|            153|          2.4711|
|            154|          2.5726|
|            164|          2.9330|
|            181|          2.0907|
|            198|          4.7079|

Based on the predicted value, the subjects with the highest musical ability are subjects 121, 126, 133, 146, 151, 153, 154, 164, 181 and 198.
