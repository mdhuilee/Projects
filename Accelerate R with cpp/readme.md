 
# Log likelihood and gradient function in R and Rcpp
    * [Summary](#summary)
    * [Code](#code)
      * [Log likelihood function in R](#log-likelihood-function-in-r)
      * [Gradient function in R](#gradient-function-in-r)
      * [Log likelihood function in Rcpp](#log-likelihood-function-in-rcpp)
      * [Gradient function in Rcpp](#gradient-function-in-rcpp)
    * [Results](#results)

## Summary

To create four functions: the log likelihood and gradient function in R and Rcpp. To test the accuracy, I used a simple data set. The results showed that R and Rcpp code of the two functions produced the same results respectively.

## Code    
### Log likelihood function in R

```r
library(knitr)
opts_chunk$set(tidy = TRUE, cache = TRUE, autodep = TRUE, message = FALSE)

log_like_r <- function(beta, y, x) {
    len <- length(x)  #to create a vector storing the length of vector x
    one <- rep(1, len)  #to create a vector of elements one with the same 
    # length as x
    m <- cbind(one, x)  #to create a matrix with ones as the first column and
    # x as the second column
    log_like <- 0
    for (i in 1:len) {
        n <- y[i] * (t(beta) %*% m[i, ]) - log(1 + exp(t(beta) %*% m[i, ]))
        # to calculate the log likehood function for each i
        log_like = log_like + n  #to add each obtained value n to log_like
    }
    as.vector(log_like)
}
```

### Gradient function in R

```r
gradient_r <- function(beta, y, x) {
    len <- length(x)
    gradient <- 0
    for (i in 1:len) {
        a <- c(1, x[i]) * (y[i] - exp(beta[1] + beta[2] * x[i])/(1 + exp(beta[1] + 
            beta[2] * x[i])))
        gradient <- gradient + a
    }
    gradient
}
```

```r
# to load packages used in Rcpp codes
library(inline)
library(Rcpp)
library(RcppArmadillo)
```

### Log likelihood function in Rcpp    

```r
log_like_code <- "
//to create a numericvector object yc based on y
Rcpp::NumericVector yc(y); 
//to create a armadillo vector x_a based on x
arma :: vec x_a = Rcpp :: as < arma :: vec >( x ); 
//to create a armadillo vector beta_a based on beta    
arma :: vec beta_a = Rcpp :: as < arma :: vec >( beta );
    

int n = x_a.n_elem; //to get the length of armadillo vectro x_a
arma::vec v(n);     //to create a armadillo vector v with length n
v.ones();           //to create a armadillo vector with elements one and length n
arma::mat X(n, 2);  //to create a amadillo matrix with n rows and 2 columns
X.col(0) = v;       //assign vector v to the first column of X
X.col(1) = x_a;     //assign vector x_a to the second column of X

double each = 0; 
double sum = 0;
for (int i = 0; i < n; i++){
//to calculate each value in the sum of gradient function for each i
    each = as_scalar(yc[i] * beta_a.t() * X.rows(i,i).t() -
    log(1 + exp(beta_a.t() * X.rows(i,i).t())));
    sum = sum + each;
}
return Rcpp::wrap(sum);
"
log_like_c <- cxxfunction(signature(beta = "numeric", x = "numeric",
    y = "numeric"), body=log_like_code, plugin="RcppArmadillo")
```

### Gradient function in Rcpp  

```r
gradient_code <- "\narma :: vec y_a = Rcpp :: as < arma :: vec >( y ); \narma :: vec x_a = Rcpp :: as < arma :: vec >( x );\narma :: vec beta_a = Rcpp :: as < arma :: vec >( beta );\n\nint n = x_a.n_elem; \narma::vec v(n); \nv.ones(); //to create a vector of elements ones\narma::mat X(n, 2);\nX.col(0) = v; \nX.col(1) = x_a;\n\narma::vec each(2); \narma::vec sum(2);\nsum.zeros();\nfor (int i = 0; i < n; i++){\n    each = X.rows(i,i).t() * (y_a[i] - (exp(beta_a.t() * X.rows(i,i).t()) \n    / (1 + exp(beta_a.t() * X.rows(i,i).t()))));\n    sum = sum + each;\n}\nreturn wrap(sum);\n"
gradient_c <- cxxfunction(signature(beta = "numeric", x = "numeric", y = "numeric"), 
    body = gradient_code, plugin = "RcppArmadillo")
```

## Results


```r
beta <- c(1, 5)
x <- 1:10
y <- c(rep(0, 5), rep(1, 5))

# to test if the log likelihood function in R and Rcpp produce the same
# result
log_like_r(x = x, y = y, beta = beta)
```

```
## [1] -80.00249
```

```r
log_like_c(x = x, y = y, beta = beta)
```

```
## Error in .Primitive(".Call")(<pointer: 0x0>, beta, x, y): NULL value passed as symbol address
```

```r
all.equal(log_like_r(x = x, y = y, beta = beta), log_like_c(x = x, y = y, beta = beta))
```

```
## Error in .Primitive(".Call")(<pointer: 0x0>, beta, x, y): NULL value passed as symbol address
```

```r
# to test if the gradient function in R and Rcpp produce the same result
gradient_r(x = x, y = y, beta = beta)
```

```
## [1]  -4.997511 -14.997494
```

```r
gradient_c(x = x, y = y, beta = beta)
```

```
## Error in .Primitive(".Call")(<pointer: 0x0>, beta, x, y): NULL value passed as symbol address
```

```r
all.equal(gradient_r(x = x, y = y, beta = beta), c(gradient_c(x = x, y = y, 
    beta = beta)))
```

```
## Error in .Primitive(".Call")(<pointer: 0x0>, beta, x, y): NULL value passed as symbol address
```




