<link rel="stylesheet" href="rmd_custom_style.css" />


Scaling Gemm Coefficients
========================================================
*Joe Tidwell, Last Updated: Sun Mar  2 01:30:47 2014*

The document walks through two methods, one incorrect and one correct, to transform GeMM coefficients to the scale of the metric criterion and to obtain an intercept. 
For each step in both methods, I've attempted to provide both a mathematical description of the step, as well as `R` code to implement it.
Method II is simpler both in mathematical definition as well as `R` implementation.

The `R` examples use the `IATDataSiegel.csv` dataset.




## `R` Setup

Load data in R and define `m1` as the model:

`atbmean ~ poliatDmean + raceampdiff + imsmean + emsmean + pamean + Rstroopeffect + ssrt`


```r
#### Setup
library(gemmR)
setwd("~/Dropbox/org/projects/Dougherty/GeMM")
d1 <-as.matrix(read.csv("IATDataSiegel.csv"))

# Get variable names so we can reuse and not type them repeatedly
labels <- colnames(d1)[c(1,4:10)]
labels <- list(y=labels[1], X=labels[-1])

# Define the model
# This is more complex (obviously), than just typing in 
# a model like y ~ a + b, but it allows us to change the model
# by changing a couple numbers versus typing in a long
# model string
RS <- paste(labels$X, collapse=" + ")
m1 <- eval(parse(text=paste(labels$y, " ~ ", RS)))
```



## Definitions

### Criterion

$y = 
 \begin{bmatrix}
 y_1 \\
 \vdots \\
 y_n
 \end{bmatrix}$, where $n$ is the number of observations


```r
y   <- d1[, labels$y]          # criterion
```


### Model Matrices

Without Intercept:
$X = 
 \begin{bmatrix}
 X_{1,1} & \cdots & X_{1,k} \\
 \vdots  & \ddots & \vdots  \\
 X_{n,1} & \cdots & X_{n,k}
 \end{bmatrix}$, Where $k$ is the number of predictors



```r
X   <- d1[, labels$X]          # model matrix w/out intercept
```

<br />

With Intercept:
$W = 
 \begin{bmatrix}
 1 & X_{1,1} & \cdots & X_{1,k} \\
 \vdots & \vdots  & \ddots & \vdots  \\
 1 & X_{n,1} & \cdots & X_{n,k}
 \end{bmatrix}$


```r
W <- cbind(intercept=1,X)   # model matrix w/ intercept
```

<br />

## Method 1 (Incorrect, converts GeMM coefficients back to OLS coefficients)

*Mike, this is basically what you did in your email, excepting the last step. This is very similar to one of the ways we discussed, but it ends up just being a long circular route back to original OLS weights.*

### Get OLS betas (W/ Intercept)

$b = 
 \begin{bmatrix}
 b_0 \\
 b_1 \\
 \vdots \\
 b_k
 \end{bmatrix} = (W^TW)^{-1}W^Ty$


```r
b <- solve(crossprod(W))%*%crossprod(W,y)
# Alternatively: 
# solve(t(W) %*% W ) %*% (t(W) %*% y)
```

<br />

### Get GeMM 'betas' (No Intercept)
$b^g = 
 \begin{bmatrix}
 b_1^g \\
 \vdots \\
 b_k^g
 \end{bmatrix} = GeMM(y~X)$


```r
# Fit GeMM model
fit.gemm <- gemm(m1, data=as.data.frame(d1), p.est=1, 
                 n.gens=10, n.chains=4, parallel=TRUE, 
                 fit.metric="tau")

# Gemm output ordering is reversed for tau (not on purpose)
# so get the last row of coefficients. When working correctly,
# the simpler command would be: coef(fit.gemm)
b.g <- fit.gemm$coefficients[4,]
```

<br />

### Scale model matrix $X$ by $b^g$ 

$X^g = Xb^g$  


```r
X.g <- X%*%diag(b.g)
```

<br />

### Create GeMM scaled model matrix w/ intercept

$W^g = [1_{1Xn}, X^g]_{nX(1+k)}$


```r
W.g <- cbind(1,X.g)
```

<br />

### Obtain coefficients from regressing $y$ on $W^g$ (Incorrect Step)

*I believe this is the equivalent step where you stopped at in the code your email. This indeed will give you screwy coefficients, because these coefficients essentially model the factor by which each predictor needs to change so that the dot product of the coefficients and the model matrix ($\hat{y}$)minimizes the model error, but since our model matrix in this case is generated from the GeMM weights, this is the same thing as saying what is the factor we need to multiply each GeMM coefficient by in order to obtain the OLS weights.*


```r
b.g.prime <- solve(crossprod(W.g))%*%crossprod(W.g,y)
rownames(b.g.prime) <- rownames(b)
b.g.prime
```

```
##                 [,1]
## intercept     4.0738
## poliatDmean   1.0788
## raceampdiff   1.3989
## imsmean       2.5096
## emsmean       1.0000
## pamean        1.0000
## Rstroopeffect 1.0000
## ssrt          0.6022
```


To make the point, if you use these weights to scale the GeMM weights, you'll just be right back at the original OLS weights.


```r
cbind("Incorrectly Scaled GeMM Weights"=c(intercept=b.g.prime[1],
                                          b.g.prime[-1]*b.g),
      "OLS Weights"=c(b))
```

```
##               Incorrectly Scaled GeMM Weights OLS Weights
## intercept                           4.0738096   4.0738096
## poliatDmean                        -0.1442575  -0.1442575
## raceampdiff                         0.0191087   0.0191087
## imsmean                            -0.0772506  -0.0772506
## emsmean                            -0.0916556  -0.0916556
## pamean                             -0.0571903  -0.0571903
## Rstroopeffect                       0.0204485   0.0204485
## ssrt                               -0.0002941  -0.0002941
```


## Method II (Correct) Regress $y$ on $\hat{y}^g$

### Get GeMM fitted values and create model matrix w/ intercept

$\hat{y}^g = Xb^g $

$G = 
 \begin{bmatrix}
 1 & \hat{y}^g_1 \\
 \vdots & \vdots  \\
 1 & \hat{y}^g_n
 \end{bmatrix}$


```r
# You could also have gotten the rowSums of W.g from earlier steps,
# but this allows you to skip a lot of the other math/code

# If gemmR was ordering chains correctly, also could have done:
# y.hat.g <- fitted(fit.gemm)
y.hat.g <- X%*%b.g
G <- cbind(1,y.hat.g)
```

<br />

### Obtain intercept and scaling factor for GeMM coefficients

$b_s = 
 \begin{bmatrix}
 b^{g_s}_0 \\
 b^g_f  \\
 \end{bmatrix} = (G^TG)^{-1}G^Ty$, where $b^{g_s}_0$ is the intercept for the metric scaled GeMM coefficients, and $b^g_f$ is the metric scaling factor to apply to the GeMM coefficients

$b^g_s = 
\begin{bmatrix}
 b^g_0 \\
 b^{g_s}_1 \\
 \vdots \\
 b^{g_s}_k
 \end{bmatrix} = 
 \begin{bmatrix}
 b^g_0 \\
 b^g_1b^g_f \\
 \vdots \\
 b^{g_s}_kb^g_f
 \end{bmatrix}$, where $b^g_s$ represents the metric scaled GeMM coefficients, including and intercept


```r
b.s <- solve(crossprod(G))%*%crossprod(G,y)
b.g.s <- c(intercept=b.s[1], b.g*b.s[-1])
```


### Compare OLS and scaled GeMM coefficients


```r
cbind("Metric GeMM Weights"=b.g.s,
      "OLS Weights"=c(b))
```

```
##               Metric GeMM Weights OLS Weights
## intercept               4.1408552   4.0738096
## poliatDmean            -0.1631611  -0.1442575
## raceampdiff             0.0166677   0.0191087
## imsmean                -0.0375605  -0.0772506
## emsmean                -0.1118370  -0.0916556
## pamean                 -0.0697830  -0.0571903
## Rstroopeffect           0.0249510   0.0204485
## ssrt                   -0.0005958  -0.0002941
```


### Check that fitted values from scaled GeMM coefficients produce ordinally equivalent fitted vales compared to raw GeMM fitted values

If scaled properly, $\tau = 1$


```r
cor(W%*%b.g.s,y.hat.g,method="kendall")
```

```
##      [,1]
## [1,]    1
```

