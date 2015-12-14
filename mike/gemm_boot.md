<link rel="stylesheet" href="rmd_custom_style.css" />


GeMM Bootstrap Metric Coefficients
========================================================
*Joe Tidwell, Last Updated: Fri Feb 28 02:10:29 2014*








## Bootstrap Code


```r
library(foreach)
library(gemmR)
library(MASS)
library(arm)
library("doMC", quietly = TRUE)
library("multicore", quietly = TRUE)
registerDoMC(multicore:::detectCores() - 2)  # use all cores minus 2

bootGemm <- function(reps, model, mydata) {
    out <- foreach(i = 1:reps) %dopar% {
        boot.data <- mydata[sample(1:nrow(mydata), replace = TRUE), ]
        gfit <- gemm(model, data = boot.data, n.beta = 1000, n.chains = 1, fit.metric = "tau")
        scale.coef <- coef(lm(boot.data[, 1] ~ gfit$fitted.values))
        c(scale.coef[1], coef(gfit) * scale.coef[2], coef(lm(model, boot.data)))
    }
    out <- do.call(rbind, out)
    coef.ests <- rbind(`<U+03BC>` = apply(out, 2, mean), `<U+03C3> (SE)` = apply(out, 
        2, sd), apply(out, 2, quantile, c(0.025, 0.975)))
    return(list(coefs.boot = coef.ests, data.boot = out))
}

#### Fake Data
Sigma <- matrix(c(1, 0.3, 0.5, 0.3, 1, 0, 0.5, 0, 1), nrow = 3)

mu <- c(10, 5, 8)
mydata.fake <- data.frame(mvrnorm(100, mu, Sigma))
model.fake <- X1 ~ X2 + X3

#### COH Data
coh <- read.csv("~/Dropbox/org/projects/Dougherty/OCLO/R/henry2009.csv")
coh <- coh[, c("murderrate", "gini_usethis", "percent_pastures", "GNPpercapita")]
names(coh) <- c("homicide", "gini", "pasture", "GNP")
model.coh <- homicide ~ gini + pasture + GNP

#### Bootstraps
reps <- 1000
lm.fake <- lm(model.fake, mydata.fake)
out.fake <- bootGemm(reps, model.fake, mydata.fake)
lm.coh <- lm(model.coh, coh)
out.coh <- bootGemm(reps, model.coh, coh)
```


## Results

### Fake Data

#### Regression

<img src="tab1.png" style="width: 400px">

#### Bootstrap Regression

<img src="tab3.png" style="width: 300px">

#### Bootstrap GeMM

<img src="tab2.png" style="width: 300px">

### Culture of Honor Data

#### Regression

<img src="tab4.png" style="width: 400px">

#### Bootstrap Regression

<img src="tab6.png" style="width: 300px">

#### Bootstrap GeMM

<img src="tab5.png" style="width: 300px">





