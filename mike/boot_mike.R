library(gemmR)

setwd("~/Dropbox/org/projects/Dougherty/GeMM")
d1 <-read.csv("IATDataSiegel.csv")

var.names <- names(data.1)[c(1,4:10)]
RS <- paste(var.names[-1], collapse=" + ")
LS <- var.names[1]
m1 <- eval(parse(text=paste(LS, " ~ ", RS)))


lm(m1, data=d1)

OCLO <- gemm(m1, data=d1, p.est=1, n.gens=10, 
             n.chains=4, parallel=TRUE, fit.metric="tau")


X<-d1[,var.names[-1]]

X<-matrix(X,nrow=198)

X<-t(t(X) * (OCLO$coefficients[4,]))


X<-cbind(1,X)
Y<-df$atbmean

coef<-solve(t(X)%*%X) %*% t(X)%*%Y

coef