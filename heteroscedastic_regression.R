gfunc = function(x){ (x + 1) * (x > -1)}
N = 1000

X = rnorm(N, -1, 1)
eps = c()
for (i in 1:N) eps = c(eps, rnorm(1, 0, gfunc(X[i])))
Y = 2 - X + eps
data5_1<-data.frame(Y,X)
colnames(data5_1)<-c("Y","X")
model5_1<-lm(Y~X, data = data5_1)
b0_1_5<- model5_1$coefficients[1]
b1_1_5<- model5_1$coefficients[2]

vv<-(model5_1$residuals)^2
data5_2<-data.frame(vv, X^2, X)
colnames(data5_2)<-c("v","Xsq", "X")
model5_2<-lm(vv~Xsq+X, data = data5_2)
ww<-1/pmax(model5_2$fitted.values, 1e-3)

model5_3w<-lm(Y~X, weights = ww, data = data5_1)
b0_2_5<-model5_3w$coefficients[1]
b1_2_5<-model5_3w$coefficients[2]

