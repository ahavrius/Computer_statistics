filenames=list.files(path="d:/Users/Admin/Desktop/R/lib", full.names=TRUE)
y<-read.csv(file=filenames[1],header=F)[,c(1,6)]
colnames(y)<-c("data", unlist(strsplit(filenames[1],"[_.]"))[2])
for(i in 2:10) {
  x0<-read.csv(file=filenames[i],header=F)[,c(1,6)]; 
  colnames(x0)<-c("data", unlist(strsplit(filenames[i],"[_.]"))[2]);
  y<-merge(y, x0, by = "data")}
n<-nrow(y)
Data<-y[-nrow(y),-1]
Data$agn<-y$agn[-1]
nn<-nrow(Data)
data3_1<-Data[1:(nn-20),]
data3_2<-Data[(nn-69):(nn-20),]
library(MASS)
#model1 
fit1 <- lm.ridge(agn ~ ., data = data3_1,lambda = seq(0.001, 50, .01))
plot(fit1$lambda,fit1$GCV,type="l",xlab="mu",ylab="CV")
i_min1<-which.min(fit1$GCV)
abline(v=fit1$lambda[i_min1],col="red")

matplot(fit1$lambda,t(fit1$coef),type="l",col=1:8,lty=1:8,xlab="mu",ylab="coeficients")
abline(v=fit1$lambda[i_min1],col="red")
legend("topright",col=1:9,legend=colnames(Data)[1:8],lty=1:8)
coefs1<-matrix(fit1$coef[,i_min1]/fit1$scales,ncol=1)

#model2
fit2 <- lm.ridge(agn ~ .,data = data3_2,lambda = seq(0.001, 50, .01))
plot(fit2$lambda,fit2$GCV,type="l",xlab="mu",ylab="CV")
i_min2<-which.min(fit2$GCV)
abline(v=fit2$lambda[i_min2],col="red")

matplot(fit2$lambda,t(fit2$coef),type="l",col=1:8,lty=1:8,xlab="mu",ylab="coeficients")
abline(v=fit2$lambda[i_min2],col="red")
legend("topright",col=1:9,legend=colnames(Data)[1:8],lty=1:8)
coefs2<-matrix(fit2$coef[,i_min2]/fit2$scales,ncol=1)

#predict
data3_3 = Data[(nn-19):nn,]
xagn3_0 = as.matrix(data3_3)[,1]
xagn3_1 = as.matrix(data3_3)[,-1] %*% coefs1
xagn3_2 = as.matrix(data3_3)[,-1] %*% coefs2 + mean(xagn3_0)

plot(1:20, abs(xagn3_0-xagn3_1),  ylim = c(0,20),  pch = 19, col = "blue", main = "True vs Fitted values", xlab = "Observations", ylab = "")
par(new=TRUE)
plot(1:20, abs(xagn3_0-xagn3_2), ylim = c(0,20), pch = 19, col = "red", main = "True vs Fitted values", xlab = "Observations", ylab = "")


plot(1:20, abs(xagn3_0-xagn3_2), ylim = c(0,20),  pch = 19, col = "green", main = "True vs Fitted values", xlab = "Observations", ylab = "")
par(new=TRUE)
plot(1:20, abs(xagn3_0-xagn2_2), ylim = c(0,20), pch = 19, col = "red", main = "True vs Fitted values", xlab = "Observations", ylab = "")
par(new=TRUE)
plot(1:20, abs(xagn3_0-xagn1_2), ylim = c(0,115),  pch = 19, col = "blue", main = "True vs Fitted values", xlab = "Observations", ylab = "")

