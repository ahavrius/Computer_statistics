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
data4_1<-Data[1:(nn-20),]
data4_2<-Data[(nn-69):(nn-20),]

library(leaps)
res1<-regsubsets(agn~.,data=data4_1,nbest=5,nvmax=9)
p1<-apply(summary(res1)$which,1,sum)+1
Cp1<-summary(res1)$cp
plot(p1,Cp1)
abline(0,1,col="red")
plot(p1,Cp1,ylim=c(0,50), xlim = c(0, 30))
abline(0,1,col="red")
identify(p1, Cp1)

res2<-regsubsets(agn~.,data=data4_2,nbest=3,nvmax=9)
p2<-apply(summary(res2)$which,1,sum)+1
Cp2<-summary(res2)$cp
plot(p2,Cp2)
abline(0,1,col="red")
plot(p2,Cp2,ylim=c(0,15), xlim = c(0, 15))
abline(0,1,col="red")
identify(p2, Cp2)

model4_1<-lm(agn~.-all, data = data4_1)
summary(model4_1)
shapiro.test(model1$residuals)
plot(model4_1$fitted.values, data4_1$agn, xlab = "fitted values", ylab = "agn", main = "?????????????? - ????????????, ???????????? 1")

model4_2<-lm(agn~.-aiv -all -alxn, data = data4_2)
summary(model4_2)
shapiro.test(model4_2$residuals)
plot(model4_2$fitted.values, data4_2$agn, xlab = "fitted values", ylab = "agn", main =  "True vs Fitted values")
abline(0,1, col = "red")
plot(1:191, modelf4_2$residuals, pch = 19, col = "blue", main = "Residuals")

model4_3<-lm(agn~.-aiv -alxn, data = data4_2)
summary(model4_3)
shapiro.test(model4_3$residuals)
plot(model4_3$fitted.values, data4_2$agn, xlab = "fitted values", ylab = "agn", main =  "True vs Fitted values")
abline(0,1, col = "red")
plot(1:191, modelf4_3$residuals, pch = 19, col = "blue", main = "Residuals")

xagn4_2<-predict(model4_2, Data[(nn-19):nn,])
xagn4_0 = Data[(nn-19):nn,]$agn
xagn4_3<-predict(model4_3, Data[(nn-19):nn,])

plot(1:20, abs(xagn4_0-xagn4_2), ylim = c(0, 12), pch = 19, col = "blue", main = "True vs Fitted values", xlab = "Observations", ylab = "")
par(new=TRUE)
plot(1:20, abs(xagn4_-xagn4_3), ylim = c(0, 12),  pch = 19, col = "red", main = "True vs Fitted values", xlab = "Observations", ylab = "")
sum(abs(xagn4_-xagn4_2))
sum(abs(xagn4_-xagn4_3))

