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
Data1_1<-Data[-nn,]
for(i in 1:10) Data1_1[,i]<-diff(log(Data[,i]), lag = 1)
nn<-nn-1
data1_1<-Data1_1[(nn-59):(nn-10),]
model1_1<-lm(agn~.-agn, data = data1_1)
summary(model1_1)
shapiro.test(model1_1$residuals)
plot(model1_1$fitted.values, data1_1$agn, xlab = "fitted values", ylab = "agn", main = "?????????????? - ????????????, ???????????? 1")
abline(0,1, col = "red")
plot(model1_1$fitted.values, model1_1$residuals, xlab = "fitted values", ylab = "residials", main = "??????????????-??????????????, ???????????? 1")
library(carData)
library(car)
influencePlot(model1_1)
data1_2 = data1_1[-(c(2335,2358,2360,2361,2374)-2334+1),]
model1_1<-lm(agn~.-agn, data = data1_1)
model1_2<-lm(agn~.-agn, data = data1_2)
summary(model1_2)
plot(model1_2$fitted.values, data1_2$agn, xlab = "fitted values", ylab = "agn", main = "?????????????? - ????????????, ???????????? 2")
abline(0,1, col = "red")
plot(model1_2$fitted.values, model1_2$residuals, xlab = "fitted values", ylab = "residials", main = "??????????????-??????????????, ???????????? 2")
shapiro.test(model1_2$residuals)
influencePlot(model1_2)

xagn1_0 = Data[(nn-19):nn,]$agn
xagn1_1<-predict(model1_1, Data[(nn-19):nn,]) + mean(xagn1_0)
xagn1_2<-predict(model1_2, Data[(nn-19):nn,]) + mean(xagn1_0)

plot(1:20, abs(xagn1_0-xagn1_1), ylim = c(0, 10),  pch = 19, col = "blue", main = "True vs Fitted values", xlab = "Observations", ylab = "")
par(new=TRUE)
plot(1:20, abs(xagn1_0-xagn1_2), ylim = c(0, 10),  pch = 19, col = "red", main = "True vs Fitted values", xlab = "Observations", ylab = "")

plot(1:50, 1.5*model1_1$residuals, type = "n", main = "True vs Fitted values", xlab = "Observations", ylab = "")
points(1:50, model1_1$residuals, pch = 19, col = "blue")
points(1:length(model1_2$residuals), model1_2$residuals, pch = 19, col = "red")


modelf1_1<-lm(agn~.-agn,data=Data[(nn-200):(nn-10),])
summary(modelf1_1)
influencePlot(modelf1_1)
shapiro.test(modelf1_1$residuals)

modelf1_2<-lm(agn~.-agn,data=data1_2[(nn-200):(nn-10),])
summary(modelf1_2)
influencePlot(modelf1_2)
shapiro.test(modelf1_2$residuals)

plot(1:191, 1.5*modelf1_1$residuals, type = "n", main = "True vs Fitted values", xlab = "Observations", ylab = "")
points(1:191, modelf1_1$residuals, pch = 19, col = "blue")
points(1:191, modelf1_2$residuals, pch = 19, col = "red")

xagnf1_0 = Data[1:191,]$agn
xagnf1_1<-predict(model1_1, Data[1:191,]) #+ mean(xagnf1_0)
xagnf1_2<-predict(model1_2, Data[1:191,]) #+ mean(xagnf1_0)

plot(1:191, abs(xagnf1_0-xagnf1_1), pch = 19, col = "blue", main = "True vs Fitted values", xlab = "Observations", ylab = "")
par(new=TRUE)
plot(1:191, abs(xagnf1_0-xagnf1_2), pch = 19, col = "red", main = "True vs Fitted values", xlab = "Observations", ylab = "")
#points(1:191, abs(xadif- xadif2), pch = 19, col = "red")
sum(abs(xadif-xadif1))
sum(abs(xadif-xadif2))
