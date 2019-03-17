filenames=list.files(path="./lib", full.names=TRUE)
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
Data1<-Data[-nn,]
for(i in 1:10) Data1[,i]<-diff(log(Data[,i]), lag = 1)
nn<-nn-1
datnf<-Data1[(nn-59):(nn-10),]
model1<-lm(agn~.-agn, data = datnf)
summary(model1)
shapiro.test(model1$residuals)
plot(model1$fitted.values, datnf$agn, xlab = "fitted values", ylab = "agn", main = "?????????????? - ????????????, ???????????? 1")
abline(0,1, col = "red")
plot(model1$fitted.values, model1$residuals, xlab = "fitted values", ylab = "residials", main = "??????????????-??????????????, ???????????? 1")
library(carData)
library(car)
influencePlot(model1)
model1<-lm(agn~.-agn, data = datnf)
model2<-lm(agn~.-agn, data = datnf[-c(2360-2334+1),])
summary(model2)
plot(model2$fitted.values, datnf[-c(2360-2334+1),1], xlab = "fitted values", ylab = "agn", main = "?????????????? - ????????????, ???????????? 2")
abline(0,1, col = "red")
plot(model2$fitted.values, model2$residuals, xlab = "fitted values", ylab = "residials", main = "??????????????-??????????????, ???????????? 2")
model1$coefficients-model2$coefficients
shapiro.test(model2$residuals)
influencePlot(model2)

xadi1<-predict(model1, Data[(nn-9):nn,])
xadi2<-predict(model2, Data[(nn-9):nn,])
xadi = Data[(nn-9):nn,]$agn

plot(1:10, abs(xadi-xadi1),  pch = 19, col = "blue", main = "True vs Fitted values", xlab = "Observations", ylab = "")
par(new=TRUE)
points(1:10, abs(xadi- xadi2), pch = 19, col = "red")

sum(abs(xadi-xadi1))
sum(abs(xadi-xadi2))

plot(1:50, 1.5*model1$residuals, type = "n", main = "True vs Fitted values", xlab = "Observations", ylab = "")
points(1:50, model1$residuals, pch = 19, col = "blue")
points(1:49, model2$residuals, pch = 19, col = "red")


modelf1<-lm(agn~.-agn,data=Data[(nn-200):(nn-10),])
summary(modelf1)
influencePlot(modelf1)
shapiro.test(modelf1$residuals)

modelf2<-lm(agn~.-agn,data=Data[-c(2360),][(nn-200):(nn-10),])
summary(modelf2)
influencePlot(modelf2)
shapiro.test(modelf2$residuals)

plot(1:191, 1.5*modelf1$residuals, type = "n", main = "True vs Fitted values", xlab = "Observations", ylab = "")
points(1:191, modelf1$residuals, pch = 19, col = "blue")
points(1:191, modelf2$residuals, pch = 19, col = "red")

xadif1<-predict(model1, Data[1:191,])
xadif2<-predict(model2, Data[1:191,])
xadif = Data[1:191,]$agn

plot(1:191, abs(xadif-xadif1),  pch = 19, col = "blue", main = "True vs Fitted values", xlab = "Observations", ylab = "")
par(new=TRUE)
plot(1:191, abs(xadif-xadif2),  pch = 19, col = "red", main = "True vs Fitted values", xlab = "Observations", ylab = "")
#points(1:191, abs(xadif- xadif2), pch = 19, col = "red")
sum(abs(xadif-xadif1))
sum(abs(xadif-xadif2))
