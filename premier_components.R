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
#Data1<-Data[-nn,]
#for(i in 1:10) Data1[,i]<-diff(log(Data[,i]), lag = 1)
#nn<-nn-1
data2_1<-Data[1:(nn-20),]
data2_2<-Data[(nn-69):(nn-20),]

#model1
pc1<-princomp(data2_1[, -1], cor = T)
# pc11<-prcomp(data1, center = TRUE, scale = TRUE)
#print(pc11$rotation)
summary(pc1)
plot(pc1, main = "the principal values' diagram") # =SS = variation 
loadings(pc1) #coordinats of pv = wages == rotation

#model2
pc2<-princomp(data2_2[, -1], cor = T)
summary(pc2)
plot(pc2, main = "the principal values' diagram")
loadings(pc2)

#general
standart <- function(x){(x - mean(x))}
#model1
data_stand1 <- apply (data2_1[, -1], 2, standart)
data_pc1 <- data_stand1 %*% pc1$loadings
data_pc1 <- cbind(data_pc1, data2_1[,1])
colnames(data_pc1)[10] = "agn"
#model1<-lm(agn~., data = as.data.frame(data_pc1))
model2_1<-lm(agn~Comp.1 + Comp.2, data = as.data.frame(data_pc1))
summary(model2_1)
shapiro.test(model2_1$residuals)

#model2
data_stand2 <- apply (data2_2[, -1], 2, standart)
data_pc2 <- data_stand2 %*% pc2$loadings
data_pc2 <- cbind(data_pc2, data2_2[,1])
colnames(data_pc2)[10] = "agn"
#model1<-lm(agn~., data = as.data.frame(data_pc1))
model2_2<-lm(agn~Comp.1 + Comp.2 + Comp.3 + Comp.4 + Comp.5, data = as.data.frame(data_pc1))
summary(model2_2)
shapiro.test(model2_2$residuals)

#data_predict
data_stand_predict <- apply (Data[(nn-19):nn,-1], 2, standart)
new_pc = princomp(Data[(nn-19):nn,], cor = TRUE)
data_predict = data_stand_predict %*% pc1$loadings
data_predict = cbind(data_predict, Data[(nn-19):nn,1])
colnames(data_predict)[10] = "agn"

y2_1<-predict(model2_1, as.data.frame(data_predict))
y2_2<-predict(model2_2, as.data.frame(data_predict))
y2_0 = Data[(nn-19):nn,]$agn


plot(1:20, abs(y2_0 - y2_1),  pch = 19, col = "blue", main = "True vs Fitted values", xlab = "Observations", ylab = "")
par(new=TRUE)
points(1:20, abs(y2_0 - y2_2), pch = 19, col = "red")
#model2 is better


plot(1:20, abs(y2_0-xagn1_2), ylim = c(0,95),  pch = 19, col = "green", main = "True vs Fitted values", xlab = "Observations", ylab = "")
par(new=TRUE)
plot(1:20, abs(y2_0 - y2_2),ylim = c(0,95), pch = 19, col = "red", main = "True vs Fitted values", xlab = "Observations", ylab = "")

