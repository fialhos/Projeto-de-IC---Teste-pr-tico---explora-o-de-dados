rm(list = ls())
set.seed(123)
library("caret")
library("som")
library("corrplot")

##Carregamento


baseDados <- read.csv("C:/Users/Vini/Desktop/Bit COin/data.csv",header = FALSE,na.string="NA")

summary(baseDados)

#PLOT

# plot(baseDados2[,1],baseDados2[,2],type = "l",col="red",xlim = c(0,30000), ylim = c(0,30),pch=19)
# points(baseDados2[,1],baseDados2[,3],type = "l",col="blue",pch=19)


baseDados2 <- som::normalize(baseDados,byrow = FALSE)

plot(baseDados[,1],baseDados2[,2],type = "l",col="red",ylab="Stock(Reb),Currency(Blue)",xlab="Tempo")
points(baseDados[,1],baseDados2[,3],type = "l",col="blue")

boxplot(baseDados2[,2],baseDados2[,3],main="Boxplot",names=c("Stock","Currency"))

#Correlaçao

correlationMatrix <- cor(baseDados)
corrplot(correlationMatrix, method="circle", type="lower", order="hclust")

cor(baseDados)
cor.test(baseDados[,2],baseDados[,3])
cor.test(baseDados[,1],baseDados[,2])
cor.test(baseDados[,1],baseDados[,3])

#medias
mean(baseDados[,2])
mean(baseDados[,3])

sd(baseDados[,2])
sd(baseDados[,3])

var(baseDados[,2])
var(baseDados[,3])


mediastock= matrix(nrow = 100, ncol = 2)
mediacurrency = matrix(nrow = 100, ncol = 2)

b1 <- c()
b2 <- c()
n=0
for(i in 1:100){
  for(j in 1:100){
  
    b1[j]<-baseDados[n+j,2]
    b2[j]<-baseDados[n+j,3]
    }
  mediastock[i,1]<-mean(b1)
  mediastock[i,2]<-i
  mediacurrency[i,1]<-mean(b2)
  mediacurrency[i,2]<-i
  n=n+100
}


#mediastock <- som::normalize(mediastock,byrow = FALSE)
#mediacurrency <- som::normalize(mediacurrency,byrow = FALSE)

windows()
plot(mediastock[,1],mediacurrency[,1])
abline(lm(mediacurrency[,1] ~ mediastock[,1]))

plot(mediastock[,2],mediastock[,1],type = "l",col="red")
points(mediacurrency[,2],mediacurrency[,1],type = "l",col="blue")

ajuste=lm(mediastock[,1] ~ mediacurrency[,1])

summary(ajuste)

anova(ajuste)

confint(ajuste)

summary(ajuste)

windows()
plot(fitted(ajuste),residuals(ajuste),xlab="Valores Ajustados",ylab="Resíduos")
abline(h=0)
windows()
plot(mediastock[,1],residuals(ajuste),xlab="Experiência",ylab="Resíduos")
abline(h=0)


