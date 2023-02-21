# Library
library(readxl)
library(car)
library(broom)
# Read data
data<-read_excel(file.choose(),sheet = 2)

# Uji Normalitas 
boxplot(data$Jarak,main="Boxplot Jarak Asli",
        xlab="Jarak",ylab="Values",col = "lightblue")

Nisbah_Asli = (median(data$Jarak)-quantile(data$Jarak,0.25))/IQR(data$Jarak)
Nisbah_Asli

# Transformasi Akar
data$JarakTF = sqrt(data$Jarak) 
boxplot(data$JarakTF,main="Boxplot Jarak Transformed",
        xlab="Jarak",ylab="Values",col = "lightblue")

Nisbah_JarakTF = (median(data$JarakTF)-quantile(data$JarakTF,0.25))/IQR(data$JarakTF)
Nisbah_JarakTF

# Transformasi -1/y
data$JarakTF2 = -1/(data$Jarak) 

boxplot(data$JarakTF2,main="Boxplot Jarak Transformed -1/y",
        xlab="Jarak",ylab="Values",col = "lightblue")

Nisbah_JarakTF2 = (median(data$JarakTF2)-quantile(data$JarakTF2,0.25))/IQR(data$JarakTF2)
Nisbah_JarakTF2

# Transformasi boxcox
#Transformasi Box-cox dengan library car
library(car)
out = powerTransform(data$Jarak, family = "bcPower")
out$lambda

data$JarakTFB <- (data$Jarak)^(0.4950762)

boxplot(data$JarakTFB,main="Boxplot Jarak Transformed Box-Cox",
        xlab="Jarak",ylab="Values",col = "lightblue")

Nisbah_JarakTFB = (median(data$JarakTFB)-quantile(data$JarakTFB,0.25))/IQR(data$JarakTFB)
Nisbah_JarakTFB

# Linearitas
# Scatterplot
plot(data$Kecepatan,data$Jarak,
     main="Kecepatan VS Jarak",
     xlab = "Kecepatan",
     ylab = "Jarak",col="blue",pch = 19)
# Garis Regresi linear
abline(lm(data$Jarak~data$Kecepatan,data = data),
       col="red",lwd=2)

# model summary
model <-lm(data$Jarak~data$Kecepatan,data = data)
glance(model)[c(1,2,3)]
cor(data$Jarak,data$Kecepatan)

# Model Regresi 
modelfix <-lm(data$JarakTFB~data$Kecepatan,data = data)

# Uji Overall
summary.aov(modelfix)

#Uji Parsial
summary(modelfix)

# Model terbentuk
modelfix$coefficients 

