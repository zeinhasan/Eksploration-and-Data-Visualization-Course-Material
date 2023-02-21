# Library
library(readxl)
library(car)
library(broom)
# Read data
data<-read_excel(file.choose(),sheet = 3)

# Uji Normalitas 
boxplot(data$`Wind(mph)`,main="Boxplot Kecepatan Angin Asli",
        xlab="Jarak",ylab="Values",col = "lightblue")

# Transformasi
data$WindTFA <- sqrt(data$`Wind(mph)`)
Nisbah_WindTFA = (median(data$WindTFA)-quantile(data$WindTFA,0.25))/IQR(data$WindTFA)
Nisbah_WindTFA

boxplot(data$WindTFA,main="Boxplot Kecepatan Angin Transformed Akar",
        xlab="Wind",ylab="Values",col = "lightblue")

#Transformasi Box-cox dengan library car
library(car)
out = powerTransform(data$`Wind(mph)`, family = "bcPower")
out$lambda

data$WindTFB <- (data$`Wind(mph)`)^0.6952258 

Nisbah_WindTFB = (median(data$WindTFB)-quantile(data$WindTFB,0.25))/IQR(data$WindTFB)
Nisbah_WindTFB

boxplot(data$WindTFB,main="Boxplot Kecepatan Angin Transformed Boxcox",
        xlab="Wind",ylab="Values",col = "lightblue")

# Linearitas
# Scatterplot
plot(data$`Temp(F)`,data$`Wind(mph)`,
     main="Kecepatan VS Temperatur",
     xlab = "Temperatur",
     ylab = "Kecepatan",col="blue",pch = 19)
# Garis Regresi linear
abline(lm(data$`Wind(mph)`~data$`Temp(F)`,data = data),
       col="red",lwd=2)

# model summary
model <-lm(data$`Wind(mph)`~data$`Temp(F)`,data = data)
glance(model)[c(1,2,3)]
cor(data$`Wind(mph)`,data$`Temp(F)`)

# Model Regresi 
modelfix <-lm(data$WindTFB~data$`Temp(F)`,data = data)

# Uji Overall
summary.aov(modelfix)

#Uji Parsial
summary(modelfix)

# Model terbentuk
modelfix$coefficients 
