# Library
library(readxl)
library(car)
library(broom)
# Read data
data<-read_excel(file.choose(),sheet = 1)

# Uji Normalitas 
boxplot(data$`Jumlah Cacat Produksi`,main="Boxplot Jumlah Cacat Produksi Asli",
        xlab="Cacat Produksi",ylab="Values",col = "lightblue")

# Cek Nisbah
Nisbah_Asli = (median(data$`Jumlah Cacat Produksi`)-quantile(data$`Jumlah Cacat Produksi`,0.25))/IQR(data$`Jumlah Cacat Produksi`)
Nisbah_Asli

# Transformasi Satu angkatan
# Transformasi x^2
data$Ytf2 <-(data$`Jumlah Cacat Produksi`)^2
Nisbah_Ytf2 = (median(data$Ytf2)-quantile(data$Ytf2,0.25))/IQR(data$Ytf2)
Nisbah_Ytf2

# Transformasi x^3
data$Ytf3 <-(data$`Jumlah Cacat Produksi`)^3
Nisbah_Ytf3 = (median(data$Ytf3)-quantile(data$Ytf3,0.25))/IQR(data$Ytf3)
Nisbah_Ytf3

# Transformasi x^4
data$Ytf4 <-(data$`Jumlah Cacat Produksi`)^4
Nisbah_Ytf4 = (median(data$Ytf4)-quantile(data$Ytf4,0.25))/IQR(data$Ytf4)
Nisbah_Ytf4

# Transformasi x^5
data$Ytf5 <-(data$`Jumlah Cacat Produksi`)^5
Nisbah_Ytf5 = (median(data$Ytf5)-quantile(data$Ytf5,0.25))/IQR(data$Ytf5)
Nisbah_Ytf5


# Cek hasil transformasi
boxplot(data$Ytf2,main="Boxplot Jumlah Cacat Produksi Transformed X^2"
        ,xlab="Cacat Produksi",ylab="Values",col = "lightblue")

boxplot(data$Ytf3,main="Boxplot Jumlah Cacat Produksi Transformed X^3"
        ,xlab="Cacat Produksi",ylab="Values",col = "lightblue")

boxplot(data$Ytf4,main="Boxplot Jumlah Cacat Produksi Transformed X^4"
        ,xlab="Cacat Produksi",ylab="Values",col = "lightblue")

boxplot(data$Ytf5,main="Boxplot Jumlah Cacat Produksi Transformed X^5"
        ,xlab="Cacat Produksi",ylab="Values",col = "lightblue")

# Linearitas
# Scatterplot
plot(data$`Rata-rata Suhu Ruangan`,data$`Jumlah Cacat Produksi`,
     main="Rata-rata Suhu Ruangan VS Jumlah Cacat Produksi",
     xlab = "Rata-rata Suhu Ruangan",
     ylab = "Jumlah Cacat Produksi",col="blue",pch = 19)
# Garis Regresi linear
abline(lm(data$`Jumlah Cacat Produksi`~data$`Rata-rata Suhu Ruangan`,data = data),
       col="red",lwd=2)

# Regresi linear sederhana
# model summary
model <-lm(data$`Jumlah Cacat Produksi`~data$`Rata-rata Suhu Ruangan`,data = data)
glance(model)[c(1,2,3)]
cor(data$`Rata-rata Suhu Ruangan`,data$`Jumlah Cacat Produksi`)

# Model Regresi 
modelfix <-lm(Ytf3~`Rata-rata Suhu Ruangan`,data = data)

# Uji Overall
summary.aov(modelfix)

#Uji Parsial
summary(modelfix)

# Model terbentuk
modelfix$coefficients
