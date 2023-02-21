# Read data
library(readxl)
data1<- read_xlsx(file.choose(),sheet = 1)

# No 1 Uji Z
library(BSDA)

# Check normalitas 
boxplot(data1$hasilpanen,
        ylab="count",
        xlab='Kacang Tanah',
        main="Boxplot Hasil Panen Kacang Tanah (kg)")

summary(data1$hasilpanen)

# Ks Test Intermezzo
ks.test(data1$hasilpanen,'pnorm',mean=mean(data1$hasilpanen),sd=sd(data1$hasilpanen))

# Uji Rata-Rata 1 Populasi (Uji Z)
z.test(data1$hasilpanen,alternative="less",
       mu=19500,sigma.x=10000,
       conf.level = 0.95)

qnorm(0.05,lower.tail = F)
s
# No 2 Uji Z
data2 <- read_xlsx(file.choose(),sheet = 2)

# Check normalitas 
boxplot(data2$`Usia Pegawai Kantor`,
        ylab="count",
        xlab='Kacang Tanah',
        main="Boxplot Usia Pegawai Kantor")

# Estimasi standar deviasi
sd(data2$`Usia Pegawai Kantor`)

# Uji Rata-Rata 1 Populasi (Uji Z)
z.test(data2$`Usia Pegawai Kantor`,alternative="greater",
       mu=50,sigma.x = sd(data2$`Usia Pegawai Kantor`),
       conf.level = 0.95)

qnorm(0.05,lower.tail = F)



#Contoh 1 data Berdistribusi Normal
#Uji lah apakah berat anak ayam = 150?

#Menyiapkan data
chick<-chickwts

#Alasan pemilihan uji??

#Cek Asumsi
boxplot(chick$weight,col="blue",border="skyblue",
        main="Cek Normalitas",xlab="Anak ayam",
        ylab="Berat")

#t test
t.test(chick$weight, alternative = "two.sided", 
       mu=150,conf.level = 0.95)

#daerah kritis
qt(0.975,70)


#untuk contoh 1 diatas uji z bisa digunakan??
library(BSDA)
z.test(chick$weight, alternative = "two.sided", mu=150,
       sigma.x =78.0737, conf.level = 0.95)


#Contoh 2, Data tidak berdistribusi normal
#Kasus: Ujilah hipotesis bahwa rata-rata waktu pendaftaran kurang dari 50 
#menit. Gunakan tingkat signifikansi 0.01 

#Library yang dibutuhkan
library(AID)
library(MASS)
library(car)
library(tidyverse)

#menyiapkan data
my_data<-data.frame(waktu=c(10,100,500,18.8,67.4,
                            46,56,47,58.9,50.2,25,25))
#Cek Normalitas
boxplot(my_data$waktu,col="blue",border="skyblue",
        main="Cek Normalitas",
        ylab="Waktu")

#transformasi dengan boxcox

#1. Transformasi Box-cox dengan library AID
out = boxcoxnc(my_data$waktu, method = "mle",
               lambda = seq(-2,2,0.001), verbose = F, plot
               = F)
out$lambda.hat

#2.Transformasi Box-cox dengan library MASS
out = boxcox(my_data$waktu~1, lambda =
                     seq(-2,2,0.0001), plotit = F)
out$x[which.max(out$y)]

#3. Transformasi Box-cox dengan library Car
out = powerTransform(my_data$waktu, family =
                             "bcPower")
out$lambda

#Data ditransformasi dengan dantum^-0.3
transformasi.bc=function(data, lambda){
        for(i in 1:length(data)){
                data[i]=-1*(data[i]^lambda)
        }
        return(data)
}

my_data2=my_data %>%
        mutate(tfbc=transformasi.bc(my_data$waktu,-0.3))

#melihat boxplot setelah transformasi
boxplot(my_data2$tfbc,col="blue",border="skyblue",
        main="Cek Normalitas",
        ylab="Waktu")

#Suplemen Secara Inferensi
shapiro.test(my_data$waktu)
shapiro.test(my_data2$tfbc)

#Uji Hipotesis Satu Angkatan
t.test(my_data2$tfbc,alternative = "less", mu=-50^(-0.3),conf.level = 0.99)

#t table
qt(0.99,11)
