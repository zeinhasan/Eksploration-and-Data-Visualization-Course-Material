# Uji 2 Rata-Rata Independen 
# library
library(tidyverse)
library(plotly) # untuk plot
library(reshape)# untuk reshape/melt data
# Data
baru = c(82,87,85,78,91,94,85,81,84)
standar = c(85,91,79,75,84,95,77,84,81)
# Make data frame
data = data.frame(baru,standar)

# melted data
data2 <- melt(data)

# Boxplot 
x = ggplot(data2, aes(x=variable,y=value,fill=variable))+
  geom_boxplot()+
  ggtitle("data nilai siswa kelas A (baru) dan B (standar)")+
  xlab("Variabel")+
  ylab("Values")

ggplotly(x)

#Asumsi Kesamaan Dua Variansi
# Secara deskriptif
#Variansi dari variabel baru
var(baru)
#Variansi dari variabel standar
var(standar)

#Secara inferensi
var.test(baru,standar,ratio = 1,alternative = "two.sided",
         conf.level = 0.95)

#Uji Hipotesis
#Melakukan uji t 2 sampel independen baru dan standar
t.test(baru,standar,alternative="greater", mu=0,
       var.equal=TRUE)
# tcrit
qt(0.05,16,lower.tail = F)


###############################################################

#UJI MEAN DUA POPULASI DEPENDEN
#Asumsi Normalitas
#menginput data
sebelum = c(78,77,73,65,59,53,61,62,62,60,61)
sesudah = c(78,73,74,61,59,49,60,62,57,58,65)
#membentuk data frame
data_d = data.frame(sebelum,sesudah)
library(tidyverse)
library(plotly)
#menyatukan variabel baru dan standar dari data frame sebagai data_d2
data_d2 = data_d %>%
  gather(key="keterangan", value="bb")

#membuat boxplot variabel baru dan standar
a = ggplot(data_d2, aes(x=keterangan,y=bb,fill=keterangan))+
  geom_boxplot()+ggtitle("Boxplot Berat Badan Sebelum dan Sesudah")+xlab("Variabel")+ylab("Berat Badan")
ggplotly(a)
#melakukan transformasi box-cox
library(car)
out = powerTransform(data_d2$bb, family = "bcPower")
out$lambda
#melakukan transformasi terhadap data
transformasi.bc=function(data, lambda){
  for(i in 1:length(data)){
    data[i]=-1*data[i]^lambda
  }
  return(data)
}
data_d$sebelum=transformasi.bc(data_d$sebelum,-0.6)
data_d$sesudah=transformasi.bc(data_d$sesudah,-0.6)
datad.bc=gather(data_d, key="keterangan", value="tfbb")

#membuat boxplot hasil transformasi
b = ggplot(datad.bc, aes(x=keterangan, y=tfbb,fill=keterangan))+
  geom_boxplot()+ggtitle("Boxplot Transformasi Berat Badan Sebelum dan Sesudah")+xlab("Variabel")+ylab("Berat Badan")
ggplotly(b)

#Uji Hipotesis
#Melakukan uji t 2 sampel dependen sebelum dan sesudah
t.test(x = data_d$sebelum, y = data_d$sesudah, alternative = "greater", mu = 0, 
       paired = TRUE, conf.level = 0.95)

#t tabel
qt(0.95,10)




