#menyiapkan data
library(openxlsx)
data=read.xlsx("D:\\KULIAH\\Asprak\\P-4.xlsx")

#boxplot sebelum transformasi
library(plotly)
data_boxplot=ggplot(data,aes(y=AKB))+
  geom_boxplot(fill="gold",color="yellow")+
  ggtitle("Boxplot of AKB")+
  xlab("AKB")+ylab("Value")
ggplotly(data_boxplot)

#Membuat transformasi AKB
tfAKB=-1/data$AKB^2

#Membuat dataframe data sebelum dan setelah transformasi
transformasi=data.frame(data$AKB,tfAKB)

#membuat boxplot gabungan dengan ringkasan 5 angka setelah transformasi
#menyatukan kolom data menggunakan data wrangling dengan dataframe transformasi

library(tidyverse)
data2 = gather(transformasi,key="variabel", value="value")
data2
boxplot_tf = ggplot(data2, aes(x=variabel, y=value,
                    fill=variabel))+geom_boxplot()+
                    ggtitle("Boxplot AKB dan transformasi AKB")+
                    xlab("Variabel")+ylab("Values")
ggplotly(boxplot_tf)

#standarisasi
library(robustHD)
standardisasi=standardize(transformasi,centerFun = median,scaleFun = IQR)

#boxplot setelah ditransformasi
#menjadikan data menjadi 1 kolom menggunakan data wrangling 
data3 = gather(standardisasi,key="variabel", value="value")
data3
boxplotstd = ggplot(data3, aes(x=variabel, y=value,
                fill=variabel))+geom_boxplot()+
                ggtitle("Boxplot Standardisasi AKB dan transformasi AKB")+
                xlab("Variabel")+ylab("Values")
ggplotly(boxplotstd)
#Cek nisbah
summary(tfAKB)

#cara biar engga satu satu
#membuat boxplot
library(plotly)
data_boxplot=ggplot(data,aes(y=AKB))+
  geom_boxplot(fill="gold",color="yellow")+
  ggtitle("Boxplot of AKB")+
  xlab("AKB")+ylab("Value")
ggplotly(data_boxplot)

#menjurai ke atas
#transformasi tukey
data1=transform(data,Akar_AKB=sqrt(data$AKB),paling_kuat=-1/data$AKB^2)
data1

#membuat boxplot hasil transformasi
#menyiapkan data
library(tidyverse)
data_transformasi=data1%>%
  select(-Provinsi)
data2=data1%>%
  select(-Provinsi)%>%
  gather(data1)

#boxplot
data_boxplot1=ggplot(data2,aes(x=data1,y=value))+
  geom_boxplot(fill="pink",color="red")+
  ggtitle("Boxplot of AKB")+
  xlab("AKB")+ylab("Value")
ggplotly(data_boxplot1)

#standardisasi
library(robustHD)
std_data=standardize(data_transformasi,centerFun = median, scaleFun = IQR)

#boxplot setelah standardisasi
boxplot(std_data[1:3],col="pink",border="red",
        main="Boxplot of AKB", xlab="Jenis Transformasi",
        ylab="Value")

#bisa dicek nisbah ya!

#transformasi boxcox
library(openxlsx)
data_box=read.xlsx("D:\\KULIAH\\Asprak\\P-4.xlsx",sheet=2)
library(tidyverse)
#Mengganti variabel bernilai nol
seleksi=function(x){
  for(i in 1:length(x)){
    if(x[i]==0){
      x[i]=0.0001
    }
  }
  return(x)
}

#menyiapkan data untuk dibuat boxplot
dataku=data_box%>%
  gather(key="bulan",
         value="penderita",
         -Kabupaten)

databaru=select(dataku,-Kabupaten)
databaru$penderita=seleksi(databaru$penderita)

#boxplot sebelum transformasi
library(plotly)
a = ggplot(databaru, aes(x=bulan, y=penderita,fill=bulan))+geom_boxplot()+
  ggtitle("Boxplot Transformasi Penderita DBD Bulan Januari-April")+
  xlab("Bulan")+ylab("Jumlah")
ggplotly(a)



#Mencari Nilai Lamda
#Ada beberapa cara

#1. Transformasi Box-cox dengan library AID
install.packages("AID")
library(AID)
out = boxcoxnc(databaru$penderita, method = "mle", 
               lambda = seq(-2,2,0.001), verbose = F, plot = F)
out$lambda.hat


#2. Transformasi Box-cox dengan library MASS
install.packages("MASS")
library(MASS)
out = boxcox(databaru$penderita~1, lambda = seq(-2,2,0.0001), plotit = F)
out$x[which.max(out$y)]

#3.Transformasi Box-cox dengan library car
library(car)
out = powerTransform(databaru$penderita, family = "bcPower")
out$lambda

#jika lamda minus?
#Transformasi data dengan Metode Box-Cox
#Melakukan transformasi terhadap data
transformasi.bc=function(data, lambda){
  for(i in 1:length(data)){
    data[i]=data[i]^lambda
  }
  return(data)
}
databaru.bc=databaru %>% 
  mutate(tfbc=transformasi.bc(databaru$penderita,0.28))
hasil=select(databaru.bc, bulan, tfbc)

#membuat boxplot setelah ditransformasi
library(plotly)
y = ggplot(databaru.bc, aes(x=bulan, y=tfbc,
            fill=bulan))+geom_boxplot()+
            ggtitle("Boxplot Transformasi Penderita DBD Bulan 
                    Januari-April")+xlab("Bulan")+ylab("Jumlah")
ggplotly(y)

#Untuk melihat lebih jelas distribusi angkatan bisa distandardisasi kemudian dibuat boxplot lagi
