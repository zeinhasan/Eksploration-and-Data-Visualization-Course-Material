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

databaru= dplyr::select(dataku,-Kabupaten)
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
#install.packages("AID")
library(AID)
out = boxcoxnc(databaru$penderita, method = "mle", 
               lambda = seq(-2,2,0.001), verbose = F, plot = F)
out$lambda.hat


#2. Transformasi Box-cox dengan library MASS
#install.packages("MASS")
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
hasil=dplyr::select(databaru.bc, bulan, tfbc)

#membuat boxplot setelah ditransformasi
library(plotly)
y = ggplot(databaru.bc, aes(x=bulan, y=tfbc,
                            fill=bulan))+geom_boxplot()+
  ggtitle("Boxplot Transformasi Penderita DBD Bulan 
                    Januari-April")+xlab("Bulan")+ylab("Jumlah")
ggplotly(y)

#Untuk melihat lebih jelas distribusi angkatan bisa distandardisasi kemudian dibuat boxplot lagi
