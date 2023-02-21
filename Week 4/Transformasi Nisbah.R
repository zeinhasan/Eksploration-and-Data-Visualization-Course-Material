# Panggil Library yang dibutuhkan 
library(readxl)

# Read data 
data <- read_excel(file.choose())

# Check data
head(data)

# pilih variabel yg digunakan
mydata <- data[,-1]

# buat boxplot 
boxplot(mydata,col = c("red","blue","green","yellow"),
        main="Boxplot DBD Jawa Barat Bulan Januari - April")

# Tentukan masing-masing median dan IQR dari setiap angkatan
library(tidyverse)
ringkas <- mydata %>% 
  gather(key="Bulan",value = "Counts") %>% 
  group_by(Bulan) %>% 
  summarise(iqr=IQR(Counts),Med=median(Counts))

ringkas

# Plot log median sebagai sumbu (X) vs log IQR sebagai sumbu Y.
ringkas$logIQR <- log(ringkas$iqr,base = 10)
ringkas$logMed <- log(ringkas$Med,base = 10)

ringkas

plot(ringkas$logMed,ringkas$logIQR,pch = 16, cex = 1,col="blue",
     main = "Plot Log Median vs Log IQR",xlab="logMed",ylab="logIQR")
abline(lm(ringkas$logIQR~ringkas$logMed))

#Cek hasil nisbah berdasarkan nilai
summary(lm(ringkas$logIQR~ringkas$logMed))

# lakukan transformasi 
mydataTRF <- sqrt(mydata)
head(mydataTRF)

# Buat Boxplot hasil transformasi
boxplot(mydataTRF,col = c("red","blue","green","yellow"),
        main="Boxplot Transformed data DBD Jawa Barat Bulan Januari - April")
