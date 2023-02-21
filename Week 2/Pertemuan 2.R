# Panggil Library 
library(openxlsx)

# Baca Data
mydata <- read.xlsx("D:\\Kuliah\\Praktikum Eksplorasi dan Visualisasi Data\\Pertemuan 2\\Salary.xlsx")

# Cek sebagian data
head(mydata)
tail(mydata)

###########################    Ukuran Pusat    #################################
# Mencari Mean 
rataMiami <- mean(mydata$Miami)
rataMiami

# Mencari Median
medianMiami <- median(mydata$Miami)
medianMiami

# Mencari Kuartil
# Kuartil 1
Q1_Miami = quantile(mydata$Miami, prob=0.25)
Q1_Miami

# Kuartil 2 
Q2_Miami = quantile(mydata$Miami, prob=0.50)
Q2_Miami 

# Kuartil 3
Q3_Miami = quantile(mydata$Miami, prob=0.75)
Q3_Miami

# Modus
Modus <-function(x){
  u <- unique(x)
  tab <- tabulate(match(x,u))
  u[tab ==max(tab)]
}

Modus(mydata$Miami)

#######################      Ukuran Sebaran         ############################
# Range / Jangkauan
Jangkauan <- function(x){
  jangkauan = max(x)-min(x)
  return(jangkauan)
}
Jangkauan(mydata$Miami)

# Variansi 
varian_Miami <- var(mydata$Miami)
options(digits = 20) 
varian_Miami

# Standar Deviasi
sd_Miami <- sd(mydata$Miami)
sd_Miami

# Ringkasan 
country <- c("Miami")
ringkasan <- data.frame(country,
                        meanSallary = mean(mydata$Miami),
                        MedianSallary=median(mydata$Miami),
                        Q1 = quantile(mydata$Miami,prob=0.25),
                        Q3=quantile(mydata$Miami,prob=0.75),
                        variansi = var(mydata$Miami),
                        Std_dev=sd(mydata$Miami),
                        IQR=IQR(mydata$Miami),
                        min=min(mydata$Miami),
                        max=max(mydata$Miami),
                        row.names = NULL)
ringkasan
