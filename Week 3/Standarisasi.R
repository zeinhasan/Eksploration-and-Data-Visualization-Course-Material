# Read data
library(readxl)
dataku <- read_excel("D:\\Kuliah\\Praktikum Eksplorasi dan Visualisasi Data\\Pertemuan 3\\Pertemuan 4.xlsx")

# Check Data
head(dataku)

boxplot(dataku,xlab="Variable",ylab="Count",
        main="BoxplotDaily Spending VS Entertainment Spending",
        col = c("red","blue"))

legend(legend=c("Daily Spending","Entertainment Spending"),
       col = c("red","blue"),'topright',
       cex=0.8,fill=c("red","blue"),title="Legenda")


# Summary 
summary(dataku)
IQR(dataku$`Daily Spending`)
IQR(dataku$`Entertainment Spending`)


# Standarisasi 
standarisasi <- function(data,pusat,sebaran){
  z=(data-pusat)/sebaran
  return(z)
}

# Standarisasi Daily Spending
std_Dailyspd <- standarisasi(dataku$`Daily Spending`,
                             median(dataku$`Daily Spending`),
                             IQR(dataku$`Daily Spending`))
head(std_Dailyspd)

# Standarisasi Entertaiment Spending
std_Entairspd <- standarisasi(dataku$`Entertainment Spending`,
                             median(dataku$`Entertainment Spending`),
                             IQR(dataku$`Entertainment Spending`))
head(std_Entairspd)

# Mengunakan Packages
######## Warning Tidak Semua Pusat dan Sebaran Digunakan ###########
library(robustHD)
std_data <- standardize(dataku, centerFun = median, scaleFun = IQR)

head(std_data)


# Boxplot Hasil Standarisasi 
data_terstd <- data.frame(std_Dailyspd,std_Entairspd)

boxplot(data_terstd,xlab="Variable",ylab="Count",
        main="Boxplot Daily Spending VS Entertainment Spending (Standardized)",
        col = c("red","blue"))

legend(legend=c("Daily Spending","Entertainment Spending"),
       col = c("red","blue"),'topright',
       cex=0.8,fill=c("red","blue"),title="Legenda")


