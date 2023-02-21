# No 1
# Panggil Library 
library(openxlsx)

# Baca Data
mydata <- read.xlsx("D:\\Kuliah\\Praktikum Eksplorasi dan Visualisasi Data\\Pertemuan 2\\Salary.xlsx")

# Cek sebagian data
head(mydata)
tail(mydata)

# Fungsi Trirata 
data <- read.delim("clipboard")
Trirata <- function(data){
  Trirata = (quantile(data,probs = 0.75)+quantile(data,probs = 0.25)
             +2*median(data))/4
  return(Trirata)
}

Trirata(data$data)

# No 2

ringkas <- mydata %>% 
  gather(key="Country",value = "Pendapatan") %>% 
  group_by(Country) %>% 
  summarise(meanSallary = mean(Pendapatan),MedianSallary=median(Pendapatan),
            Q1 = quantile(Pendapatan,prob=0.25),Q3=quantile(Pendapatan,prob=0.75),
            variansi = var(Pendapatan),Std_dev=sd(Pendapatan),IQR=IQR(Pendapatan),
            min=min(Pendapatan),max=max(Pendapatan))
ringkas

