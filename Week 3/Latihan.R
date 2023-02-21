# Latihan
mydata <- read.delim("clipboard")

head(mydata)


# Plot data cara 1
boxplot(mydata[2:5],xlab="Variable",ylab="Count",
        main="Boxplot  tingkat kematian laki-laki karena penyakit jantung",
        col = c("red","blue","green","yellow"))

legend(legend=c("Usia.20.29.tahun","Usia.30.39.tahun","Usia.40.49.tahun","Usia.50.59.tahun"),
       col = c("red","blue","green","yellow"),'topleft',
       cex=0.8,fill=c("red","blue","green","yellow"),title="Legenda")


# Plot data cara 2
install.packages("reshape")
install.packages("ggplot2")
library(reshape)
library(ggplot2)

data_long <- melt(mydata)
head(data_long)

ggplot(data_long, aes(x=variable,y=value,fill=variable))+
  geom_boxplot()+
  ggtitle("Boxplot tingkat kematian laki-laki karena penyakit jantung")+
  xlab("variabel")+
  ylab("Jumlah")

ggplot(data_long, aes(x=variable,y=value,color=variable))+
  geom_boxplot()+
  ggtitle("Boxplot  tingkat kematian laki-laki karena penyakit jantung")+
  xlab("variabel")+
  ylab("Jumlah")

ggplot(data_long, aes(x=variable,y=value))+
  geom_boxplot(color="black",fill="blue")+
  ggtitle("Boxplot  tingkat kematian laki-laki karena penyakit jantung")+
  xlab("variabel")+
  ylab("Jumlah")


# Checking Outlier with Boxplot  
library(tidyverse)
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

data_long %>%
  group_by(variable) %>%
  mutate(outlier = ifelse(is_outlier(value), value, as.numeric(NA))) %>%
  ggplot(., aes(x = variable, y = value,fill=variable)) +
  geom_boxplot() +
  geom_text(aes(label = c(outlier)), na.rm = TRUE, hjust = -0.3)+
  ggtitle("Boxplot  tingkat kematian laki-laki karena penyakit jantung")+
  xlab("variabel")+
  ylab("Jumlah")


# Interactive Boxplot
library(plotly)

x <- data_long %>%
     group_by(variable) %>%
     mutate(outlier = ifelse(is_outlier(value), value, as.numeric(NA))) %>%
     ggplot(., aes(x = variable, y = value,fill=variable)) +
     geom_boxplot() +
     geom_text(aes(label = c(outlier)), na.rm = FALSE, hjust = -0.3)+
     ggtitle("Boxplot")+
     xlab("variabel")+
     ylab("Jumlah")

ggplotly(x)



#####################       Standarisasi        #######################
library(robustHD)
std_data <- standardize(mydata[2:5], centerFun = median, scaleFun = IQR)

head(std_data)

boxplot(std_data,xlab="Variable",ylab="Count",
        main="Boxplot  tingkat kematian laki-laki karena penyakit jantung (Standardized)",
        col = c("red","blue","green","yellow"))

legend(legend=c("Usia.20.29.tahun","Usia.30.39.tahun","Usia.40.49.tahun","Usia.50.59.tahun"),
       col = c("red","blue","green","yellow"),'topleft',
       cex=0.8,fill=c("red","blue","green","yellow"),title="Legenda")

std_data_long <- melt(std_data)
head(std_data_long)

# Checking Outlier with Boxplot  
std_data_long %>%
  group_by(variable) %>%
  mutate(outlier = ifelse(is_outlier(value), value, as.numeric(NA))) %>%
  ggplot(., aes(x = variable, y = value,fill=variable)) +
  geom_boxplot() +
  geom_text(aes(label = c(outlier)), na.rm = TRUE, hjust = -0.3)+
  ggtitle("Boxplot  tingkat kematian laki-laki karena penyakit jantung (Standardized)")+
  xlab("variabel")+
  ylab("Jumlah")