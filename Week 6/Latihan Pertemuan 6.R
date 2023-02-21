### No 1

# Read data
library(readxl)
datalat1<- read_xlsx(file.choose(),sheet = 3)

# No 1 Uji Z
library(BSDA)

# Check normalitas 
boxplot(datalat1$PanenPadi,
        ylab="count (Kg)",
        xlab='Panen Padi',
        main="Boxplot data sampel panen padi (kg)")

library(plotly)
library(tidyverse)
a=ggplot(datalat1,aes(y=PanenPadi))+geom_boxplot()
ggplotly(a)

# Uji Rata-Rata 1 Populasi (Uji Z)
z.test(datalat1$PanenPadi,alternative="greater",
       mu=18700,sigma.x=10000,
       conf.level = 0.90)

qnorm(0.1,lower.tail = F)

### no 2
zsum.test(mean.x = 871, sigma.x = sqrt(441),n.x = 200,
          alternative = "two.sided", mu=880,
          conf.level = 0.95)

qnorm(0.025,lower.tail = F)
