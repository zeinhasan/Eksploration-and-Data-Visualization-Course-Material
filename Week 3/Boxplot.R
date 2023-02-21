#menyiapkan data
help("state.x77")
data=state.x77
data=as.data.frame(data)

#membuat boxplot
boxplot(data$Murder, xlab="Murder Rate", ylab="Rate",
        main="Boxplot Murder Rate",col="skyblue",border = "blue")

boxplot(data$Murder)

#ggplot2
library(ggplot2)
ggplot(data, aes(y=Murder))+
  geom_boxplot(fill="green",color="blue")+
  ggtitle("Boxplot of Murder")+
  xlab("Murder")+
  ylab("Value")

ggplot(data, aes(y=Murder))+
  geom_boxplot()

#plotly
install.packages("plotly")
library(plotly)
murder=ggplot(data, aes(y=Murder))+
  geom_boxplot(fill="green",color="yellow")+
  ggtitle("Boxplot of Murder")+
  xlab("Murder")+
  ylab("Value")
ggplotly(murder)

#boxplot dengan menggabungkan data
library(tidyverse)
data1=select(data,Population,Area)
data2=data1 %>% gather(key=variabel,
                       value=Value)

boxplot(data2$Value, xlab="Murder Rate", ylab="Rate",
        main="Boxplot Murder Rate",col="skyblue",border = "blue")

#boxplot lebih dari 1 angkatan
#standar
boxplot(data[1:3],xlab="Variable", col=c("yellow","blue","gold"))
legend("topright",legend =c("popullation","Income","lliteracy"),
       fill=c("yellow","blue","gold"),cex=1,title = "legend")

#ggplot2
install.packages("gapminder")
library(gapminder)
mydat=gapminder
ggplot(mydat,aes(continent,lifeExp,color=continent))+
  geom_boxplot(fill="gold",outlier.color = "green",
               outlier.size=2,outlier.shape=17)

#plotly
yeayy=ggplot(mydat,aes(continent,lifeExp,color=continent))+
  geom_boxplot(fill="gold",outlier.color = "green",
               outlier.size=2,outlier.shape=17)
ggplotly(yeayy)


