# Contoh pengambilan data sampel
# definisikan populasinya terlebih dahulu
pop <- 1:10

#pengambilan sampel dengan pengembalian
set.seed(123) 
sample1 <- sample(pop,size=5,replace=T)
sample1

#pengambilan sampel tanpa pengembalian
set.seed(123)
sample2 <- sample(pop,size=5,replace=F)
sample2

#Distribusi Normal
# Peluang skor TMAS pasien tersebut adalah 50
a = dnorm(50,mean=47.6,sd=10.3)
a

# Peluang seorang pasien mendapatkan skor kurang dari 47,6
b = pnorm(47.6,mean=47.6,sd=10.3,lower.tail = TRUE)
b

# Peluang seorang pasien mendapatkan skor lebih dari 45,5
c = 1-pnorm(45.5,mean=47.6,sd=10.3,lower.tail = TRUE)
c
# atau 
c2 = pnorm(45.5,mean=47.6,sd=10.3,lower.tail = FALSE)
c2

# Inverse CDF 0.9
d = qnorm(0.1,mean=47.6,sd=10.3,lower.tail = FALSE)
d
# atau 
d2 = qnorm(0.9,mean=47.6,sd=10.3,lower.tail = TRUE)
d2

#Distribusi t

#1. probability density function dt(x,df)
#a, calculate the probability density function at  x = 1.96 with df 5000
dt(1.96,5000)
#b. calculate the probability density function at t=-4,2,0,2,4 with df=5
x<-seq(from=-4, to=4, by=2)
a<-dt(x,5)
a

#2.Untuk mencari peluang kumulatif digunakan pt(x,df)
#default lower.tail = TRUE

# Mencari P(X<=-2) dengan df 5
pt(-2,5)

# Mencari P(X<=-2), P(X<=0), dan P(X<=2) dengan df 5
df <- 5
ji <- c(-2,0,2)
pt(ji, df = df, lower.tail = TRUE)

#jika lower.tail = FALSE 
df <- 5
ki <- c(-2,0,2)
f<-pt(ki, df = df, lower.tail = FALSE)
f

#3. nilai invers dari probabilitas kumulatif

#Mencari nilai x jika P(X<=x)=0.05096974 dengan df 5
qt(0.05096974,5)

#Mencari nilai masing-masing x jika P(X<x)=0.05096974,P(X<x)=0.50000000, dan P(X<x)=0.94903026
#dengan nilai df masing-masing 5
qt(c(0.05096974,0.50000000,0.94903026),5)

#
hasil<-qt(f,5,lower.tail = FALSE)
data.frame(Nilai_probabilitas=f,
           Nilai_invers=hasil)

#Distribusi F
#1. Probability density function df(x,df1,df2)

#Mencari P(X=2) dengan df1 = 10, df2 = 20
df(1.2, df1 = 10, df2 = 20)

#2. cumulative distribution function
#We use the pf() to calculate the area under the curve for the interval [0,1.5]
#and the interval [1.5,+infinity) of a F-curve with with v1=10 and v2=20. 
#Further we ask R if the sum of the intervals [0,1.5] and [1.5,+infinity) sums up to 1

x = 1.5
v1 = 10
v2 = 20

# interval [0,1.5]
pf(x, df = v1, df2 = v2, lower.tail = TRUE)

# interval [1.5,+inf)
pf(x, df = v1, df2 = v2, lower.tail = FALSE)

#apakah jumlahnya mencapai 1?
pf(x, df = v1, df2 = v2, lower.tail = TRUE) + 
  pf(x, df = v1, df2 = v2, lower.tail = FALSE) == 1


#3. We use the qf() to calculate the quantile for a given area (= probability)
#under the curve for a F-curve with v1=10 and v2=20 
#that corresponds to q=0.25,0.5,0.75 and 0.999. 
#We set lower.tail = TRUE in order the get the area for the interval [0,q].

#Nilai probabilitas
q <- c(0.25, 0.5, 0.75, 0.999)
#df 1
v1=10
#df2
v2=20
#Mencari nilai x dari P(X<=x)=0.25 dengan df1 10 dan df2 20
qf(q[1], df1 = v1, df2 = v2, lower.tail = TRUE)
#
qf(q[2], df1 = v1, df2 = v2, lower.tail = TRUE)
#
qf(q[3], df1 = v1, df2 = v2, lower.tail = TRUE)
#
qf(q[4], df1 = v1, df2 = v2, lower.tail = TRUE)


#install.packages("visualize")
library(visualize)

#plot distribusi t
#visualize.t(stat = 1, df = 3, section = "lower")
visualize.t(stat = -2, df = 5, section = "lower")
visualize.t(stat = 0, df = 5, section = "lower")
visualize.t(stat = 0, df = 5, section = "upper")
visualize.t(stat = c(-2,0), df = 5, section = "bounded")

#plot distribusi f
#visualize.f(stat = 1, df1 = 5, df2 = 4, section = "lower")
visualize.f(stat=1.5, df1=10, df2=20,section="lower")
visualize.f(stat=1.5, df1=10, df2=20,section="upper")


# Normal probability plot 
# Plot peluang seorang pasien mendapatkan skor kurang dari 47,6
library(visualize)
visualize.norm(stat = 47.6,mu=47.6,sd=10.3,section = "lower")

# Plot peluang seorang pasien mendapatkan skor lebih dari 45,5
visualize.norm(stat = 45.5,mu=47.6,sd=10.3,section = "upper")



