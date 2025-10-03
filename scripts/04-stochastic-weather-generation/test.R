#load libraries needed
library(MASS)
library(stochmod)
library(mcmc)
library(msm)
library(hmm.discnp)
library(depmix)

dir_name = "Weight Matrix"
maindir = "Temp/ML-regression-multiGCM"

# data file
data.f = "coordinate.csv"
# result file
result.f = "distance.csv"


data <- read.table(data.f, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

data.n = dim(data)[1]


# Check working directory
dir_name1 = dir_name
L=file.exists(dir_name1)  
if(!L) dirfile=dir.create(dir_name1)
dirfile=(dir_name1)

dir1=dirname(file.path("D:",paste(maindir),paste(dirfile),"dummy"))
dir0=dirname(file.path("D:",paste(maindir),"dummy"))

x.col = 3
y.col = 4

weight.m = array(NA,c(data.n,data.n)

for(i in 1:data.n){
			for(j in 1:data.n){
						w.ij = ((data[i,x.col]-data[j,x.col)^2+(data[i,y.col]-data[j,y.col)^2)^0.5
						weight.m[i,j] = w.ij
						}
			}

setwd(dir1)
write.csv(weight.m, file = result.f)
setwd(dir0)



n.data = 30

m.real = runif(n.data,1,99)
m.real1 = matrix(runif(n.data,1,99),nrow = n.data,ncol=1)
m.real2 = matrix(runif(n.data,1,99),nrow = n.data,ncol=1)
m.int = matrix(sample(1:(n.data-1),100,replace=TRUE),nrow = n.data, ncol=2)

avg.m.real = mean(m.real)
n.m.real = length(m.real)

exp.m.real = -log(1-runif(n.m.real,0,1))*avg.m.real
summary(exp.m.real)
plot(m.real)
lines(exp.m.real, col = "red")