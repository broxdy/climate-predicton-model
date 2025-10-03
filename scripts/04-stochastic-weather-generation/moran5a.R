#load libraries needed
library(MASS)
library(stochmod)
library(mcmc)
library(msm)
library(hmm.discnp)
library(depmix)

#test = c(0,0)
#for(i in -100:100){test = rbind(test, c(i,moran.i(runif(25,-10,10)*i,weight.m)))}
#plot(test)


cat("Set moran.i function\n")
## Function to find Moran's I (moran.i)
moran.i <- function(data, weight.matrix) { 

# define X in moran equation (the average of monthly data)
data.x = data
ln = dim(weight.matrix)[1]
###############################
## calculate Moran's I
# based on http://www.spatialanalysisonline.com/output/html/MoranIandGearyC.html#_Ref114481622
#cat("\nstart Calculating Moran's I:")
mean.x = mean(data.x)

# term sum [xi-mean.x]
a = data.x-mean.x

# term sum[w(ij)*(xi-mean.x)]
b1.sum = 0
b2.sum = 0
for(i in 1:ln){
		  for(j in 1:ln){
				    b1 = weight.matrix[i,j]*(data.x[j]-mean.x)
				    b1.sum = b1.sum + b1
				    }
		  b2 = a[i]*b1.sum
		  b2.sum = b2.sum + b2
		  b1.sum = 0
		  }
a.b = b2.sum

# term sum [w(ij)]
c = sum(weight.matrix)

# term sum[xi-mean.x^2]/n
d = sum((data.x-mean.x)^2)/ln

moran.i = a.b/c/d

} 
################# END Moran's I function ##################





cat("Start\n")
dir_name = "Weight Matrix"
maindir = "Temp/Stochastic"

# data file
coord.f = "coordinate.csv"
name.col = 1
x.col = 3
y.col = 4

data.f = "monthly-obs_filled1971-2006(4).csv"
#data.f = "Daily Filled all climate 1971-2006_except SLR ab1981 (bad prd)v2.csv"
unusedcol.obs = 2
month.col = 2

# result file
distance.f = "distance.csv"
weight.f = "weight.csv"



coord = read.table(coord.f, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
data.org =  read.table(data.f, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

unusedcol.data = 16
# also remove "X478201" (col. 18)
data = data.org[,-c(1:unusedcol.data,18)]
month.list = data.org[,month.col]


coord.n = dim(coord)[1]

# Check working directory
dir_name1 = dir_name
L=file.exists(dir_name1)  
if(!L) dirfile=dir.create(dir_name1)
dirfile=(dir_name1)

dir1=dirname(file.path("D:",paste(maindir),paste(dirfile),"dummy"))
dir0=dirname(file.path("D:",paste(maindir),"dummy"))
source("clear.R")
clear()

########### parameter estimation #########################

## Find distance putting into matrix
distance.m = array(NA,c(coord.n,coord.n))

for(i in 1:coord.n){
			for(j in 1:coord.n){
						w.ij = ((coord[i,x.col]-coord[j,x.col])^2+(coord[i,y.col]-coord[j,y.col])^2)^0.5
						distance.m[i,j] = w.ij
						}
			}
colnames(distance.m) = coord[,name.col]
rownames(distance.m) = coord[,name.col]

## remove no need column
no.need.col = 1:14
# also remove "X478201" (col. 16)
weight.d = distance.m[-c(no.need.col,16),-c(no.need.col,16)]
weight.n = dim(weight.d)[1]

setwd(dir1)
write.csv(weight.d, file = distance.f)
setwd(dir0)

# inverse distance to spatial weight
weight.m = 1/weight.d
#for(i in 1:weight.n){weight.m[i,i] = 1/weight.m}

# diagonal to be zero
for(i in 1:weight.n){weight.m[i,i] = 0}
## row-standardized
for(i in 1:weight.n){
			  weight.m[i,] = weight.m[i,]/sum(weight.m[i,])
			 }

setwd(dir1)
write.csv(weight.m, file = weight.f)
setwd(dir0)


## calculate range of ma.coef
w.max = max(eigen(weight.m)$values)
w.min = min(eigen(weight.m)$values)
ma.min = 1/w.max*-1
ma.max = 1/w.min*-1
random.digit = 5

# define mean of data each month
data.12.mean = array(NA, c(12,weight.n))
for(m in 1:12){
data.12.mean[m,] = mean(data[which(month.list==m),])
}

##################
#### change the amount of mean rainfall
##################
setwd(dir1)
pdf(paste("Vplot.pdf"))
setwd(dir0)

for(rain.i in 1:12){
cat("\n**\n Month :",rain.i,"\n")
# extract from data for weight.n station(s)
mean.rain = data.12.mean[rain.i,]

par(ps =8,mfrow=c(2,1))
par(mai = c(.8, .8, .2, .9))  # Or change '.9' to a larger number for even 


## calculate SPATTIALLY AUTOCORRELATED RANDOM NUMBER
ma.coef.n = 30

# set number of seed to plot normal distribution [into seed.v.ecdf = runif(seed.v.ndist.n,min(v.predict[n,]),max(v.predict[n,]))]
seed.v.ndist.n = 1000

# set number of realization using in weather generator function
weather.gen.realiz.n = 1000



# set the result matrix of MA.coef and Moran's I
ma.coef.and.i = array(NA, c(ma.coef.n,2))
colnames(ma.coef.and.i) = c("Moving Average Coef.","Moran's I")
rain.and.ma.coef = array(NA, c(ma.coef.n,2))
rain.and.i = array(NA, c(seed.v.ndist.n,2))

colnames(ma.coef.and.i) = c("Moving Average Coef.","Moran's I")
colnames(rain.and.ma.coef) = c("Moving Average Coef.","rain")
colnames(rain.and.i) = c("Moran's I.","rain")


## find rang of ma.coef
# define number of plot value [ma.coef vs I]
cat("Find rang of ma.coef\n")
ma.coef.range = integer(0)
for(i in 1:ma.coef.n){ma.coef.range = c(ma.coef.range,ma.min+i*(ma.max - ma.min)/(ma.coef.n+2))} # ma.max and ma.min is not included in the rang





#### Generate V, rain.predict spattially autocorrelation random variables


				## chang ma.coef within ma.coef.range
				v.predict = array(NA, c(weight.n,ma.coef.n,seed.v.ndist.n))

				## chang the random number (u) following the range of ma.coef.rang
				cat("Calculate v values following rang of ma.coef\n")
				seed.u.m = array(runif(weight.n*seed.v.ndist.n,0,1), c(weight.n,1,seed.v.ndist.n))

				# Generate V matrix by changing ma.coef and u [uniform random number]
				for(i in 1:ma.coef.n){
				for(m in 1:seed.v.ndist.n){
							v.predict[,i,m] = ma.coef.range[i]*(weight.m%*%seed.u.m[,,m])+seed.u.m[,,m]
							}
							}



				# set matrix for v (normalized) and rain.predict
				v.normal = array(NA, c(weight.n,ma.coef.n,weather.gen.realiz.n)) # set the matrix
				rain.predict = array(NA, c(weight.n,ma.coef.n)) # set the matrix
				rownames(v.normal) = rownames(weight.m)
				rownames(rain.predict) = rownames(weight.m)

				## normalize v.predict by cummulative distribution function from V matrix (seed.v.ndist.n sets)
				cat("Normalize v value\n")
				for(n in 1:ma.coef.n){
							## set seed to plot normal distribution 
							#cat("Generating seed for transformation to uniform[0,1] n=",seed.v.ndist.n,"\n")
							# set cuumulative distribution model
							v.normal.model = ecdf(v.predict[,n,])
							# define for each station
							seed.v.ecdf = array(runif(weight.n*weather.gen.realiz.n,min(v.predict[,n,]),max(v.predict[,n,])), c(weight.n,weather.gen.realiz.n))
							for(rlz in 1:weather.gen.realiz.n){
							v.normal[,n,rlz] = v.normal.model(seed.v.ecdf[,rlz])
							}

							# calculate rainfall amount at station n
							#rain.predict[,n] = -log(1-v.normal[,n])*as.numeric(mean.rain)
							rain.predict[,n] = rowMeans(-log(1-v.normal[,n,])*array(as.numeric(mean.rain),c(weight.n,weather.gen.realiz.n)))

							#rain.predict[,n,] = -log(1-v.normal[,n,])*mean.rain
							#rain.predict[,n,] = -log(1-v.normal[,n,])*runif(1,0,100)

							# plot to pdf
							cat("Moving average coefficint :",ma.coef.range[n],"\n")
							
							#for(k in 1:seed.v.ndist.n){

											plot(rain.predict[,n], type = "l", xlab = paste("moving avrg coef. =",round(ma.coef.range[n],digits=2)," at average",round(mean.rain,digits=1),"mm. of month:",rain.i), ylab = "Rainfall (mm)", lty = 1, lwd = 2)
											par(new = TRUE)
											plot(rowMeans(v.normal[,n,]), type = "l", ann = FALSE, yaxt = "n", col = "blue", lty = 3, lwd = 2)
											axis(4)
											legend(x = "top",  bty = "n",  lty = c(1,3),  lwd = c(2,2), col = c("black", "blue"),  legend = paste(c("rain","v"), c("(left  y-axis)", "(right y-axis)")))
											#}
							
							}


#### END Generate V spattially autocorrelation random variables



# define columns to use in this calculation col.all[xx]
#for(i in 1:weight.n){col.all = c(col.all,which(colnames(data)==colnames(weight.m)[i]))}
#col.all = integer(0)
moran.i.all = integer(0)
rain.predict.all = integer(0)


for(i in 1:ma.coef.n){
#for(j in 1:seed.v.ndist.n){
#							    cat("ma.coef =",ma.coef.range[i]," Moran's I =",moran.i(rain.predict[,i,j],weight.m),"\n")
							    cat("ma.coef =",ma.coef.range[i]," Moran's I =",moran.i(rain.predict[,i],weight.m),"\n")
#							    moran.i.all = c(moran.i.all , moran.i(rain.predict[,i,j],weight.m))
							    moran.i.all = c(moran.i.all , moran.i(rain.predict[,i],weight.m))

#							    rain.predict.all = c(rain.predict.all , rain.predict[,i,j])
							    rain.predict.all = c(rain.predict.all , rain.predict[,i])

#							    }


			    if(i == 1){
					    ma.coef.and.i = moran.i.all
					    rain.and.ma.coef = rain.predict.all
					    rain.and.i =rain.predict.all
					   }else{
						    ma.coef.and.i = rbind(ma.coef.and.i, cbind(ma.coef.range[i],moran.i.all))
						    rain.and.ma.coef = rbind(rain.and.ma.coef,cbind(ma.coef.range[i],rain.predict.all))
						    rain.and.i = rbind(rain.and.i,cbind(moran.i.all,rain.predict.all))
						    }



			    rain.predict.all = integer(0)
			    moran.i.all = integer(0)
			    }


par(ps =12,mfrow=c(1,1))
plot(ma.coef.and.i[,2:1])
lm.fit = lm(ma.coef.and.i[,1] ~ ma.coef.and.i[,2])
lm.fit.pl = spline(ma.coef.and.i[,1], ma.coef.and.i[,2])
abline(lm.fit,col="blue")
lines(lm.fit.pl,col="red",lty=3)
legend(x = "top",  bty = "n",  lty = c(1,3),  lwd = c(2,2), col = c("blue", "red"),  legend = c("linear","polynomial"))

#plot(lm.fit, las = 1)
title(main=paste("Amount of Rainfalll at", weight.n," stations for month:",rain.i," [R2 =",round(summary(lm.fit)$r.squared,digits=2),"]"))

#dev.new()
#plot(rain.and.ma.coef[,1:2])
#dev.new()
#plot(rain.and.i[,1:2])


}

dev.off()
##################
#### END change the amount of mean rainfall
##################

			   


runthissub = FALSE
if(runthissub){
m.real = runif(weight.n,1,99)
m.real1 = matrix(runif(weight.n,1,99),nrow = weight.n,ncol=1)
m.real2 = matrix(runif(weight.n,1,99),nrow = weight.n,ncol=1)
m.int = matrix(sample(1:(weight.n-1),100,replace=TRUE),nrow = weight.n, ncol=2)

avg.m.real = mean(m.real)
n.m.real = length(m.real)

exp.m.real = -log(1-runif(n.m.real,0,1))*avg.m.real
summary(exp.m.real)
plot(m.real)
}


