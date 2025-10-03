#load libraries needed
library(MASS)
library(stochmod)
library(mcmc)
library(msm)
library(hmm.discnp)
library(depmix)

run.this.function = TRUE
if(run.this.function){rm(list=ls(all=TRUE))}

run.this.function = TRUE
if(run.this.function){if(!is.null(dev.list())){dev.off()}}

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





## calculate SPATTIALLY AUTOCORRELATED RANDOM NUMBER
ma.coef.n = 50

# set number of seed to plot normal distribution [into seed.v.ecdf = runif(seed.v.ndist.n,min(v.predict[n,]),max(v.predict[n,]))]
seed.v.ndist.n = 1000

# set number of realization using in weather generator function
curve.gen.n = 1 # (have to be 1) to build curve of MA.coef and I(rain)
weather.gen.realiz.n = 1000 # (have to be 1) to build curve of MA.coef and I(rain)
weather.gen.predict.realiz.n = 1000 # to prediction

dir_name = "Weight Matrix 50 intervals and Rainfall generations-MoransI 3"
maindir = "Temp/Stochastic"


# data file
coord.f = "coordinate.csv"
name.col = 1
x.col = 3
y.col = 4

data.f.monthly = "monthly-obs_filled1971-2006(4).csv"
data.f.daily = "Daily Filled all climate 1971-2006_except SLR ab1981 (bad prd)v2.csv"
daily.unusedcol.org = 2
monthly.unusedcol.org = 2



# result file
distance.f = "distance.csv"
weight.f = "weight.csv"


# Daily data
data.daily.org =  read.table(data.f.daily, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
daily.unusedcol.data = 18
# also remove "X478201" (col. 20)
data.daily = data.daily.org[,-c(1:daily.unusedcol.data,20)]
daily.month.col = 2
daily.day.col = 3
daily.month.list = data.daily.org[,daily.month.col]
daily.day.list = data.daily.org[,daily.day.col]


# Monthly data
data.monthly.org =  read.table(data.f.monthly, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
monthly.unusedcol.data = 16
# also remove "X478201" (col. 18)
data.monthly = data.monthly.org[,-c(1:monthly.unusedcol.data,18)]
monthly.month.col = 2
monthly.month.list = data.monthly.org[,monthly.month.col]


coord = read.table(coord.f, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")


total.n = dim(data.daily)[1]
year.n = length(which(data.monthly.org[,monthly.month.col] == monthly.month.list[1]))
day.n = length(unique(daily.day.list))
month.n = length(unique(daily.month.list))
station.n = dim(data.daily)[2]




cat("Start\n")








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
weight.m = 1/(weight.d^2)
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



# Define daily Moran's I
moran.i.daily.all  = array(NA,c(year.n,12,length(unique(data.daily.org[,daily.day.col]))))
daily.moran.i.sim = array(NA,c(12,31))
daily.ma.coef  = array(NA,c(12,length(unique(data.daily.org[,daily.day.col]))))
#rain.predict.gen = array(NA, c(12,weight.n,31)) # set the matrix of rainfall amount results
rain.predict.gen = array(NA, c(12,weight.n,31,weather.gen.predict.realiz.n)) # set the matrix of rainfall amount results
rain.predict.gen.avg = array(NA, c(12,weight.n,31)) # set the matrix of rainfall amount results



# define mean of data each month
data.12.mean = array(NA, c(12,weight.n))
for(m in 1:12){
data.12.mean[m,] = mean(data.monthly[which(monthly.month.list==m),])
}



##################
#### change the amount of mean rainfall
##################
setwd(dir1)
pdf(paste("Vplot.pdf"))
setwd(dir0)

for(month.i in 1:12){
cat("\n**\n Month :",month.i,"\n")
# extract from data for weight.n station(s)
mean.rain = data.12.mean[month.i,]

par(ps =8,mfrow=c(2,1))
par(mai = c(.8, .8, .2, .9))  # Or change '.9' to a larger number for even 







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
				v.re.predict = array(NA, c(weight.n,ma.coef.n,seed.v.ndist.n))

				## chang the random number (u) following the range of ma.coef.rang
				cat("Calculate v values following rang of ma.coef\n")
				seed.u.m = array(runif(weight.n*seed.v.ndist.n,0,1), c(weight.n,1,seed.v.ndist.n))

				# 1) Generate V matrix by changing ma.coef and u [uniform random number]
				for(i in 1:ma.coef.n){
				for(m in 1:seed.v.ndist.n){
							v.predict[,i,m] = ma.coef.range[i]*(weight.m%*%seed.u.m[,,m])+seed.u.m[,,m]
							}
							}

				# 2) Generate seeds to re-produce v (normalized) after ( Normalize the uni-form model) from max and min of seed.u.m
				runthissub = FALSE
				if(runthissub){
				seed.u.m.predict = array(runif(weight.n*seed.v.ndist.n,min(seed.u.m),max(seed.u.m)), c(weight.n,1,seed.v.ndist.n))
				for(i in 1:ma.coef.n){
				for(m in 1:seed.v.ndist.n){
							v.re.predict[,i,m] = ma.coef.range[i]*(weight.m%*%seed.u.m.predict[,,m])+seed.u.m.predict[,,m]
							}
							}
				}


				# set matrix for v (normalized) and rain.predict
				v.normal = array(NA, c(weight.n,ma.coef.n,curve.gen.n)) # set the matrix
				rain.predict = array(NA, c(weight.n,ma.coef.n)) # set the matrix
				rownames(v.normal) = rownames(weight.m)
				rownames(rain.predict) = rownames(weight.m)



				## normalize v.predict by cummulative distribution function from V matrix (seed.v.ndist.n sets)
				cat("Normalize v value : set cuumulative distribution model\n ma.coef =")
				# set cuumulative distribution model
				v.normal.model = ecdf(v.predict[,,])



				
				max.predict.rain = 0

				# run to plot and calculate through ma.coef
				for(n in ma.coef.n:1){
							## set seed to plot normal distribution 
							# cat("Generating seed for transformation to uniform[0,1] n=",seed.v.ndist.n,"\n")
							# v.normal.model = ecdf(v.predict[,n,])

							# define for each station
							# cat("Generate v(i) from cuumulative distribution model")
							for(rlz in 1:curve.gen.n){
												v.normal[,n,rlz] = v.normal.model(v.predict[,n,rlz])

												# generate new V, if v = 1, to prevent Inf value
												runthissub = TRUE
												if(runthissub){
														  while(max(v.normal[,n,rlz], na.rm=TRUE) == 1){
																      cat("\n+***+ regenerate V (model)\n")
																	re.seed.u.m = array(runif(weight.n,0,1), c(weight.n,1))
																	v.re.predict = ma.coef.range[n]*(weight.m%*%re.seed.u.m)+re.seed.u.m
																	v.normal[,n,rlz] = v.normal.model(v.re.predict)
																	}
														  }

							}

							# calculate rainfall amount at station n
							rain.predict[,n] = rowMeans(-log(1-v.normal[,n,])*array(as.numeric(mean.rain),c(weight.n,curve.gen.n)))

							# plot to pdf
							# cat("Plot moving average coefficint")
							cat(ma.coef.range[n],", ")
							# define maximum rain.predict to set y-axis
							if(max.predict.rain < max(rain.predict[,n])){max.predict.rain = max(rain.predict[,n])}
							plot(rain.predict[,n], type = "l", xlab = paste("moving avrg coef. =",round(ma.coef.range[n],digits=2),"   at average",round(mean.rain,digits=1),"mm.(chart avg.=",round(mean(rain.predict[,n]),digits=1),"mm.)   of month:",month.i),
							ylab = "Rainfall (mm)", lty = 1, lwd = 2, ylim=c(0,max.predict.rain))

							par(new = TRUE)
							if(curve.gen.n == 1){plot(v.normal[,n,], type = "l", ann = FALSE, yaxt = "n", col = "blue", lty = 3, lwd = 2, ylim=c(0,1))
												}else{plot(rowMeans(v.normal[,n,]), type = "l", ann = FALSE, yaxt = "n", col = "blue", lty = 3, lwd = 2, ylim=c(0,1))}
							axis(4)
							legend(x = "top",  bty = "n",  lty = c(1,3),  lwd = c(2,2), col = c("black", "blue"),  legend = paste(c("rain","v"), c("(left  y-axis)", "(right y-axis)")))
							
							}


#### END Generate V spattially autocorrelation random variables



# define columns to use in this calculation col.all[xx]
#for(i in 1:weight.n){col.all = c(col.all,which(colnames(data.monthly)==colnames(weight.m)[i]))}
#col.all = integer(0)
moran.i.all = integer(0)
rain.predict.all = integer(0)


for(i in 1:ma.coef.n){
							    #cat("ma.coef =",ma.coef.range[i]," Moran's I =",moran.i(rain.predict[,i],weight.m),"\n")
							    moran.i.all = c(moran.i.all , moran.i(rain.predict[,i],weight.m))

							    rain.predict.all = c(rain.predict.all , rain.predict[,i])



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
cat("\nplot Moran's I")
plot(ma.coef.and.i[,2:1],xlab = "Moran's I of Rainfall amount",ylab="moving average coef.")
not.na.row = which(is.na(ma.coef.and.i[, 2]) == FALSE)
y1 = as.numeric(ma.coef.and.i[not.na.row,1])
x1 = as.numeric(ma.coef.and.i[not.na.row,2])
lm.fit = lm(y1 ~ x1)
lm.fit.pl.2 = lm(y1 ~., data=poly(x1,2,raw=TRUE))
lm.fit.pl.3 = lm(y1 ~., data=poly(x1,3,raw=TRUE))
lm.fit.pl.4 = lm(y1 ~., data=poly(x1,4,raw=TRUE))
abline(lm.fit,col="blue")
lines(x1,predict(lm.fit.pl.2,poly(x1,2,raw=TRUE)),col="red",lty=2,lwd = 1)
lines(x1,predict(lm.fit.pl.3,poly(x1,3,raw=TRUE)),col="green",lty=2,lwd = 1)
lines(x1,predict(lm.fit.pl.4,poly(x1,4,raw=TRUE)),col="violet",lty=2,lwd = 1)
legend(x = "top",  bty = "n",  lty = c(1,3,3,3),  lwd = c(2,2,2,2), col = c("blue","red","green","violet"),  legend = c(paste("linear R2=",round(summary(lm.fit)$r.squared,digits=3)),paste("polynomial (degree 2) R2=",round(summary(lm.fit.pl.2)$r.squared,digits=3)),paste("polynomial (degree 3) R2=",round(summary(lm.fit.pl.3)$r.squared,digits=3)),paste("polynomial (degree 4) R2=",round(summary(lm.fit.pl.4)$r.squared,digits=3))))
title(main=paste("Amount of Rainfalll at", weight.n," stations for month:",month.i," [R2 =",round(summary(lm.fit)$r.squared,digits=2),"]"))







################ END of Parameter estimation phase (providing equation of I and ma.coef)
cat("\nFinish Parameter estimation")



##########
## Calculate observed daily Moran's I
cat("\nCalculate observed daily Moran's I\n")
row.sel = which(data.daily.org[,daily.month.col] == daily.month.list[month.i])
day.in.month = length(unique(data.daily.org[row.sel,daily.day.col]))

for(day.i in 1:day.in.month){
			row.sel2 = which((data.daily.org[,daily.month.col] == month.i) & (data.daily.org[,daily.day.col] == day.i))
			for(row.n in 1:length(row.sel2)){
							moran.i.daily.all[row.n,month.i,day.i] = moran.i(as.numeric(data.daily[row.sel2[row.n],]),weight.m)
							}
			cat("\nMonth :",month.i," Day :",day.i," -- Moran's I :",mean(moran.i.daily.all[,month.i,day.i], na.rm = TRUE))
			}

plot(moran.i.daily.all)
hist(moran.i.daily.all, breaks=50)





#############################
# Forecast rainfall (rainfall generation)
#############################

##########
## Extract ma.coef from average of daily Moran's I into daily.ma.coef
daily.avg.moran.i = colMeans(moran.i.daily.all,na.rm = TRUE)


# Define rainy day
#rainy.day = sample(0:1,length(which(!is.na(daily.avg.moran.i[month.i,]))),replace=TRUE)
rainy.day = sample(1,length(which(!is.na(daily.avg.moran.i[month.i,]))),replace=TRUE)
rainy.day.n = length(which(rainy.day==1))
choose.rainy.day = which(rainy.day==1)

for(day.i in 1:31){
			a = daily.avg.moran.i[month.i,day.i]
			if(!is.na(a)){
					ma.coef.sel = predict(lm.fit.pl.4,poly(c(a,1:10),4,raw=TRUE))
					daily.ma.coef[month.i,day.i]  = ma.coef.sel[1]
					}else{
						daily.ma.coef[month.i,day.i] = NA
						}

			}

## Plot selected moving average coef.
plot(daily.ma.coef[month.i,])

##########
## Generate V from ma.coef and random u
cat("\n\n- Generate random seed to prediction:",weather.gen.realiz.n, "seed(s)")

## 1) Generate seed to build v.normal.model.gen 
# Generate V matrix by changing ma.coef and u [uniform random number]
seed.u.gen.cal.model = array(runif(weight.n*weather.gen.realiz.n,0,1), c(31,weight.n,weather.gen.realiz.n))
v.predict.gen = array(NA, c(31,weight.n, weather.gen.realiz.n))

for(day.i in 1:length(which(!is.na(daily.ma.coef[month.i,])))){
		for(m in 1:weather.gen.realiz.n){
							v.predict.gen[day.i,,m] = daily.ma.coef[month.i,day.i]*(weight.m%*%seed.u.gen.cal.model[day.i,,m])+seed.u.gen.cal.model[day.i,,m]
						  }
		}


# set cuumulative distribution model
v.normal.model.gen = ecdf(v.predict.gen[,,])
max.predict.rain = 0


## 2) Generate seed to predict using v.normal.model.gen 
# set seed for v that is random parameter for rain generation
seed.u.gen.predict = array(runif(weight.n*weather.gen.predict.realiz.n,min(seed.u.gen.cal.model),max(seed.u.gen.cal.model)), c(31,weight.n,weather.gen.predict.realiz.n))
v.predict.gen.seed = array(runif(31*weight.n*weather.gen.predict.realiz.n,min(v.predict.gen),max(v.predict.gen)), c(31,weight.n, weather.gen.predict.realiz.n))

for(day.i in 1:length(which(!is.na(daily.ma.coef[month.i,])))){
		for(m in 1:weather.gen.predict.realiz.n){
							v.predict.gen.seed[day.i,,m] = daily.ma.coef[month.i,day.i]*(weight.m%*%seed.u.gen.predict[day.i,,m])+seed.u.gen.predict[day.i,,m]
						  }
		}


# V (normalized) and rain.predict
v.normal.gen = array(NA, c(weight.n,rainy.day.n,weather.gen.realiz.n)) # set the matrix
rownames(v.normal.gen) = rownames(weight.m)





#############################################################################
## Generate rainfall from cummulative distribution model (v.normal.model.gen)
# define for each day
# cat("Generate v(i) from cuumulative distribution model")
for(n in 1:rainy.day.n){
for(seed.n in 1:weather.gen.predict.realiz.n){
							v.normal.gen[,n,seed.n] = v.normal.model.gen(v.predict.gen.seed[choose.rainy.day[n],,seed.n])

							# generate new V, if v = 1, to prevent Inf value
							runthissub = TRUE
							if(runthissub){
									  while(max(v.normal.gen[,n,seed.n], na.rm=TRUE) == 1){
											cat("\n+***+ regenerate V (prediction)\n")
											re.seed.u.m = array(runif(weight.n,0,1), c(weight.n,1))
											v.re.predict = daily.ma.coef[month.i,choose.rainy.day[n]]*(weight.m%*%re.seed.u.m)+re.seed.u.m
											v.normal.gen[,n,seed.n] = v.normal.model.gen(v.re.predict)
											}
									  }
							


							# calculate rainfall amount at station n
							rain.predict.gen[month.i,,choose.rainy.day[n],seed.n] = -log(1-v.normal.gen[,n,seed.n])*array(as.numeric(mean.rain),c(weight.n,1))
							}

				# Average the realization
				rain.predict.gen.avg[month.i,,choose.rainy.day[n]] = rowMeans(rain.predict.gen[month.i,,choose.rainy.day[n],],na.rm = TRUE)
				}




# plot to pdf
cat("\n- Plot Rainfall each station in month: ",month.i)
# define maximum rain.predict to set y-axis
plot(colMeans(rain.predict.gen.avg[month.i,,]), type = "h",
	xlab = paste("Stations / Rainy day =",rainy.day.n,"day(s) at avg. rainfall =",round(mean.rain,digits=1),"mm.(chart avg.=",round(mean(rain.predict.gen.avg[month.i,,],na.rm = TRUE),digits=1),"mm.) of month:",month.i),
	ylab = "Rainfall (mm)", lty = 1, lwd = 5, ylim=c(0,max(rain.predict.gen.avg[month.i,,choose.rainy.day])))
for(k in 1:weight.n){
				lines(rain.predict.gen.avg[month.i,k,], type = "p", col = k)
				}

legend(x = "right",  bty = "n",  lty = c(rep(1,weight.n)),  lwd = rep(2,weight.n), col = c(1:weight.n),  legend = rownames(weight.m))
							




#######
# Generate simulated daily Moran's
cat("\n- Generate simulated daily Moran's I\n")
for(day.i in 1:31){
			daily.moran.i.sim[month.i,day.i] = moran.i(rain.predict.gen.avg[month.i,,day.i],weight.m)
			}





}

dev.off()
##################
#### END change the amount of mean monhtly rainfall
##################


cat("\n***\nEnd of monthly calculation***\n\nPlot and Conclusion\n")


##################
## Plot Moran's I

setwd(dir1)
pdf(paste("Conclude I.pdf"), width = 15 ,height = 9)
setwd(dir0)
cat("\nPlot obs. and sim. daily Moran's I")
plot(1:(12*31),rep(0,12*31),type = "n",ylim = c(-0.3,0.5))
for(i in 1:12){
			for(year.i in 1:year.n){
							lines(c((1+((i-1)*31)):(31+((i-1)*31))),moran.i.daily.all[year.i,i,],type = "p", lwd = 1, cex = 0.5)
							}
				lines(c((1+((i-1)*31)):(31+((i-1)*31))),daily.avg.moran.i[i,], lwd = 2 ,col = "red", lty = 1)
				lines(c((1+((i-1)*31)):(31+((i-1)*31))),daily.moran.i.sim[i,], lwd = 2 ,col = "blue", lty = 1)
				}
legend(x = "top",  bty = "n",  lty = c(1,1),  lwd = c(2,2), col = c("red","blue"),  legend = c("Observed Moran's I","Simulated Moran's I"))
dev.off()



setwd(dir1)
pdf(paste("Conclude I and ma-coef.pdf"), width = 15 ,height = 9)
setwd(dir0)
## Plot selected moving average coef. whole year
#plot(1:(12*31),rep(0,12*31),type = "n",ylim = c(min(daily.ma.coef[,],rm.na=TRUE),max(daily.ma.coef[,],rm.na=TRUE)))
plot(1:(12*31),rep(0,12*31),type = "n",ylim = c(-2,2))
for(i in 1:12){
lines(c((1+((i-1)*31)):(31+((i-1)*31))),daily.ma.coef[i,], lwd = 2 ,col = "red", lty = 1)
lines(c((1+((i-1)*31)):(31+((i-1)*31))),daily.avg.moran.i[i,], lwd = 2 ,col = "blue", lty = 1)
}
legend(x = "top",  bty = "n",  lty = c(1,1),  lwd = c(2,2), col = c("red","blue"),  legend = c("Selected moving average coef.","Observed Moran's I"))
dev.off()



###########################
## Plot Rainfall prediction

setwd(dir1)
pdf(paste("Conclude Rain Generation.pdf"), width = 15 ,height = 9)
par(ps =10)
setwd(dir0)
cat("\nPlot daily rainfall prediction")
plot(1:(12*31),rep(0,12*31),type = "n",ylim = c(0,max(rain.predict.gen.avg, na.rm = TRUE)), ylab="Rainfall (mm.)", xlab ="Days")
#plot(1:(12*31),rep(0,12*31),type = "n")

for(i in 1:12){
			lines(c((1+((i-1)*31)):(31+((i-1)*31))),colMeans(rain.predict.gen.avg[i,,]), lwd = 2, type ="h")
			for(n in 1:weight.n){
							lines(c((1+((i-1)*31)):(31+((i-1)*31))),rain.predict.gen.avg[i,n,],col = (n),type = "p", lwd = 1, cex = 0.6)
							}
				}

dev.off()





			   






