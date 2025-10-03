#load libraries needed
#install.packages("stochmod")
#install.packages("mcmc")
#install.packages("msm")
#install.packages("hmm.discnp")
#install.packages("depmix")
#install.packages("date")



library(MASS)
library(stochmod)
library(mcmc)
library(msm)
library(hmm.discnp)
library(depmix)
library(date)
library(msm)
library(foreign)


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


error.count = 0

# month to run
run.month = c(1:12)

## calculate SPATTIALLY AUTOCORRELATED RANDOM NUMBER
ma.coef.n = 30 # number of ma.coef curve interval

# set number of seed to plot normal distribution [into seed.v.ecdf = runif(seed.v.ndist.n,min(v.predict[n,]),max(v.predict[n,]))]
seed.v.ndist.n = 1000

# set number of realization using in weather generator function
curve.gen.n = 400 # (have to be 1) to build curve of MA.coef and I(rain)
weather.gen.realiz.n = 1000 # (might be up to 1000 to neglect white noise) to build curve of MA.coef and I(rain)

weather.gen.predict.realiz.n = 30 # (OK) realization to predict to build curve daily I(rain)
#weather.gen.predict.realiz.n = 40 # realization to predict to build curve daily I(rain)

use.daily.ma.coef = TRUE #  use daily ma.coef (TRUE) / use monthly ma.coef (FALSE)
prob.rainy.by.eq = TRUE  #  use eq relate to Makrov Chain (TRUE) / use predicted rainy.n/day.in.month (FALSE)

record.random.seed = FALSE
read.seed.file = TRUE





maindir = "Temp/Stochastic"
#dir_name = "predict_moran37c2_sel curve - new ecdf + extrm charts+limit extrm v4b"
dir_name = "StochasticPCPv41rds_cal1971-1985 obs1971-2006RDSread"
show.v.chart = FALSE
plot.daily.rainfall = FALSE

# mean V limit For day =< n.limit.v.1
n.limit.v.1 = 2 # number of day to use this limit
#max.limit.v.1 = 0.99
#up.limit.v.1 = 0.85 # mean limit - normal
up.limit.v.1 = 0.90 # mean limit - for new ecdf
#up.limit.v.1 = 0.99999 # mean limit - test case
low.limit.v.1 = 0.00
count.limit.1 = 12


# mean V limit For day > n.limit.v.1
#max.limit.v.2 = 0.95
up.limit.v.2 = 0.90 # mean limit - normal
#up.limit.v.2 = 0.99999 # mean limit - test case
low.limit.v.2 = 0.1
count.limit.2 = 12


## Input files (parameter estimation)
#data.f.daily = "Daily Filled all climate 1971-2006_except SLR ab1981 (bad prd)v2+WetDay+WetRatio-withDaySeries.csv"
data.f.daily = "Daily Filled all climate 1971-1985+Wet_V2.csv"
#data.f.monthly = "monthly-obs_filled1971-2006(4).csv"
daily.unusedcol.org = 2
monthly.unusedcol.org = 2

## Input files (rainfall generation)
future.f.mean.rain = "PCP obs1971-2000.csv"
#future.f.mean.rain = "PCP1971-1999-prd2000-2008.csv"
#future.f.mean.rain = "PCP1971-1999-prd2000-2096.csv"
#future.f.mean.rain = "future-mean rain-y2075.csv"
#future.f.mean.rain = "future-rain1mm2006.csv"
#future.f.mean.rain = "future-1rain-y2075test5rain.csv"

future.f.rainy.day = "WDY OBS1971-2000.csv"
#future.f.rainy.day = "WetDay1971-1999-prd2000-2008.csv"
#future.f.rainy.day = "WetDay1971-1999-prd2000-2096.csv"
#future.f.rainy.day = "WetDay1971-2075.csv"
#future.f.rainy.day = "WetDay1971-2075_test20rain.csv"

## Acc. files
# coordinate
coord.f = "coordinate.csv"
name.col = 1
x.col = 3
y.col = 4
# number of day in a month
day.in.month.f = "day in month.csv"


## result file
distance.f = "distance.csv"
weight.f = "weight.csv"




############# calibration files
## Daily data
data.daily.org =  read.table(data.f.daily, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
daily.unusedcol.data = 19
daily.unusedcol2.data = 44
# also remove "X478201" (col. 21)
data.daily = data.daily.org[,-c(1:daily.unusedcol.data,21,(daily.unusedcol2.data+1):dim(data.daily.org)[2])]
station.title = colnames(data.daily)

daily.year.col = 1
daily.month.col = 2
daily.day.col = 3
daily.365day.col = 5
daily.year.list = data.daily.org[,daily.year.col]
daily.month.list = data.daily.org[,daily.month.col]
daily.day.list = data.daily.org[,daily.day.col]
daily.365day.list = data.daily.org[,daily.365day.col]

############# future files
## Monthly rain amount data
future.monthly.org =  read.table(future.f.mean.rain, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
cat("\nRemove negative values future.monthly.org")
for(i in 1:ncol(future.monthly.org)){
cat("\n Col.",i,":",future.monthly.org[which(future.monthly.org[,i] < 0),i])
future.monthly.org[which(future.monthly.org[,i] < 0),i] = 0
}
# Future wet data
future.wetday.org =  read.table(future.f.rainy.day, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
cat("\nRemove negative values future.wetday.org")
for(i in 1:ncol(future.wetday.org)){
cat("\n Col.",i,":",future.wetday.org[which(future.wetday.org[,i] < 0),i])
future.wetday.org[which(future.wetday.org[,i] < 0),i] = 0
}

# also remove "X478201" (col. 4)
future.monthly = future.monthly.org[,-c(1:2,4)]
future.wetday = future.wetday.org[,-c(1:2,4)]

monthly.month.col = 2
monthly.year.col = 1
future.month.list = future.monthly.org[,monthly.month.col]
future.year.list = future.monthly.org[,monthly.year.col]
#future.n = dim(future.monthly)[1]

future.all.year = unique(future.year.list)

################# rainy day files
# build rainy state matrix
# also remove "X478201" (col. 46)
daily.wet.state = data.daily.org[,-c(1:daily.unusedcol2.data,46)]
#daily.sumwet.state = data.daily.org[,70]
mcm.eq.table = integer(0)
mcm.eq.error.table = integer(0)

# Rainy day in month into table
cat("\nCreating rainy.in.month table")
rainy.in.month = future.wetday.org[,1:2]
rainy.in.month = cbind(rainy.in.month,array(NA,c(dim(rainy.in.month)[1],dim(future.wetday)[2]*weather.gen.predict.realiz.n)))
for(i in 1:dim(future.wetday)[2]){
				for(j in 1:weather.gen.predict.realiz.n){colnames(rainy.in.month)[(i-1)*weather.gen.predict.realiz.n+j+2] = paste(colnames(future.wetday)[i],"-",j,sep="")}
				}



################# Acc. files
day.in.month.list =  read.table(day.in.month.f, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")





coord = read.table(coord.f, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")


total.n = dim(data.daily)[1]
year.n = length(which(future.monthly.org[,monthly.month.col] == future.month.list[1]))
day.n = length(unique(daily.day.list))
month.n = length(unique(daily.month.list))
station.n = dim(data.daily)[2]





#****************************************************************************************************#

cat("\nStart\n")

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


################
## Random seed
################

##
# Random seed.u.gen.predict
runif(10); .Random.seed
year.seed = round((length(future.year.list)/12)+1)
setwd(dir1)
cat("\nseed.u.gen.predict")
if(record.random.seed){
	rd.array = array(NA,c(626,year.seed))
	for(i in 1:weather.gen.predict.realiz.n){
	for(j in 1:year.seed){
				runif(1000)
				rd.array[,j] = .Random.seed
				}
			write.csv(rd.array, file = paste("rdsRS-seed-u-gen-predict-",i,".csv",sep=""), row.names = FALSE)
			}
}

if(read.seed.file){
			seed.u.gen.predict.rs.array = array(NA,c(626,year.seed,weather.gen.predict.realiz.n))
			for(i in 1:weather.gen.predict.realiz.n){
				temp.rs = read.table(paste("rdsRS-seed-u-gen-predict-",i,".csv",sep=""), stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".")
				seed.u.gen.predict.rs.array[,,i] = as.real(unlist(temp.rs))
				}
}

setwd(dir0)

##
# Random random.rain.prob
setwd(dir1)
cat("\nrandom.rain.prob")
if(record.random.seed){
	rd.array = array(NA,c(626,year.seed))
	for(i in 1:12){
	for(j in 1:year.seed){
				runif(1000)
				rd.array[,j] = .Random.seed
				}
	write.csv(rd.array, file = paste("rds-random-rain-prob-",i,".csv",sep=""), row.names = FALSE)
	}
}
if(read.seed.file){
			random.rain.prob.rs.array = array(NA,c(626,year.seed,12))
			for(i in 1:12){
				temp.rs = read.table(paste("rds-random-rain-prob-",i,".csv",sep=""), stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".")
				random.rain.prob.rs.array[,,i] = as.real(unlist(temp.rs))
				}
}
setwd(dir0)

##
# Random seed.u.gen.cal.model
setwd(dir1)
cat("\nseed.u.gen.cal.model")
if(record.random.seed){
				rd.array = .Random.seed
				write.csv(rd.array, file = "rds-seed-u-gen-cal-model.csv", row.names = FALSE)
				}
if(read.seed.file){seed.u.gen.cal.model.rs.array = read.table("rds-seed-u-gen-cal-model.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".")}
setwd(dir0)

#######
# END Random Seed
#######




##########################################################
########### parameter estimation #########################
##########################################################


######################
## Determine relationship mean and extrme
######################
##
## mean and max
##

# Equation to limit extreme value (max value)
beyond.eq.multi.plx = 2 # portion of mean rain after limit.eq.multi.plx
limit.eq.multi.plx = 65.5 # limit of equation use at max average rain(mm/day[wet day])
# slope limit will be at the end of equation curve

# at only sel.sta.v.2 will affect from beyond.eq.multi.plx2
#sel.sta.v.2 = c(5,11,12,13,14,15,16,18,20,22)
sel.sta.v.2 = c(5,12,15)
sel.sta.v.3 = c(15)
beyond.eq.multi.plx2 = 1 # portion of mean rain after limit.eq.multi.plx2
limit.eq.multi.plx2 = 110 # limit 2 of equation use at max average rain(mm/day[wet day])
# slope limit will be at the end of equation curve after first line (beyond.eq.multi.plx)




cat("\n## Determine relationship mean and extrme")
mean.and.max = array(NA, c(station.n*12*year.n,2))
mean.and.max.sta = array(NA, c(station.n,12*year.n,2))
colnames(mean.and.max) = c("mean","max")
for(year.i in 1:year.n){
for(month.i in 1:12){
row.sel.1 = which(data.daily.org[,daily.year.col] == future.all.year[year.i])
row.sel.2 = which(data.daily.org[row.sel.1,daily.month.col] == month.i)

for(sta in 1:station.n){

row.sel.3 = which(data.daily[row.sel.1[row.sel.2],sta]>0)

max.m = max(data.daily[row.sel.1[row.sel.2[row.sel.3]],sta])
mean.m =mean(data.daily[row.sel.1[row.sel.2[row.sel.3]],sta])
mean.and.max[(year.i-1)*(12*station.n)+(month.i-1)*(station.n) + sta,] = c(mean.m,max.m)
mean.and.max.sta[sta,(year.i-1)*12+month.i-1,] = c(mean.m,max.m)
}
}
}
setwd(dir1)
pdf("Mean and Max.pdf", width = 6 ,height = 6)
#par(mgp=c(2.5,1,0), ps =16)
par(mgp=c(2.5,1,0), ps =16, mfrow=c(1,1))
par(mai =  c(.8, .8, .8, .8))
setwd(dir0)

max.slope.all = integer(0) # define maximum slope each station
max.slope.all2 = integer(0) # define maximum slope each station , when over limit.eq

# loop for each station
for(sta.i in 1:station.n){
cat("\nstation :",sta.i)

max.all = max(mean.and.max.sta[sta.i,,], na.rm = TRUE)
slope.m = mean.and.max.sta[sta.i,,2]/mean.and.max.sta[sta.i,,1]
max.slope.all = c(max.slope.all,max(slope.m,na.rm = TRUE))
max.slope = which(slope.m == max(slope.m,na.rm = TRUE))
extrem.eq = lm(c(0,mean.and.max.sta[sta.i,max.slope,2])~ c(0,mean.and.max.sta[sta.i,max.slope,1]))

# Plot mean and slope (ratio max/mean)
plot(mean.and.max.sta[sta.i,,1],mean.and.max.sta[sta.i,,2]/mean.and.max.sta[sta.i,,1],ylim = c(0,max(mean.and.max[,2]/mean.and.max[,1], na.rm = TRUE)), xlim=c(0,max(mean.and.max[,1], na.rm = TRUE)), xlab = "mean rainfall [wet day] (mm/day)", ylab = "ratio of max/mean", main = paste("extreme rainfall ration of sta:",sta.i), col= "blue", cex = 0.8, pch = 19)

# Plot mean and extreme values each station
cat("\n- Plot mean.and.max")
plot(mean.and.max.sta[sta.i,,],ylim = c(0,max(mean.and.max[,2], na.rm = TRUE)), xlim=c(0,max(mean.and.max[,1], na.rm = TRUE)), xlab = "mean rainfall [wet day] (mm/day)", ylab = "extreme rainfall (mm/day)", main = paste("extreme rainfall ration at sta :",sta.i), col= "blue", cex = 0.8, pch = 19)
abline(extrem.eq,col= "red" , lwd = 3)

# slope 2
sel.max2 = which(mean.and.max.sta[sta.i,,1] > limit.eq.multi.plx)
if(length(sel.max2)>0){
				max.slope2 = which(slope.m[sel.max2] == max(slope.m[sel.max2],na.rm = TRUE))
				extrem.eq2 = lm(c(0,mean.and.max.sta[sta.i,sel.max2[max.slope2],2])~ c(0,mean.and.max.sta[sta.i,sel.max2[max.slope2],1]))
				m.max2 = as.real(extrem.eq2$coefficients[2])

				abline(extrem.eq2,col= "red" , lwd = 2, lty = 2)
				legend("topright", cex = 0.8, y.intersp = 1.15,legend = c("extreme value","extreme trend line","extrme trend line 2"), col=c("blue","red","red"), lwd =c(1,3,2), lty=c(-1,1,2), pch = c(19,-1,-1),bg = "white")

				}else{
					m.max2 = max(slope.m,na.rm = TRUE)
					legend("topright", cex = 0.8, y.intersp = 1.15,legend = c("extreme value","extreme trend line"), col=c("blue","red"), lwd =c(1,3), lty=c(-1,1), pch = c(19,-1),bg = "white")
					}
max.slope.all2 = c(max.slope.all2,m.max2)

}
# END loop for each station



# Plot mean and extreme values of every station
cat("\n- Plot mean.and.max")
plot(mean.and.max,ylim = c(0,max(mean.and.max[,2], na.rm = TRUE)), xlim=c(0,max.all), xlab = "mean rainfall [wet day] (mm/day)", ylab = "extreme rainfall (mm/day)", main = "extreme rainfall ration of all station", col= "blue", cex = 0.8, pch = 19)
slope.m = mean.and.max[,2]/mean.and.max[,1]
max.slope = which(slope.m == max(slope.m,na.rm = TRUE))
extrem.eq = lm(c(0,mean.and.max[max.slope,2])~ c(0,mean.and.max[max.slope,1]))
abline(extrem.eq,col= "red" , lwd = 3)
legend("topright", cex = 0.8, y.intersp = 1.15,legend = c("extreme value","extreme trend line"), col=c("blue","red"), lwd =c(1,3), lty=c(-1,1), pch = c(19,-1),bg = "white")


max.all = max(mean.and.max[,], na.rm = TRUE)
mean.at.max = mean.and.max[which(mean.and.max[,2] == max.all),1]

##
## mean and multiple term of mean
##
extreme.multi.data = cbind(mean.and.max[,1],mean.and.max[,2]/mean.and.max[,1])
sel.row.pl = which(!is.na(mean.and.max[,1]))
order.pl = order(mean.and.max[sel.row.pl,1])
extreme.multi = integer(0)

# Seach for sloping up
extreme.temp = 0
for(i in 1:length(sel.row.pl)){
if(extreme.multi.data[sel.row.pl[order.pl[i]],2] > extreme.temp){
	extreme.multi = rbind(extreme.multi , c(extreme.multi.data[sel.row.pl[order.pl[i]],1],extreme.multi.data[sel.row.pl[order.pl[i]],2]))
	extreme.temp = extreme.multi[dim(extreme.multi)[1],2]
	max.at.x = i
}
}

# Seach for sloping down
extreme.multi.2 = integer(0)
extreme.temp = 0
for(i in length(sel.row.pl):max.at.x){
if(mean.and.max[sel.row.pl[order.pl[i]],2]/mean.and.max[sel.row.pl[order.pl[i]],1] > extreme.temp){
	extreme.multi.2 = rbind(extreme.multi.2 , c(extreme.multi.data[sel.row.pl[order.pl[i]],1],extreme.multi.data[sel.row.pl[order.pl[i]],2]))
	extreme.temp = extreme.multi.2[dim(extreme.multi.2)[1],2]
}
}
extreme.multi = rbind(extreme.multi, extreme.multi.2[order(extreme.multi.2[,1]),])

## Plot mean and multi values of extreme points
cat("\n- Plot extreme equations")
plot(extreme.multi.data, ylim = c(0,max(extreme.multi.data[,2],na.rm = TRUE)), xlab = "mean rainfall [wet day] (mm/day)", ylab = "ratio of max/mean",type="h",  main = paste("select limit line of extrme rainfall",sta.i))
#lines(extreme.multi, col = "green")
for(i in 5:8){
plx.order = i
eq.multi.plx = lm(extreme.multi[,2]~., data=poly(extreme.multi[,1],plx.order,raw=TRUE))
#lines(extreme.multi[,1],predict(eq.multi.plx,poly(extreme.multi[,1],plx.order,raw=TRUE)),col=(i-2),lty=3,lwd = 2)
#lines(1:round(extreme.multi[dim(extreme.multi)[1],1]),predict(eq.multi.plx,poly(1:round(extreme.multi[dim(extreme.multi)[1],1]),plx.order,raw=TRUE)),col=(i-2),lty=3,lwd = 2)
lines(1:80,predict(eq.multi.plx,poly(1:80,plx.order,raw=TRUE)),col=(i-2),lty=2,lwd = 2)
}

# Select order plx.order to use in equation
pl.extreme.order.lim = 7
eq.multi.plx.lim = lm(extreme.multi[,2]~., data=poly(extreme.multi[,1],pl.extreme.order.lim,raw=TRUE))

# Plot final limit conclusion
sel.limit.cover.all = integer(0)
for(pt.i in seq(0,max(mean.and.max[,1],na.rm = TRUE),by = 0.5)){
		new.mean.rain = pt.i  # transform mean.rain fit to number of rainy day
		if(max(new.mean.rain) > limit.eq.multi.plx){limit.extreme = 1-exp(-beyond.eq.multi.plx)
									 }else{if(max(new.mean.rain) != 0){limit.extreme = 1-exp(-predict(eq.multi.plx.lim,poly(c(max(new.mean.rain),1:9),pl.extreme.order.lim,raw=TRUE))[1])}else{limit.extreme = 1}
										}
sel.limit.cover.all = rbind(sel.limit.cover.all, cbind(pt.i, -log(1-limit.extreme)))
}
lines(sel.limit.cover.all, lwd = 5, col = "red") # Plot limit conclusion

legend("topright", cex = 0.8, y.intersp = 1.15,legend = c("obs. extreme value",paste("trend line: poly. order",(5:8)), "selecte line of limit"), col=c("black",c(5:8)-2,"red"), lwd =c(2,rep(2,6),5), lty=c(0,rep(2,4),1),pch = c(124,rep(-1,4),-1),bg = "white")



# plot limit of ration max/mean
#plot(cbind(c(0,0),c(1,1)), ylim = c(0,max(mean.and.max[,2],na.rm = TRUE)), xlim = c(0,max(mean.and.max[,1],na.rm = TRUE)), type = "n", xlab = "mean rain (mm/day)", ylab = "ration of max/mean")
#plot(cbind(c(0,0),c(1,1)), ylim = c(0.8,1), xlim = c(0,max(mean.and.max[,1],na.rm = TRUE)), type = "n", xlab = "mean rain (mm/day)", ylab = "ration of max/mean")
#plot(cbind(c(0,0),c(1,1)), ylim = c(0,max(max.slope.all,na.rm = TRUE)), xlim = c(0,max(mean.and.max[,1],na.rm = TRUE)), type = "n", xlab = "mean rain (mm/day)", ylab = "ratio of max/mean", main = "limit of extrme rainfall")
plot(sel.limit.cover.all, ylim = c(0,max(max.slope.all,na.rm = TRUE)), xlim = c(0,max(mean.and.max[,1],na.rm = TRUE)), type = "l", xlab = "mean rain (mm/day)", ylab = "ratio of max/mean", main = "limit of extrme rainfall", lwd = 5)

#lines(max.slope.all)

dev.off()
######################
## END Determine relationship mean and extrme
######################

# max.v.all for cut peak curve , max.v.all2 for beyond the extreme values then set to narrow random
max.v.all = 1-exp(-max.slope.all)
ratio.limit.point = 2



######################
## Determine relationship mean and extrme v2c
######################
##
## mean and multiple term of mean v2c
##
setwd(dir1)
pdf("Mean and Max v2c.pdf", width = 6 ,height = 6)
par(mgp=c(2.5,1,0), ps =16, mfrow=c(1,1))
par(mai =  c(.8, .8, .8, .8))
setwd(dir0)

###*** fix peak at station 8 ######
temp.pos = which.max(mean.and.max.sta[8,,2]/mean.and.max.sta[8,,1])
temp = mean.and.max.sta[8,,1][temp.pos]
mean.and.max.sta[8,,1][temp.pos] = NA
#mean.and.max.sta[8,,1][temp.pos] = temp
###*** END fix peak at station 8 ######


# Loop for every station
max.all.mean = max(mean.and.max.sta[,,1],na.rm = TRUE)
max.all.slope = max(mean.and.max.sta[,,2]/mean.and.max.sta[,,1],na.rm = TRUE)
mean.seq = seq(0,max.all.mean,by = 0.5)
sel.limit.all = array(mean.seq, c(length(mean.seq),1))
ratio.change.at = integer(0) # ratio.change.at --> gives value of limit max/min = ratio.limit.point at mean ??
colnames(sel.limit.all) = "mean"
for(sta.i in 1:station.n){

extreme.multi.data = cbind(mean.and.max.sta[sta.i,,1],mean.and.max.sta[sta.i,,2]/mean.and.max.sta[sta.i,,1])
sel.row.pl = which(!is.na(mean.and.max.sta[sta.i,,1]))
order.pl = order(mean.and.max.sta[sta.i,sel.row.pl,1])
extreme.multi = integer(0)

# Seach for sloping up
extreme.temp = 0
for(i in 1:length(sel.row.pl)){
if(extreme.multi.data[sel.row.pl[order.pl[i]],2] > extreme.temp){
	extreme.multi = rbind(extreme.multi , c(extreme.multi.data[sel.row.pl[order.pl[i]],1],extreme.multi.data[sel.row.pl[order.pl[i]],2]))
	extreme.temp = extreme.multi[dim(extreme.multi)[1],2]
	max.at.x = i
}
}

# Seach for sloping down
extreme.multi.2 = integer(0)
extreme.temp = 0
for(i in length(sel.row.pl):max.at.x){
if(mean.and.max.sta[sta.i,sel.row.pl[order.pl[i]],2]/mean.and.max.sta[sta.i,sel.row.pl[order.pl[i]],1] > extreme.temp){
	extreme.multi.2 = rbind(extreme.multi.2 , c(extreme.multi.data[sel.row.pl[order.pl[i]],1],extreme.multi.data[sel.row.pl[order.pl[i]],2]))
	extreme.temp = extreme.multi.2[dim(extreme.multi.2)[1],2]
}
}
extreme.multi = rbind(extreme.multi, extreme.multi.2[order(extreme.multi.2[,1]),])

# Select order plx.order to use in equation
pl.extreme.order = 5
eq.multi.plx = lm(extreme.multi[,2]~., data=poly(extreme.multi[,1],pl.extreme.order,raw=TRUE))
# Select value at limit of equation
beyond.eq.multi.plx.2 = 1
prd.list = predict(eq.multi.plx,poly(c(mean.seq,1:9),pl.extreme.order,raw=TRUE))[1:length(mean.seq)]
# limit value till the value reach value of 1 (max/min)
#limit.eq.multi.plx.2 = mean.seq[which.min(abs(prd.list-1))]
#limit.eq.multi.plx.2 = mean.seq[min(order(abs(prd.list-1))[1:3])] # minimum number at lowest 3 values
#limit.eq.multi.plx.2 = mean.seq[-c(1:5)][which.min(abs(prd.list[-c(1:5)]-1))] # minimum number selected at lowest 3 values , except first 5 values (0.5,1,0,1.5,...)
limit.eq.multi.plx.2 = mean.seq[-c(1:5)][min(order(abs(prd.list[-c(1:5)]-ratio.limit.point))[1:2])] # minimum number selected at lowest 3 values , except first 5 values (0.5,1,0,1.5,...)
ratio.change.at = c(ratio.change.at,limit.eq.multi.plx.2)
# ratio.change.at --> gives value of limit max/min = ratio.limit.point at mean ??





cat("\n- Plot extreme equations sta:",sta.i)

###*** fix peak at station 8 ######
if(sta.i == 8){mean.and.max.sta[8,,1][temp.pos] = temp}
###*** END fix peak at station 8 ######

## Plot mean and multi values of extreme points
plot(mean.and.max.sta[sta.i,,1],mean.and.max.sta[sta.i,,2]/mean.and.max.sta[sta.i,,1],xlim = c(0,max.all.mean), ylim = c(0,max.all.slope), xlab = "mean rainfall [wet day] (mm/day)", ylab = "ratio of max/mean",type="h", main = paste("select limit line of extrme rainfall",sta.i))
#plot(extreme.multi.data,xlim = c(0,max.all.mean), ylim = c(0,max.all.slope), xlab = "mean rainfall [wet day] (mm/day)", ylab = "ratio of max/mean",type="h", main = "select limit line of extrme rainfall")

# Plot global limit from all station limit
lines(sel.limit.cover.all, type = "l", xlab = "mean rain (mm/day)", lty = 1, lwd = 2, col = "red")

#lines(mean.seq,prd.list)

#lines(extreme.multi, col = "green")
#for(i in 5:8){
#plx.order = i
#eq.multi.plx = lm(extreme.multi[,2]~., data=poly(extreme.multi[,1],plx.order,raw=TRUE))
#lines(1:50,predict(eq.multi.plx,poly(1:50,plx.order,raw=TRUE)),col=(i-2),lty=2,lwd = 2)
#}

# Select order plx.order to use in equation
eq.multi.plx = lm(extreme.multi[,2]~., data=poly(extreme.multi[,1],pl.extreme.order,raw=TRUE))

# Plot final limit conclusion
#sel.limit = integer(0)
#for(pt.i in seq(0,max.all.mean,by = 0.5)){
#		new.mean.rain = pt.i  # transform mean.rain fit to number of rainy day
#		if(max(new.mean.rain) > limit.eq.multi.plx.2){limit.extreme = 1-exp(-beyond.eq.multi.plx.2)
#									 }else{if(max(new.mean.rain) != 0){limit.extreme = 1-exp(-predict(eq.multi.plx,poly(c(max(new.mean.rain),1:9),pl.extreme.order,raw=TRUE))[1])}else{limit.extreme = 1}
#										}
#sel.limit = rbind(sel.limit, cbind(pt.i, -log(1-limit.extreme)))
#}
# Plot limit conclusion
#lines(sel.limit, lwd = 5, col = "red") 

# Plot limit upper1
lines(c(0,limit.eq.multi.plx),rep(max.slope.all[sta.i],2),lty = 3, lwd = 2, col = "blue")
# Plot limit upper2
lines(c(limit.eq.multi.plx,max.all.mean),rep(max.slope.all2[sta.i],2),lty = 3, lwd = 2, col = "blue")

#legend("topright", cex = 0.8, y.intersp = 1.15,legend = c("obs. extreme value",paste("trend line: poly. order",(5:8)), "selecte line of limit"), col=c("black",c(5:8)-2,"red"), lwd =c(2,rep(2,6),5), lty=c(0,rep(2,4),1),pch = c(124,rep(-1,4),-1),bg = "white")
legend("topright", cex = 0.8, y.intersp = 1.15,legend = c("obs. extreme value", "over-all limit line","station limit line"), col=c("black","red","blue"), lwd =c(2,2,2), lty=c(0,1,3),pch = c(124,-1,-1),bg = "white")




# plot limit of ration max/mean
#plot(sel.limit, ylim = c(0,max(max.slope.all,na.rm = TRUE)), xlim = c(0,max(mean.and.max.sta[sta.i,,1],na.rm = TRUE)), type = "l", xlab = "mean rain (mm/day)", ylab = "ratio of max/mean", main = "limit of extrme rainfall", lwd = 5)

#sel.limit.all = cbind(sel.limit.all,sel.limit[,2])

}
# END Loop for every station (sta.i)
dev.off()

#sel.limit.check = sel.limit.all[-1,]
# ratio.change.at --> gives value of limit max/min = 1 at mean ??
######################
## END Determine relationship mean and extrme v2c
######################





######################
## Determine relationship mean and extrme v2d
######################
##
## mean and multiple term of mean v2d
##
setwd(dir1)
pdf("Mean and Max v2d.pdf", width = 6 ,height = 6)
par(mgp=c(2.5,1,0), ps =16, mfrow=c(1,1))
par(mai =  c(.8, .8, .8, .8))
setwd(dir0)

###*** fix peak at station 8 ######
temp.pos = which.max(mean.and.max.sta[8,,2]/mean.and.max.sta[8,,1])
temp = mean.and.max.sta[8,,1][temp.pos]
mean.and.max.sta[8,,1][temp.pos] = NA
#mean.and.max.sta[8,,1][temp.pos] = temp
###*** END fix peak at station 8 ######


# Loop for every station
max.all.mean = max(mean.and.max.sta[,,1],na.rm = TRUE)
max.all.slope = max(mean.and.max.sta[,,2]/mean.and.max.sta[,,1],na.rm = TRUE)
mean.seq = seq(0,max.all.mean,by = 0.5)
sel.limit.all = array(mean.seq, c(length(mean.seq),1))
ratio.change.at = integer(0) # ratio.change.at --> gives value of limit max/min = ratio.limit.point at mean ??
colnames(sel.limit.all) = "mean"
for(sta.i in 1:station.n){

extreme.multi.data = cbind(mean.and.max.sta[sta.i,,1],mean.and.max.sta[sta.i,,2]/mean.and.max.sta[sta.i,,1])
sel.row.pl = which(!is.na(mean.and.max.sta[sta.i,,1]))
order.pl = order(mean.and.max.sta[sta.i,sel.row.pl,1])
extreme.multi = integer(0)

# Seach for sloping up
extreme.temp = 0
for(i in 1:length(sel.row.pl)){
if(extreme.multi.data[sel.row.pl[order.pl[i]],2] > extreme.temp){
	extreme.multi = rbind(extreme.multi , c(extreme.multi.data[sel.row.pl[order.pl[i]],1],extreme.multi.data[sel.row.pl[order.pl[i]],2]))
	extreme.temp = extreme.multi[dim(extreme.multi)[1],2]
	max.at.x = i
}
}

# Seach for sloping down
extreme.multi.2 = integer(0)
extreme.temp = 0
for(i in length(sel.row.pl):max.at.x){
if(mean.and.max.sta[sta.i,sel.row.pl[order.pl[i]],2]/mean.and.max.sta[sta.i,sel.row.pl[order.pl[i]],1] > extreme.temp){
	extreme.multi.2 = rbind(extreme.multi.2 , c(extreme.multi.data[sel.row.pl[order.pl[i]],1],extreme.multi.data[sel.row.pl[order.pl[i]],2]))
	extreme.temp = extreme.multi.2[dim(extreme.multi.2)[1],2]
}
}
extreme.multi = rbind(extreme.multi, extreme.multi.2[order(extreme.multi.2[,1]),])

# Select order plx.order to use in equation
pl.extreme.order = 5
eq.multi.plx = lm(extreme.multi[,2]~., data=poly(extreme.multi[,1],pl.extreme.order,raw=TRUE))
# Select value at limit of equation
beyond.eq.multi.plx.2 = 1
prd.list = predict(eq.multi.plx,poly(c(mean.seq,1:9),pl.extreme.order,raw=TRUE))[1:length(mean.seq)]
# limit value till the value reach value of 1 (max/min)
#limit.eq.multi.plx.2 = mean.seq[which.min(abs(prd.list-1))]
#limit.eq.multi.plx.2 = mean.seq[min(order(abs(prd.list-1))[1:3])] # minimum number at lowest 3 values
#limit.eq.multi.plx.2 = mean.seq[-c(1:5)][which.min(abs(prd.list[-c(1:5)]-1))] # minimum number selected at lowest 3 values , except first 5 values (0.5,1,0,1.5,...)
limit.eq.multi.plx.2 = mean.seq[-c(1:5)][min(order(abs(prd.list[-c(1:5)]-ratio.limit.point))[1:2])] # minimum number selected at lowest 3 values , except first 5 values (0.5,1,0,1.5,...)
ratio.change.at = c(ratio.change.at,limit.eq.multi.plx.2)
# ratio.change.at --> gives value of limit max/min = ratio.limit.point at mean ??

cat("\n- Plot extreme equations sta:",sta.i)

###*** fix peak at station 8 ######
if(sta.i == 8){mean.and.max.sta[8,,1][temp.pos] = temp}
###*** END fix peak at station 8 ######

## Plot mean and multi values of extreme points
plot(mean.and.max.sta[sta.i,,1],mean.and.max.sta[sta.i,,2]/mean.and.max.sta[sta.i,,1],xlim = c(0,max.all.mean), ylim = c(0,max.all.slope), xlab = "mean rainfall [wet day] (mm/day)", ylab = "ratio of max/mean",type="h", main = paste("select limit line of extrme rainfall",sta.i))
#plot(extreme.multi.data,xlim = c(0,max.all.mean), ylim = c(0,max.all.slope), xlab = "mean rainfall [wet day] (mm/day)", ylab = "ratio of max/mean",type="h", main = "select limit line of extrme rainfall")

# Plot global limit from all station limit
lines(sel.limit.cover.all, type = "l", xlab = "mean rain (mm/day)", lty = 1, lwd = 2, col = "red")

#lines(mean.seq,prd.list)

#lines(extreme.multi, col = "green")
#for(i in 5:8){
#plx.order = i
#eq.multi.plx = lm(extreme.multi[,2]~., data=poly(extreme.multi[,1],plx.order,raw=TRUE))
#lines(1:50,predict(eq.multi.plx,poly(1:50,plx.order,raw=TRUE)),col=(i-2),lty=2,lwd = 2)
#}

# Select order plx.order to use in equation
eq.multi.plx = lm(extreme.multi[,2]~., data=poly(extreme.multi[,1],pl.extreme.order,raw=TRUE))
if(sta.i == sel.sta.v.3){
				eq.v.3 = eq.multi.plx
				limit.eq.v.3 = limit.eq.multi.plx.2
				}

# Plot final limit conclusion
sel.limit = integer(0)
for(pt.i in seq(0,max.all.mean,by = 0.5)){
		new.mean.rain = pt.i  # transform mean.rain fit to number of rainy day
		if(max(new.mean.rain) > limit.eq.multi.plx.2){limit.extreme = 1-exp(-beyond.eq.multi.plx.2)
									 }else{if(max(new.mean.rain) != 0){limit.extreme = 1-exp(-predict(eq.multi.plx,poly(c(max(new.mean.rain),1:9),pl.extreme.order,raw=TRUE))[1])}else{limit.extreme = 1}
										}
sel.limit = rbind(sel.limit, cbind(pt.i, -log(1-limit.extreme)))
}
# Plot limit conclusion
lines(sel.limit,lty = 2, lwd = 2, col = "red") 

# Plot limit upper1
lines(c(0,limit.eq.multi.plx),rep(max.slope.all[sta.i],2),lty = 3, lwd = 2, col = "blue")
# Plot limit upper2
lines(c(limit.eq.multi.plx,max.all.mean),rep(max.slope.all2[sta.i],2),lty = 3, lwd = 2, col = "blue")

#legend("topright", cex = 0.8, y.intersp = 1.15,legend = c("obs. extreme value",paste("trend line: poly. order",(5:8)), "selecte line of limit"), col=c("black",c(5:8)-2,"red"), lwd =c(2,rep(2,6),5), lty=c(0,rep(2,4),1),pch = c(124,rep(-1,4),-1),bg = "white")
legend("topright", cex = 0.8, y.intersp = 1.15,legend = c("obs. extreme value", "over-all limit line","station limit line"), col=c("black","red","blue"), lwd =c(2,2,2), lty=c(0,1,3),pch = c(124,-1,-1),bg = "white")


# plot limit of ration max/mean
#plot(sel.limit, ylim = c(0,max(max.slope.all,na.rm = TRUE)), xlim = c(0,max(mean.and.max.sta[sta.i,,1],na.rm = TRUE)), type = "l", xlab = "mean rain (mm/day)", ylab = "ratio of max/mean", main = "limit of extrme rainfall", lwd = 5)

#sel.limit.all = cbind(sel.limit.all,sel.limit[,2])

}
# END Loop for every station (sta.i)
dev.off()

#sel.limit.check = sel.limit.all[-1,]
# ratio.change.at --> gives value of limit max/min = 1 at mean ??
######################
## END Determine relationship mean and extrme v2d
######################




######################
## Determine relationship mean and extrme v3
######################
##
## mean and multiple term of mean v3
##
setwd(dir1)
pdf("Mean and Max v3.pdf", width = 6 ,height = 6)
par(mgp=c(2.5,1,0), ps =16, mfrow=c(1,1))
par(mai =  c(.8, .8, .8, .8))
setwd(dir0)

# Loop for every station
for(sta.i in 1:station.n){

cat("\n- Plot extreme equations sta:",sta.i)


## Plot mean and multi values of extreme points
plot(mean.and.max.sta[sta.i,,1],mean.and.max.sta[sta.i,,2]/mean.and.max.sta[sta.i,,1],xlim = c(0,max.all.mean), ylim = c(0,max.all.slope), xlab = "mean rainfall [wet day] (mm/day)", ylab = "ratio of max/mean",type="h", main = paste("select limit line of extrme rainfall",sta.i))
# Plot global limit from all station limit
plot.limit = which(sel.limit.cover.all[,1] < limit.eq.multi.plx)
#lines(sel.limit.cover.all, type = "l", xlab = "mean rain (mm/day)", lty = 1, lwd = 2, col = "red")
lines(sel.limit.cover.all[plot.limit,], type = "l", xlab = "mean rain (mm/day)", lty = 1, lwd = 2, col = "red")
# check is it sel.sta.v.2
if(length(intersect(sel.sta.v.2,sta.i))){
							# Plot limit upper1
							lines(c(0,limit.eq.multi.plx),rep(max.slope.all[sta.i],2),lty = 2, lwd = 2, col = "blue")
							# Plot limit # 2
							lines(c(limit.eq.multi.plx,limit.eq.multi.plx2),c(beyond.eq.multi.plx,beyond.eq.multi.plx),lty = 2, lwd = 2, col = "red")
							# Plot limit # 3
							lines(c(limit.eq.multi.plx2,500),c(beyond.eq.multi.plx2,beyond.eq.multi.plx2),lty = 2, lwd = 2, col = "red")
							}else{
								# Plot limit # 2
								lines(c(limit.eq.multi.plx,500),c(beyond.eq.multi.plx,beyond.eq.multi.plx),lty = 2, lwd = 2, col = "red")
								#lines(c(limit.eq.multi.plx2,500),c(beyond.eq.multi.plx2,beyond.eq.multi.plx2),lty = 2, lwd = 2, col = "red")
								}

#legend("topright", cex = 0.8, y.intersp = 1.15,legend = c("obs. extreme value",paste("trend line: poly. order",(5:8)), "selecte line of limit"), col=c("black",c(5:8)-2,"red"), lwd =c(2,rep(2,6),5), lty=c(0,rep(2,4),1),pch = c(124,rep(-1,4),-1),bg = "white")
legend("topright", cex = 0.8, y.intersp = 1.15,legend = c("obs. extreme value", "over-all limit line","station limit line"), col=c("black","red","blue"), lwd =c(2,2,2), lty=c(0,1,3),pch = c(124,-1,-1),bg = "white")



# Plot real limit in use
sel.limit = integer(0)
plot(mean.and.max.sta[sta.i,,1],mean.and.max.sta[sta.i,,2]/mean.and.max.sta[sta.i,,1],xlim = c(0,max.all.mean), ylim = c(0,max.all.slope), xlab = "mean rainfall [wet day] (mm/day)", ylab = "ratio of max/mean",type="h", main = paste("select limit line of extrme rainfall",sta.i))
for(i in seq(1,round(max.all.mean),0.5)){
new.mean.rain = rep(i,station.n)

		limit.extreme = rep(1-exp(-beyond.eq.multi.plx),station.n)
		sel.sta = which((new.mean.rain-limit.eq.multi.plx)<0)
		limit.extreme[sel.sta] = 1-exp(-predict(eq.multi.plx.lim,poly(c((new.mean.rain),1:9),pl.extreme.order.lim,raw=TRUE)))[sel.sta]

		# Cut peak of equation curve at selected station (sel.sta.v.2)
		sel.cut.peak = which((limit.extreme[sel.sta.v.2] - max.v.all[sel.sta.v.2]) > 0)
		if(length(sel.cut.peak) > 0){limit.extreme[sel.sta.v.2][sel.cut.peak] = max.v.all[sel.sta.v.2][sel.cut.peak]}


		#**# limit v3
		if(new.mean.rain[sel.sta.v.3] > limit.eq.v.3){limit.extreme[sel.sta.v.3] = 1-exp(-beyond.eq.multi.plx)
									   }else{limit.extreme[intersect(sel.sta,sel.sta.v.3)] = 1-exp(-predict(eq.v.3,poly(c((new.mean.rain),1:9),pl.extreme.order.lim,raw=TRUE)))[intersect(sel.sta,sel.sta.v.3)]}


		# Cut peak of linear after equation at selected station (sel.sta.v.2)  - control extreme values not to be exceeded 
		sel.cut.peak.2 = which((new.mean.rain[sel.sta.v.2] - limit.eq.multi.plx2) > 0)
		if(length(sel.cut.peak.2) > 0){limit.extreme[sel.sta.v.2][sel.cut.peak.2] = rep(1-exp(-beyond.eq.multi.plx2),length(sel.cut.peak.2))}

#lines(i,limit.extreme[sel.sta.v.3],type = "p", pch = 19, col = "red")
sel.limit = rbind(sel.limit,cbind(i,-log(1-limit.extreme[sta.i])))
}
lines(sel.limit,type = "l", lty = 1, lwd = 3, col = "red")


}
# END Loop for every station (sta.i)
dev.off()

######################
## END Determine relationship mean and extrme v3
######################







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
weigth.n = dim(weight.d)[1]




setwd(dir1)
write.csv(weight.d, file = distance.f)
setwd(dir0)

# inverse distance to spatial weight
weight.m = 1/(weight.d^2)
#for(i in 1:weigth.n){weight.m[i,i] = 1/weight.m}

# diagonal to be zero
for(i in 1:weigth.n){weight.m[i,i] = 0}
## row-standardized
for(i in 1:weigth.n){
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
data.12.mean = array(NA, c(12,station.n))
for(m in 1:12){
			sel.row = which(daily.month.list == m)

			#data.12.mean[m,] = mean(future.monthly[which(future.month.list==m),])
			# Amount / wetday of every station
			for(j in 1:station.n){
						   data.12.mean[m,j] = sum(data.daily[sel.row,j])/length(which(data.daily[sel.row,j]>0))

						   }
		}



##################
#### change the amount of mean rainfall
##################
setwd(dir1)
pdf(paste("Vplot.pdf"))
par(mgp=c(2,1,0), ps =10)
setwd(dir0)

if(show.v.chart){
			dev.new(3)
			dev.set(dev.prev())
			}


# Define daily Moran's I parameters
moran.i.daily.all  = array(NA,c(year.n,12,length(unique(data.daily.org[,daily.day.col]))))
daily.moran.i.sim = array(NA,c(12,31))
daily.ma.coef  = array(NA,c(12,length(unique(data.daily.org[,daily.day.col]))))

# Define prediction parameters
#rain.predict.gen = array(NA, c(12,station.n,day.in.month)) # set the matrix of rainfall amount results
#rain.predict.gen = array(NA, c(12,station.n,31,weather.gen.predict.realiz.n)) # set the matrix of rainfall amount results
#rain.predict.gen.avg = array(NA, c(12,station.n,31)) # set the matrix of rainfall amount results
rain.predict.gen = array(NA, c(year.n,12,station.n,31,weather.gen.predict.realiz.n)) # set the matrix of rainfall amount results
rain.predict.gen.avg = array(NA, c(year.n,12,station.n,31)) # set the matrix of rainfall amount results


##################################################
# Great loop to chang month   ####################
##################################################
for(month.i in run.month){
cat("\n****************\n Month :",month.i,"\n****************\n")

# extract from data for station.n station(s)
mean.rain = data.12.mean[month.i,]

##########
## Calculate observed daily Moran's I
cat("\nCalculate observed daily Moran's I\n")
row.sel = which(data.daily.org[,daily.month.col] == month.i)

### day.in.month.list[,2] : year, day.in.month.list[,2] : month, day.in.month.list[,5] : day
###
day.in.month = max(day.in.month.list[which(day.in.month.list[,2]==month.i),5])
#day.in.month = length(unique(data.daily.org[row.sel,daily.day.col]))


for(day.i in 1:day.in.month){
			row.sel2 = which((data.daily.org[,daily.month.col] == month.i) & (data.daily.org[,daily.day.col] == day.i))
			for(row.n in 1:length(row.sel2)){
							moran.i.daily.all[row.n,month.i,day.i] = moran.i(as.numeric(data.daily[row.sel2[row.n],]),weight.m)
							}
			cat("\nMonth :",month.i," Day :",day.i," -- Moran's I :",mean(moran.i.daily.all[,month.i,day.i], na.rm = TRUE))
			}

# Plot histrogram of daily Moran's I
par(mgp=c(2.5,1,0), ps =16, mfrow=c(1,1))
par(mai =  c(.8, .8, .8, .8))
avai.i = moran.i.daily.all[,month.i,][!is.na(moran.i.daily.all[,month.i,])]
plot(avai.i, main = paste("Histrogram of daily Moran's I at month",month.i), xlab = "data set", ylab = "daily Moran's I")
hist(avai.i, breaks=100, main = paste("Histrogram of daily Moran's I at month",month.i), xlab = "daily Moran's I")

daily.avg.moran.i = colMeans(moran.i.daily.all,na.rm = TRUE)

# Plot obs daily Moran's I
plot(c(1,31),c(min(moran.i.daily.all[,month.i,],na.rm = TRUE),max(moran.i.daily.all[,month.i,],na.rm = TRUE)), type = "n",main = paste("daily Moran's I at month",month.i), xlab = "day", ylab = "daily Moran's I")
for(yr.i in 1:dim(moran.i.daily.all)[1]){
lines(1:31,moran.i.daily.all[yr.i,month.i,], type = "p", pch = yr.i,main = paste("daily Moran's I at month",month.i), col = "grey", cex = 0.8)
}
lines(1:31,daily.avg.moran.i[month.i,], type = "l", lwd = 3, lty = 1, col = "red")
legend("topright", y.intersp = 1.15, cex = 0.8, legend = c("obsvered moran's I","mean Moran's I"), lty = c(-1,1), pch = c(1,-1), col = c("grey","red"), lwd = c(0,3), bg = "white")










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
cat("\nFind rang of ma.coef\n")
ma.coef.range = integer(0)
for(i in 1:ma.coef.n){ma.coef.range = c(ma.coef.range,ma.min+i*(ma.max - ma.min)/(ma.coef.n+2))} # ma.max and ma.min is not included in the rang





#### Generate V, rain.predict spattially autocorrelation random variables


				## change ma.coef within ma.coef.range
				v.predict = array(NA, c(station.n,ma.coef.n,seed.v.ndist.n))
				v.re.predict = array(NA, c(station.n,ma.coef.n,seed.v.ndist.n))

				## change the random number (u) following the range of ma.coef.rang
				cat("Calculate v values following rang of ma.coef\n")

				# write seed data use common random seed as seed.u.gen.cal.model.rs.array
				if(read.seed.file){
							cat("\nseed.u.gen.cal.model")
							rnd.seed = seed.u.gen.cal.model.rs.array
							.Random.seed = as.integer(unlist(rnd.seed))
							}

				seed.u.m = array(runif(station.n*seed.v.ndist.n,0,1), c(station.n,seed.v.ndist.n))


				# 1) Generate V matrix by changing ma.coef and u [uniform random number]
				for(i in 1:ma.coef.n){
				for(m in 1:seed.v.ndist.n){
							v.predict[,i,m] = ma.coef.range[i]*(weight.m%*%seed.u.m[,m])+seed.u.m[,m]
							}
							}

				# 2) Generate seeds to re-produce v (normalized) after ( Normalize the uni-form model) from max and min of seed.u.m
				runthissub = FALSE
				if(runthissub){
				seed.u.m.predict = array(runif(station.n*seed.v.ndist.n,min(seed.u.m),max(seed.u.m)), c(station.n,1,seed.v.ndist.n))
				# write seed data
				#setwd(dir1)
				#cat("\nseed.u.m.predict")
				#if(record.random.seed){write.csv(seed.u.m.predict, file = "rds-seed-u-m-predict.csv", row.names = FALSE)}
				#if(read.seed.file){seed.u.m.predict = read.table("rds-seed-u-m-predict.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".")}
				#setwd(dir0)
				for(i in 1:ma.coef.n){
				for(m in 1:seed.v.ndist.n){
							v.re.predict[,i,m] = ma.coef.range[i]*(weight.m%*%seed.u.m.predict[,,m])+seed.u.m.predict[,,m]
							}
							}
				}


				# set matrix for v (normalized) and rain.predict
				v.normal = array(NA, c(station.n,ma.coef.n,curve.gen.n)) # set the matrix
				rain.predict = array(NA, c(station.n,ma.coef.n)) # set the matrix
				rownames(v.normal) = rownames(weight.m)
				rownames(rain.predict) = rownames(weight.m)



				## normalize v.predict by cummulative distribution function from V matrix (seed.v.ndist.n sets)
				cat("Normalize v value : set cuumulative distribution model\n")
				# set cuumulative distribution model
				v.normal.model = ecdf(v.predict[,,])






				##############
				## Select realization to plot curve in Moran's I
				##############
				
				# use below when test Moran's I and ma.coef sensitivity
				#setwd(dir1)
				#pdf(paste("I sim test.pdf"))
				#par(mgp=c(2,1,0), ps =10)
				#setwd(dir0)

				cat("\nGenerate data to plot curves of Moran's I")
				curve.list = array(NA, c(ma.coef.n, curve.gen.n))
				for(ma.i in c(1:ma.coef.n)){
					for(rlz in 1:curve.gen.n){

									# define for each station
									# cat("Generate v(i) from cuumulative distribution model")
									v.normal[,ma.i,rlz] = v.normal.model(v.predict[,ma.i,rlz])

									# generate new V, if v = 1, to prevent Inf value
									runthissub = TRUE
									if(runthissub){
											  while(max(v.normal[,ma.i,rlz], na.rm=TRUE) == 1){
													      cat("\n+***+ regenerate V (model)\n")
														re.seed.u.m = array(runif(station.n,0,1), c(station.n,1))
														v.re.predict = ma.coef.range[ma.i]*(weight.m%*%re.seed.u.m)+re.seed.u.m
														v.normal[,ma.i,rlz] = v.normal.model(v.re.predict)
														}
											  }



									rain.temp = -log(1-v.normal[,ma.i,rlz])*array(as.numeric(mean.rain),c(station.n,1))
									curve.list[ma.i,rlz] = moran.i(rain.temp, weight.m)
									#cat(curve.list[ma.i,rlz])

					}
					#cat("\n")
				}

				cat("\n-Select best realization to plot curves in Moran's I")

				# compare best(n sel.1st.rlz) rlz to fit polynomial degree 4
				obs.sel.rlz = c(min(daily.avg.moran.i[month.i,]),max(daily.avg.moran.i[month.i,]))
				mean.obs.sel = mean(daily.avg.moran.i[month.i,], na.rm = TRUE)
				sel.rlz.ord = order(abs(curve.list[1,]-obs.sel.rlz[1])+ abs(curve.list[ma.coef.n,]-obs.sel.rlz[2]), decreasing = FALSE) # find most close to obs. curve 
				#sel.rlz.ord = order(colMeans(curve.list[,])-mean.obs.sel, decreasing = TRUE) # find most above curve over obs.
				#sel.rlz.ord = order(abs(curve.list[1,]-curve.list[ma.coef.n,]), decreasing = TRUE) # find biggest range
				#sel.rlz.ord = order(abs(curve.list[1,]-curve.list[ma.coef.n,]), decreasing = FALSE) # find smallest range
				sel.1st.rlz = 3
				r2.sel.rlz = integer(0)
 				for(rlz.i in 1:sel.1st.rlz){
					y1 = ma.coef.range
					x1 = curve.list[,sel.rlz.ord[rlz.i]]
					#lm.fit = lm(y1 ~ x1)
					lm.fit.pl.4 = lm(y1 ~., data=poly(x1,4,raw=TRUE))	
					r2.sel.rlz = c(r2.sel.rlz,summary(lm.fit.pl.4)$r.squared)
				}
				# select best curve which Max R2
				sel.rlz = sel.rlz.ord[order(r2.sel.rlz, decreasing = TRUE)[1]]


				cat("\n ** Select curve realization number:",sel.rlz,"\n")

				##
				# Plot Moran's I curves and selection
				par(mgp=c(2.5,1,0), ps =16, mfrow=c(1,1))
				par(mai =  c(.8, .8, .8, .8))
				ma.coef.range2 = ma.coef.range[1]+(ma.coef.range[ma.coef.n]- ma.coef.range[1])/(31-1)*c(0:30) # rang for plot daily Moran's I on Moran's I curve chart
				plot(ma.coef.range[c(1,ma.coef.n)],c(min(curve.list[,sel.rlz.ord[1:sel.1st.rlz]],na.rm = TRUE),max(curve.list[,sel.rlz.ord[1:sel.1st.rlz]],na.rm = TRUE)), type ="n", ylab = "Moran's I", xlab = "moving average coef.", main = paste("Moran's I curves and obs. month",month.i))
				
				# Plot curve.gen top sel.1st.rlz
				#for(curve.i in 1:curve.gen.n){
				#lines(ma.coef.range,curve.list[,curve.i], type = "p",pch = curve.i, cex = 0.8)
				for(curve.i in 1:sel.1st.rlz){
				lines(ma.coef.range,curve.list[,sel.rlz.ord[curve.i]], type = "p",pch = curve.i, cex = 0.8 , lty = 3, col = "grey")
				}
				# obs mean
				#obs.moran.i.plot = cbind(rep(ma.coef.range2,dim(moran.i.daily.all)[1]),as.real(moran.i.daily.all[,month.i,]))
				#lines(obs.moran.i.plot, col = "red", type = "p", pch = 19, cex = 0.3)
				lines(ma.coef.range2,daily.avg.moran.i[month.i,],col = "red", lwd = 3)


				# hightlight curve.gen top sel.1st.rlz
				#lines(rep(ma.coef.range,sel.1st.rlz),curve.list[,sel.rlz.ord[1:sel.1st.rlz]],col = "blue", lwd = 1, lty = 2)

				# curve.gen best one (selected)
				lines(ma.coef.range,curve.list[,sel.rlz],col = "blue", lwd = 2, lty = 2)

				legend("bottomright", y.intersp = 1.15, cex = 0.8, legend = c("obsvered mean moran's I","simulated Moran's I curve","selected Moran's I curve"), lty = c(1,-1,2), pch = c(-1,1,-1), col = c("red","grey","blue"), lwd = c(3,1,2), bg = "white")


				###### output is at sel.rlz (selected realization)
				## END Select realization to plot curve in Moran's I







				
				max.predict.rain = 0

				cat("\n calcualte at ma.coef =")
				# run to plot and calculate through ma.coef
				for(n in ma.coef.n:1){
							## set seed to plot normal distribution 
							# cat("Generating seed for transformation to uniform[0,1] n=",seed.v.ndist.n,"\n")
							# v.normal.model = ecdf(v.predict[,n,])

							# define for each station
							# cat("Generate v(i) from cuumulative distribution model")

							#for(rlz in 1:curve.gen.n){
							#					v.normal[,n,rlz] = v.normal.model(v.predict[,n,rlz])

												# generate new V, if v = 1, to prevent Inf value
							#					runthissub = TRUE
							#					if(runthissub){
							#							  while(max(v.normal[,n,rlz], na.rm=TRUE) == 1){
							#									      cat("\n+***+ regenerate V (model)\n")
							#										re.seed.u.m = array(runif(station.n,0,1), c(station.n,1))
							#										v.re.predict = ma.coef.range[n]*(weight.m%*%re.seed.u.m)+re.seed.u.m
							#										v.normal[,n,rlz] = v.normal.model(v.re.predict)
							#										}
							#							  }

							#}

							# calculate rainfall amount at station n
							#if(dim(v.normal[,n,])[1] == 1){ rain.predict[,n] = mean(-log(1-v.normal[,n,])*array(as.numeric(mean.rain),c(station.n,curve.gen.n))) 
							#					}else{ rain.predict[,n] = rowMeans(-log(1-v.normal[,n,])*array(as.numeric(mean.rain),c(station.n,curve.gen.n))) }
							#rain.predict[,n] = rowMeans(-log(1-rowMeans(v.normal[,n,]))*array(as.numeric(mean.rain),c(station.n,curve.gen.n))) 
							rain.predict[,n] = -log(1-v.normal[,n,sel.rlz])*array(as.numeric(mean.rain),c(station.n,1))



							###########################
							##### Plot ma.coef
							# plot to pdf
							# cat("Plot moving average coefficint")
							par(mgp=c(2.5,1,0), ps =16,mfrow=c(2,1))
							par(mai = c(.8, .8, .2, .9))  # Or change '.9' to a larger number for even 

							cat(ma.coef.range[n],", ")
							# define maximum rain.predict to set y-axis
							if(max.predict.rain < max(rain.predict[,n])){max.predict.rain = max(rain.predict[,n])}
							par(mgp=c(2,1,0), ps =12)
							plot(rain.predict[,n], type = "l", xlab = paste("moving avrg coef. =",round(ma.coef.range[n],digits=2),"   at average",round(mean(mean.rain),digits=1),"mm.(chart avg.=",round(mean(rain.predict[,n]),digits=1),"mm.)   of month:",month.i),
							ylab = "Rainfall (mm)", lty = 1, lwd = 3, ylim=c(0,max.predict.rain))

							par(new = TRUE)
							plot(v.normal[,n,sel.rlz], type = "l", ann = FALSE, yaxt = "n", col = "blue", lty = 2, lwd = 2, ylim=c(0,1))
							
							axis(4)
							legend(x = "top", cex = 0.8, y.intersp = 1.15, bty = "n",  lty = c(1,2),  lwd = c(3,2), col = c("black", "blue"),  legend = paste(c("rain","v"), c("(left  y-axis)", "(right y-axis)")))
							
							}


#### END Generate V spattially autocorrelation random variables



# define columns to use in this calculation col.all[xx]
#for(i in 1:station.n){col.all = c(col.all,which(colnames(future.monthly)==colnames(weight.m)[i]))}
#col.all = integer(0)
moran.i.all = integer(0)
rain.predict.all = integer(0)


for(i in 1:ma.coef.n){
							    #cat("ma.coef =",ma.coef.range[i]," Moran's I =",moran.i(rain.predict[,i],weight.m),"\n")
							    moran.i.all = c(moran.i.all , moran.i(rain.predict[,i],weight.m))

							    rain.predict.all = c(rain.predict.all , rain.predict[,i])



			    if(i == 1){
					    ma.coef.and.i = c(ma.coef.range[1],moran.i.all)
					    rain.and.ma.coef = cbind(ma.coef.range[1],rain.predict.all)
					    rain.and.i = cbind(moran.i.all,rain.predict.all)
					   }else{
						    ma.coef.and.i = rbind(ma.coef.and.i, cbind(ma.coef.range[i],moran.i.all))
						    rain.and.ma.coef = rbind(rain.and.ma.coef,cbind(ma.coef.range[i],rain.predict.all))
						    rain.and.i = rbind(rain.and.i,cbind(moran.i.all,rain.predict.all))
						    }



			    rain.predict.all = integer(0)
			    moran.i.all = integer(0)
			    }


par(mgp=c(2.5,1,0), ps =16,mfrow=c(1,1))
par(mai =  c(.8, .8, .8, .8))
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
lines(x1,predict(lm.fit.pl.3,poly(x1,3,raw=TRUE)),col="green",lty=3,lwd = 1)
lines(x1,predict(lm.fit.pl.4,poly(x1,4,raw=TRUE)),col="violet",lty=4,lwd = 1)
legend(x = "top", cex = 0.8, y.intersp = 1.15, bty = "n",  lty = c(1,2,3,4),  lwd = c(2,2,2,2), col = c("blue","red","green","violet"),  legend = c(paste("linear R2=",round(summary(lm.fit)$r.squared,digits=3)),paste("polynomial (degree 2) R2=",round(summary(lm.fit.pl.2)$r.squared,digits=3)),paste("polynomial (degree 3) R2=",round(summary(lm.fit.pl.3)$r.squared,digits=3)),paste("polynomial (degree 4) R2=",round(summary(lm.fit.pl.4)$r.squared,digits=3))))
title(main=paste("Amount of Rainfalll at", station.n," stations for month:",month.i))
# use lm.fit.pl.4 to predict ma.coef






################ END of Parameter estimation phase (providing equation of I and ma.coef)
cat("\nFinish Parameter estimation")






##########
## Extract ma.coef from average of daily Moran's I into daily.ma.coef

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
plot(daily.ma.coef[month.i,] ,xlab = "Days", ylab = "Moving average coef.")


#### END parameter estimation ############################




###################################################
###################################################
# Forecast rainfall (rainfall generation)
###################################################

##########
## Generate V from ma.coef and random u
cat("\n\n- Generate random seed to prediction:",weather.gen.realiz.n, "seed(s)")

# write seed data
if(read.seed.file){
			cat("\nseed.u.gen.cal.model")
			rnd.seed = seed.u.gen.cal.model.rs.array
			.Random.seed = as.integer(unlist(rnd.seed))
			}

## 1) Generate seed to build v.normal.model.gen 
# Generate V matrix by changing ma.coef and u [uniform random number]
seed.u.gen.cal.model = array(runif(station.n*weather.gen.realiz.n,0,1), c(day.in.month,station.n,weather.gen.realiz.n))


v.predict.gen = array(NA, c(day.in.month,station.n, weather.gen.realiz.n))

for(day.i in 1:length(which(!is.na(daily.ma.coef[month.i,])))){
		for(m in 1:weather.gen.realiz.n){
							#-# Use daily ma.coef
							if(use.daily.ma.coef){
								v.predict.gen[day.i,,m] = daily.ma.coef[month.i,day.i]*(weight.m%*%seed.u.gen.cal.model[day.i,,m])+seed.u.gen.cal.model[day.i,,m]
								}else{
								## Use monthly ma.coef
								v.predict.gen[day.i,,m] = mean(daily.ma.coef[month.i,],na.rm =TRUE)*(weight.m%*%seed.u.gen.cal.model[day.i,,m])+seed.u.gen.cal.model[day.i,,m]
								}
						  }
		}


# set cuumulative distribution model
#v.normal.model.gen = ecdf(v.predict.gen[,,])
max.predict.rain = 0







#############################################
# Start estimating rainy day paramters
###

cat("\n\n#Start estimate value for rainy day prediction")

############
# station loop
for(i in 1:length(station.title)){

############
# month loop
#for(j in 1:12){

sumval.state.eq = integer(0)
sel.year1 = which(daily.month.list == month.i)
k.year = unique(daily.year.list[sel.year1])
############
# year loop
for(k in 1:length(k.year)){

sel.year2 = which(daily.year.list[sel.year1] == k.year[k])
month.state = daily.wet.state[,3]

select.day = which(daily.month.list==month.i)
state.state = daily.wet.state[sel.year1[sel.year2],i]+1
year.state = daily.year.list[sel.year1[sel.year2]]
day.state =  daily.day.list[sel.year1[sel.year2]]
serie.state =  daily.365day.list[sel.year1[sel.year2]]
total.rainy.day = sum(daily.wet.state[sel.year1[sel.year2],i])
prob.rainy.day = total.rainy.day/day.in.month


state.table = statetable.msm(state.state, year.state) # (rainy state, month, data matrix)
# check if state.table have no both 1 and 2
if(sum(dim(state.table))<4){q.table = crudeinits.msm(state.state ~ day.state, year.state,qmatrix = rbind(c(0.5,0.5),c(0.5,0.5)))
				   }else{q.table = crudeinits.msm(state.state ~ day.state, year.state,qmatrix = state.table)}
#q.table = crudeinits.msm(state.state ~ serie.state, year.state,qmatrix = q.table)
#rainy.msm = msm(state.state ~ day.state, subject = year.state, qmatrix = q.table, obstype = 2)
#prob.msm = prevalence.msm(rainy.msm)$"Expected percentages"[2,1]

uni.state = unique(state.state)
if(length(uni.state) == 1){
				if(uni.state == 1){
							prob.msm.01 = q.table[1,2]
							prob.msm.11 = 0
							}else{
							prob.msm.01 = 1
							prob.msm.11 = 1+q.table[2,2]
							}

}else{
	prob.msm.01 = q.table[1,2]
	prob.msm.11 = 1+q.table[2,2]
	}




# Print results
cat("\nRainy day preidct # Station :",station.title[i],"(",i,")","Year :",k.year[k]," Month :",month.i," -- P01:",format(prob.msm.01,digits= 1),"   /P11:",format(prob.msm.11,digits= 1))

plotthissub = FALSE
if(plotthissub){
plot.prevalence.msm(rainy.msm)
mtext(paste("Station:",station.title[i],"Month: ",month.i))
}

#sumval.state.eq = rbind(sumval.state.eq,cbind(prob.rainy.day,prob.msm))
sumval.state.eq = rbind(sumval.state.eq,cbind(prob.rainy.day,prob.msm.01,prob.msm.11))



}
# END year loop
############

#plot(sumval.state.eq, main = paste("Rainy prob. Station:",station.title[i],"  Month:",month.i),ylim = c(0,1),xlim=c(0,1))
plot(sumval.state.eq[,c(1,2)], main = paste("Rainy prob. Station:",station.title[i],"  Month:",month.i), xlab = "probability of wet day", ylab = "Pc",ylim = c(0,1),xlim=c(0,1),col="red",type="p",pch= 22, cex = 0.8)
lines(sumval.state.eq[,c(1,3)], type="p", col="blue",pch= 24, cex = 0.8)
#legend(0.5,0.8,legend = c("P01","P11","linear","order 2","order 3","order 4"), col=c("red","blue","black","pink","grey","green"), lwd =c(4,4,1,2,2,2), lty=c(1,1,1,2,3,2),bg = "white")
legend("topright", cex = 0.8, y.intersp = 1.15,legend = c("P01","P11","linear","poly. order 2","poly. order 3"), col=c("red","blue","black","pink","green"), lwd =c(1,1,2,2,2), lty=c(-1,-1,1,2,3), pch = c(22,24,-1,-1,-1),bg = "white")
#mcm.eq = lm(prob.msm~prob.rainy.day,data = as.data.frame(sumval.state.eq))

# 1st order
mcm.01.eq = lm(prob.msm.01~prob.rainy.day,data = as.data.frame(sumval.state.eq))
mcm.11.eq = lm(prob.msm.11~prob.rainy.day,data = as.data.frame(sumval.state.eq))
abline(mcm.01.eq,col="black", lwd =2)
abline(mcm.11.eq,col="black", lwd =2)
# record error
mcm.eq.error = c(i,station.title[i],month.i,mean(abs(mcm.01.eq$residuals)),(mean((mcm.01.eq$residuals)^2))^.5,mean(abs(mcm.11.eq$residuals)),(mean((mcm.11.eq$residuals)^2))^.5)

if(length(which(unique(sumval.state.eq[,1])>0))>2){
# 2nd order
plx.order = 2
mcm.01.eq.plx = lm(as.data.frame(sumval.state.eq)[,2] ~., data=poly(as.data.frame(sumval.state.eq)[,1],plx.order,raw=TRUE))
mcm.11.eq.plx = lm(as.data.frame(sumval.state.eq)[,3] ~., data=poly(as.data.frame(sumval.state.eq)[,1],plx.order,raw=TRUE))
lines(as.data.frame(sumval.state.eq)[order(as.real(sumval.state.eq[,1])),1],predict(mcm.01.eq.plx,poly(as.data.frame(sumval.state.eq)[order(as.real(sumval.state.eq[,1])),1],plx.order,raw=TRUE)),col="pink",lty=2,lwd = 2)
lines(as.data.frame(sumval.state.eq)[order(as.real(sumval.state.eq[,1])),1],predict(mcm.11.eq.plx,poly(as.data.frame(sumval.state.eq)[order(as.real(sumval.state.eq[,1])),1],plx.order,raw=TRUE)),col="pink",lty=2,lwd = 2)
# record error
mcm.eq.error = c(mcm.eq.error,mean(abs(mcm.01.eq.plx$residuals)),(mean((mcm.01.eq.plx$residuals)^2))^.5,mean(abs(mcm.11.eq.plx$residuals)),(mean((mcm.11.eq.plx$residuals)^2))^.5)
}

if(length(which(unique(sumval.state.eq[,1])>0))>3){
# order to use inequation
# 3rd order
plx.order = 3
mcm.01.eq.plx = lm(as.data.frame(sumval.state.eq)[,2] ~., data=poly(as.data.frame(sumval.state.eq)[,1],plx.order,raw=TRUE))
mcm.11.eq.plx = lm(as.data.frame(sumval.state.eq)[,3] ~., data=poly(as.data.frame(sumval.state.eq)[,1],plx.order,raw=TRUE))
lines(as.data.frame(sumval.state.eq)[order(as.real(sumval.state.eq[,1])),1],predict(mcm.01.eq.plx,poly(as.data.frame(sumval.state.eq)[order(as.real(sumval.state.eq[,1])),1],plx.order,raw=TRUE)),col="green",lty=3,lwd = 2)
lines(as.data.frame(sumval.state.eq)[order(as.real(sumval.state.eq[,1])),1],predict(mcm.11.eq.plx,poly(as.data.frame(sumval.state.eq)[order(as.real(sumval.state.eq[,1])),1],plx.order,raw=TRUE)),col="green",lty=3,lwd = 2)
# record error
mcm.eq.error = c(mcm.eq.error,mean(abs(mcm.01.eq.plx$residuals)),(mean((mcm.01.eq.plx$residuals)^2))^.5,mean(abs(mcm.11.eq.plx$residuals)),(mean((mcm.11.eq.plx$residuals)^2))^.5)
}

## Record equation
# Record Poly 3 has 4 terms
mcm.eq.table = rbind(mcm.eq.table,c(i,station.title[i],month.i,
mcm.01.eq.plx$coefficients,mean(abs(mcm.01.eq.plx$residuals)),(mean((mcm.01.eq.plx$residuals)^2))^.5,
mcm.11.eq.plx$coefficients,mean(abs(mcm.11.eq.plx$residuals)),(mean((mcm.11.eq.plx$residuals)^2))^.5))
# Record equation errors
mcm.eq.error.table = rbind(mcm.eq.error.table,mcm.eq.error)

#}
# END Month loop
############

}
# END station loop
############
cat("\nRecord mcm.eq.table")
colnames(mcm.eq.table) = c("Statio.i","Station","month","Intcp.01","m1.01","m2.01","m3.01","abs.mean.error.01","rmsq.error.01","Intcp.11","m1.11","m2.11","m3.11","abs.mean.error.11","rmsq.error.11")
colnames(mcm.eq.error.table) = c("Statio.i","Station","month","abs.mean.error.01-pl1","rmsq.error.01-pl1","abs.mean.error.11-pl1","rmsq.error.11-pl1","abs.mean.error.01-pl2","rmsq.error.01-pl2","abs.mean.error.11-pl2","rmsq.error.11-pl2","abs.mean.error.01-pl3","rmsq.error.01-pl3","abs.mean.error.11-pl3","rmsq.error.11-pl3")

setwd(dir1)
write.csv(mcm.eq.table, file = paste("mcm eq table",month.i,".csv"),row.names=FALSE)
write.csv(mcm.eq.error.table, file = paste("mcm eq error",month.i,".csv"),row.names=FALSE)
setwd(dir0)

###
# END estimating rainy day paramters
#############################################










###################################
# Great loop to chang year    #####
###################################
for(year.i in 1:length(future.year.list[which(future.month.list==month.i)])){
cat("\n## Month :",month.i,"/ Year :",future.all.year[year.i]," ##")
#year.i = 1
day.in.month = day.in.month.list[which(day.in.month.list[,2]==month.i),5][year.i]

#####################
# Define mean rain
#####################
#mean.rain = future.monthly[month.i,1]

row.sel = which(future.year.list == future.all.year[year.i])
# mean of all stations
mean.rain = rep(NA,station.n)
for(i in 1:station.n){
				# for averag daily rain input (mm/day)
				if(future.wetday[row.sel[month.i],i] != 0){
											#mean.rain[i] = future.monthly[row.sel[month.i],i]*day.in.month/future.wetday[row.sel[month.i],i]
											mean.rain[i] = future.monthly[row.sel[month.i],i]
											}else{mean.rain[i] = 0}
				
				# for monthly rain input (mm/month)
				#mean.rain[i] = future.monthly[row.sel[month.i],i]/future.wetday[row.sel[month.i],i]

				}
mean.rain[which(is.na(mean.rain))] = 0

#####################
# Define rainy day
#####################
#rainy.day = sample(0:1,length(which(!is.na(daily.avg.moran.i[month.i,]))),replace=TRUE)
#rainy.day = sample(1,length(which(!is.na(daily.avg.moran.i[month.i,]))),replace=TRUE)
rainy.n = future.wetday[row.sel[month.i],]

## 1) using normal distribution to Random the position of rainy day
#rainy.n.pos1 = sample(1:day.in.month,rainy.n)


## 2) using stocahstic generation Makrov Chain
sel.row = which(as.real(mcm.eq.table[,3])==month.i)
#coef.1 = as.real(mcm.eq.table[sel.row,4])
#coef.2 = as.real(mcm.eq.table[sel.row,5])
coef.1.01 = as.real(mcm.eq.table[sel.row,4])
coef.2.01 = as.real(mcm.eq.table[sel.row,5])
coef.3.01 = as.real(mcm.eq.table[sel.row,6])
coef.4.01 = as.real(mcm.eq.table[sel.row,7])

coef.1.11 = as.real(mcm.eq.table[sel.row,10])
coef.2.11 = as.real(mcm.eq.table[sel.row,11])
coef.3.11 = as.real(mcm.eq.table[sel.row,12])
coef.4.11 = as.real(mcm.eq.table[sel.row,13])




# (still single value of rainy day) Calculate the limit prob of rainy day
if(prob.rainy.by.eq){
	#prob.rainy = (rainy.n/day.in.month)*coef.2+coef.1
	prb.rain  = rainy.n/day.in.month
#	prob.rainy.01 = (prb.rain^4)*coef.5.01+(prb.rain^3)*coef.4.01+(prb.rain^2)*coef.3.01+(prb.rain^1)*coef.2.01+coef.1.01
#	prob.rainy.11 = (prb.rain^4)*coef.5.11+(prb.rain^3)*coef.4.11+(prb.rain^2)*coef.3.11+(prb.rain^1)*coef.2.11+coef.1.11
	prob.rainy.01 = (prb.rain^3)*coef.4.01+(prb.rain^2)*coef.3.01+(prb.rain^1)*coef.2.01+coef.1.01
	prob.rainy.11 = (prb.rain^3)*coef.4.11+(prb.rain^2)*coef.3.11+(prb.rain^1)*coef.2.11+coef.1.11
}else{prob.rainy = 100-rainy.n/day.in.month*100}


# Set the day postion which random prob. >  limit prob of rainy day
random.rain.prob = array(NA, c(day.in.month,weather.gen.predict.realiz.n))
#random.rain.prob.01 = random.rain.prob
#random.rain.prob.11 = random.rain.prob
#random.rain.prob.11 = array(NA, c(day.in.month,weather.gen.predict.realiz.n))

# write seed data
if(read.seed.file){
			cat("\nrandom.rain.prob:",month.i)
			rnd.seed = random.rain.prob.rs.array[,year.i,month.i]
			.Random.seed = as.integer(unlist(rnd.seed))
			}
for(random.i in 1:weather.gen.predict.realiz.n){random.rain.prob[,random.i] = runif(day.in.month)}
#for(random.i in 1:weather.gen.predict.realiz.n){random.rain.prob.01[,random.i] = runif(day.in.month)}
#for(random.i in 1:weather.gen.predict.realiz.n){random.rain.prob.11[,random.i] = runif(day.in.month)}

#random.rain.prob = runif(day.in.month)

#1)# use SAME seed as in v.normal.model.gen(v.predict.gen.seed) ==> v(i)
#2)# use new set of uniform distribution
#random.rain.prob = array(v.normal.model.gen(v.predict.gen.seed[,,1]), c(31,station.n))
#v.predict.gen.seed = array(runif(31*station.n*weather.gen.predict.realiz.n,min(v.predict.gen),max(v.predict.gen)), c(31,station.n,  weather.gen.predict.realiz.n))
#v.predict.gen.seed = array(runif(31*station.n*weather.gen.predict.realiz.n,min(v.predict.gen),max(v.predict.gen)-0.01), c(31,station.n,  weather.gen.predict.realiz.n))




rainy.n.pos = array(NA,c(day.in.month,station.n,weather.gen.predict.realiz.n))
rainy.day.n = array(NA,c(station.n,weather.gen.predict.realiz.n))
#choose.rainy.day = array(NA,c(31,station.n))
rainy.day = array(0, c(day.in.month,station.n,weather.gen.predict.realiz.n))

# Define rainy day for every station
for(sta in 1:station.n){
post.random.prob = array(NA, c(day.in.month,weather.gen.predict.realiz.n))
cat("\n-Station",station.title[sta],"(",sta,") :","Rainy-day  Obs.=",unlist(rainy.n[sta]))
	#cat("\nMCM prob.:",unlist(prob.rainy[sta]))
	for(rlz.i in 1:weather.gen.predict.realiz.n){
	##################################################################
	#post.random.prob = which(random.rain.prob[1:day.in.month,sta]*100 > prob.rainy[sta])
	#pos = which(random.rain.prob[,rlz.i]*100 > as.real(prob.rainy[sta]))
	plot.wet = FALSE

	### -- Define rainyday -- by MCM
	pos = integer(0)
	# check that rowmean is less than 0.5 -- possible dry
	if((rainy.n[sta]/day.in.month) < 0.5){
								# possible first dry
								# if today is dry use p01
								if(plot.wet){cat("\nFirst day is dry")}
								prob.rain.use = 1-as.real(prob.rainy.01[sta])
								#random.rain.use = random.rain.prob.01[,rlz.i]

								for(day.i in 1:day.in.month){
												 if(plot.wet){cat("\nRnd:",random.rain.use[i],"Pc",prob.rain.use)}
												 if(random.rain.prob[day.i,rlz.i] > prob.rain.use){
																			if(plot.wet){cat(" - wet")}
																			pos = c(pos,day.i)
																			# if today is wet use p11
																			prob.rain.use = 1-as.real(prob.rainy.11[sta])
																			#random.rain.use = random.rain.prob.11[,rlz.i]
																			}else{
																				if(plot.wet){cat(" - dry")}
																				# if today is dry use p01
																				prob.rain.use = 1-as.real(prob.rainy.01[sta])
																				#random.rain.use = random.rain.prob.01[,rlz.i]
																				}
												 }


				 		# possible first wet
						# if today is wet use p11
						}else{
							if(plot.wet){cat("\nFirst day is wet")}
							prob.rain.use = 1-as.real(prob.rainy.11[sta])
							#random.rain.use = random.rain.prob.11[,rlz.i]

							for(day.i in 1:day.in.month){
											 if(plot.wet){cat("\nRnd:",random.rain.use[i],"Pc",prob.rain.use)}
											 if(random.rain.prob[day.i,rlz.i] > prob.rain.use){
																		if(plot.wet){cat(" - wet")}
																		pos = c(pos,day.i)
																		# if today is wet use p11
																		prob.rain.use = 1-as.real(prob.rainy.11[sta])
																		#random.rain.use = random.rain.prob.11[,rlz.i]
																		}else{
																			# if today is dry use p01
																			if(plot.wet){cat(" - dry")}
																			prob.rain.use = 1-as.real(prob.rainy.01[sta])
																			#random.rain.use = random.rain.prob.01[,rlz.i]
																			}
											 }
						} # end else
	### END Define rainyday --





	#cat("\nRlz",rlz.i,"Rainy-day=",length(pos)," // rainy prob.=",length(pos)/day.in.month,"/ error:",round(as.real(prob.rainy[sta])-(100-length(pos)/day.in.month*100),digits = 1),"%")
	#post.random.prob[1:length(pos),rlz.i] = pos
	if(length(which(!is.na(pos))) > 0){
										  post.random.prob[1:length(pos),rlz.i] = pos
										  rainy.n.pos[1:length(pos),sta,rlz.i] = post.random.prob[1:length(pos),rlz.i]
										  }else{rainy.n.pos[1,sta,rlz.i] = 0}
	##################################################################
	#cat(length(which(!is.na(rainy.n.pos[,sta,rlz.i]))),",")
	#cat(length(which(!is.na(rainy.n.pos[,sta,rlz.i]))),"=",rainy.n.pos[which(!is.na(rainy.n.pos[,sta,rlz.i])),sta,rlz.i])

	##

	rainy.day[rainy.n.pos[1:length(pos),sta,rlz.i],sta,rlz.i] = 1
	rainy.day.n[sta,rlz.i] = length(which(rainy.day[,sta,rlz.i]==1))
	#choose.rainy.day[,sta] = which(rainy.day[,sta]==1)

	#if(rainy.day.n[sta] == 0){	cat("** NO rain in this month")
	}
	cat("// Sim. = ",round(mean(rainy.day.n[sta,]),digits=1))					

}
# END Define rainy day for every station


# Recording rainy in month each year
cat("\nRecording rainy.in.month")
rainy.in.month.temp = integer(0)
for(i in 1:station.n){rainy.in.month.temp = c(rainy.in.month.temp,rainy.day.n[i,])}
rainy.in.month[(year.i-1)*12+month.i,-c(1,2)] = rainy.in.month.temp

# Define unique rainy day
sel.unique.station = array(NA,c(station.n,day.in.month,weather.gen.predict.realiz.n)) # station for each rainy day classification
sel.unique.station.n = array(NA, c(day.in.month,weather.gen.predict.realiz.n)) # number of station for each rainy day classification
pos.unique.rain.day = array(NA,c(day.in.month,day.in.month,weather.gen.predict.realiz.n)) # day position for each rainy day classification
unique.n = rep(NA,weather.gen.predict.realiz.n)
unique.rainy.day.n = array(NA,c(day.in.month,weather.gen.predict.realiz.n))


for(rlz.i in 1:weather.gen.predict.realiz.n){
	uni.day = unique(rainy.day.n[,rlz.i])[order(unique(rainy.day.n[,rlz.i]))] #number day in each rainy day classification and realization
	unique.rainy.day.n[1:length(uni.day),rlz.i] =  uni.day #number day in each rainy day classification and realization [number of rainyday,rlz.i]
	unique.n[rlz.i] = length(which(!is.na(unique.rainy.day.n[,rlz.i]))) # number of classified rainyday

	for(uni in 1:unique.n[rlz.i]){
				n.sel.station = which(rainy.day.n[,rlz.i] == unique.rainy.day.n[uni,rlz.i]) # select station which fit the number of rainyday
				sel.unique.station[1:length(n.sel.station),uni,rlz.i] = n.sel.station # put station into sel.unique.station
				sel.unique.station.n[uni,rlz.i] = length(n.sel.station)
				pos.unique.rain.day[,uni,rlz.i]= rainy.n.pos[,sel.unique.station[1,uni,rlz.i],rlz.i]
				}
}


# END Define rainy day for every station
# limit.rainy.day.n  # number of rainy day (filled at least 1 day)
# rainy.day.n # real number of rainy day (can be zero)
# rainy.n.pos # day which is rainy

# unique.n to rain.predict function # number of classification
# unique.rainy.day.n # number of day in each rainy day classification
# sel.unique.station  # station for each rainy day classification [station, unique , rlz.i ]
# sel.unique.station.n  # number of station for each rainy day classification
# pos.unique.rain.day # day position for each rainy day classification [i,uni,rlz.i]










#######################################
### V (normalized) and rain.predict

# regenrate seed in case of max(v.normal.gen[,n,seed.n]) = 1
#re.seed.u.m.all = array(runif(station.n*weather.gen.predict.realiz.n,0,1), c(station.n,weather.gen.predict.realiz.n))





# write seed data
if(read.seed.file){
			cat("\nseed.u.gen.predict :",year.i)
			rnd.seed = seed.u.gen.predict.rs.array[,year.i,rlz.i]
			.Random.seed = as.integer(unlist(rnd.seed))
			}

###
# Run each realization
for(rlz.i in 1:weather.gen.predict.realiz.n){
###





# Run each unique number of rainy day
for(uni in 1:unique.n[rlz.i]){
	cat("\n\n--",month.i,"/",future.all.year[year.i],"/ rlz",rlz.i,": Generating for",unique.rainy.day.n[uni,rlz.i],"day(s):",station.title[sel.unique.station[1:sel.unique.station.n[uni,rlz.i],uni,rlz.i]],"\n")

	#
	## Generate rainfall from cummulative distribution model (v.normal.model.gen)
	# define for each day
	# cat("Generate v(i) from cuumulative distribution model")


	###
	# Check rainy.day.n= 0, in case no rainy day this month, set all daily rain to 0 (skipping an error of no rain)
	###
	if(unique.rainy.day.n[uni,rlz.i] != 0){


	################################
	# Set v(n,1) for prediction
	################################
		#-# set cuumulative distribution model for each day (included 24 station)
		# seed.u.gen.predict -> v.predict.gen.seed -> v.normal.gen => rain.predict.gen

		day.pos = pos.unique.rain.day[which(!is.na(pos.unique.rain.day[,uni,rlz.i])),uni,rlz.i]
		a = sel.unique.station[,uni,rlz.i] # select only rainy station

		#v.normal.model.gen = ecdf(v.predict.gen[day.pos,a[which(!is.na(a))],]) # fit model at selected days (day.pos) and staions (a[which(!is.na(a))])
		v.normal.model.gen = ecdf(v.predict.gen[,a[which(!is.na(a))],]) # fit model at selected days (day.pos) and staions (a[which(!is.na(a))])
		#pos.unique.rain.day[n,uni,rlz.i]




		## 2) Generate seed to predict using v.normal.model.gen 
		# set seed for v that is random parameter for rain generation
		# generate seed.u.gen.predict by multiple random loop
		seed.u.gen.predict = array(NA, c(unique.rainy.day.n[uni,rlz.i],station.n))
		v.predict.gen.seed = array(NA, c(unique.rainy.day.n[uni,rlz.i],station.n))
		v.normal.gen = array(NA, c(station.n,unique.rainy.day.n[uni,rlz.i]))

		rownames(v.normal.gen) = rownames(weight.m)
		#cat("generating seed.u.gen.predict by multiple random loop\n")






		#####################################
		# set starting values for count limit exceed.v.pos
		count.min.exceed = 0
		count.loop = 0
		min.l.exceed = 999

		# Loop to check seed.u.gen in up.limit.v and low.limit.v
		repeat{
		for(random.i in 1:station.n){
							random.temp = runif(unique.rainy.day.n[uni,rlz.i],min(seed.u.gen.cal.model),max(seed.u.gen.cal.model))
							#random.temp = runif(unique.rainy.day.n[uni,rlz.i],min(v.predict.gen[day.pos,a[which(!is.na(a))],])+0.001,max(v.predict.gen[day.pos,a[which(!is.na(a))],])-0.001)
							seed.u.gen.predict[,random.i] = random.temp
							} # END random.i




		for(day.i in 1:unique.rainy.day.n[uni,rlz.i]){
									#-# Use daily ma.coef
									if(use.daily.ma.coef){
										if(pos.unique.rain.day[day.i,uni,rlz.i] < 1){v.predict.gen.seed[day.i,]= 0
												}else{v.predict.gen.seed[day.i,] = daily.ma.coef[month.i,pos.unique.rain.day[day.i,uni,rlz.i]]*(weight.m%*%seed.u.gen.predict[day.i,])+seed.u.gen.predict[day.i,]}
										}else{
										## Use monthly ma.coef
										v.predict.gen.seed[day.i,] = mean(daily.ma.coef[month.i,], na.rm=TRUE)*(weight.m%*%seed.u.gen.predict[day.i,])+seed.u.gen.predict[day.i,]
										}

									v.normal.gen[,day.i] = v.normal.model.gen(v.predict.gen.seed[day.i,])
									}



		if(unique.rainy.day.n[uni,rlz.i]>n.limit.v.1){count.limit = count.limit.2
								up.limit.v = up.limit.v.2
								low.limit.v = low.limit.v.2
								}else{
									count.limit = count.limit.1
									up.limit.v = up.limit.v.1
									low.limit.v = low.limit.v.1
									}



		# check rainy day > n.limit.v.1 ?
		#if(unique.rainy.day.n[uni,rlz.i] >= n.limit.v.1){ 
		#							random.v.mean = rowMeans(v.normal.gen)
		#							}else{
		#								#cat("select only used v-",sel.unique.station.n[uni,rlz.i])
		#								random.v.mean = rowMeans(v.normal.gen)[sel.unique.station[1:sel.unique.station.n[uni,rlz.i],uni,rlz.i]] # select only used v value(s)
		#								}
		random.v.mean = rowMeans(v.normal.gen)[sel.unique.station[1:sel.unique.station.n[uni,rlz.i],uni,rlz.i]] # select only used v value(s)
		up.count = length(which(as.real(random.v.mean) > up.limit.v))
		low.count = length(which(as.real(random.v.mean) < low.limit.v))



		sel.sta.v = sel.unique.station[1:sel.unique.station.n[uni,rlz.i],uni,rlz.i]
		# check extreme value
		new.mean.rain = as.numeric(mean.rain)*day.in.month/unique.rainy.day.n[uni,rlz.i]  # transform mean.rain fit to number of rainy day


		#new.mean.rain.sel = new.mean.rain[sel.unique.station[1:sel.unique.station.n[uni,rlz.i],uni,rlz.i]]
		#if(max(new.mean.rain[sel.unique.station[1:sel.unique.station.n[uni,rlz.i],uni,rlz.i]]) > limit.eq.multi.plx){limit.extreme = 1-exp(-beyond.eq.multi.plx)
		#if(max(new.mean.rain - ratio.change.at)> limit.eq.multi.plx){limit.extreme = 1-exp(-beyond.eq.multi.plx)
		#							limit.v.all = max.v.all2
		#							v.limit.new = which(limit.v.all > limit.extreme)
		#							limit.v.all[v.limit.new] = limit.extreme
		#							}else{if(max(new.mean.rain) != 0){limit.extreme = 1-exp(-predict(eq.multi.plx,poly(c(max(new.mean.rain[sel.sta]),1:9),pl.extreme.order,raw=TRUE))[1])
		#													}else{limit.extreme = 1}
		#								limit.v.all = max.v.all
		#								v.limit.new = which(limit.v.all > limit.extreme)
		#								limit.v.all[v.limit.new] = limit.extreme
		#								}

		limit.extreme = rep(1-exp(-beyond.eq.multi.plx),station.n)
		sel.sta = which((new.mean.rain-limit.eq.multi.plx)<0)
		limit.extreme[sel.sta] = 1-exp(-predict(eq.multi.plx.lim,poly(c((new.mean.rain),1:9),pl.extreme.order.lim,raw=TRUE)))[sel.sta]

		# Cut peak of equation curve at selected station (sel.sta.v.2)
		sel.cut.peak = which((limit.extreme[sel.sta.v.2] - max.v.all[sel.sta.v.2]) > 0)
		if(length(sel.cut.peak) > 0){limit.extreme[sel.sta.v.2][sel.cut.peak] = max.v.all[sel.sta.v.2][sel.cut.peak]}

		# limit v3
		if(new.mean.rain[sel.sta.v.3] > limit.eq.v.3){limit.extreme[sel.sta.v.3] = 1-exp(-beyond.eq.multi.plx)
									   }else{limit.extreme[intersect(sel.sta,sel.sta.v.3)] = 1-exp(-predict(eq.v.3,poly(c((new.mean.rain),1:9),pl.extreme.order.lim,raw=TRUE)))[intersect(sel.sta,sel.sta.v.3)]}

		# Cut peak of linear after equation at selected station (sel.sta.v.2)  - control extreme values not to be exceeded 
		sel.cut.peak.2 = which((new.mean.rain[sel.sta.v.2] - limit.eq.multi.plx2) > 0)
		if(length(sel.cut.peak.2) > 0){limit.extreme[sel.sta.v.2][sel.cut.peak.2] = rep(1-exp(-beyond.eq.multi.plx2),length(sel.cut.peak.2))}


		#limit.extreme = 0.99999 # test limit extrme


		# checck exceed position
		exceed.v.pos = which((v.normal.gen[sel.sta.v,] - limit.extreme[sel.sta.v]) > 0)
		if(length(exceed.v.pos) >= 1){
							if(length(exceed.v.pos) < (unique.rainy.day.n[uni,rlz.i]*length(sel.sta.v)/10)){
																cat("/**xtrm refill(",length(exceed.v.pos),") ", sep ="")
																#v.normal.gen[exceed.v.pos] = runif(length(exceed.v.pos),0.5,limit.extreme)
																limit.v.all.fill = array(limit.extreme,c(station.n,dim(v.normal.gen)[2]))

																# creat new random matrix and put back to v.normal.gen
																v.normal.gen[sel.sta.v,][exceed.v.pos] = runif(length(exceed.v.pos),min(v.normal.gen),limit.v.all.fill[sel.sta.v,][exceed.v.pos])

																}else{
																	cat("**over xtrm:",length(exceed.v.pos))
																	if(min.l.exceed == length(exceed.v.pos)){count.min.exceed = count.min.exceed +1}
																	min.l.exceed = min(length(exceed.v.pos),min.l.exceed)
																	up.count = 100+up.count

																	# if min.l.exceed meet lowest point for 3 times then refill v.normal.gen and go next step
																	if(count.min.exceed == 3){
																					up.count = up.count - 100
																					limit.v.all.fill = array(limit.extreme,c(station.n,dim(v.normal.gen)[2]))
																					v.normal.gen[sel.sta.v,][exceed.v.pos] = runif(length(exceed.v.pos),min(v.normal.gen),limit.v.all.fill[sel.sta.v,][exceed.v.pos])
																					}

																	# Reset counting parameters when reaching 200 loops
																	count.loop = count.loop + 1
																	if(count.loop > 200){
																					count.loop = 0
																					min.l.exceed = 999
																					cat("\n-reset counting\n")
																					}

																	}
							}
		# end check extreme value
		
		if((up.count+low.count) <= count.limit){break()
						    }else{cat("/**u:",up.count,"d:",low.count, sep ="")}

		}
		#####################################	
		# END Loop to check seed.u.gen in up.limit.v and low.limit.v
		# Result to v.normal.gen







	#### Give the distribution of v(n,1) in v.normal.gen[station.n ,unique.rainy.day.n[uni,rlz.i] ) for station x rainy day in this realization
	################################
	# END Set v(n,1) for prediction
	################################

	#cat(" at day:")
	# Start unique rainy day
	for(n in 1:unique.rainy.day.n[uni,rlz.i]){
	#cat("\nGenerating rain day :",n)
	#for(seed.n in 1:weather.gen.predict.realiz.n){

									##
									#v.normal.gen[,n] = v.normal.model.gen(v.predict.gen.seed[pos.unique.rain.day[n,uni,rlz.i],,rlz.i])

									# generate new V, if v = 1, to prevent Inf value
									runthissub = TRUE
									if(runthissub){
											  while(max(v.normal.gen[,n], na.rm=TRUE) == 1){
													cat("\n+***+ regenerate V (prediction)\n")
													#re.seed.u.m = array(runif(unique.rainy.day.n[uni,rlz.i] ,min(seed.u.gen.cal.model),max(seed.u.gen.cal.model)), c(station.n,1))
									
													### Use daily ma.coef
													#if(use.daily.ma.coef){
													#	v.re.predict = daily.ma.coef[month.i,pos.unique.rain.day[n,uni,rlz.i]]*(weight.m%*%re.seed.u.m)+re.seed.u.m
													#	}else{
													#	## Use monthly ma.coef
													#	v.re.predict = mean(daily.ma.coef[month.i,], na.rm=TRUE)*(weight.m%*%re.seed.u.m)+re.seed.u.m
													#	}
													#v.normal.gen[,n] = v.normal.model.gen(v.re.predict)

													#v.normal.gen[,n] = v.normal.model.gen(runif(station.n, min(v.predict.gen.seed)+0.001, max(v.predict.gen.seed)+0.001))
													
													## replace value 1 from v.normal.gen with new random
													v.normal.gen[which(v.normal.gen[,n]>=0.99999),n] = runif(length(which(v.normal.gen[,n]>=0.99999)),0.5,1)
													}
											  }
									
									##
									# calculate rainfall amount at station n
									#rain.predict.gen[month.i,,pos.unique.rain.day[n,uni,rlz.i],rlz.i] = -log(1-v.normal.gen[,n])*array(as.numeric(mean.rain),c(station.n,1))
									new.mean.rain = as.numeric(mean.rain)*day.in.month/unique.rainy.day.n[uni,rlz.i]  # transform mean.rain fit to number of rainy day
									rain.predict.gen[year.i,month.i,sel.unique.station[1:sel.unique.station.n[uni,rlz.i],uni,rlz.i],pos.unique.rain.day[n,uni,rlz.i],rlz.i] = (-log(1-v.normal.gen[,n])*array(new.mean.rain,c(station.n,1)))[sel.unique.station[1:sel.unique.station.n[uni,rlz.i],uni,rlz.i]]
									#} # END seed.n
	cat(" /",pos.unique.rain.day[n,uni,rlz.i])
	} # END n
	###

	###
	# END if when zero rain // rainy.day.n[uni,rlz.i] = 0
	}



		
}
###
# END Run each unique number of rainy day


# show mean values of V for each station 
if(show.v.chart){
	if(unique.rainy.day.n[uni,rlz.i] != 0){
			dev.set(3)
			if(dim(v.normal.gen)[2] == 1){plot(v.normal.gen[,], type ="h",ylim=c(0,1), main = paste("Relization:",rlz.i,month.i,"/",future.all.year[year.i]), lwd = 8, col = "red")
							    }else{plot(rowMeans(v.normal.gen[,],na.rm = TRUE), type ="h",ylim=c(0,1), main = paste("Date:",month.i,"/",future.all.year[year.i]), lwd = 8, col = "red")}
			if(dim(v.predict.gen.seed)[1] == 1){lines(as.real(v.predict.gen.seed), type ="h",ylim=c(0,1.2), main = paste("V(n,1) of",month.i,"/",future.all.year[year.i]), lwd = 4, col = "blue")
									}else{lines(colMeans(v.predict.gen.seed[,],na.rm = TRUE), type ="h",ylim=c(0,1.2), main = paste("V(n,1) of",month.i,"/",future.all.year[year.i]), lwd = 4, col = "blue")}
			lines(rep(0.5,station.n), col = "green")
			legend("topright", cex = 0.8, y.intersp = 1.15,legend = c("v.normal.gen-rainfall prediction","v.predict.gen.seed-v prediction"), col=c("red","blue"), lwd =c(8,4), bg = "white")
			dev.set(dev.prev())
	}
}
# END show mean values of V for each station 


}

###
# END Run each realization
###











# set daily rain of other non-rainy days to 0
for(i in 1:station.n){
				rain.predict.gen.avg[year.i,month.i,i,which(is.na(rain.predict.gen.avg[year.i,month.i,i,]))] = 0
				for(rlz.i in 1:weather.gen.predict.realiz.n){rain.predict.gen[year.i,month.i,i,which(is.na(rain.predict.gen[year.i,month.i,i,,rlz.i])),rlz.i] = 0	}
				}


# Average the realization from rainy day (pos.unique.rain.day[n,uni]) and rainy station (sel.unique.station[1:sel.unique.station.n[uni],uni]) from rain.predict.gen.avg
cat("\nAveraging the",weather.gen.predict.realiz.n,"realization")
for(station.i in 1:station.n){
for(day.i in 1:day.in.month){
					rain.predict.gen.avg[year.i,month.i,station.i,day.i]= mean(rain.predict.gen[year.i,month.i,station.i,day.i,],na.rm = TRUE)
					}
					}





###
### END Generate rainfall from cummulative distribution model (v.normal.model.gen)
# Results are in rain.predict.gen







#########################################
# record daily future file, put results into rain.predict.conclude
# rotate matrix rain.predict.gen.avt and put into rain.predict.conclude
# ** Combine rain.predict.gen.avg INTO rain.predict.conclude
if((month.i+year.i) == 2){
				rain.predict.conclude = array(NA, c(366*length(future.all.year),station.n))
				rain.predict.conclude.allrlz = array(NA, c(366*length(future.all.year),station.n,weather.gen.predict.realiz.n))
				colnames(rain.predict.conclude) = station.title
				start.org.date = as.integer(mdy.date(1, 1, future.all.year[1]))
				}

start.data.date = as.integer(mdy.date(month.i, 1, future.all.year[year.i]))
end.data.date = as.integer(mdy.date(month.i, day.in.month, future.all.year[year.i]))

if(length(which(!is.na(rain.predict.conclude[(start.data.date-start.org.date +1):(end.data.date-start.org.date + 1),])))>0){error.count =error.count+1}


##
# put time-seires into rain.predict.conclude
##
rain.predict.conclude[(start.data.date-start.org.date +1):(end.data.date-start.org.date + 1),] = matrix(rain.predict.gen.avg[year.i,month.i,,1:day.in.month], ncol = station.n, nrow = day.in.month, byrow = TRUE)
for(rlz.i in 1:weather.gen.predict.realiz.n){rain.predict.conclude.allrlz[(start.data.date-start.org.date +1):(end.data.date-start.org.date + 1),,rlz.i] = matrix(rain.predict.gen[year.i,month.i,,1:day.in.month,rlz.i], ncol = station.n, nrow = day.in.month, byrow = TRUE)}



#print.rain.predict.conclude = rain.predict.conclude[-which(is.na(rain.predict.conclude[,1])),]
setwd(dir1)
write.csv(rain.predict.conclude, file = "conclude average rain.csv", row.names = FALSE)
write.csv(rainy.in.month, file = "conclude rainy in month.csv", row.names = FALSE)
setwd(dir0)





# Loop to plot to pdf
if(plot.daily.rainfall){
cat("\n* Plot Rainfall each station in month:",month.i,":")
for(station.i in 1:station.n){

# define maximum rain.predict to set y-axis
if(max(rain.predict.gen.avg[year.i,month.i,station.i,]) == 0){	y.max =1
						}else{y.max = max(rain.predict.gen.avg[year.i,month.i,station.i,])}

plot(colMeans(rain.predict.gen.avg[year.i,month.i,,]), type = "h",
	xlab = paste("Stations / Station=",station.i,"day(s) at avg. rainfall =",round(mean(mean.rain),digits=1),"mm.(chart avg.=",round(mean(rain.predict.gen.avg[year.i,month.i,station.i,],na.rm = TRUE),digits=1),"mm.) of month:",month.i," year:",future.all.year[year.i]),
	ylab = "Rainfall (mm)", lty = 1, lwd = 5, ylim=c(0,y.max))
for(k in 1:station.n){
				lines(rain.predict.gen.avg[year.i,month.i,k,], type = "p", col = k)
				}

legend(x = "right", cex = 0.8, y.intersp = 1.15, bty = "n",  lty = c(rep(1,station.n)),  lwd = rep(2,station.n), col = c(1:station.n),  legend = rownames(weight.m))

cat(station.title[station.i],"-plotted/",sep="")
}
#END Loop to plot to pdf
}



							


#######
# Generate simulated daily Moran's
cat("\n- Generate simulated daily Moran's I\n")
for(day.i in 1:day.in.month){
			daily.moran.i.sim[month.i,day.i] = moran.i(rain.predict.gen.avg[year.i,month.i,,day.i],weight.m)
			}



}
###################################
# END Great loop to chang year   ##
###################################


}
dev.off()
##################
# END change the amount of mean monhtly rainfall
# END Great loop to chang month   ####################
##################################################
cat("\n***\nEnd of monthly calculation***\n\nPlot and Conclusion\n")







#######################################
# Run the below to conclude the results
#######################################
# Condclude all realization into conclude.allrlz.rearng

future.n = mdy.date(future.month.list[length(future.month.list)],1,future.year.list[length(future.year.list)])+31-mdy.date(1,1,future.year.list[1])
#future.n = length(which(!is.na(rain.predict.conclude.allrlz[,1,1])))
#future.n = dim(rain.predict.conclude.allrlz)[1]
future.t = date.mdy(mdy.date(1,1,future.year.list[1]):(mdy.date(1,1,future.year.list[1])+future.n))

loop.run = station.n # separate into 6 parts to reduce size of file
#conclude.allrlz.rearng0 = cbind(future.t$year,future.t$month,future.t$day) # put month into coloumn


############################# sep conclude rlz set col.run
for(col.run in 1:loop.run){

conclude.allrlz.rearng = cbind(future.t$year,future.t$month,future.t$day) # put month into coloumn

col.n = dim(conclude.allrlz.rearng)[2] # existing column in conclude.allrlz.rearng

#rearrange rain.predict.conclude.allrlz
for(station.i in (1+(col.run-1)*station.n/loop.run):((col.run)*station.n/loop.run)){
for(rlz.i in 1:weather.gen.predict.realiz.n){

								conclude.allrlz.rearng = cbind(conclude.allrlz.rearng,rain.predict.conclude.allrlz[1:(future.n),station.i,rlz.i])
								col.n = col.n + 1
								colnames(conclude.allrlz.rearng)[col.n] = paste(station.title[station.i],rlz.i,sep = "-")
								cat("\n",col.run,")Recorded col:",col.n)
								}
								}
colnames(conclude.allrlz.rearng)[1:3] = c("year","month","day")
# print each station results
cat("\n ",col.run,")Print into: conclude.allrlz.rearng ",col.run)
setwd(dir1)
write.csv(conclude.allrlz.rearng, file = paste("conclude rain all-rlz sta ",col.run,".csv", sep =""), row.names = FALSE)
setwd(dir0)

cat("\n ",col.run,")Merging into: conclude.allrlz.rearng0")
#conclude.allrlz.rearng0 = cbind(conclude.allrlz.rearng0, conclude.allrlz.rearng[,-c(1:3)])

}

# Print conclusion file
#setwd(dir1)
#write.csv(conclude.allrlz.rearng0, file = "conclude rain all-rlz all-sta.csv",row.names=FALSE)
#setwd(dir0)



# Expand memory limit to maximum
memory.limit(4000)

#****!!!!****#
# run when above procedures cannot record data because of exceeding memory limit
run.this.loop = TRUE
if(run.this.loop){

# Re-run if above script false due to memmory limit and close
memory.limit(4000)
future.n = mdy.date(future.month.list[length(future.month.list)],1,future.year.list[length(future.year.list)])+31-mdy.date(1,1,future.year.list[1])

#****!!!!****#
# Run to read conclusion file
## Record all-realization all-station in one file
setwd(dir1)
cat("\n- Reading: conclude rain all rlz..")
future.t = date.mdy(mdy.date(1,1,future.year.list[1]):(mdy.date(1,1,future.year.list[1])+future.n))
org.f = cbind(future.t$year,future.t$month,future.t$day)
for(i in 1:station.n){
data.f = paste("conclude rain all-rlz sta ",i,".csv",sep="")
data = read.table(data.f, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")[,-c(1:3)]
colnames(data) = paste(station.title[i],c(1:30),sep="-")
cat("\nRead Data:",data.f)
org.f = cbind(org.f,data)
}




colnames(org.f)[1:3] = c("year","month","day")
cat("\nWriting: conclude rain all rlz 0.csv")
write.csv(org.f, file = "conclude rain all-rlz all-sta.csv",row.names = FALSE)
setwd(dir0)

## Record each realization of all-station in one file
setwd(dir1)
for(i in 1:weather.gen.predict.realiz.n){
cat("\nWriting: conclude rain rlz :",i)
write.csv(org.f[,c(1:3,3+(c(1:station.n)-1)*weather.gen.predict.realiz.n+i)], file = paste("conclude rain all-sta rlz",i,".csv") ,row.names = FALSE)
}
setwd(dir0)

}
# END run when above procedures cannot record data because of exceeding memory limit

## END make the conclusion file
#****!!!!****#









##############################
### Plot acc. charts and files
##############################


###############################################################
##################### PLOT MORAN's I ########################
###############################################################

## Calculate Moran's I of prediction
# Generate simulated daily Moran's (from all direct rain realization)
cat("\nCalculate daily Moran's I")
moran.i.all.rlz = array(NA, c(year.n,12,31,weather.gen.predict.realiz.n))
#moran.i.all.day = array(NA, c(year.n,12,31))
for(month.i in c(1:12)){
	day.in.month = max(day.in.month.list[which(day.in.month.list[,2]==month.i),5])
	for(day.i in 1:day.in.month){
	for(year.i in 1:year.n){
		for(rlz.i in 1:weather.gen.predict.realiz.n){
					#if(length(which(is.na(rain.predict.gen[year.i,month.i,,day.i,rlz.i])))==0){
					row.i = mdy.date(month.i,day.i,future.all.year[year.i]) - mdy.date(1,1,future.year.list[1]) + 1
					if(length(which(is.na(org.f[row.i,(0:(station.n-1))*weather.gen.predict.realiz.n+rlz.i+3])))==0){
																#moran.i.all.rlz[year.i,month.i,day.i,rlz.i] = moran.i(rain.predict.gen[year.i,month.i,,day.i,rlz.i],weight.m)
																moran.i.all.rlz[year.i,month.i,day.i,rlz.i] = moran.i(as.real(org.f[row.i,(0:(station.n-1))*weather.gen.predict.realiz.n+rlz.i+3]),weight.m)
																}
					}
	cat("\nRecord Moran's I row:",row.i," -- day:",day.i," month:",month.i," year:",future.all.year[year.i])
	#moran.i.all.day[year.i,month.i,day.i] = mean(moran.i.all.rlz[year.i,month.i,day.i,], na.rm = TRUE)
	}
	}
}



##################
## Plot Moran's I

## Plot Moran's I OBS
setwd(dir1)
pdf(paste("Conclude I obs.pdf"), width = 10 ,height = 6)
#par(mgp=c(2.5,1,0), ps =18)
par(mgp=c(2.5,0.8,0), ps =18)
par(mai = c(0.8, 0.8, 0.8, 0.3))
setwd(dir0)
plot(1:(12*31),rep(0,12*31),type = "n", xlab="Days",ylab="daily Moran's I", ylim=c(-0.5,1))


for(i in 1:12){
				plotrawdata = TRUE
				if(plotrawdata){
				for(year.i in 1:year.n){
							lines(c((1+((i-1)*31)):(31+((i-1)*31))),moran.i.daily.all[year.i,i,],type = "p", lwd = 1, cex = 0.3, pch = 19,col = "grey")
							}
				}

				# Plot trend OBS average year
				lines(c((1+((i-1)*31)):(31+((i-1)*31))),daily.avg.moran.i[i,], lwd = 3 ,col = "red", lty = 1)

				}
lines(c(-100,500),c(0,0), col = "black", lwd = 2, lty = 2)
legend("topright", cex = 0.8, y.intersp = 1.15, bg = "white", pch = c(19,-1), lty = c(0,1),  lwd = c(0,3), pt.cex = c(0.5,0), col = c("blue","red"),  legend = c("daily Moran's I",paste("average ",year.n,"-year daily Moran's I",sep ="")))
dev.off()


## Plot Moran's I SIM
moran.i.rearrange = array(NA, c(12*31,4))
colnames(moran.i.rearrange) = c("month","day","obs","sim")

setwd(dir1)
pdf(paste("Conclude I sim.pdf"), width = 10 ,height = 6)
par(mgp=c(2.5,0.8,0), ps =18)
par(mai = c(0.8, 0.8, 0.8, 0.3))
#par(mgp=c(2.5,1,0), ps =18)

setwd(dir0)

#########################
### Plot without showing realization
cat("\nPlot obs. and sim. daily Moran's I without showing realization")
plot(1:(12*31),rep(0,12*31),type = "n", xlab="Days",ylab="daily Moran's I", ylim=c(-0.3,0.3))


for(i in 1:12){
				##########
				# Plot DOT
				##########

				plotrawdata = FALSE
				if(plotrawdata){
				for(year.i in 1:year.n){
							lines(c((1+((i-1)*31)):(31+((i-1)*31))),moran.i.daily.all[year.i,i,],type = "p", lwd = 1, cex = 0.3, pch = 4)
							}
				}


				# Plot SIM average year every rlz
				moran.rlz.i = integer(0)
				for(rlz.i in 1:weather.gen.predict.realiz.n){
							moran.rlz.i = rbind(moran.rlz.i, colMeans(moran.i.all.rlz[,i,,rlz.i], na.rm = TRUE))
							lines(c((1+((i-1)*31)):(31+((i-1)*31))), moran.rlz.i[rlz.i,],  cex = 0.3 ,col = "grey", type = "p", pch = 19)
							}

				# Plot SIM average rlz every year
				moran.year.i = integer(0)
				for(year.i in 1:year.n){
							moran.year.i = rbind(moran.year.i, rowMeans(moran.i.all.rlz[year.i,i,,], na.rm = TRUE))
							#lines(c((1+((i-1)*31)):(31+((i-1)*31))), moran.year.i[year.i,],  cex = 0.3 ,col = "grey", type = "p", pch = 19)
							}


				##########
				# Plot TREND
				##########

				# Plot trend OBS average year
				lines(c((1+((i-1)*31)):(31+((i-1)*31))),daily.avg.moran.i[i,], lwd = 2,col = "red", lty = 2)

				## Plot average line Moran's I
				# Plot trend SIM average rlz of average year (point = rlz.n)
				avg.moran.sim = colMeans(moran.rlz.i, na.rm = TRUE)
				lines(c((1+((i-1)*31)):(31+((i-1)*31))), avg.moran.sim, lwd = 3 ,col = "blue", lty = 1)

				# Plot RLZ.I = sample.i sample trend SIM average rlz of average year
				lines(c((1+((i-1)*31)):(31+((i-1)*31))), moran.rlz.i[i,], lwd = 1 ,col = "black", lty = 1)

				}
grid(NA, 2, col = "black", lwd = 1, lty = 2)
legend("topright", y.intersp = 1.15, cex = 0.8, bg = "white", pch = c(-1,-1,19), lty = c(2,1,0),  lwd = c(2,3,0), pt.cex = c(0,0,0.5), col = c("red","blue","grey"),  legend = c("Observed Moran's I","Average simulated Moran's I",paste("Simulated Moran's I (",weather.gen.predict.realiz.n,"realizations )")))


#########################
### Plot each realization
for(sample.i in 1:weather.gen.predict.realiz.n){
cat("\nPlot obs. and sim. daily Moran's I")
plot(1:(12*31),rep(0,12*31),type = "n", xlab="Days",ylab="daily Moran's I", ylim=c(-0.3,0.3))


for(i in 1:12){
				##########
				# Plot DOT
				##########

				plotrawdata = FALSE
				if(plotrawdata){
				for(year.i in 1:year.n){
							lines(c((1+((i-1)*31)):(31+((i-1)*31))),moran.i.daily.all[year.i,i,],type = "p", lwd = 1, cex = 0.5)
							}
				}


				# Plot SIM average year every rlz
				moran.rlz.i = integer(0)
				for(rlz.i in 1:weather.gen.predict.realiz.n){
							moran.rlz.i = rbind(moran.rlz.i, colMeans(moran.i.all.rlz[,i,,rlz.i], na.rm = TRUE))
							lines(c((1+((i-1)*31)):(31+((i-1)*31))), moran.rlz.i[rlz.i,],  cex = 0.3 ,col = "grey", type = "p", pch = 19)
							}

				# Plot SIM average rlz every year
				moran.year.i = integer(0)
				for(year.i in 1:year.n){
							moran.year.i = rbind(moran.year.i, rowMeans(moran.i.all.rlz[year.i,i,,], na.rm = TRUE))
							#lines(c((1+((i-1)*31)):(31+((i-1)*31))), moran.year.i[year.i,],  cex = 0.3 ,col = "brown", type = "p", pch = 19)
							}


				##########
				# Plot TREND
				##########

				# Plot trend OBS average year
				lines(c((1+((i-1)*31)):(31+((i-1)*31))),daily.avg.moran.i[i,], lwd = 2,col = "red", lty = 2)

				## Plot average line Moran's I
				# Plot trend SIM average rlz of average year (point = rlz.n)
				avg.moran.sim = colMeans(moran.rlz.i, na.rm = TRUE)
				lines(c((1+((i-1)*31)):(31+((i-1)*31))), avg.moran.sim, lwd = 3 ,col = "blue", lty = 1)


				# Plot RLZ.I = sample.i sample trend SIM average rlz of average year
				lines(c((1+((i-1)*31)):(31+((i-1)*31))), moran.rlz.i[sample.i,], lwd = 1 ,col = "black", lty = 1)

				# Plot trend SIM average avg-rlz year (point = year.n)
				#avg.moran.sim.2 = colMeans(moran.year.i, na.rm = TRUE)
				#lines(c((1+((i-1)*31)):(31+((i-1)*31))), avg.moran.sim.2, lwd = 3 ,col = "brown", lty = 1)



				moran.i.rearrange[((i-1)*31+1):(i*31),1] = i
				moran.i.rearrange[((i-1)*31+1):(i*31),2] = c(1:31)
				moran.i.rearrange[((i-1)*31+1):(i*31),3] = daily.avg.moran.i[i,]
				moran.i.rearrange[((i-1)*31+1):(i*31),4] = avg.moran.sim

				}
grid(NA, 2, col = "black", lwd = 1, lty = 2)
legend("topright", y.intersp = 1.15, cex = 0.8, bg = "white", pch = c(-1,-1,-1,19), lty = c(2,1,1,0),  lwd = c(2,3,1,0), pt.cex = c(0,0,0,0.5), col = c("red","blue","black","grey"),  legend = c("Observed Moran's I","Average simulated Moran's I","Simulated Moran's I of a realization",paste("Simulated Moran's I (",weather.gen.predict.realiz.n,"realizations )")))
}
# END Plot SIM Moran's I
dev.off()


setwd(dir1)
write.csv(moran.i.rearrange, file = paste("conclude I.csv") ,row.names = FALSE)
setwd(dir0)

# Combine daily data
total.daily.ma.coef = integer(0)
total.daily.avg.moran.i = integer(0)
for(i in 1:12){
total.daily.ma.coef = c(total.daily.ma.coef,daily.ma.coef[i,])
total.daily.avg.moran.i = c(total.daily.avg.moran.i,daily.avg.moran.i[i,])
}

## Plot selected moving average coef. whole year
setwd(dir1)
pdf(paste("Conclude I and ma-coef.pdf"), width = 10 ,height = 6)
#par(mgp=c(2.5,1,0), ps =16)
par(mgp=c(2.5,0.8,0), ps =18)
par(mai = c(0.8, 0.8, 0.8, 0.3))
setwd(dir0)
par(mar=c(5,4,4,5)+.1)
plot(total.daily.ma.coef, type = "l", lwd = 3 ,col = "red", lty = 1, ylab = "Moving average coef.",xlab = "Days", main = paste("selected moving average coef., month:",month.i))
par(new = TRUE)
plot(total.daily.avg.moran.i, type = "l", yaxt = "n" , lwd = 2 , col = "blue", lty = 2, ylab ="",ylim = c(-1,0.3), xlab ="")
axis(4)
legend("topright", y.intersp = 1.15,  cex = 0.8, lty = c(1,2),  lwd = c(3,2), col = c("red","blue"),  legend = c("Selected moving average coef. (left  y-axis)","Observed Moran's I (right y-axis)"))
mtext("Moran's I", side=4, line = 3, col = "blue")

dev.off()



###############################################################
##################### PLOT RAIN ########################
###############################################################

###########################
## Plot Rainfall prediction
setwd(dir1)
pdf(paste("Conclude Rain Generation.pdf"), , width = 10 ,height = 6)
#par(mgp=c(2.5,1,0), ps =16)
par(mgp=c(2.5,0.8,0), ps =18)
par(mai = c(0.8, 0.8, 0.8, 0.3))
setwd(dir0)

cat("\nPlot daily rainfall prediction")
plot(1:(12*31),rep(0,12*31),type = "n",ylim = c(0,20), ylab=paste("Average",station.n,"-stations daily rainfall (mm/day)", sep=""), xlab ="Days")

for(i in 1:12){
			#lines(c((1+((i-1)*31)):(31+((i-1)*31))),colMeans(rain.predict.gen.avg[i,,]), lwd = 2, type ="h")
			for(n in 1:weather.gen.predict.realiz.n){
							#lines(c((1+((i-1)*31)):(31+((i-1)*31))),colMeans(rain.predict.gen[1,i,,,n],na.rm=TRUE),col = "blue",type = "p", lwd = 1, cex = 0.4)
							lines(c((1+((i-1)*31)):(31+((i-1)*31))),colMeans(colMeans(rain.predict.gen[,i,,,n],na.rm = TRUE),na.rm=TRUE),col = "blue",type = "p", lwd = 1, cex = 0.4)
							}
			#lines(c((1+((i-1)*31)):(31+((i-1)*31))),colMeans(rain.predict.gen.avg[1,i,,]), lwd = 2, type ="h")
			lines(c((1+((i-1)*31)):(31+((i-1)*31))),colMeans(colMeans(rain.predict.gen.avg[,i,,],na.rm = TRUE),na.rm=TRUE), lwd = 2, type ="h")
			}
legend("topright", cex = 0.8, y.intersp = 1.15,legend = c("average daily rainfall",paste("simulated rainfall (",weather.gen.predict.realiz.n,"realizations)")), col=c("black","blue"), lwd =c(3,0.5), lty=c(0,-1),pch = c(124,20),bg = "white")
dev.off()



############################################
## Plot all Rainfall DAILY prediction and observation
setwd(dir1)
#pdf(paste("Conclude Rain Gen+Obs in 1 chart.pdf"), width = 10 ,height = 6)
png(paste("daily-Conclude Rain Gen+Obs all sta.png"), width = 1500 ,height = 750)
#tiff(paste("Conclude Rain Gen+Obs.tif"), width = 1500 ,height = 750)
par(mgp=c(3.3,1.2,0), ps =30)
par(mai = c(1.2, 1.5, 1, 1)) 
setwd(dir0)

cat("\nPlot daily rainfall prediction+obs. time-series")
#z = ts(rep(0,length(daily.day.list)),start = c(daily.year.list[1], 1), frequency = 372)
z = ts(rep(0,length(daily.day.list)),start = c(daily.year.list[1], 1), frequency = 365.25)
#y.lim = max(org.f, na.rm = TRUE)
y.lim = 300
plot.ts(z, type = "n",ylim = c(0,y.lim), ylab=paste("daily rainfall (mm/day)", sep=""), xlab ="Days")


# Plot observation
for(i in 1:12){
			row.sel = which(daily.month.list == i)
			for(j in 1:max(daily.day.list[row.sel])){
										# Plot obs
										row.sel.2 = which(daily.day.list[row.sel] == j)
										ts.show = rep(time(z)[row.sel[row.sel.2]], station.n)
										lines(ts.show,as.real(unlist(data.daily[row.sel[row.sel.2],])),type="h", col = "blue")
										# Plot predict
										rain.plot = rain.predict.conclude.allrlz[row.sel[row.sel.2],,]
										#if(max(rain.plot, na.rm=TRUE)>200){cat(i,",",j,",",k)}
										for(k in 1:weather.gen.predict.realiz.n){
																	lines(ts.show,as.real(unlist(rain.plot[,,k])),col = "red",type = "p", lwd = 1, cex = 0.3)
																	}
										}

			}
# Plot prediction
#legend("topright", cex = 0.8, y.intersp = 1.15, daily.year.list[1]-0.5,300, y.intersp = 2,legend = c("observed daily rainfall",paste("simulated rainfall (",weather.gen.predict.realiz.n," realizations)",sep="")), col=c("blue","red"), lwd =c(3,0.5), lty=c(0,-1),pch = c(124,19),bg = "white")
legend("topright", cex = 0.8, y.intersp = 2,legend = c("observed daily rainfall",paste("simulated rainfall (",weather.gen.predict.realiz.n," realizations)",sep="")), col=c("blue","red"), lwd =c(3,0.5), lty=c(0,-1),pch = c(124,19),bg = "white")


dev.off()



############################################
## Plot EXTREME DAILY rainfall prediction and observation
setwd(dir1)
#pdf(paste("Conclude extreme Rain Gen+Obs in 1 chart.pdf"), width = 10 ,height = 6)
png(paste("daily-Conclude extreme Rain Gen+Obs all sta.png"), width = 1500 ,height = 750)
par(mgp=c(3.3,1.2,0), ps =30)
par(mai = c(1.2, 1.5, 1, 1)) 
setwd(dir0)

cat("\nPlot daily rainfall prediction+obs. time-series")
#z = ts(rep(0,length(daily.day.list)),start = c(daily.year.list[1], 1), frequency = 372)
z = ts(rep(0,length(daily.day.list)),start = c(daily.year.list[1], 1), frequency = 366)


#y.lim = max(org.f, na.rm = TRUE)
y.lim = 300
plot.ts(z, type = "n",ylim = c(0,y.lim), ylab=paste("daily rainfall (mm/day)", sep=""), xlab ="Days")


# Plot observation
for(i in 1:12){
			row.sel = which(daily.month.list == i)
			for(j in 1:max(daily.day.list[row.sel])){
										row.sel.2 = which(daily.day.list[row.sel] == j)
										ts.show = rep(time(z)[row.sel[row.sel.2]], station.n)
										rain.plot = rain.predict.conclude.allrlz[row.sel[row.sel.2],,]
										for(row.n in 1:length(row.sel.2)){
																# Plot obs
																lines(ts.show[row.n], max(unlist(data.daily[row.sel[row.sel.2[row.n]],])),type="h", col = "blue")
																max.temp = integer(0)
																for(k in 1:station.n){
																		# Plot predict
																		max.temp = c(max.temp,max(rain.plot[row.n,k,]))
																		}
																lines(ts.show[row.n],max(max.temp),col = "red",type = "p", lwd = 1, cex = 0.5, pch = 19)
																}
										cat("\nPlot month:",i," day:",j)
										}
			}
# Plot prediction
legend("topright", cex = 0.8, y.intersp = 2,legend = c("observed daily rainfall",paste("simulated extreme rainfall (",weather.gen.predict.realiz.n," realizations)",sep="")), col=c("blue","red"), lwd =c(3,0.5), lty=c(0,-1),pch = c(124,19),bg = "white")

dev.off()




############################################
## Plot AVERAGE MONTHLY rainfall prediction and observation
setwd(dir1)
pdf(paste("monthly-Conclude average Rain Gen+Obs in 1 chart.pdf"), width = 10 ,height = 6)
#png(paste("Conclude average monthly Rain Gen+Obs.png"), width = 1500 ,height = 750)
#par(mgp=c(2,0.8,0), ps =18)
#par(mgp=c(3.3,1.2,0), ps =30)
par(mgp=c(2.5,0.8,0), ps =18)
par(mai = c(0.8, 0.8, 0.8, 0.3))

#par(mai = c(1.2, 1.5, 1, 1)) 
setwd(dir0)

cat("\nPlot AVERAGE daily rainfall prediction+obs. time-series")
#z = ts(rep(0,length(daily.day.list)),start = c(daily.year.list[1], 1), frequency = 372)
z = ts(rep(0,length(future.month.list)),start = c(daily.year.list[1], 1), frequency = 12)


#y.lim = max(org.f, na.rm = TRUE)
y.lim = 35
plot.ts(z, type = "n",ylim = c(0,y.lim), ylab=paste("monthly rainfall (mm/day)", sep=""), xlab ="Year")


# Plot observation
for(i in 1:12){
			row.sel = which(daily.month.list == i)
			year.row.sel = which(future.month.list == i)

			year.temp = future.year.list[which(future.month.list == i)]
			for(year.i in 1:length(year.temp)){
									row.sel.2 = which(daily.year.list[row.sel] == year.temp[year.i])
									year.row.sel.2 = which(future.year.list[year.row.sel] == year.temp[year.i])
									rain.sel = rain.predict.conclude.allrlz[row.sel[row.sel.2],,]
									rain.plot = rowMeans(colMeans(rain.sel,na.rm = TRUE),na.rm = TRUE)

									ts.show = rep(time(z)[year.row.sel[year.row.sel.2]], station.n)
									# Plot obs
									lines(ts.show, colMeans(data.daily[row.sel[row.sel.2],], na.rm = TRUE),type="h", col = "blue", lwd = 1)
									# Plot predict
									lines(ts.show,rain.plot,col = "red",type = "p", lwd = 1, cex = 0.5, pch = 19)

									cat("\nPlot month:",i," year:",year.temp[year.i])
									}
			}
# Plot prediction
legend("topright", cex = 0.8, y.intersp = 2, legend = c("observed rainfall",paste("simulated average rainfall (",weather.gen.predict.realiz.n," realizations)",sep="")), col=c("blue","red"), lwd =c(3,0.8), lty=c(0,-1),pch = c(124,19),bg = "white")
#legend(daily.year.list[1]-0.5, 33, y.intersp = 2,legend = c("observed rainfall",paste("simulated average rainfall (",weather.gen.predict.realiz.n," realizations)",sep="")), col=c("blue","red"), lwd =c(3,0.8), lty=c(0,-1),pch = c(124,19),bg = "white")

dev.off()



############################################
## Plot AVERAGE EXTREME rainfall prediction and observation
setwd(dir1)
pdf(paste("monthly-Conclude avg Extreme Rain Gen+Obs in 1 chart.pdf"), width = 10 ,height = 6)
#par(mgp=c(2,0.8,0), ps =18)
par(mgp=c(2.5,0.8,0), ps =18)
par(mai = c(0.8, 0.8, 0.8, 0.3))

#par(mai = c(1.2, 1.5, 1, 1)) 
setwd(dir0)

cat("\nPlot AVERAGE daily rainfall prediction+obs. time-series")
#z = ts(rep(0,length(daily.day.list)),start = c(daily.year.list[1], 1), frequency = 372)
z = ts(rep(0,length(future.month.list)),start = c(daily.year.list[1], 1), frequency = 12)


#y.lim = max(org.f, na.rm = TRUE)
y.lim = 350
plot.ts(z, type = "n",ylim = c(0,y.lim), ylab=paste("monthly rainfall (mm/day)", sep=""), xlab ="Year")


# Plot observation
for(i in 1:12){
			row.sel = which(daily.month.list == i)
			year.row.sel = which(future.month.list == i)

			year.temp = future.year.list[which(future.month.list == i)]
			for(year.i in 1:length(year.temp)){
									row.sel.2 = which(daily.year.list[row.sel] == year.temp[year.i])
									year.row.sel.2 = which(future.year.list[year.row.sel] == year.temp[year.i])
									rain.sel = rain.predict.conclude.allrlz[row.sel[row.sel.2],,]

									ts.show = time(z)[year.row.sel[year.row.sel.2]]
									for(station.i in 1:station.n){
										# Plot obs
										lines(ts.show, max(data.daily[row.sel[row.sel.2],station.i], na.rm = TRUE),type="h", col = "blue", lwd = 1)
										# Plot predict
										rain.plot = integer(0)
										for(rlz.i in 1:weather.gen.predict.realiz.n){
												rain.plot = c(rain.plot, max(rain.sel[,station.i,rlz.i],na.rm = TRUE))
												}
										lines(ts.show, mean(rain.plot), col = "red",type = "p", lwd = 1, cex = 0.5, pch = 19)

										cat("\nPlot station:",station.i,"month:",i," year:",year.temp[year.i])
										}
									}
			}
# Plot prediction
legend("topright", cex = 0.8, y.intersp = 1.15, legend = c("observed rainfall",paste("simulated average rainfall (",weather.gen.predict.realiz.n," realizations)",sep="")), col=c("blue","red"), lwd =c(3,0.8), lty=c(0,-1),pch = c(124,19),bg = "white")
#legend(daily.year.list[1]-0.5, 33, y.intersp = 2,legend = c("observed rainfall",paste("simulated average rainfall (",weather.gen.predict.realiz.n," realizations)",sep="")), col=c("blue","red"), lwd =c(3,0.8), lty=c(0,-1),pch = c(124,19),bg = "white")

dev.off()




############################################
## Plot Each Station all DAILY Rainfall prediction and observation
setwd(dir1)
pdf(paste("daily-Conclude Rain Gen+Obs all sta.pdf"), width = 10 ,height = 6)
#par(mgp=c(2,0.8,0), ps =18)
par(mgp=c(2.5,0.8,0), ps =18)
par(mai = c(0.8, 0.8, 0.8, 0.3))
setwd(dir0)


for(station.i in 1:station.n){
cat("\nPlot daily rainfall prediction+obs. time-series")
#z = ts(rep(0,length(daily.day.list)),start = c(daily.year.list[1], 1), frequency = 372)
z = ts(rep(0,length(daily.day.list)),start = c(daily.year.list[1], 1), frequency = 365.25)
#y.lim = max(org.f, na.rm = TRUE)
y.lim = 300
plot.ts(z, type = "n",ylim = c(0,y.lim), ylab=paste("daily rainfall (mm/day)", sep=""), xlab ="Days", main = station.title[station.i])


# Plot observation
for(i in 1:12){
			row.sel = which(daily.month.list == i)
			for(j in 1:max(daily.day.list[row.sel])){
										# Plot obs
										row.sel.2 = which(daily.day.list[row.sel] == j)
										ts.show = time(z)[row.sel[row.sel.2]]
										lines(ts.show,as.real(unlist(data.daily[row.sel[row.sel.2],station.i])),type="h", col = "blue")
										# Plot predict
										rain.plot = rain.predict.conclude.allrlz[row.sel[row.sel.2],,]
										#if(max(rain.plot, na.rm=TRUE)>200){cat(i,",",j,",",k)}
										for(k in 1:weather.gen.predict.realiz.n){
																	lines(ts.show,as.real(unlist(rain.plot[,station.i,k])),col = "red",type = "p", lwd = 1, cex = 0.3)
																	}
										cat("\nPlot station:",station.i,"month:",i," day:",j)
										}

			}
# Plot prediction
legend("topright", cex = 0.8, y.intersp = 1.15, legend = c("observed daily rainfall",paste("simulated rainfall (",weather.gen.predict.realiz.n," realizations)",sep="")), col=c("blue","red"), lwd =c(3,0.5), lty=c(0,-1),pch = c(124,19),bg = "white")

}
dev.off()





############################################
## Plot Each Station all Rainfall Avg-RLZ DAILY prediction and observation
setwd(dir1)
pdf(paste("daily-Conclude Rain Gen+Obs all sta avg-rlz.pdf"), width = 10 ,height = 6)
#par(mgp=c(2,0.8,0), ps =18)
par(mgp=c(2.5,0.8,0), ps =18)
par(mai = c(0.8, 0.8, 0.8, 0.3))
setwd(dir0)


for(station.i in 1:station.n){
cat("\nPlot daily rainfall prediction+obs. time-series")
#z = ts(rep(0,length(daily.day.list)),start = c(daily.year.list[1], 1), frequency = 372)
z = ts(rep(0,length(daily.day.list)),start = c(daily.year.list[1], 1), frequency = 365.25)
#y.lim = max(org.f, na.rm = TRUE)
y.lim = 300
plot.ts(z, type = "n",ylim = c(0,y.lim), ylab=paste("daily rainfall (mm/day)", sep=""), xlab ="Days", main = station.title[station.i])


# Plot observation
for(i in 1:12){
			row.sel = which(daily.month.list == i)
			for(j in 1:max(daily.day.list[row.sel])){
										# Plot obs
										row.sel.2 = which(daily.day.list[row.sel] == j)
										ts.show = time(z)[row.sel[row.sel.2]]
										lines(ts.show,as.real(unlist(data.daily[row.sel[row.sel.2],station.i])),type="h", col = "blue")
										# Plot predict
										rain.plot = rain.predict.conclude.allrlz[row.sel[row.sel.2],,]
										#if(max(rain.plot, na.rm=TRUE)>200){cat(i,",",j,",",k)}
										lines(ts.show,as.real(rowMeans(rain.plot[,station.i,], na.rm = TRUE)),col = "red",type = "p", lwd = 1, cex = 0.3)
										cat("\nPlot station:",station.i,"month:",i," day:",j)
										}

			}
# Plot prediction
legend("topright", cex = 0.8, y.intersp = 1.15, legend = c("observed daily rainfall",paste("simulated rainfall (",weather.gen.predict.realiz.n," realizations)",sep="")), col=c("blue","red"), lwd =c(3,0.5), lty=c(0,-1),pch = c(124,19),bg = "white")

}
dev.off()






############################################
## Plot Each Station EXTREME MONTHLY rainfall prediction and observation

setwd(dir1)
pdf(paste("monthly-Conclude avg Extreme Rain Gen+Obs all sta.pdf"), width = 10 ,height = 6)
#par(mgp=c(2,0.8,0), ps =18)
par(mgp=c(2.5,0.8,0), ps =18)
par(mai = c(0.8, 0.8, 0.8, 0.3))
setwd(dir0)

for(station.i in 1:station.n){

cat("\nPlot AVERAGE daily rainfall prediction+obs. time-series")
#z = ts(rep(0,length(daily.day.list)),start = c(daily.year.list[1], 1), frequency = 372)
z = ts(rep(0,length(future.month.list)),start = c(daily.year.list[1], 1), frequency = 12)


#y.lim = max(org.f, na.rm = TRUE)
y.lim = 350
plot.ts(z, type = "n",ylim = c(0,y.lim), ylab=paste("monthly rainfall (mm/day)", sep=""), xlab ="Year", main = station.title[station.i])


# Plot observation
for(i in 1:12){
			row.sel = which(daily.month.list == i)
			year.row.sel = which(future.month.list == i)

			year.temp = future.year.list[which(future.month.list == i)]
			for(year.i in 1:length(year.temp)){
									row.sel.2 = which(daily.year.list[row.sel] == year.temp[year.i])
									year.row.sel.2 = which(future.year.list[year.row.sel] == year.temp[year.i])
									rain.sel = rain.predict.conclude.allrlz[row.sel[row.sel.2],,]
									ts.show = time(z)[year.row.sel[year.row.sel.2]]

										# Plot obs
										lines(ts.show, max(data.daily[row.sel[row.sel.2],station.i], na.rm = TRUE),type="h", col = "blue", lwd = 1)
										# Plot predict
										rain.plot = integer(0)
										for(rlz.i in 1:weather.gen.predict.realiz.n){
												rain.plot = c(rain.plot, max(rain.sel[,station.i,rlz.i],na.rm = TRUE))
												}
										lines(ts.show, mean(rain.plot), col = "red",type = "p", lwd = 1, cex = 0.5, pch = 19)

										cat("\nPlot station:",station.i,"month:",i," year:",year.temp[year.i])
									}
			}
# Plot prediction
legend("topright", cex = 0.8, y.intersp = 1.15, legend = c("observed rainfall",paste("simulated average rainfall (",weather.gen.predict.realiz.n," realizations)",sep="")), col=c("blue","red"), lwd =c(3,0.8), lty=c(0,-1),pch = c(124,19),bg = "white")
#legend(daily.year.list[1]-0.5, 33, y.intersp = 2,legend = c("observed rainfall",paste("simulated average rainfall (",weather.gen.predict.realiz.n," realizations)",sep="")), col=c("blue","red"), lwd =c(3,0.8), lty=c(0,-1),pch = c(124,19),bg = "white")

}
dev.off()







############################################
## Plot Each Station EXTREME MONTHLY rainfall prediction and observation in 12 months
## -- plot 12 - month points
setwd(dir1)
pdf(paste("monthly-Conclude avg Extreme Rain Gen+Obs all sta - 12 month.pdf"), width = 10,height = 6)
#par(mgp=c(2,0.8,0), ps =18)
par(mgp=c(2.5,0.8,0), ps =18)
par(mai = c(0.8, 0.8, 0.8, 0.3))
setwd(dir0)

max.extrme.obs.rlz = integer(0)

for(station.i in 1:station.n){

cat("\nPlot monthly extreme rainfall prediction+obs time-series - all sta 12 months")
#z = ts(rep(0,length(future.month.list)),start = c(daily.year.list[1], 1), frequency = 12)
z = 1:12

#y.lim = max(org.f, na.rm = TRUE)
y.lim = 100
plot(z, type = "n",ylim = c(0,y.lim), ylab=paste("monthly extreme rainfall (mm/day)", sep=""), xlab ="Month", main = paste("monthly extreme rainfall", station.title[station.i]))

rain.plot.obs = integer(0)

for(i in 1:12){
			day.in.month = max(day.in.month.list[which(day.in.month.list[,2]==month.i),5])
			row.sel = which(daily.month.list == i)

			#rain.sel = rain.predict.conclude.allrlz[row.sel,station.i,]

			# Plot predict

			rain.sel.year.obs = integer(0)
			rain.plot = integer(0)

			for(rlz.i in 1:weather.gen.predict.realiz.n){
					#rain.plot = c(rain.plot, max(rain.sel[,rlz.i],na.rm = TRUE))

					rain.sel.year = integer(0)

					for(year.i in unique(daily.year.list[row.sel])){
												row.sel.2 = which(daily.year.list[row.sel] == year.i)
												rain.sel.year = c(rain.sel.year, max(rain.predict.conclude.allrlz[row.sel[row.sel.2],station.i,rlz.i],na.rm = TRUE))

												if(rlz.i == 1){rain.sel.year.obs = c(rain.sel.year.obs, max(data.daily[row.sel[row.sel.2],station.i],na.rm = TRUE))}
												}

					rain.plot = c(rain.plot, mean(rain.sel.year, na.rm = TRUE)) # average every year
					}

			rain.plot.obs = c(rain.plot.obs, mean(rain.sel.year.obs, na.rm = TRUE)) # average every year
			max.extrme.obs.rlz = rbind(max.extrme.obs.rlz,cbind(rain.sel.year.obs, rain.sel.year))

			# Plot prediction
			lines(rep(z[i],weather.gen.predict.realiz.n), rain.plot, col = "grey",type = "p", lwd = 1, cex = 0.5, pch = 19)
			lines(z[i], mean(rain.plot, na.rm = TRUE), col = "red", type = "p", lwd = 1, cex = 1, pch = 22)

			cat("\nPlot station:",station.i,"month:",i)
			}
# Plot obs
lines(z, rain.plot.obs, type="l", col = "blue", lwd = 3)

legend("topright", cex = 0.8, y.intersp = 1.15, legend = c("observed average extrme rainfall",paste("simulated extreme rainfall (",weather.gen.predict.realiz.n," realizations)",sep=""),"average simulated extreme rainfall"), col=c("blue","grey","red"), lwd =c(3,0.8), lty=c(1,-1,-1),pt.cex = c(1,0.5,1), pch = c(-1,19,22),bg = "white")
}
dev.off()


### Plot v2
setwd(dir1)
pdf(paste("monthly-Conclude avg Extreme Rain Gen+Obs all sta - 12 month2.pdf"), width = 10,height = 6)
#par(mgp=c(2,0.8,0), ps =18)
par(mgp=c(2.5,0.8,0), ps =18)
par(mai = c(0.8, 0.8, 0.8, 0.3))
setwd(dir0)

max.extrme.obs.rlz = integer(0)

for(station.i in 1:station.n){

cat("\nPlot monthly extreme rainfall prediction+obs time-series - all sta 12 month2")
#z = ts(rep(0,length(future.month.list)),start = c(daily.year.list[1], 1), frequency = 12)
z = 1:12

#y.lim = max(org.f, na.rm = TRUE)
y.lim = 200
plot(z, type = "n",ylim = c(0,y.lim), ylab=paste("monthly extreme rainfall (mm/day)", sep=""), xlab ="Month", main = paste("monthly extreme rainfall", station.title[station.i]))

rain.plot.obs = integer(0)

for(i in 1:12){
			day.in.month = max(day.in.month.list[which(day.in.month.list[,2]==month.i),5])
			row.sel = which(daily.month.list == i)

			#rain.sel = rain.predict.conclude.allrlz[row.sel,station.i,]

			# Plot predict

			rain.sel.year.obs = integer(0)
			rain.sel.year.all = integer(0)
			rain.plot = integer(0)

			for(rlz.i in 1:weather.gen.predict.realiz.n){
					#rain.plot = c(rain.plot, max(rain.sel[,rlz.i],na.rm = TRUE))

					rain.sel.year = integer(0)

					for(year.i in unique(daily.year.list[row.sel])){
												row.sel.2 = which(daily.year.list[row.sel] == year.i)
												rain.sel.year = c(rain.sel.year, max(rain.predict.conclude.allrlz[row.sel[row.sel.2],station.i,rlz.i],na.rm = TRUE))

												if(rlz.i == 1){rain.sel.year.obs = c(rain.sel.year.obs, max(data.daily[row.sel[row.sel.2],station.i],na.rm = TRUE))}
												}

					rain.plot = c(rain.plot, mean(rain.sel.year, na.rm = TRUE)) # average every year
					rain.sel.year.all = c(rain.sel.year.all, rain.sel.year) # every year, every realization
					}

			rain.plot.obs = c(rain.plot.obs, mean(rain.sel.year.obs, na.rm = TRUE)) # average every year
			max.extrme.obs.rlz = rbind(max.extrme.obs.rlz,cbind(rain.sel.year.obs, rain.sel.year))

			# Plot prediction
			#lines(rep(z[i],weather.gen.predict.realiz.n), rain.plot, col = "grey",type = "p", lwd = 1, cex = 0.5, pch = 21)
			lines(rep(z[i]+0.1,length(rain.sel.year.all)), rain.sel.year.all, col = "grey",type = "p", lwd = 1, cex = 0.5, pch = 21)
			lines(z[i], mean(rain.plot, na.rm = TRUE), col = "red", type = "p", lwd = 1, cex = 1, pch = 22)

			# Plot obs (each year)
			lines(rep(z[i]-0.1,length(rain.sel.year.obs)), rain.sel.year.obs, col = "grey",type = "p", lwd = 1, cex = 0.5, pch = 4)

			cat("\nPlot station:",station.i,"month:",i)
			}

# Plot obs (average year)
lines(z, rain.plot.obs, type="l", col = "blue", lwd = 3)

legend("topright", cex = 0.8, y.intersp = 1.15, legend = c("monthly extreme rainfall","observed average extrme rainfall",paste("simulated monthly extreme rainfall (",weather.gen.predict.realiz.n," realizations)",sep=""),"average simulated extreme rainfall"), col=c("black","blue","black","red"), lwd =c(0,3,0,0), lty=c(-1,1,-1,-1),pt.cex = c(0.5,1,0.8,1), pch = c(4,-1,21,22),bg = "white")
}
dev.off()

############################################
## Plot Each Station EXTREME MONTHLY rainfall prediction and observation in diagonal plots
## results from max.extrme.obs.rlz (above process)
setwd(dir1)
pdf(paste("monthly-Conclude avg Extreme Rain Gen+Obs all sta - 12 month-diagonal.pdf"), width = 6,height = 6)
par(mgp=c(2.5,0.8,0), ps =18)
par(mai = c(0.8, 0.8, 0.8, 0.3))
setwd(dir0)

for(station.i in 1:station.n){

cat("\nPlot monthly extreme rainfall prediction+obs time-series - diagonal, station :", station.i)

#y.lim = max(max.extrme.obs.rlz, na.rm = TRUE)
y.lim = 300
plot(max.extrme.obs.rlz[((station.i-1)*12*weather.gen.predict.realiz.n+1):((station.i)*12*weather.gen.predict.realiz.n),], type = "p", cex = 0.5, pch = 19, xlim = c(0,y.lim), ylim = c(0,y.lim), ylab="simulated rainfall (mm/day)", xlab = "observed rainfall (mm/day)", main = paste("monthly extreme rainfall",station.title[station.i]))
lines(c(-100,y.lim+100),c(-100,y.lim+100), col = "blue")
}
dev.off()





############################################
## Plot Each Station EXTREME MONTHLY rainfall prediction and observation in 12 months
## -- plot 12 every year - month points
setwd(dir1)
pdf(paste("monthly-Conclude avg Extreme Rain Gen+Obs all sta - 12 month-every year.pdf"), width = 10,height = 6)
#par(mgp=c(2,0.8,0), ps =18)
par(mgp=c(2.5,0.8,0), ps =18)
par(mai = c(0.8, 0.8, 0.8, 0.3))
setwd(dir0)

max.extrme.obs.rlz = integer(0)
for(station.i in 1:station.n){

cat("\nPlot monthly extreme rainfall prediction+obs time-series - 366 days")
z = 1:12

#y.lim = max(org.f, na.rm = TRUE)
y.lim = 350
plot(z, type = "n",ylim = c(0,y.lim), ylab=paste("monthly extreme rainfall (mm/day)", sep=""), xlab ="Month", main = paste("monthly extreme rainfall,", station.title[station.i]))
max.extrme.obs = integer(0)

rain.plot.year = integer(0)
rain.plot.obs.year = integer(0)
for(i in 1:12){
			row.sel = which(daily.month.list == i)
			year.temp = future.year.list[which(future.month.list == i)]

			rain.plot = integer(0)
			rain.plot.obs = integer(0)
			for(year.i in 1:length(year.temp)){

									# calculate predict
									row.sel.2 = which(daily.year.list[row.sel] == year.temp[year.i])
									rain.sel = integer(0)
									for(j in 1:weather.gen.predict.realiz.n){
															rain.sel = c(rain.sel, max(rain.predict.conclude.allrlz[row.sel[row.sel.2],station.i,][,j],na.rm = TRUE))
															}
									# mean of realization
									rain.plot = c(rain.plot ,mean(rain.sel,na.rm = TRUE))


									# calculate obs
									rain.plot.obs = c(rain.plot.obs, max(data.daily[row.sel[row.sel.2],station.i], na.rm = TRUE))
									}


			# Plot obs
			lines(rep(z[i],length(year.temp)), rain.plot.obs, type="p", cex = 0.5, pch =4, col = "black")
			rain.plot.obs.year = c(rain.plot.obs.year, mean(rain.plot.obs, na.rm = TRUE))

			# Plot predict
			lines(rep(z[i],length(year.temp)), rain.plot, col = "grey",type = "p", cex = 0.5, pch = 19)
			rain.plot.year = c(rain.plot.year, mean(rain.plot, na.rm = TRUE))



			cat("\nPlot station:",station.i,"month:",i," year:",year.temp[year.i])
			max.extrme.obs.rlz = rbind(max.extrme.obs.rlz, cbind(rain.plot.obs,rain.plot))
			}

# Plot predict
lines(z, rain.plot.year, type="l", col = "red", lty = 1, lwd = 3)
# Plot obs
lines(z, rain.plot.obs.year, type="l", col = "blue", lty = 2, lwd = 2)

# Plot prediction
legend("topright", cex = 0.8, y.intersp = 1.2, legend = c(paste("observed extrme rainfall(",year.n," years)",sep=""),paste("simulated extreme rainfall(",year.n," years)",sep=""),"observed average extreme rainfall" , "simulated average extreme rainfall"), col=c("grey","black","blue","red"), lwd =c(0,0,2,3), lty=c(-1,-1,2,1),pt.cex = c(0.5,0.5,1,1), pch = c(19,4,-1,-1),bg = "white")
}
dev.off()


############################################
## Plot Each Station EXTREME MONTHLY rainfall prediction and observation in diagonal plots
## results from max.extrme.obs.rlz (above process)
setwd(dir1)
pdf(paste("monthly-Conclude avg Extreme Rain Gen+Obs all sta - 12 month-every year-diagonal.pdf"), width = 6,height = 6)
par(mgp=c(2.5,0.8,0), ps =18)
par(mai = c(0.8, 0.8, 0.8, 0.3))
setwd(dir0)

for(station.i in 1:station.n){

cat("\nPlot monthly extreme rainfall prediction+obs time-series - daigonal, station :", station.i)

y.lim = max(max.extrme.obs.rlz, na.rm = TRUE)
plot(max.extrme.obs.rlz[((station.i-1)*12*weather.gen.predict.realiz.n+1):((station.i)*12*weather.gen.predict.realiz.n),], type = "p", cex = 0.5, pch = 19, xlim = c(0,y.lim), ylim = c(0,y.lim), ylab="simulated rainfall (mm/day)", xlab = "observed rainfall (mm/day)", main = paste("monthly extreme rainfall",station.title[station.i]))
lines(c(-100,y.lim+100),c(-100,y.lim+100), col = "blue")
}
dev.off()





############################################
## Plot Each Station EXTREME MONTHLY rainfall prediction and observation in 366 days
## -- plot 366 - day points
setwd(dir1)
pdf(paste("monthly-Conclude avg Extreme Rain Gen+Obs all sta - 366 days.pdf"), width = 10,height = 6)
#par(mgp=c(2,0.8,0), ps =18)
par(mgp=c(2.5,0.8,0), ps =18)
par(mai = c(0.8, 0.8, 0.8, 0.3))
setwd(dir0)

max.extrme.obs.rlz = integer(0)

for(station.i in 1:station.n){

cat("\nPlot monthly extreme rainfall prediction+obs time-series - 366 days")
#z = ts(rep(0,length(future.month.list)),start = c(daily.year.list[1], 1), frequency = 12)
z = 1:366

#y.lim = max(org.f, na.rm = TRUE)
y.lim = 350
plot(z, type = "n",ylim = c(0,y.lim), ylab=paste("daily extreme rainfall (mm/day)", sep=""), xlab ="day", main = paste("daily extreme rainfall", station.title[station.i]))
max.extrme.obs = integer(0)

year.n = dim(rain.predict.conclude.allrlz)[1]/366
for(i in 1:366){
			row.sel = c(1:year.n-1)*366 + i
			rain.sel = rain.predict.conclude.allrlz[row.sel,station.i,]

			# Plot predict
										rain.plot = integer(0)
			for(rlz.i in 1:weather.gen.predict.realiz.n){
					rain.plot = c(rain.plot, max(rain.sel[,rlz.i],na.rm = TRUE))
					}

			lines(rep(z[i],weather.gen.predict.realiz.n), rain.plot, col = "grey",type = "p", lwd = 1, cex = 0.5, pch = 19)
			lines(z[i], mean(rain.plot, na.rm = TRUE), col = "red",type = "p", lwd = 1, cex = 1, pch = 22)

			cat("\nPlot station:",station.i,"day:",i," year:",year.temp[year.i])
			max.extrme.obs.rlz = rbind(max.extrme.obs.rlz,cbind(max(data.daily[row.sel,station.i], na.rm = TRUE), rain.plot))

			max.extrme.obs = c(max.extrme.obs,max(data.daily[row.sel,station.i], na.rm = TRUE))
			}
# Plot obs
lines(z, max.extrme.obs, type="l", col = "blue", lwd = 3)

# Plot prediction
legend("topright", cex = 0.8, y.intersp = 1.15, legend = c("observed extrme rainfall",paste("simulated extreme rainfall (",weather.gen.predict.realiz.n," realizations)",sep=""),"average simulated extreme rainfall"), col=c("blue","grey","red"), lwd =c(3,0.8), lty=c(1,-1,-1),pt.cex = c(1,0.5,1), pch = c(-1,19,22),bg = "white")
}
dev.off()


############################################
## Plot Each Station EXTREME MONTHLY rainfall prediction and observation in diagonal plots
## results from max.extrme.obs.rlz (above process)
setwd(dir1)
pdf(paste("monthly-Conclude avg Extreme Rain Gen+Obs all sta - 366 days-diagonal.pdf"), width = 6,height = 6)
par(mgp=c(2.5,0.8,0), ps =18)
par(mai = c(0.8, 0.8, 0.8, 0.3))
setwd(dir0)

for(station.i in 1:station.n){

cat("\nPlot monthly extreme rainfall prediction+obs time-series - daigonal, station :", station.i)

y.lim = max(max.extrme.obs.rlz, na.rm = TRUE)
plot(max.extrme.obs.rlz[((station.i-1)*366*weather.gen.predict.realiz.n+1):((station.i)*366*weather.gen.predict.realiz.n),], type = "p", cex = 0.5, pch = 19, xlim = c(0,y.lim), ylim = c(0,y.lim), ylab="simulated rainfall (mm/day)", xlab = "observed rainfall (mm/day)", main = paste("monthly extreme rainfall",station.title[station.i]))
lines(c(-100,y.lim+100),c(-100,y.lim+100), col = "blue")
}
dev.off()




############################################
## Plot Each Station AVERAGE-RLZ MONTHLY rainfall prediction and observation


setwd(dir1)
pdf(paste("monthly-Conclude average-rlz Rain Gen+Obs all sta.pdf"), width = 10 ,height = 6)
#par(mgp=c(2,0.8,0), ps =18)
par(mgp=c(2.5,0.8,0), ps =18)
par(mai = c(0.8, 0.8, 0.8, 0.3))
setwd(dir0)

for(station.i in 1:station.n){

cat("\nPlot AVERAGE daily rainfall prediction+obs. time-series of station:")
#z = ts(rep(0,length(daily.day.list)),start = c(daily.year.list[1], 1), frequency = 372)
z = ts(rep(0,length(future.month.list)),start = c(daily.year.list[1], 1), frequency = 12)


#y.lim = max(org.f, na.rm = TRUE)
y.lim = 35
plot.ts(z, type = "n",ylim = c(0,y.lim), ylab=paste("monthly rainfall (mm/day)", sep=""), xlab ="Year", main = station.title[station.i])


# Plot observation
for(i in 1:12){
			row.sel = which(daily.month.list == i)
			year.row.sel = which(future.month.list == i)

			year.temp = future.year.list[which(future.month.list == i)]
			for(year.i in 1:length(year.temp)){
									row.sel.2 = which(daily.year.list[row.sel] == year.temp[year.i])
									year.row.sel.2 = which(future.year.list[year.row.sel] == year.temp[year.i])
									rain.sel = rain.predict.conclude.allrlz[row.sel[row.sel.2],station.i,]
									rain.plot = mean(rain.sel,na.rm = TRUE)

									#ts.show = rep(time(z)[year.row.sel[year.row.sel.2]], station.n)
									ts.show = time(z)[year.row.sel[year.row.sel.2]]
									# Plot obs
									lines(ts.show, mean(data.daily[row.sel[row.sel.2],station.i], na.rm = TRUE),type="h", col = "blue", lwd = 1)
									# Plot predict
									lines(ts.show,rain.plot,col = "red",type = "p", lwd = 1, cex = 0.5, pch = 19)

									cat("\nPlot station:",station.i,"month:",i," year:",year.temp[year.i])
									}
			}
# Plot prediction
legend("topright", cex = 0.8, y.intersp = 1.15, legend = c("observed rainfall",paste("simulated average rainfall (",weather.gen.predict.realiz.n," realizations)",sep="")), col=c("blue","red"), lwd =c(3,0.8), lty=c(0,-1),pch = c(124,19),bg = "white")
#legend(daily.year.list[1]-0.5, 33, y.intersp = 2,legend = c("observed rainfall",paste("simulated average rainfall (",weather.gen.predict.realiz.n," realizations)",sep="")), col=c("blue","red"), lwd =c(3,0.8), lty=c(0,-1),pch = c(124,19),bg = "white")

}
dev.off()






############################################
## Plot Each Station EACH-RLZ MONTHLY rainfall prediction and observation


setwd(dir1)
pdf(paste("monthly-Conclude each-rlz Rain Gen+Obs all sta.pdf"), width = 10 ,height = 6)
#par(mgp=c(2,0.8,0), ps =18)
par(mgp=c(2.5,0.8,0), ps =18)
par(mai = c(0.8, 0.8, 0.8, 0.3))
setwd(dir0)

for(station.i in 1:station.n){

cat("\nPlot AVERAGE daily rainfall prediction+obs. time-series of station:")
#z = ts(rep(0,length(daily.day.list)),start = c(daily.year.list[1], 1), frequency = 372)
z = ts(rep(0,length(future.month.list)),start = c(daily.year.list[1], 1), frequency = 12)


#y.lim = max(org.f, na.rm = TRUE)
y.lim = 35
plot.ts(z, type = "n",ylim = c(0,y.lim), ylab=paste("monthly rainfall (mm/day)", sep=""), xlab ="Year", main = station.title[station.i])


# Plot observation
for(i in 1:12){
			row.sel = which(daily.month.list == i)
			year.row.sel = which(future.month.list == i)

			year.temp = future.year.list[which(future.month.list == i)]
			for(year.i in 1:length(year.temp)){
									row.sel.2 = which(daily.year.list[row.sel] == year.temp[year.i])
									year.row.sel.2 = which(future.year.list[year.row.sel] == year.temp[year.i])
									rain.sel = rain.predict.conclude.allrlz[row.sel[row.sel.2],station.i,]
									rain.plot = colMeans(rain.sel,na.rm = TRUE)

									#ts.show = rep(time(z)[year.row.sel[year.row.sel.2]], station.n)
									ts.show = time(z)[year.row.sel[year.row.sel.2]]
									# Plot obs
									lines(ts.show, mean(data.daily[row.sel[row.sel.2],station.i], na.rm = TRUE),type="h", col = "blue", lwd = 1)
									# Plot predict
									lines(rep(ts.show,weather.gen.predict.realiz.n) ,rain.plot,col = "red",type = "p", lwd = 1, cex = 0.5, pch = 19)

									cat("\nPlot station:",station.i,"month:",i," year:",year.temp[year.i])
									}
			}
# Plot prediction
legend("topright", cex = 0.8, y.intersp = 1.15, legend = c("observed rainfall",paste("simulated average rainfall (",weather.gen.predict.realiz.n," realizations)",sep="")), col=c("blue","red"), lwd =c(3,0.8), lty=c(0,-1),pch = c(124,19),bg = "white")
#legend(daily.year.list[1]-0.5, 33, y.intersp = 2,legend = c("observed rainfall",paste("simulated average rainfall (",weather.gen.predict.realiz.n," realizations)",sep="")), col=c("blue","red"), lwd =c(3,0.8), lty=c(0,-1),pch = c(124,19),bg = "white")

}
dev.off()



############################################
## Plot Each Station EXTREME ANNUAL rainfall prediction and observation
## -- plot Annually -
setwd(dir1)
pdf(paste("Annual Extreme Rain Prob Gen+Obs all sta-log-y.pdf"), width = 8,height = 6)
#par(mgp=c(2,0.8,0), ps =18)
par(mgp=c(2.5,0.8,0), ps =18)
par(mai = c(0.8, 0.8, 0.8, 0.3))
setwd(dir0)

max.extrme.obs.rlz = integer(0)

for(station.i in 1:station.n){

cat("\nPlot Annual Extreme Rain Prob Gen+Obs all sta-log-y")
#z = ts(rep(0,length(future.month.list)),start = c(daily.year.list[1], 1), frequency = 12)

#y.lim = max(org.f, na.rm = TRUE)
#y.lim =  max(rain.predict.conclude.allrlz,na.rm = TRUE)
y.lim = 600
z = cbind(c(0,1),c(1,y.lim))


plot(z, type = "n",xlim = c(0.998,0.001), log = "y", yaxt ="n", xaxt ="n", ylab=paste("extreme rainfall (mm/day)", sep=""), xlab ="Probability", main = paste("monthly extreme rainfall", station.title[station.i]))
#plot(z, type = "n",xlim = c(0.998,0.00001), log = "x", yaxt ="n", xaxt ="n", ylab=paste("extreme rainfall (mm/day)", sep=""), xlab ="Probability", main = paste("monthly extreme rainfall", station.title[station.i]))
#plot(z, type = "n",xlim = c(0.998,0.002), yaxt ="n", xaxt ="n", ylab=paste("extreme rainfall (mm/day)", sep=""), xlab ="Probability", main = paste("monthly extreme rainfall", station.title[station.i]))
axis(2, c(1,2,5,10,50,100,200,500,1000)) # draw y axis with required labels 
axis(1, c(0.998,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1,0.001)) # draw x axis with required labels 
#axis(1, c(0.998,0.7,0.5,0.3,0.2,0.1,0.01,0.001,0.001)) # draw log-x axis with required labels 



# Plot predict

rain.obs = integer(0)
rain.prd = integer(0)
rain.all.prd = integer(0)

for(rlz.i in 1:weather.gen.predict.realiz.n){
		rain.prd = integer(0)

		# run loop not include last year which is not complete whole year
		for(year.i in unique(daily.year.list)[-length(unique(daily.year.list))]){
								row.sel = which(daily.year.list == year.i)
								rain.prd = c(rain.prd, max(rain.predict.conclude.allrlz[row.sel,station.i,rlz.i],na.rm = TRUE)) # predict values
								if(rlz.i == 1){rain.obs = c(rain.obs, max(data.daily[row.sel,station.i],na.rm = TRUE))} # obs values
								}
		rain.all.prd = cbind(rain.all.prd, rain.prd[order(rain.prd,decreasing = TRUE)]) # every year, every realization
		}

# Plot prediction
rain.plot.xy = integer(0)
x.rain.prd = c(1:length(rain.prd))/(length(rain.prd)+1)
for(rlz.i in 1:weather.gen.predict.realiz.n){
	lines(x.rain.prd , rain.all.prd[,rlz.i], col = "grey",type = "p", lwd = 1, cex = 0.5, pch =19) # plot every realization
	rain.plot.xy = rbind(rain.plot.xy, cbind(x.rain.prd, rain.all.prd[,rlz.i]))
	}
lines(x.rain.prd, rowMeans(rain.all.prd), col = "red", type = "b",lty = 2, lwd = 2, cex = 0.8, pch = 4) # plot average prediction

# Plot trend line semi-log of simulated line
x = rain.plot.xy[,1]
y = log(rain.plot.xy[,2])
x.y.mat = cbind(x,y)
if(length(which(x.y.mat[,2] < log(0.1))) != 0){x.y.mat = x.y.mat[-which(x.y.mat[,2] < log(0.1)),]}
y = x.y.mat[,2]
x = x.y.mat[,1]
eq = lm(y~x)
new = data.frame(x =c(0.001,0.998))
lines(c(0.001,0.998),exp(predict(eq, new)), type = "l", lty = 2)

# Plot obs
lines(x.rain.prd, rain.obs[order(rain.obs,decreasing = TRUE)], col = "blue",type = "b",lty = 1, lwd = 3, cex = 0.8, pch = 22, bg = "blue")

cat("\nPlot station:",station.i,"month:",i)
legend("topleft", cex = 0.8, y.intersp = 1.15, legend = c("observed extreme rainfall",paste("simulated extreme rainfall (",weather.gen.predict.realiz.n," realizations)",sep=""),"average simulated extreme rainfall"), col=c("blue","grey","red"), lwd =c(3,0,2), lty=c(1,-1,2), pch = c(22,19,4),bg = "white")
}

dev.off()





############################################
## Plot Each Station EXTREME ANNUAL rainfall prediction and observation
## -- plot Annually -
setwd(dir1)
pdf(paste("Annual Extreme Rain Prob Gen+Obs all sta-log-x.pdf"), width = 8,height = 6)
#par(mgp=c(2,0.8,0), ps =18)
par(mgp=c(2.5,0.8,0), ps =18)
par(mai = c(0.8, 0.8, 0.8, 0.3))
setwd(dir0)

# remove extreme mistake at sta.6
daily.fixed = data.daily
daily.fixed[which.max(daily.fixed[,6]),6] = NA

max.extrme.obs.rlz = integer(0)

for(station.i in 1:station.n){

cat("\nAnnual Extreme Rain Prob Gen+Obs all sta-log-x")
#z = ts(rep(0,length(future.month.list)),start = c(daily.year.list[1], 1), frequency = 12)

#y.lim = max(org.f, na.rm = TRUE)
#y.lim =  max(rain.predict.conclude.allrlz,na.rm = TRUE)
y.lim = 600
z = cbind(c(0.001,0.999)*100,c(0.01,y.lim))


#plot(z, type = "n",xlim = c(0.998,0.00001), log = "y", yaxt ="n", xaxt ="n", ylab=paste("extreme rainfall (mm/day)", sep=""), xlab ="Probability", main = paste("monthly extreme rainfall", station.title[station.i]))
plot(z, type = "n",xlim = c(99.99,0.9), log = "x", xaxt ="n", ylab=paste("extreme rainfall (mm/day)", sep=""), xlab ="Probability (Perecent %)", main = paste("monthly extreme rainfall", station.title[station.i]))
#plot(z, type = "n",xlim = c(0.998,0.002), yaxt ="n", xaxt ="n", ylab=paste("extreme rainfall (mm/day)", sep=""), xlab ="Probability", main = paste("monthly extreme rainfall", station.title[station.i]))
#axis(2, c(1,2,5,10,50,100,200,500,1000)) # draw log-y axis with required labels 
#axis(1, c(0.998,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1,0.001)) # draw x axis with required labels 
axis(1, c(99.9,50,20,10,1,0.1)) # draw log-x axis with required labels 



# Plot predict

rain.obs = integer(0)
rain.prd = integer(0)
rain.all.prd = integer(0)


for(rlz.i in 1:weather.gen.predict.realiz.n){
		rain.prd = integer(0)

		# run loop not include last year which is not complete whole year
		for(year.i in unique(daily.year.list)[-length(unique(daily.year.list))]){
								row.sel = which(daily.year.list == year.i)
								rain.prd = c(rain.prd, max(rain.predict.conclude.allrlz[row.sel,station.i,rlz.i],na.rm = TRUE)) # predict values
								if(rlz.i == 1){rain.obs = c(rain.obs, max(daily.fixed[row.sel,station.i],na.rm = TRUE))} # obs values
								}
		rain.all.prd = cbind(rain.all.prd, rain.prd[order(rain.prd,decreasing = TRUE)]) # every year, every realization

		}

# Plot prediction
rain.plot.xy = integer(0)
x.rain.prd = c(1:length(rain.prd))/(length(rain.prd)+1)*100
for(rlz.i in 1:weather.gen.predict.realiz.n){
	lines(x.rain.prd , rain.all.prd[,rlz.i], col = "grey",type = "p", lwd = 1, cex = 0.5, pch =19) # plot every realization
	rain.plot.xy = rbind(rain.plot.xy, cbind(x.rain.prd, rain.all.prd[,rlz.i]))
	}
lines(x.rain.prd, rowMeans(rain.all.prd), col = "red", type = "b",lty = 2, lwd = 2, cex = 0.8, pch = 4) # plot average prediction

# Plot trend line semi-log of simulated line
x = log(rain.plot.xy[,1])
y = rain.plot.xy[,2]
eq = lm(y~x)
new = data.frame(x =c(1,99.9))
lines(exp(unlist(new)),(predict(eq, new)), type = "l", lty = 2)

# Plot obs
lines(x.rain.prd, rain.obs[order(rain.obs,decreasing = TRUE)], col = "blue",type = "b",lty = 1, lwd = 3, cex = 0.8, pch = 22, bg = "blue")

cat("\nPlot station:",station.i,"month:",i)
legend("topleft", cex = 0.8, y.intersp = 1.15, legend = c("observed extreme rainfall",paste("simulated extreme rainfall (",weather.gen.predict.realiz.n," realizations)",sep=""),"average simulated extreme rainfall", "trend line of simulated extreme rainfall"), col=c("blue","grey","red","black"), lwd =c(3,0,2,1), lty=c(1,-1,2,2), pch = c(22,19,4,-1),bg = "white")
}

dev.off()




############################################
## Record MONTHLY rainfall prediction and observation

monthly.sum.conclude = rainy.in.month
monthly.sum.conclude[,-c(1:2)] = NA
colnames(monthly.sum.conclude)[-c(1:2)] = colnames(org.f)[-c(1:3)]
# output.year.list = future.t$year
# output.month.list = future.t$month
# output.day.list = future.t$day

for(station.i in 1:station.n){

cat("\nRecord monthly rainfall prediction time-series of station:",station.i)


for(month.i in 1:12){
			row.sel = which(future.t$month == month.i)
			year.temp = unique(future.t$year[which(future.t$month == month.i)])
			for(year.i in 1:length(year.temp)){
									row.sel.2 = which(future.t$year[row.sel] == year.temp[year.i])
									sum.rain = integer(0)
									for(rlz.i in 1:weather.gen.predict.realiz.n){
												sum.rain = c(sum.rain,sum(rain.predict.conclude.allrlz[row.sel[row.sel.2],station.i,rlz.i]))
												}
									# Put into conclusion table
									monthly.sum.conclude[(year.i-1)*12+month.i,(3+(station.i-1)*weather.gen.predict.realiz.n):(2+station.i*weather.gen.predict.realiz.n)] = sum.rain
									cat("\nRecord row:",(year.i-1)*12+month.i," -- station:",station.i,"month:",month.i," year:",year.temp[year.i])
									}
			}

}

na.monthly = which(is.na(monthly.sum.conclude[,1]))
if(length(na.monthly) > 0){
				cat("\nremove NA in monthly table")
				monthly.sum.conclude = monthly.sum.conclude[-c(na.monthly),]
				}

# Record monthly prediction
setwd(dir1)
write.csv(monthly.sum.conclude, file = "monthly-sum all-sta.csv", row.names = FALSE)
setwd(dir0)

# Write generated results
#setwd(dir1)
#write.csv(rain.gen, file = "generated rain.csv")
#setwd(dir0)




cat("\nError :",error.count,"\n")
cat("\nEND SCRIPT")
			   





