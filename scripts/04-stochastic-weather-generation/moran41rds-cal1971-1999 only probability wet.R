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
read.seed.file = FALSE





maindir = "Temp/Stochastic"
#dir_name = "predict_moran37c2_sel curve - new ecdf + extrm charts+limit extrm v4b"
dir_name = "StochasticPCPv41 only probability wet"
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
data.f.daily = "Daily Filled all climate 1971-1999+Wet_V2.csv"
#data.f.monthly = "monthly-obs_filled1971-2006(4).csv"
daily.unusedcol.org = 2
monthly.unusedcol.org = 2

## Input files (rainfall generation)
#future.f.mean.rain = "PCP SCNopt1971-2006.csv"
future.f.mean.rain = "PCP1971-1999-prd2000-2008.csv"
#future.f.mean.rain = "PCP1971-1999-prd2000-2096.csv"
#future.f.mean.rain = "future-mean rain-y2075.csv"
#future.f.mean.rain = "future-rain1mm2006.csv"
#future.f.mean.rain = "future-1rain-y2075test5rain.csv"

#future.f.rainy.day = "WDY SCNopt1971-2006.csv"
future.f.rainy.day = "WetDay1971-1999-prd2000-2008.csv"
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
#par(mgp=c(2,1,0), ps =10)
par(mgp=c(2.5,1,0), ps =16, mfrow=c(1,1))
par(mai =  c(.8, .8, .8, .8))
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

day.in.month = max(day.in.month.list[which(day.in.month.list[,2]==month.i),5])

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
legend("bottomright", cex = 0.8, y.intersp = 1.15,legend = c("P01","P11","linear","poly. order 2","poly. order 3"), col=c("red","blue","black","pink","green"), lwd =c(1,1,2,2,2), lty=c(-1,-1,1,2,3), pch = c(22,24,-1,-1,-1),bg = "white")
#mcm.eq = lm(prob.msm~prob.rainy.day,data = as.data.frame(sumval.state.eq))

# 1st order
mcm.01.eq = lm(prob.msm.01~prob.rainy.day,data = as.data.frame(sumval.state.eq))
mcm.11.eq = lm(prob.msm.11~prob.rainy.day,data = as.data.frame(sumval.state.eq))
abline(mcm.01.eq,col="black", lwd =2)
abline(mcm.11.eq,col="black", lwd =2)
# record error
mcm.eq.error = c(i,station.title[i],month.i,
summary(mcm.01.eq)$r.squared,
(mean((mcm.01.eq$residuals)^2))^.5,
summary(mcm.11.eq)$r.squared,
(mean((mcm.11.eq$residuals)^2))^.5)

if(length(which(unique(sumval.state.eq[,1])>0))>2){
# 2nd order
plx.order = 2
mcm.01.eq.plx = lm(as.data.frame(sumval.state.eq)[,2] ~., data=poly(as.data.frame(sumval.state.eq)[,1],plx.order,raw=TRUE))
mcm.11.eq.plx = lm(as.data.frame(sumval.state.eq)[,3] ~., data=poly(as.data.frame(sumval.state.eq)[,1],plx.order,raw=TRUE))
lines(as.data.frame(sumval.state.eq)[order(as.real(sumval.state.eq[,1])),1],predict(mcm.01.eq.plx,poly(as.data.frame(sumval.state.eq)[order(as.real(sumval.state.eq[,1])),1],plx.order,raw=TRUE)),col="pink",lty=2,lwd = 2)
lines(as.data.frame(sumval.state.eq)[order(as.real(sumval.state.eq[,1])),1],predict(mcm.11.eq.plx,poly(as.data.frame(sumval.state.eq)[order(as.real(sumval.state.eq[,1])),1],plx.order,raw=TRUE)),col="pink",lty=2,lwd = 2)
# record error
mcm.eq.error = c(mcm.eq.error,summary(mcm.01.eq.plx)$r.squared,
(mean((mcm.01.eq.plx$residuals)^2))^.5,
summary(mcm.11.eq.plx)$r.squared,
(mean((mcm.11.eq.plx$residuals)^2))^.5)}

if(length(which(unique(sumval.state.eq[,1])>0))>3){
# order to use inequation
# 3rd order
plx.order = 3
mcm.01.eq.plx = lm(as.data.frame(sumval.state.eq)[,2] ~., data=poly(as.data.frame(sumval.state.eq)[,1],plx.order,raw=TRUE))
mcm.11.eq.plx = lm(as.data.frame(sumval.state.eq)[,3] ~., data=poly(as.data.frame(sumval.state.eq)[,1],plx.order,raw=TRUE))
lines(as.data.frame(sumval.state.eq)[order(as.real(sumval.state.eq[,1])),1],predict(mcm.01.eq.plx,poly(as.data.frame(sumval.state.eq)[order(as.real(sumval.state.eq[,1])),1],plx.order,raw=TRUE)),col="green",lty=3,lwd = 2)
lines(as.data.frame(sumval.state.eq)[order(as.real(sumval.state.eq[,1])),1],predict(mcm.11.eq.plx,poly(as.data.frame(sumval.state.eq)[order(as.real(sumval.state.eq[,1])),1],plx.order,raw=TRUE)),col="green",lty=3,lwd = 2)
# record error
mcm.eq.error = c(mcm.eq.error,summary(mcm.01.eq.plx)$r.squared,
(mean((mcm.01.eq.plx$residuals)^2))^.5,
summary(mcm.11.eq.plx)$r.squared,
(mean((mcm.11.eq.plx$residuals)^2))^.5)
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
#colnames(mcm.eq.error.table) = c("Statio.i","Station","month","abs.mean.error.01-pl1","rmsq.error.01-pl1","abs.mean.error.11-pl1","rmsq.error.11-pl1","abs.mean.error.01-pl2","rmsq.error.01-pl2","abs.mean.error.11-pl2","rmsq.error.11-pl2","abs.mean.error.01-pl3","rmsq.error.01-pl3","abs.mean.error.11-pl3","rmsq.error.11-pl3")
colnames(mcm.eq.error.table) = c("Statio.i","Station","month","R2.01-pl1","rmsq.error.01-pl1","R2.11-pl1","rmsq.error.11-pl1","R2.01-pl2","rmsq.error.01-pl2","R2.11-pl2","rmsq.error.11-pl2","R2.01-pl3","rmsq.error.01-pl3","R2.11-pl3","rmsq.error.11-pl3")


setwd(dir1)
write.csv(mcm.eq.table, file = paste("mcm eq table",month.i,".csv"),row.names=FALSE)
write.csv(mcm.eq.error.table, file = paste("mcm eq error",month.i,".csv"),row.names=FALSE)
setwd(dir0)

###
# END estimating rainy day paramters
#############################################


}
dev.off()
##################
# END change the amount of mean monhtly rainfall
# END Great loop to chang month   ####################
##################################################
cat("\n***\nEnd of monthly calculation***\n\nPlot and Conclusion\n")




