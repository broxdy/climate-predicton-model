#load libraries needed
library(MASS)
library(stochmod)
library(mcmc)
library(msm)
library(hmm.discnp)
library(depmix)
library(date)
library(msm)


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


## calculate SPATTIALLY AUTOCORRELATED RANDOM NUMBER
ma.coef.n = 30 # number of ma.coef curve interval

# set number of seed to plot normal distribution [into seed.v.ecdf = runif(seed.v.ndist.n,min(v.predict[n,]),max(v.predict[n,]))]
seed.v.ndist.n = 1000

# set number of realization using in weather generator function
curve.gen.n = 1 # (have to be 1) to build curve of MA.coef and I(rain)
weather.gen.realiz.n = 1000 # (might be up to 1000 to neglect white noise) to build curve of MA.coef and I(rain)

#weather.gen.predict.realiz.n = 30 # (OK) realization to predict to build curve daily I(rain)
weather.gen.predict.realiz.n = 1 # realization to predict to build curve daily I(rain)


maindir = "Temp/Stochastic"
dir_name = "predict_v17b_predict rainyday"





## Input files (parameter estimation)
#data.f.daily = "Daily Filled all climate 1971-2006_except SLR ab1981 (bad prd)v2+WetDay+WetRatio.csv"
#data.f.daily = "Daily Filled all climate 1971-2006_except SLR ab1981 (bad prd)v2+WetDay+WetRatio-withDaySeries.csv"
data.f.daily = "Daily Filled all climate 1971-2006+Wet_V2.csv"
#data.f.monthly = "monthly-obs_filled1971-2006(4).csv"
daily.unusedcol.org = 2
monthly.unusedcol.org = 2

## Input files (rainfall generation)
future.f.mean.rain = "future-mean rain.csv"
future.f.rainy.day = "monthly-obs_WetDay1971-2006.csv"

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
# Future wet data
future.wetday.org =  read.table(future.f.rainy.day, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

# also remove "X478201" (col. 4)
future.monthly = future.monthly.org[,-c(1:2,4)]
future.wetday = future.wetday.org[,-c(1:2,4)]

monthly.month.col = 2
monthly.year.col = 1
future.month.list = future.monthly.org[,monthly.month.col]
future.year.list = future.monthly.org[,monthly.year.col]

future.all.year = unique(future.year.list)

################# rainy day files
# build rainy state matrix
# also remove "X478201" (col. 46)
daily.wet.state = data.daily.org[,-c(1:daily.unusedcol2.data,46)]
#daily.sumwet.state = data.daily.org[,70]
mcm.eq.table = integer(0)


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
par(ps = 12)
setwd(dir0)

# Define daily Moran's I parameters
moran.i.daily.all  = array(NA,c(year.n,12,length(unique(data.daily.org[,daily.day.col]))))
daily.moran.i.sim = array(NA,c(12,31))
daily.ma.coef  = array(NA,c(12,length(unique(data.daily.org[,daily.day.col]))))


##################################################
# Great loop to chang month   ####################
##################################################
for(month.i in c(1,6:10)){
cat("\n****************\n Month :",month.i,"\n****************\n")

# extract from data for station.n station(s)
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
				v.predict = array(NA, c(station.n,ma.coef.n,seed.v.ndist.n))
				v.re.predict = array(NA, c(station.n,ma.coef.n,seed.v.ndist.n))

				## chang the random number (u) following the range of ma.coef.rang
				cat("Calculate v values following rang of ma.coef\n")
				seed.u.m = array(runif(station.n*seed.v.ndist.n,0,1), c(station.n,1,seed.v.ndist.n))

				# 1) Generate V matrix by changing ma.coef and u [uniform random number]
				for(i in 1:ma.coef.n){
				for(m in 1:seed.v.ndist.n){
							v.predict[,i,m] = ma.coef.range[i]*(weight.m%*%seed.u.m[,,m])+seed.u.m[,,m]
							}
							}

				# 2) Generate seeds to re-produce v (normalized) after ( Normalize the uni-form model) from max and min of seed.u.m
				runthissub = FALSE
				if(runthissub){
				seed.u.m.predict = array(runif(station.n*seed.v.ndist.n,min(seed.u.m),max(seed.u.m)), c(station.n,1,seed.v.ndist.n))
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
																	re.seed.u.m = array(runif(station.n,0,1), c(station.n,1))
																	v.re.predict = ma.coef.range[n]*(weight.m%*%re.seed.u.m)+re.seed.u.m
																	v.normal[,n,rlz] = v.normal.model(v.re.predict)
																	}
														  }

							}

							# calculate rainfall amount at station n
							#if(dim(v.normal[,n,])[1] == 1){ rain.predict[,n] = mean(-log(1-v.normal[,n,])*array(as.numeric(mean.rain),c(station.n,curve.gen.n))) 
							#					}else{ rain.predict[,n] = rowMeans(-log(1-v.normal[,n,])*array(as.numeric(mean.rain),c(station.n,curve.gen.n))) }
							rain.predict[,n] = rowMeans(-log(1-v.normal[,n,])*array(as.numeric(mean.rain),c(station.n,curve.gen.n))) 

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
#for(i in 1:station.n){col.all = c(col.all,which(colnames(future.monthly)==colnames(weight.m)[i]))}
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
title(main=paste("Amount of Rainfalll at", station.n," stations for month:",month.i," [R2 =",round(summary(lm.fit)$r.squared,digits=2),"]"))







################ END of Parameter estimation phase (providing equation of I and ma.coef)
cat("\nFinish Parameter estimation")



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

plot(moran.i.daily.all)
hist(moran.i.daily.all, breaks=50)


##########
## Extract ma.coef from average of daily Moran's I into daily.ma.coef
daily.avg.moran.i = colMeans(moran.i.daily.all,na.rm = TRUE)


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


#### END parameter estimation ############################




###################################################
###################################################
# Forecast rainfall (rainfall generation)
###################################################

##########
## Generate V from ma.coef and random u
cat("\n\n- Generate random seed to prediction:",weather.gen.realiz.n, "seed(s)")

## 1) Generate seed to build v.normal.model.gen 
# Generate V matrix by changing ma.coef and u [uniform random number]
seed.u.gen.cal.model = array(runif(station.n*weather.gen.realiz.n,0,1), c(31,station.n,weather.gen.realiz.n))
v.predict.gen = array(NA, c(31,station.n, weather.gen.realiz.n))

for(day.i in 1:length(which(!is.na(daily.ma.coef[month.i,])))){
		for(m in 1:weather.gen.realiz.n){
							## Use daily ma.coef
							#v.predict.gen[day.i,,m] = daily.ma.coef[month.i,day.i]*(weight.m%*%seed.u.gen.cal.model[day.i,,m])+seed.u.gen.cal.model[day.i,,m]
							## Use monthly ma.coef
							v.predict.gen[day.i,,m] = mean(daily.ma.coef[month.i,],na.rm =TRUE)*(weight.m%*%seed.u.gen.cal.model[day.i,,m])+seed.u.gen.cal.model[day.i,,m]
						  }
		}


# set cuumulative distribution model
v.normal.model.gen = ecdf(v.predict.gen[,,])
max.predict.rain = 0


## 2) Generate seed to predict using v.normal.model.gen 
# set seed for v that is random parameter for rain generation
seed.u.gen.predict = array(runif(station.n*weather.gen.predict.realiz.n,min(seed.u.gen.cal.model),max(seed.u.gen.cal.model)), c(31,station.n,weather.gen.predict.realiz.n))
v.predict.gen.seed = array(runif(31*station.n*weather.gen.predict.realiz.n,min(v.predict.gen),max(v.predict.gen)), c(31,station.n, weather.gen.predict.realiz.n))

for(day.i in 1:length(which(!is.na(daily.ma.coef[month.i,])))){
		for(m in 1:weather.gen.predict.realiz.n){
							## Use daily ma.coef
							#v.predict.gen.seed[day.i,,m] = daily.ma.coef[month.i,day.i]*(weight.m%*%seed.u.gen.predict[day.i,,m])+seed.u.gen.predict[day.i,,m]
							## Use monthly ma.coef
							v.predict.gen.seed[day.i,,m] = mean(daily.ma.coef[month.i,], na.rm=TRUE)*(weight.m%*%seed.u.gen.predict[day.i,,m])+seed.u.gen.predict[day.i,,m]
						  }
		}




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
if(sum(dim(state.table))<4){q.table = rbind(c(-0.2,0.2),c(0.2,-0.8))}else{q.table = crudeinits.msm(state.state ~ day.state, year.state,qmatrix = state.table)}
#q.table = crudeinits.msm(state.state ~ serie.state, year.state,qmatrix = state.table)
rainy.msm = msm(state.state ~ day.state, subject = year.state, qmatrix = q.table, obstype = 2)
prob.msm = prevalence.msm(rainy.msm)$"Expected percentages"[2,1]

# Print results
cat("\nRainy day preidct # Station :",station.title[i],"(",i,")","Year :",k.year[k]," Month :",month.i," -- q table:",format(q.table,digits= 1)," - Critical Prob.",format(prob.msm,digits= 1))

plotthissub = FALSE
if(plotthissub){
plot.prevalence.msm(rainy.msm)
mtext(paste("Station:",station.title[i],"Month: ",month.i))
}

sumval.state.eq = rbind(sumval.state.eq,cbind(prob.rainy.day,prob.msm))

}
# END year loop
############

plot(sumval.state.eq, main = paste("Station:",station.title[i],"  Month:",month.i),ylim = c(0,100),xlim=c(0,31))
mcm.eq = lm(prob.msm~prob.rainy.day,data = as.data.frame(sumval.state.eq))
abline(mcm.eq)
mcm.eq.table = rbind(mcm.eq.table,c(i,station.title[i],month.i,mcm.eq$coefficients,mean(abs(mcm.eq$residuals)),(mean((mcm.eq$residuals)^2))^.5))

#}
# END Month loop
############

}
# END station loop
############
cat("\nRecord mcm.eq.table")
colnames(mcm.eq.table)[c(1:3,6,7)] = c("Statio.i","Station","month","abs.mean.error","rmsq.error")

setwd(dir1)
write.csv(mcm.eq.table, file = paste("mcm eq table",month.i,".csv"))
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
				# for averag daily rain input
				mean.rain[i] = future.monthly[row.sel[month.i],i]*day.in.month/future.wetday[row.sel[month.i],i]

				# for monthly rain input
				#mean.rain[i] = future.monthly[row.sel[month.i],i]/future.wetday[row.sel[month.i],i]

				}
mean.rain[which(is.na(mean.rain))] = 0

#####################
# Define rainy day
#####################
#rainy.day = sample(0:1,length(which(!is.na(daily.avg.moran.i[month.i,]))),replace=TRUE)
#rainy.day = sample(1,length(which(!is.na(daily.avg.moran.i[month.i,]))),replace=TRUE)
rainy.n = future.wetday[row.sel[month.i],1]

## 1) using normal distribution to Random the position of rainy day
#rainy.n.pos1 = sample(1:day.in.month,rainy.n)


## 2) using stocahstic generation Makrov Chain
sel.row = which(as.real(mcm.eq.table[,3])==month.i)
coef.1 = as.real(mcm.eq.table[sel.row,4])
coef.2 = as.real(mcm.eq.table[sel.row,5])

# (still single value of rainy day) Calculate the limit prob of rainy day
prob.rainy = rainy.n/day.in.month*coef.2+coef.1

# Set the day postion which random prob. >  limit prob of rainy day
random.rain.prob = runif(day.in.month)

# use SAME seed as in v.normal.model.gen(v.predict.gen.seed) ==> v(i)
#random.rain.prob = array(v.normal.model.gen(v.predict.gen.seed[,,1]), c(31,station.n))
#plot(random.rain.prob1[1,])
#for(i in 2:31){lines(random.rain.prob1[i,],col=i)}



rainy.n.pos = array(NA,c(31,station.n))
rainy.day.n = array(NA,c(station.n))
#choose.rainy.day = array(NA,c(31,station.n))
rainy.day = array(0, c(day.in.month,station.n))

# Define rainy day for every station
for(sta in 1:station.n){
cat("\n-Station",station.title[sta],"(",sta,") :")
##################################################################
#post.random.prob = which(random.rain.prob[1:day.in.month,sta]*100 > prob.rainy[sta])
post.random.prob = which(random.rain.prob*100 > prob.rainy[sta])
if(length(post.random.prob) > 0){rainy.n.pos[1:length(post.random.prob),sta] = post.random.prob
					  }else{rainy.n.pos[1:length(post.random.prob),sta] = 0}
##################################################################


##
cat(length(which(!is.na(rainy.n.pos[,sta]))),"day(s)=",rainy.n.pos[which(!is.na(rainy.n.pos[,sta])),sta])

rainy.day[rainy.n.pos[which(!is.na(rainy.n.pos[,sta])),sta],sta] = 1
rainy.day.n[sta] = length(which(rainy.day[,sta]==1))
#choose.rainy.day[,sta] = which(rainy.day[,sta]==1)

#if(rainy.day.n[sta] == 0){	cat("** NO rain in this month")
#cat("\nTotal Wetday = ",rainy.day.n[sta])					

}
# END Define rainy day for every station


# Define unique rainy day
unique.rainy.day.n = unique(rainy.day.n)[order(unique(rainy.day.n))] # number day in each rainy day classification
unique.n = length(unique.rainy.day.n) # number of classification
sel.unique.station = array(NA,c(station.n,unique.n)) # station for each rainy day classification
sel.unique.station.n = rep(NA, unique.n) # number of station for each rainy day classification
pos.unique.rain.day = array(NA,c(31,unique.n)) # day position for each rainy day classification
for(uni in 1:unique.n){
			n.sel.station = which(rainy.day.n == unique.rainy.day.n[uni])
			sel.unique.station[1:length(n.sel.station),uni] = n.sel.station
			sel.unique.station.n[uni] = length(n.sel.station)
			pos.unique.rain.day[,uni]= rainy.n.pos[,sel.unique.station[1,uni]]
			}



# END Define rainy day for every station
# send limit.rainy.day.n  # number of rainy day (filled at least 1 day)
# send rainy.day.n # real number of rainy day (can be zero)
# send rainy.n.pos # day which is rainy

# unique.n to rain.predict function # number of classification
# unique.rainy.day.n # number of day in each rainy day classification
# sel.unique.station  # station for each rainy day classification
# sel.unique.station.n  # number of station for each rainy day classification
# pos.unique.rain.day # day position for each rainy day classification










#######################################
### V (normalized) and rain.predict

# regenrate seed in case of max(v.normal.gen[,n,seed.n]) = 1
re.seed.u.m.all = array(runif(station.n*weather.gen.predict.realiz.n,0,1), c(station.n,weather.gen.predict.realiz.n))

# Define prediction parameters
#rain.predict.gen = array(NA, c(12,station.n,day.in.month)) # set the matrix of rainfall amount results
rain.predict.gen = array(NA, c(12,station.n,day.in.month,weather.gen.predict.realiz.n)) # set the matrix of rainfall amount results
rain.predict.gen.avg = array(NA, c(12,station.n,day.in.month)) # set the matrix of rainfall amount results

###
# Run each unique number of rainy day
for(uni in 1:unique.n){
	cat("\n\n--",month.i,"/",future.all.year[year.i],"- Generating for",unique.rainy.day.n[uni],"day(s):",station.title[sel.unique.station[1:sel.unique.station.n[uni],uni]])

	v.normal.gen = array(NA, c(station.n,day.in.month,weather.gen.predict.realiz.n))
	#v.normal.gen = array(NA, c(station.n,unique.rainy.day.n[uni],weather.gen.predict.realiz.n)) # set the matrix
	#v.normal.gen = array(NA, c(station.n,limit.rainy.day.n,weather.gen.realiz.n)) # set the matrix
	rownames(v.normal.gen) = rownames(weight.m)


	#
	## Generate rainfall from cummulative distribution model (v.normal.model.gen)
	# define for each day
	# cat("Generate v(i) from cuumulative distribution model")


	###
	# Check rainy.day.n= 0, in case no rainy day this month, set all daily rain to 0 (skipping an error of no rain)
	###
	if(unique.rainy.day.n[uni] != 0){
	
	# Start unique rainy day
	for(n in 1:unique.rainy.day.n[uni]){
		cat("\nGenerating rain day :",n)

		for(seed.n in 1:weather.gen.predict.realiz.n){
									# Use  v.normal.model.gen from above fitting (distribution model) // v.predict.gen.seed from daily.ma.coef training
									v.normal.gen[,n,seed.n] = v.normal.model.gen(v.predict.gen.seed[pos.unique.rain.day[n,uni],,seed.n])
									# generate new V, if v = 1, to prevent Inf value
									runthissub = TRUE
									if(runthissub){
											  while(max(v.normal.gen[,n,seed.n], na.rm=TRUE) == 1){
													cat("\n+***+ regenerate V (prediction)\n")
													re.seed.u.m = re.seed.u.m.all[,seed.n]
													## Use daily ma.coef
													#v.re.predict = daily.ma.coef[month.i,pos.unique.rain.day[n,uni]]*(weight.m%*%re.seed.u.m)+re.seed.u.m
													## Use monthly ma.coef
													v.re.predict = mean(daily.ma.coef[month.i,], na.rm=TRUE)*(weight.m%*%re.seed.u.m)+re.seed.u.m
													v.normal.gen[,n,seed.n] = v.normal.model.gen(v.re.predict)
													}
											  }
									
	
									# calculate rainfall amount at station n
									rain.predict.gen[month.i,,pos.unique.rain.day[n,uni],seed.n] = -log(1-v.normal.gen[,n,seed.n])*array(as.numeric(mean.rain),c(station.n,1))
									} # END seed.n
		
		# Average the realization from rainy day (pos.unique.rain.day[n,uni]) and rainy station (sel.unique.station[1:sel.unique.station.n[uni],uni]) from rain.predict.gen.avg
		if(sel.unique.station.n[uni] == 1){
								# if have only one station
								rain.predict.gen.avg[month.i,sel.unique.station[1:sel.unique.station.n[uni],uni],pos.unique.rain.day[n,uni]] = mean(rain.predict.gen[month.i,sel.unique.station[1:sel.unique.station.n[uni],uni],pos.unique.rain.day[n,uni],],na.rm = TRUE)
								# if have multi station
						          	}else{
									if(!is.null(dim(rain.predict.gen[month.i,sel.unique.station[1:sel.unique.station.n[uni],uni],pos.unique.rain.day[n,uni],]))){ rain.predict.gen.avg[month.i,sel.unique.station[1:sel.unique.station.n[uni],uni],pos.unique.rain.day[n,uni]] = rowMeans(rain.predict.gen[month.i,sel.unique.station[1:sel.unique.station.n[uni],uni],pos.unique.rain.day[n,uni],],na.rm = TRUE) 
														}else{rain.predict.gen.avg[month.i,sel.unique.station[1:sel.unique.station.n[uni],uni],pos.unique.rain.day[n,uni]] = mean(rain.predict.gen[month.i,sel.unique.station[1:sel.unique.station.n[uni],uni],pos.unique.rain.day[n,uni],],na.rm = TRUE)	}
									}
		#rain.predict.gen.avg[month.i,,choose.rainy.day[n]] = rowMeans(rain.predict.gen[month.i,,choose.rainy.day[n],],na.rm = TRUE)

		cat(" / Day to rain :",pos.unique.rain.day[n,uni])
		} # END n


	###
	# start (ELSE) when zero rain // rainy.day.n[uni] = 0
	}else{rain.predict.gen.avg[month.i,,1:day.in.month] = 0}


}
###
# END Run each unique number of rainy day
###


# set daily rain of other non-rainy days to 0
for(i in 1:station.n){rain.predict.gen.avg[month.i,i,which(is.na(rain.predict.gen.avg[month.i,i,]))] = 0}


###
### END Generate rainfall from cummulative distribution model (v.normal.model.gen)
# Results are in rain.predict.gen.avg










#########################################
# Make a conclusion file, put results into rain.predict.conclude
# rotate matrix rain.predict.gen.avt and put into rain.predict.conclude
# ** Combine rain.predict.gen.avg INTO rain.predict.conclude
if((month.i+year.i) == 2){
				rain.predict.conclude = array(NA, c(366*length(future.all.year),station.n))
				colnames(rain.predict.conclude) = station.title
				start.org.date = as.integer(mdy.date(1, 1, future.all.year[1]))
				}

start.data.date = as.integer(mdy.date(month.i, 1, future.all.year[year.i]))
end.data.date = as.integer(mdy.date(month.i, day.in.month, future.all.year[year.i]))

if(length(which(!is.na(rain.predict.conclude[(start.data.date-start.org.date +1):(end.data.date-start.org.date + 1),])))>0){error.count =error.count+1}

rain.predict.conclude[(start.data.date-start.org.date +1):(end.data.date-start.org.date + 1),] = matrix(rain.predict.gen.avg[month.i,,1:day.in.month], ncol = station.n, nrow = day.in.month, byrow = TRUE)


#print.rain.predict.conclude = rain.predict.conclude[-which(is.na(rain.predict.conclude[,1])),]
setwd(dir1)
write.csv(rain.predict.conclude, file = "conclude rain.csv")
setwd(dir0)








# Loop to plot to pdf
cat("\n* Plot Rainfall each station in month:",month.i,":")
for(uni in 1:unique.n){

# define maximum rain.predict to set y-axis
if(pos.unique.rain.day[1,uni] == 0){	y.max =1
						}else{y.max = max(rain.predict.gen.avg[month.i,,pos.unique.rain.day[1:unique.rainy.day.n[uni],uni]])}

plot(colMeans(rain.predict.gen.avg[month.i,,]), type = "h",
	xlab = paste("Stations / Rainy day =",unique.rainy.day.n[uni],"day(s) at avg. rainfall =",round(mean.rain,digits=1),"mm.(chart avg.=",round(mean(rain.predict.gen.avg[month.i,,],na.rm = TRUE),digits=1),"mm.) of month:",month.i," year:",future.all.year[year.i]),
	ylab = "Rainfall (mm)", lty = 1, lwd = 5, ylim=c(0,y.max))
for(k in 1:station.n){
				lines(rain.predict.gen.avg[month.i,k,], type = "p", col = k)
				}

legend(x = "right",  bty = "n",  lty = c(rep(1,station.n)),  lwd = rep(2,station.n), col = c(1:station.n),  legend = rownames(weight.m))

cat(unique.rainy.day.n[uni],"- rain  plotted / ")

}
#END Loop to plot to pdf

							


#######
# Generate simulated daily Moran's
cat("\n- Generate simulated daily Moran's I\n")
for(day.i in 1:day.in.month){
			daily.moran.i.sim[month.i,day.i] = moran.i(rain.predict.gen.avg[month.i,,day.i],weight.m)
			}






}
###################################
# END Great loop to chang year   ##
###################################


}

dev.off()
##################
#### END change the amount of mean monhtly rainfall
##################
##################################################
# END Great loop to chang month   ####################
##################################################


cat("\n***\nEnd of monthly calculation***\n\nPlot and Conclusion\n")



# Generate simulated daily Moran's (from all direct rain realization)
cat("\nCalculate daily Moran's I")
moran.i.all.rlz = array(NA, c(12,31,weather.gen.predict.realiz.n))
moran.i.all.day = array(NA, c(12,31))
for(month.i in 1:12){
for(day.i in 1:31){
for(rlz.i in 1:weather.gen.predict.realiz.n){
			if(length(which(is.na(rain.predict.gen[month.i,,day.i,rlz])))==0){
								moran.i.all.rlz[month.i,day.i,rlz] = moran.i(rain.predict.gen[month.i,,day.i,rlz],weight.m)
														}
			}
			moran.i.all.day[month.i,day.i] = mean(moran.i.all.rlz[month.i,day.i,], na.rm = TRUE)
			}
			}





##################
## Plot Moran's I

setwd(dir1)
pdf(paste("Conclude I.pdf"), width = 15 ,height = 9)
setwd(dir0)
cat("\nPlot obs. and sim. daily Moran's I")
plot(1:(12*31),rep(0,12*31),type = "n",ylim = c(-1,1), xlab="Days",ylab="daily Moran's I")
for(i in 1:12){
				plotrawdata = FALSE
				if(plotrawdata){
				for(year.i in 1:year.n){
							lines(c((1+((i-1)*31)):(31+((i-1)*31))),moran.i.daily.all[year.i,i,],type = "p", lwd = 1, cex = 0.5)
							}
				}
				lines(c((1+((i-1)*31)):(31+((i-1)*31))),daily.avg.moran.i[i,], lwd = 2 ,col = "red", lty = 1)
				#lines(c((1+((i-1)*31)):(31+((i-1)*31))),daily.moran.i.sim[i,], lwd = 2 ,col = "blue", lty = 1)
				lines(c((1+((i-1)*31)):(31+((i-1)*31))),moran.i.all.day[i,], lwd = 2 ,col = "green", lty = 1)
				}
#legend(x = "top",  bty = "n",  lty = c(1,1,1),  lwd = c(2,2,2), col = c("red","blue","green"),  legend = c("Observed Moran's I","Simulated Moran's I (from mean rain)","Simulated Moran's I (average daily I)"))
legend(x = "top",  bty = "n",  lty = c(1,1),  lwd = c(2,2), col = c("red","green"),  legend = c("Observed Moran's I","Simulated Moran's I (average daily I)"))
dev.off()


# Combine daily data
total.daily.ma.coef = integer(0)
total.daily.avg.moran.i = integer(0)
for(i in 1:12){
total.daily.ma.coef = c(total.daily.ma.coef,daily.ma.coef[i,])
total.daily.avg.moran.i = c(total.daily.avg.moran.i,daily.avg.moran.i[i,])
}

## Plot selected moving average coef. whole year
setwd(dir1)
pdf(paste("Conclude I and ma-coef.pdf"), width = 15 ,height = 9)
setwd(dir0)
par(mar=c(5,4,4,5)+.1)
plot(total.daily.ma.coef, type = "l", lwd = 2 ,col = "red", lty = 1, ylab = "Moving average coef.")
par(new = TRUE)
plot(total.daily.avg.moran.i, type = "l", yaxt = "n" , lwd = 2 , col = "blue", lty = 1, ylab ="")
axis(4)
legend(x = "top",  bty = "n",  lty = c(1,1),  lwd = c(2,2), col = c("red","blue"),  legend = c("Selected moving average coef. (left  y-axis)","Observed Moran's I (right y-axis)"))
mtext("Moran's I", side=4, line = 3, col = "blue")

dev.off()





###########################
## Plot Rainfall prediction
rain.gen = array(NA,c(366,station.n))
colnames(rain.gen) = station.title
filled.rain.gen = 0 # to count values filled into rain.gen

setwd(dir1)
pdf(paste("Conclude Rain Generation.pdf"), width = 15 ,height = 9)
par(ps =10)
setwd(dir0)

cat("\nPlot daily rainfall prediction")
plot(1:(12*31),rep(0,12*31),type = "n",ylim = c(0,max(rain.predict.gen.avg, na.rm = TRUE)), ylab="Rainfall (mm.)", xlab ="Days")
#plot(1:(12*31),rep(0,12*31),type = "n")

for(i in 1:12){
			lines(c((1+((i-1)*31)):(31+((i-1)*31))),colMeans(rain.predict.gen.avg[i,,]), lwd = 2, type ="h")

			#count.filled = length(which(!is.na(rain.predict.gen.avg[i,1,])))
			for(n in 1:station.n){
							lines(c((1+((i-1)*31)):(31+((i-1)*31))),rain.predict.gen.avg[i,n,],col = (n),type = "p", lwd = 1, cex = 0.6)
							#rain.gen[(filled.rain.gen+1):(filled.rain.gen+count.filled),n] = rain.predict.gen.avg[i,n,which(!is.na(rain.predict.gen.avg[i,n,]))]
							}
			#filled.rain.gen = length(which(!is.na(rain.gen[,1])))
			}
dev.off()


# Write generated results
#setwd(dir1)
#write.csv(rain.gen, file = "generated rain.csv")
#setwd(dir0)




cat("\nError :",error.count,"\n")

			   






