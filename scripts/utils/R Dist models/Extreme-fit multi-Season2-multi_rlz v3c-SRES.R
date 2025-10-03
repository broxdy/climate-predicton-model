#Extreme values

sub.fix = "climate"
#sub.fix = "stream"

#input.file = "monthly-obs_filled1971-2006(4)+WetDay+WetRatio_V2.csv"
#input.file = "stream-monthly.csv"

#input.file = "conclude rain all-rlz all-sta obs1971-2006 cal1971-2006.csv"
#obs.file = "DailyPCP1971-2006 24sta.csv"
#ylabel = "precipitation (mm/day)"

#input.file = "conclude temp all-rlz all-sta obs1971-2006 cal1971-2006.csv"
#obs.file = "DailyTMP1971-2006.csv"
#ylabel = "temperature (deg C)"

#input.file = "DailyGen cal1971-2000 sim1971-2006 SWAT results 30 lrz only avaliable OBSyear.csv"
#obs.file = "stream-monthly.csv"
#ylabel = "monthly runoff (cms)"

#input.file = "DailyGen cal1971-2000 sim1971-2006 FLW - MAX only avalible OBSyear.csv"
#obs.file = "stream-monthly.csv"
#ylabel = "monthly runoff (cms)"

#input.file = "max FLW 2000-2049 - A2.csv"
#obs.file = "stream-monthly.csv"
#ylabel = "monthly runoff (cms)"

input.file = "max FLW 2000-2096 - SRES.csv"
obs.file = "stream-monthly.csv"
ylabel = "monthly runoff (cms)"

#input.file = "All SRES future2000-2096 as RLZ+20c3m PCP.csv"
#obs.file = "monthly-obs_PCP1971-1999.csv"
#ylabel = "precipitation (mm/day)"

#dir_name = "Extreme-fit-ProbCal-multi-rlz-daily_PCP"
#dir_name = "Extreme-fit-ProbCal-multi-rlz-daily_TMP"
#dir_name = "Extreme-fit-ProbCal-multi-rlz-monthly_FLW"
#dir_name = "Extreme-fit-ProbCal-multi-rlz-monthly_FLW by MaxVal 20c3m"
#dir_name = "Extreme-fit-ProbCal-multi-rlz-monthly_FLW by MaxVal 2000-2049A2"
dir_name = "Extreme-fit-ProbCal-multi-rlz-monthly_FLW by MaxVal 2000-2096allSRES"
#dir_name = "Extreme-fit-ProbCal-multi-rlz-monthly_PCP 20c3m+SRES2000-2096"


rlz.n = 4
rlz.name = c("sim. 20c3m","sim. A1B","sim. A2","sim. B1")
#rlz.name = "SRES"
obs.name= "obs. 1971-1999"
#obs.name= "observation"
trend.name = "GEV trend"

hist.d = 50 # denstiy n of density plot
pt.size = 0.8 # size of dot plot for realizations

obs.lwd = 4
obs.lty = 4
##############################
## Realization plot symbols
#rlz.ptype = rep("b",rlz.n) # plot type of realizations
rlz.ptype = c("l","b","b","b") # plot type of realizations

#rlz.ltype = rep(1,rlz.n) # line type of realizations
rlz.ltype = c(0,0,0,0) # line type of realizations
obs.type = 0

#rlz.pch = rep(4,rlz.n) # plot point symbols of realizations
rlz.pch = c(4,1,2,3) # plot point symbols of realizations

#rlz.col = rep("black",rlz.n) # plot colour of realizations
rlz.col = c("black","red","blue","darkgreen") # plot colour of realizations

#rlz.lwd = rep(2,rlz.n) # plot colour of realizations
rlz.lwd = c(4,3,3,3) # plot colour of realizations

trend.col = "black"
trend.type = 3


prob.lim.plot = 120 # limit x-axis to xxx year

#return.use = c(2,5,10,15,20,25,30,40,50,60,70,80,100,150,200)
return.use = c(2,5,10,20,50,100)
return.use.plot = c(2,5,10,20,50,75,100,150,200)
prob.use = 1-1/return.use
prob.use.plot = 1-1/return.use.plot



maindir = "temp/accessory/R Dist models"
dir0=dirname(file.path("D:",paste(maindir),"dummy"))
L=file.exists(dir_name)  
if(!L) dirfile=dir.create(dir_name)
dirfile=(dir_name)
dir1=dirname(file.path("D:",paste(maindir),paste(dirfile),"dummy"))

cat("\nReading input")
m.org =  read.table(input.file, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
obs.org =  read.table(obs.file, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")


## Define season
define.3season = c(1,1,2,2,2,2,3,3,3,3,1,1) # define season for 1)winter, 2)summer and 3)rainy
define.4season = c(2,2,2,3,3,3,4,4,4,1,1,1) # define season each calendar month
define.12season = c(1,2,3,4,5,6,7,8,9,10,11,12) # define season each calendar month

library(evd)

n.row = dim(m.org)[1]
n.col = dim(m.org)[2]
n.col.obs = dim(obs.org)[2]
n.par = (n.col-3)/rlz.n

maxy.all = max(m.org[4:n.col],na.rm=TRUE)
miny.all = min(m.org[4:n.col],na.rm=TRUE)
xsample = seq(miny.all,maxy.all,1)

rtp.val = array(NA, c(n.col,length(return.use)*2))
rtp.val.obs = array(NA, c(n.col.obs,length(return.use)*2))
rtp.val3 = array(NA, c(n.col,length(return.use)*2*3))
rtp.val3.obs = array(NA, c(n.col.obs,length(return.use)*2*3))
rtp.val4 = array(NA, c(n.col,length(return.use)*2*4))
rtp.val4.obs = array(NA, c(n.col.obs,length(return.use)*2*4))
rtp.val12 = array(NA, c(n.col,length(return.use)*2*12))
rtp.val12.obs = array(NA, c(n.col.obs,length(return.use)*2*12))

gev.all.rlz = array(NA, c(n.col,length(xsample)))
gumbel.all.rlz = array(NA, c(n.col,length(xsample)))
gev.all.obs = array(NA, c(n.col.obs,length(xsample)))
gumbel.all.obs = array(NA, c(n.col.obs,length(xsample)))



########
## Drawing + Finding probability
########
setwd(dir1)
pdf(paste("ExtremProb-RTP ",sub.fix,".pdf",sep=""), width = 10 ,height = 8)
setwd(dir0)

par(mgp=c(2.6,0.9,0), ps =24, mfrow=c(1,1))
par(mai =  c(.8, .8, .8, .8))
par(oma =  c(.8, .8, 2, .8))

#############################
### Loop to find probability Annual
#############################

# Loop for col.i
for(col.i in 1:n.par){

# find max ylim
col.rang.rlz = ((col.i-1)*rlz.n+1+3):((col.i-1)*rlz.n+rlz.n+3)
maxy = max(m.org[,col.rang.rlz],obs.org[,col.i+3],na.rm=TRUE)

#############
# Plot OBS
#############

# Loop finding extreme OBS
cat("\nFinding extrem OBS")
data.obs = integer(0)
for(year.i in 1:length(unique(obs.org$year))){
sel.row = which(obs.org$year == unique(obs.org$year)[year.i])
if(max(obs.org[sel.row,col.i+3],na.rm=TRUE)>-100){data.obs = c(data.obs,max(obs.org[sel.row,col.i+3],na.rm=TRUE))}
}
#prob.use
x = data.obs[order(data.obs)]


F1 = try(fgev(data.obs),silent = TRUE) # GEV
F2 = try(fgev(data.obs, shape=0),silent = TRUE) # Gumbel
if(length(F1)<3){F1 = fgev(data.obs, std.err = FALSE)}# GEV
if(length(F2)<3){F2 = fgev(data.obs, shape=0, std.err = FALSE)}# Gumbel

xp1 = pgev(xsample,F1$estimate[1],F1$estimate[2],F1$estimate[3])
xp2 = pgev(xsample,F2$estimate[1],F2$estimate[2],shape=0)


#####
# Plot drawing finding prob.
xq1 = qgev(prob.use,F1$estimate[1],F1$estimate[2],F1$estimate[3])
xq2 = qgev(prob.use,F2$estimate[1],F2$estimate[2],shape=0)
xq1.plot = qgev(prob.use.plot,F1$estimate[1],F1$estimate[2],F1$estimate[3])
xq2.plot = qgev(prob.use.plot,F2$estimate[1],F2$estimate[2],shape=0)

xrp1 = pgev(xsample,F1$estimate[1],F1$estimate[2],F1$estimate[3]) # prob. from new data set
xrp2 = pgev(xsample,F2$estimate[1],F2$estimate[2],shape=0) # prob. from new data set

rtp.val.obs[col.i,1:length(return.use)] = xq1
rtp.val.obs[col.i,(length(return.use)+1):(length(return.use)*2)] = xq2

###### GEV+Gumbel ######
cat("-Plotting GEV+Gumbel")

# GEV
xq=xq1.plot
xrp = xrp1

xp = (1:length(x))/(length(x)+1)
xrl = 1/(1-xrp) # x-axis
xr  = xsample[order(xrl)]
xrl = xrl[order(xrl)]
gev.all.obs[col.i,] = xrl

plot(xrl,xr, type="l",log = "x",ylim=c(1,maxy+maxy*0.02),xlim = c(1,prob.lim.plot), xlab = "return period (year)",ylab = ylabel,lwd=2,lty=trend.type,col=trend.col,main=colnames(m.org)[col.i*rlz.n+3])

points(1/(1-xp),x,pch=16,col="darkblue")

# Gumbel
xq = xq2.plot
xrp = xrp2
xp = (1:length(x))/(length(x)+1)
xrl = 1/(1-xrp) # x-axis
xr  = xsample[order(xrl)]
xrl = xrl[order(xrl)]
gumbel.all.obs[col.i,] = xrl

#lines(xrl,xr, type="l",log = "x",lwd=2,lty=2,col="darkgreen")
#points(1/(1-xp),x,pch=16,col="darkblue")

###############
# loop for plotting rlz
###############

for(rlz.i in 1:rlz.n){
cat("\n -Loop 1 - Col :",col.i,"rlz:",rlz.i)

col.sel = (col.i-1)*rlz.n+rlz.i+3 
data.col = na.omit(m.org[,col.sel])


# Loop finding extreme
cat("\nFinding extrem values")
data.m = integer(0)
for(year.i in 1:length(unique(m.org$year))){
sel.row = which(m.org$year == unique(m.org$year)[year.i])
if(max(m.org[sel.row,col.sel],na.rm=TRUE)>-100){data.m = c(data.m,max(m.org[sel.row,col.sel],na.rm=TRUE))}
}


#prob.use
x = data.m[order(data.m)]

#xsample = seq(1,maxy,1)

F1 = try(fgev(data.m),silent = TRUE) # GEV
F2 = try(fgev(data.m, shape=0),silent = TRUE) # Gumbel

if(length(F1)<3){F1 = fgev(data.m, std.err = FALSE)}# GEV
if(length(F2)<3){F2 = fgev(data.m, shape=0, std.err = FALSE)}# Gumbel

xp1 = pgev(xsample,F1$estimate[1],F1$estimate[2],F1$estimate[3])
xp2 = pgev(xsample,F2$estimate[1],F2$estimate[2],shape=0)

xq1 = qgev(prob.use,F1$estimate[1],F1$estimate[2],F1$estimate[3])
xq2 = qgev(prob.use,F2$estimate[1],F2$estimate[2],shape=0)
xq1.plot = qgev(prob.use.plot,F1$estimate[1],F1$estimate[2],F1$estimate[3])
xq2.plot = qgev(prob.use.plot,F2$estimate[1],F2$estimate[2],shape=0)

xrp1 = pgev(xsample,F1$estimate[1],F1$estimate[2],F1$estimate[3]) # prob. from new data set
xrp2 = pgev(xsample,F2$estimate[1],F2$estimate[2],shape=0) # prob. from new data set

rtp.val[col.sel,1:length(return.use)] = xq1
rtp.val[col.sel,(length(return.use)+1):(length(return.use)*2)] = xq2

###### GEV+Gumbel ######
cat("-Plotting GEV+Gumbel")

# GEV
xq=xq1.plot
xrp = xrp1
xp = (1:length(x))/(length(x)+1)
xrl = 1/(1-xrp) # x-axis
xr  = xsample[order(xrl)]
xrl = xrl[order(xrl)]
#points(1/(1-xp),x,pch=4,col="darkblue",cex = 1.2)
points(1/(1-xp),x,pch=rlz.pch[rlz.i],col=rlz.col[rlz.i],cex = 1.2)
gev.all.rlz[col.sel,] = xrl
#cat("\nGEV:",xrl)
lines(gev.all.rlz[col.sel,],xr,lwd=2,lty=trend.type,col=trend.col)

# Gumbel
xq = xq2.plot
xrp = xrp2
xp = (1:length(x))/(length(x)+1)
xrl = 1/(1-xrp) # x-axis
xr  = xsample[order(xrl)]
xrl = xrl[order(xrl)]
#points(1/(1-xp),x,pch=4,col="black",cex = 1.2)
gumbel.all.rlz[col.sel,] = xrl
#cat("\nGumbel:",xrl)



}
### END loop rlz.i

#legend("bottomright",c("observed","obs.(GEV)","obs.(Gumbel)","realizations","max rlz.(GEV)","max rlz.(Gumbel)"),
#lty = c(0,2,2,0,1,1),pch=c(16,-1,-1,4,-1,-1),cex = 0.8,col=c("darkblue","red","green","black","red","green"),pt.cex=c(1,1,1,1,1,0.5),x.intersp=1.2,y.intersp=1.5,bg="white")
#mtext(colnames(m.org)[col.sel], outer = TRUE )

legend("bottomright",c("observed",rlz.name,trend.name),lty = c(obs.type,rlz.ltype,trend.type),pch=c(16,rlz.pch,-1),cex = 0.8,col=c("darkblue",rlz.col,trend.col),x.intersp=1.2,y.intersp=1.5,bg="white")



# Plot avg line of RLZ
if(rlz.n > 1){
#lines(colMeans(gev.all.rlz[col.rang.rlz,]),xr,lwd=2,lty=trend.type,col=trend.col)
#lines(colMeans(gumbel.all.rlz[col.rang.rlz,]),xr,lwd=2,lty=1,col="darkgreen")
}else{#lines(gev.all.rlz[col.rang.rlz,],xr,lwd=2,lty=trend.type,col=trend.col)
	#lines(gumbel.all.rlz[col.rang.rlz,],xr,lwd=2,lty=1,col="darkgreen")
	}










#################################
# Loop finding Seasonal extreme : 3season
#################################
season.ch = define.3season
season.all = unique(season.ch)[order(unique(season.ch))]
season.n = length(season.all)

# Loop s.i
for(s.i in 1:season.n){

## screen seasonal values
# obs
m2.obs = obs.org[,c(1:3,col.i)]
m2.obs[,4] = NA
all.m = which(season.ch == unique(season.ch)[s.i])
for(m.i in 1:length(all.m)){
	sel.m = which(obs.org$month == all.m[m.i])
	m2.obs[sel.m,4] = obs.org[sel.m,col.i+3]
}


# find max ylim
col.rang.rlz = ((col.i-1)*rlz.n+1+3):((col.i-1)*rlz.n+rlz.n+3)
#maxy = max(m.org[,col.rang.rlz])

#############
# Plot OBS
#############

# Loop finding extreme OBS
cat("\nSS3 - Finding extrem OBS")
data.obs = integer(0)
for(year.i in 1:length(unique(obs.org$year))){
sel.row = which(obs.org$year == unique(obs.org$year)[year.i])
if(max(m2.obs[sel.row,4],na.rm=TRUE)>-100){data.obs = c(data.obs,max(m2.obs[sel.row,4],na.rm=TRUE))}
}
#prob.use
x = data.obs[order(data.obs)]


F1 = try(fgev(data.obs),silent = TRUE) # GEV
F2 = try(fgev(data.obs, shape=0),silent = TRUE) # Gumbel
if(length(F1)<3){F1 = fgev(data.obs, std.err = FALSE)}# GEV
if(length(F2)<3){F2 = fgev(data.obs, shape=0, std.err = FALSE)}# Gumbel

xp1 = pgev(xsample,F1$estimate[1],F1$estimate[2],F1$estimate[3])
xp2 = pgev(xsample,F2$estimate[1],F2$estimate[2],shape=0)


#####
# Plot drawing finding prob.
xq1 = qgev(prob.use,F1$estimate[1],F1$estimate[2],F1$estimate[3])
xq2 = qgev(prob.use,F2$estimate[1],F2$estimate[2],shape=0)
xq1.plot = qgev(prob.use.plot,F1$estimate[1],F1$estimate[2],F1$estimate[3])
xq2.plot = qgev(prob.use.plot,F2$estimate[1],F2$estimate[2],shape=0)

xrp1 = pgev(xsample,F1$estimate[1],F1$estimate[2],F1$estimate[3]) # prob. from new data set
xrp2 = pgev(xsample,F2$estimate[1],F2$estimate[2],shape=0) # prob. from new data set

###### Record value of return period
###### GEV ######
rtp.val3.obs[col.i,1:length(return.use)+(s.i-1)*length(return.use)*2] = xq1
###### Gumbel ######
rtp.val3.obs[col.i,(length(return.use)+1):(length(return.use)*2)+(s.i-1)*length(return.use)*2] = xq2

###### GEV+Gumbel ######
cat("-Plotting GEV+Gumbel")

# GEV
xq=xq1.plot
xrp = xrp1
xp = (1:length(x))/(length(x)+1)
xrl = 1/(1-xrp) # x-axis
xr  = xsample[order(xrl)]
xrl = xrl[order(xrl)]
gev.all.obs[col.i,] = xrl
plot(xrl,xr, type="l",log = "x",ylim=c(1,maxy+maxy*0.02),xlim = c(1,prob.lim.plot), xlab = "return period (year)",ylab = ylabel,lwd=2,lty=trend.type,col=trend.col,main=paste(colnames(m.org)[col.i*rlz.n+3],"ss:",unique(season.ch)[s.i],"/",season.n))
points(1/(1-xp),x,pch=16,col="darkblue")

# Gumbel
xq = xq2.plot
xrp = xrp2
xp = (1:length(x))/(length(x)+1)
xrl = 1/(1-xrp) # x-axis
xr  = xsample[order(xrl)]
xrl = xrl[order(xrl)]
gumbel.all.obs[col.i,] = xrl

#lines(xrl,xr, type="l",log = "x",lwd=2,lty=2,col="darkgreen")
#points(1/(1-xp),x,pch=16,col="darkblue")

###############
# loop for plotting rlz
###############

for(rlz.i in 1:rlz.n){
cat("\n -Loop 1 - Col :",col.i,"rlz:",rlz.i)
col.sel = (col.i-1)*rlz.n+rlz.i+3 


## screen seasonal values
# Rlz
m2 = m.org[,c(1:3,col.sel)]
m2[,4] = NA
all.m = which(season.ch == unique(season.ch)[s.i])
for(m.i in 1:length(all.m)){
	sel.m = which(m.org$month == all.m[m.i])
	m2[sel.m,4] = m.org[sel.m,col.sel]
}



# Loop finding extreme
cat("\nFinding extrem values")
data.m = integer(0)
for(year.i in 1:length(unique(m2$year))){
sel.row = which(m2$year == unique(m2$year)[year.i])
if(max(m2[sel.row,4],na.rm=TRUE)>-100){data.m = c(data.m,max(m2[sel.row,4],na.rm=TRUE))}
}


#prob.use
x = data.m[order(data.m)]

#xsample = seq(1,maxy,1)

F1 = try(fgev(data.m),silent = TRUE) # GEV
F2 = try(fgev(data.m, shape=0),silent = TRUE) # Gumbel

if(length(F1)<3){F1 = fgev(data.m, std.err = FALSE)}# GEV
if(length(F2)<3){F2 = fgev(data.m, shape=0, std.err = FALSE)}# Gumbel

xp1 = pgev(xsample,F1$estimate[1],F1$estimate[2],F1$estimate[3])
xp2 = pgev(xsample,F2$estimate[1],F2$estimate[2],shape=0)

xq1 = qgev(prob.use,F1$estimate[1],F1$estimate[2],F1$estimate[3])
xq2 = qgev(prob.use,F2$estimate[1],F2$estimate[2],shape=0)
xq1.plot = qgev(prob.use.plot,F1$estimate[1],F1$estimate[2],F1$estimate[3])
xq2.plot = qgev(prob.use.plot,F2$estimate[1],F2$estimate[2],shape=0)

xrp1 = pgev(xsample,F1$estimate[1],F1$estimate[2],F1$estimate[3]) # prob. from new data set
xrp2 = pgev(xsample,F2$estimate[1],F2$estimate[2],shape=0) # prob. from new data set

###### GEV+Gumbel ######
cat("-Plotting GEV+Gumbel")

# GEV
xq=xq1.plot
xrp = xrp1
xp = (1:length(x))/(length(x)+1)
xrl = 1/(1-xrp) # x-axis
xr  = xsample[order(xrl)]
xrl = xrl[order(xrl)]
#points(1/(1-xp),x,pch=4,col="darkblue",cex = 1.2)
points(1/(1-xp),x,pch=rlz.pch[rlz.i],col=rlz.col[rlz.i],cex = 1.2)
gev.all.rlz[col.sel,] = xrl
lines(gev.all.rlz[col.sel,],xr,lwd=2,lty=trend.type,col=trend.col)

# Gumbel
xq = xq2.plot
xrp = xrp2
xp = (1:length(x))/(length(x)+1)
xrl = 1/(1-xrp) # x-axis
xr  = xsample[order(xrl)]
xrl = xrl[order(xrl)]
#points(1/(1-xp),x,pch=4,col="black",cex = 1.2)
gumbel.all.rlz[col.sel,] = xrl


###### Record value of return period
###### GEV ######
rtp.val3[col.sel,1:length(return.use)+(s.i-1)*length(return.use)*2] = xq1
###### Gumbel ######
rtp.val3[col.sel,(length(return.use)+1):(length(return.use)*2)+(s.i-1)*length(return.use)*2] = xq2


}
### END loop rlz.i

#legend("bottomright",c("observed","obs.(GEV)","obs.(Gumbel)","realizations","max rlz.(GEV)","max rlz.(Gumbel)"),
#lty = c(0,2,2,0,1,1),pch=c(16,-1,-1,4,-1,-1),cex = 0.8,col=c("darkblue","red","green","black","red","green"),pt.cex=c(1,1,1,1,1,0.5),x.intersp=1.2,y.intersp=1.5,bg="white")
#mtext(colnames(m2)[col.sel], outer = TRUE )

legend("bottomright",c("observed",rlz.name,trend.name),lty = c(obs.type,rlz.ltype,trend.type),pch=c(16,rlz.pch,-1),cex = 0.8,col=c("darkblue",rlz.col,trend.col),x.intersp=1.2,y.intersp=1.5,bg="white")

# Plot avg line of RLZ
if(rlz.n > 1){
#lines(colMeans(gev.all.rlz[col.rang.rlz,]),xr,lwd=2,lty=trend.type,col=trend.col)
#lines(colMeans(gumbel.all.rlz[col.rang.rlz,]),xr,lwd=2,lty=1,col="darkgreen")
}else{
	#lines(gev.all.rlz[col.rang.rlz,],xr,lwd=2,lty=trend.type,col=trend.col)
	#lines(gumbel.all.rlz[col.rang.rlz,],xr,lwd=2,lty=1,col="darkgreen")
	}

} #END s.i

c.name = integer(0)
for(s.i in 1:season.n){c.name = c(c.name,paste(paste("S",s.i,"-",season.n,sep=""),c(paste("GEV-T",return.use,sep=""),paste("Gumbel-T",return.use,sep=""))))}
colnames(rtp.val3) = c.name
colnames(rtp.val3.obs) = c.name

#################################
# END : 3season
#################################





#################################
# Loop finding Seasonal extreme : 4season
#################################
season.ch = define.4season
season.all = unique(season.ch)[order(unique(season.ch))]
season.n = length(season.all)

# Loop s.i
for(s.i in 1:season.n){

## screen seasonal values
# obs
m2.obs = obs.org[,c(1:3,col.i)]
m2.obs[,4] = NA
all.m = which(season.ch == unique(season.ch)[s.i])
for(m.i in 1:length(all.m)){
	sel.m = which(obs.org$month == all.m[m.i])
	m2.obs[sel.m,4] = obs.org[sel.m,col.i+3]
}


# find max ylim
col.rang.rlz = ((col.i-1)*rlz.n+1+3):((col.i-1)*rlz.n+rlz.n+3)
#maxy = max(m.org[,col.rang.rlz])

#############
# Plot OBS
#############

# Loop finding extreme OBS
cat("\nSS4 - Finding extrem OBS")
data.obs = integer(0)
for(year.i in 1:length(unique(obs.org$year))){
sel.row = which(obs.org$year == unique(obs.org$year)[year.i])
if(max(m2.obs[sel.row,4],na.rm=TRUE)>-100){data.obs = c(data.obs,max(m2.obs[sel.row,4],na.rm=TRUE))}
}
#prob.use
x = data.obs[order(data.obs)]


F1 = try(fgev(data.obs),silent = TRUE) # GEV
F2 = try(fgev(data.obs, shape=0),silent = TRUE) # Gumbel
if(length(F1)<3){F1 = fgev(data.obs, std.err = FALSE)}# GEV
if(length(F2)<3){F2 = fgev(data.obs, shape=0, std.err = FALSE)}# Gumbel

xp1 = pgev(xsample,F1$estimate[1],F1$estimate[2],F1$estimate[3])
xp2 = pgev(xsample,F2$estimate[1],F2$estimate[2],shape=0)


#####
# Plot drawing finding prob.
xq1 = qgev(prob.use,F1$estimate[1],F1$estimate[2],F1$estimate[3])
xq2 = qgev(prob.use,F2$estimate[1],F2$estimate[2],shape=0)
xq1.plot = qgev(prob.use.plot,F1$estimate[1],F1$estimate[2],F1$estimate[3])
xq2.plot = qgev(prob.use.plot,F2$estimate[1],F2$estimate[2],shape=0)

xrp1 = pgev(xsample,F1$estimate[1],F1$estimate[2],F1$estimate[3]) # prob. from new data set
xrp2 = pgev(xsample,F2$estimate[1],F2$estimate[2],shape=0) # prob. from new data set

###### Record value of return period
###### GEV ######
rtp.val4.obs[col.i,1:length(return.use)+(s.i-1)*length(return.use)*2] = xq1
###### Gumbel ######
rtp.val4.obs[col.i,(length(return.use)+1):(length(return.use)*2)+(s.i-1)*length(return.use)*2] = xq2

###### GEV+Gumbel ######
cat("-Plotting GEV+Gumbel")

# GEV
xq=xq1.plot
xrp = xrp1
xp = (1:length(x))/(length(x)+1)
xrl = 1/(1-xrp) # x-axis
xr  = xsample[order(xrl)]
xrl = xrl[order(xrl)]
gev.all.obs[col.i,] = xrl

plot(xrl,xr, type="l",log = "x",ylim=c(1,maxy+maxy*0.02),xlim = c(1,prob.lim.plot), xlab = "return period (year)",ylab = ylabel,lwd=2,lty=trend.type,col=trend.col,main=paste(colnames(m.org)[col.i*rlz.n+3],"ss:",unique(season.ch)[s.i],"/",season.n))
points(1/(1-xp),x,pch=16,col="darkblue")

# Gumbel
xq = xq2.plot
xrp = xrp2
xp = (1:length(x))/(length(x)+1)
xrl = 1/(1-xrp) # x-axis
xr  = xsample[order(xrl)]
xrl = xrl[order(xrl)]
gumbel.all.obs[col.i,] = xrl

#lines(xrl,xr, type="l",log = "x",lwd=2,lty=2,col="darkgreen")
#points(1/(1-xp),x,pch=16,col="darkblue")

###############
# loop for plotting rlz
###############

for(rlz.i in 1:rlz.n){
cat("\n -Loop 1 - Col :",col.i,"rlz:",rlz.i)
col.sel = (col.i-1)*rlz.n+rlz.i+3 


## screen seasonal values
# Rlz
m2 = m.org[,c(1:3,col.sel)]
m2[,4] = NA
all.m = which(season.ch == unique(season.ch)[s.i])
for(m.i in 1:length(all.m)){
	sel.m = which(m.org$month == all.m[m.i])
	m2[sel.m,4] = m.org[sel.m,col.sel]
}



# Loop finding extreme
cat("\nFinding extrem values")
data.m = integer(0)
for(year.i in 1:length(unique(m2$year))){
sel.row = which(m2$year == unique(m2$year)[year.i])
if(max(m2[sel.row,4],na.rm=TRUE)>-100){data.m = c(data.m,max(m2[sel.row,4],na.rm=TRUE))}
}


#prob.use
x = data.m[order(data.m)]

#xsample = seq(1,maxy,1)

F1 = try(fgev(data.m),silent = TRUE) # GEV
F2 = try(fgev(data.m, shape=0),silent = TRUE) # Gumbel

if(length(F1)<3){F1 = fgev(data.m, std.err = FALSE)}# GEV
if(length(F2)<3){F2 = fgev(data.m, shape=0, std.err = FALSE)}# Gumbel

xp1 = pgev(xsample,F1$estimate[1],F1$estimate[2],F1$estimate[3])
xp2 = pgev(xsample,F2$estimate[1],F2$estimate[2],shape=0)

xq1 = qgev(prob.use,F1$estimate[1],F1$estimate[2],F1$estimate[3])
xq2 = qgev(prob.use,F2$estimate[1],F2$estimate[2],shape=0)
xq1.plot = qgev(prob.use.plot,F1$estimate[1],F1$estimate[2],F1$estimate[3])
xq2.plot = qgev(prob.use.plot,F2$estimate[1],F2$estimate[2],shape=0)

xrp1 = pgev(xsample,F1$estimate[1],F1$estimate[2],F1$estimate[3]) # prob. from new data set
xrp2 = pgev(xsample,F2$estimate[1],F2$estimate[2],shape=0) # prob. from new data set

###### GEV+Gumbel ######
cat("-Plotting GEV+Gumbel")

# GEV
xq=xq1.plot
xrp = xrp1
xp = (1:length(x))/(length(x)+1)
xrl = 1/(1-xrp) # x-axis
xr  = xsample[order(xrl)]
xrl = xrl[order(xrl)]
#points(1/(1-xp),x,pch=4,col="darkblue",cex = 1.2)
points(1/(1-xp),x,pch=rlz.pch[rlz.i],col=rlz.col[rlz.i],cex = 1.2)
gev.all.rlz[col.sel,] = xrl
lines(gev.all.rlz[col.sel,],xr,lwd=2,lty=trend.type,col=trend.col)


# Gumbel
xq = xq2.plot
xrp = xrp2
xp = (1:length(x))/(length(x)+1)
xrl = 1/(1-xrp) # x-axis
xr  = xsample[order(xrl)]
xrl = xrl[order(xrl)]
#points(1/(1-xp),x,pch=4,col="black",cex = 1.2)
gumbel.all.rlz[col.sel,] = xrl


###### Record value of return period
###### GEV ######
rtp.val4[col.sel,1:length(return.use)+(s.i-1)*length(return.use)*2] = xq1
###### Gumbel ######
rtp.val4[col.sel,(length(return.use)+1):(length(return.use)*2)+(s.i-1)*length(return.use)*2] = xq2


}
### END loop rlz.i

#legend("bottomright",c("observed","obs.(GEV)","obs.(Gumbel)","realizations","max rlz.(GEV)","max rlz.(Gumbel)"),
#lty = c(0,2,2,0,1,1),pch=c(16,-1,-1,4,-1,-1),cex = 0.8,col=c("darkblue","red","green","black","red","green"),pt.cex=c(1,1,1,1,1,0.5),x.intersp=1.2,y.intersp=1.5,bg="white")
#mtext(colnames(m2)[col.sel], outer = TRUE )

legend("bottomright",c("observed",rlz.name,trend.name),lty = c(obs.type,rlz.ltype,trend.type),pch=c(16,rlz.pch,-1),cex = 0.8,col=c("darkblue",rlz.col,trend.col),x.intersp=1.2,y.intersp=1.5,bg="white")

# Plot avg line of RLZ
if(rlz.n >1){
#lines(colMeans(gev.all.rlz[col.rang.rlz,]),xr,lwd=2,lty=trend.type,col=trend.col)
#lines(colMeans(gumbel.all.rlz[col.rang.rlz,]),xr,lwd=2,lty=1,col="darkgreen")
}else{#lines(gev.all.rlz[col.rang.rlz,],xr,lwd=2,lty=trend.type,col=trend.col)
	#lines(gumbel.all.rlz[col.rang.rlz,],xr,lwd=2,lty=1,col="darkgreen")
	}


} #END s.i

c.name = integer(0)
for(s.i in 1:season.n){c.name = c(c.name,paste(paste("S",s.i,"-",season.n,sep=""),c(paste("GEV-T",return.use,sep=""),paste("Gumbel-T",return.use,sep=""))))}
colnames(rtp.val4) = c.name
colnames(rtp.val4.obs) = c.name

#################################
# END : 4season
#################################




#################################
# Loop finding Seasonal extreme : 12season
#################################
season.ch = define.12season
season.all = unique(season.ch)[order(unique(season.ch))]
season.n = length(season.all)

# Loop s.i
for(s.i in 1:season.n){

## screen seasonal values
# obs
m2.obs = obs.org[,c(1:3,col.i)]
m2.obs[,4] = NA
all.m = which(season.ch == unique(season.ch)[s.i])
for(m.i in 1:length(all.m)){
	sel.m = which(obs.org$month == all.m[m.i])
	m2.obs[sel.m,4] = obs.org[sel.m,col.i+3]
}


# find max ylim
col.rang.rlz = ((col.i-1)*rlz.n+1+3):((col.i-1)*rlz.n+rlz.n+3)
#maxy = max(m.org[,col.rang.rlz])

#############
# Plot OBS
#############

# Loop finding extreme OBS
cat("\nSS12 - Finding extrem OBS")
data.obs = integer(0)
for(year.i in 1:length(unique(obs.org$year))){
sel.row = which(obs.org$year == unique(obs.org$year)[year.i])
if(max(m2.obs[sel.row,4],na.rm=TRUE)>-100){data.obs = c(data.obs,max(m2.obs[sel.row,4],na.rm=TRUE))}
}
#prob.use
x = data.obs[order(data.obs)]


F1 = try(fgev(data.obs),silent = TRUE) # GEV
F2 = try(fgev(data.obs, shape=0),silent = TRUE) # Gumbel
if(length(F1)<3){F1 = fgev(data.obs, std.err = FALSE)}# GEV
if(length(F2)<3){F2 = fgev(data.obs, shape=0, std.err = FALSE)}# Gumbel

xp1 = pgev(xsample,F1$estimate[1],F1$estimate[2],F1$estimate[3])
xp2 = pgev(xsample,F2$estimate[1],F2$estimate[2],shape=0)


#####
# Plot drawing finding prob.
xq1 = qgev(prob.use,F1$estimate[1],F1$estimate[2],F1$estimate[3])
xq2 = qgev(prob.use,F2$estimate[1],F2$estimate[2],shape=0)
xq1.plot = qgev(prob.use.plot,F1$estimate[1],F1$estimate[2],F1$estimate[3])
xq2.plot = qgev(prob.use.plot,F2$estimate[1],F2$estimate[2],shape=0)

xrp1 = pgev(xsample,F1$estimate[1],F1$estimate[2],F1$estimate[3]) # prob. from new data set
xrp2 = pgev(xsample,F2$estimate[1],F2$estimate[2],shape=0) # prob. from new data set

###### Record value of return period
###### GEV ######
rtp.val12.obs[col.i,1:length(return.use)+(s.i-1)*length(return.use)*2] = xq1
###### Gumbel ######
rtp.val12.obs[col.i,(length(return.use)+1):(length(return.use)*2)+(s.i-1)*length(return.use)*2] = xq2

###### GEV+Gumbel ######
cat("-Plotting GEV+Gumbel")

# GEV
xq=xq1.plot
xrp = xrp1
xp = (1:length(x))/(length(x)+1)
xrl = 1/(1-xrp) # x-axis
xr  = xsample[order(xrl)]
xrl = xrl[order(xrl)]
gev.all.obs[col.i,] = xrl

plot(xrl,xr, type="l",log = "x",ylim=c(1,maxy+maxy*0.02),xlim = c(1,prob.lim.plot), xlab = "return period (year)",ylab = ylabel,lwd=2,lty=trend.type,col=trend.col,main=paste(colnames(m.org)[col.i*rlz.n+3],"ss:",unique(season.ch)[s.i],"/",season.n))
points(1/(1-xp),x,pch=16,col="darkblue")

# Gumbel
xq = xq2.plot
xrp = xrp2
xp = (1:length(x))/(length(x)+1)
xrl = 1/(1-xrp) # x-axis
xr  = xsample[order(xrl)]
xrl = xrl[order(xrl)]
gumbel.all.obs[col.i,] = xrl

#lines(xrl,xr, type="l",log = "x",lwd=2,lty=2,col="darkgreen")
#points(1/(1-xp),x,pch=16,col="darkblue")

###############
# loop for plotting rlz
###############

for(rlz.i in 1:rlz.n){
cat("\n -Loop 1 - Col :",col.i,"rlz:",rlz.i)
col.sel = (col.i-1)*rlz.n+rlz.i+3 


## screen seasonal values
# Rlz
m2 = m.org[,c(1:3,col.sel)]
m2[,4] = NA
all.m = which(season.ch == unique(season.ch)[s.i])
for(m.i in 1:length(all.m)){
	sel.m = which(m.org$month == all.m[m.i])
	m2[sel.m,4] = m.org[sel.m,col.sel]
}



# Loop finding extreme
cat("\nFinding extrem values")
data.m = integer(0)
for(year.i in 1:length(unique(m2$year))){
sel.row = which(m2$year == unique(m2$year)[year.i])
if(max(m2[sel.row,4],na.rm=TRUE)>-100){data.m = c(data.m,max(m2[sel.row,4],na.rm=TRUE))}
}


#prob.use
x = data.m[order(data.m)]

#xsample = seq(1,maxy,1)

F1 = try(fgev(data.m),silent = TRUE) # GEV
F2 = try(fgev(data.m, shape=0),silent = TRUE) # Gumbel

if(length(F1)<3){F1 = fgev(data.m, std.err = FALSE)}# GEV
if(length(F2)<3){F2 = fgev(data.m, shape=0, std.err = FALSE)}# Gumbel

xp1 = pgev(xsample,F1$estimate[1],F1$estimate[2],F1$estimate[3])
xp2 = pgev(xsample,F2$estimate[1],F2$estimate[2],shape=0)

xq1 = qgev(prob.use,F1$estimate[1],F1$estimate[2],F1$estimate[3])
xq2 = qgev(prob.use,F2$estimate[1],F2$estimate[2],shape=0)
xq1.plot = qgev(prob.use.plot,F1$estimate[1],F1$estimate[2],F1$estimate[3])
xq2.plot = qgev(prob.use.plot,F2$estimate[1],F2$estimate[2],shape=0)

xrp1 = pgev(xsample,F1$estimate[1],F1$estimate[2],F1$estimate[3]) # prob. from new data set
xrp2 = pgev(xsample,F2$estimate[1],F2$estimate[2],shape=0) # prob. from new data set

###### GEV+Gumbel ######
cat("-Plotting GEV+Gumbel")

# GEV
xq=xq1.plot
xrp = xrp1
xp = (1:length(x))/(length(x)+1)
xrl = 1/(1-xrp) # x-axis
xr  = xsample[order(xrl)]
xrl = xrl[order(xrl)]

#points(1/(1-xp),x,pch=4,col="darkblue",cex = 1.2)
points(1/(1-xp),x,pch=rlz.pch[rlz.i],col=rlz.col[rlz.i],cex = 1.2)
gev.all.rlz[col.sel,] = xrl
lines(gev.all.rlz[col.sel,],xr,lwd=2,lty=trend.type,col=trend.col)


# Gumbel
xq = xq2.plot
xrp = xrp2
xp = (1:length(x))/(length(x)+1)
xrl = 1/(1-xrp) # x-axis
xr  = xsample[order(xrl)]
xrl = xrl[order(xrl)]
#points(1/(1-xp),x,pch=4,col="black",cex = 1.2)
gumbel.all.rlz[col.sel,] = xrl


###### Record value of return period
###### GEV ######
rtp.val12[col.sel,1:length(return.use)+(s.i-1)*length(return.use)*2] = xq1
###### Gumbel ######
rtp.val12[col.sel,(length(return.use)+1):(length(return.use)*2)+(s.i-1)*length(return.use)*2] = xq2


}
### END loop rlz.i

#legend("bottomright",c("observed","obs.(GEV)","obs.(Gumbel)","realizations","max rlz.(GEV)","max rlz.(Gumbel)"),
#lty = c(0,2,2,0,1,1),pch=c(16,-1,-1,4,-1,-1),cex = 0.8,col=c("darkblue","red","green","black","red","green"),pt.cex=c(1,1,1,1,1,0.5),x.intersp=1.2,y.intersp=1.5,bg="white")
#mtext(colnames(m2)[col.sel], outer = TRUE )

legend("bottomright",c("observed",rlz.name,trend.name),lty = c(obs.type,rlz.ltype,trend.type),pch=c(16,rlz.pch,-1),cex = 0.8,col=c("darkblue",rlz.col,trend.col),x.intersp=1.2,y.intersp=1.5,bg="white")

# Plot avg line of RLZ
if(rlz.n >1){
#lines(colMeans(gev.all.rlz[col.rang.rlz,]),xr,lwd=2,lty=trend.type,col=trend.col)
#lines(colMeans(gumbel.all.rlz[col.rang.rlz,]),xr,lwd=2,lty=1,col="darkgreen")
}else{#lines(gev.all.rlz[col.rang.rlz,],xr,lwd=2,lty=trend.type,col=trend.col)
	#lines(gumbel.all.rlz[col.rang.rlz,],xr,lwd=2,lty=1,col="darkgreen")
	}


} #END s.i

c.name = integer(0)
for(s.i in 1:season.n){c.name = c(c.name,paste(paste("S",s.i,"-",season.n,sep=""),c(paste("GEV-T",return.use,sep=""),paste("Gumbel-T",return.use,sep=""))))}
colnames(rtp.val12) = c.name
colnames(rtp.val12.obs) = c.name

#################################
# END : 4season
#################################




}
### END Loop col.i to find probability

dev.off()


cat("\nWriting return period")

colnames(rtp.val) = c(paste("GEV-T",return.use,sep=""),paste("Gumbel-T",return.use,sep=""))
colnames(rtp.val.obs) = c(paste("GEV-T",return.use,sep=""),paste("Gumbel-T",return.use,sep=""))
rownames(rtp.val) = colnames(m.org)
rownames(rtp.val.obs) = colnames(obs.org)
rownames(rtp.val3) = colnames(m.org)
rownames(rtp.val3.obs) = colnames(obs.org)
rownames(rtp.val4) = colnames(m.org)
rownames(rtp.val4.obs) = colnames(obs.org)
rownames(rtp.val12) = colnames(m.org)
rownames(rtp.val12.obs) = colnames(obs.org)

setwd(dir1)
write.csv(rtp.val,file=paste("returnT-",sub.fix,".csv",sep=""))
write.csv(rtp.val3,file=paste("returnT-3ss",sub.fix,".csv",sep=""))
write.csv(rtp.val4,file=paste("returnT-4ss",sub.fix,".csv",sep=""))
write.csv(rtp.val.obs,file=paste("OBSreturnT-",sub.fix,".csv",sep=""))
write.csv(rtp.val3.obs,file=paste("OBSreturnT-3ss",sub.fix,".csv",sep=""))
write.csv(rtp.val4.obs,file=paste("OBSreturnT-4ss",sub.fix,".csv",sep=""))
setwd(dir0)

cat("\nEnd of script\n")


