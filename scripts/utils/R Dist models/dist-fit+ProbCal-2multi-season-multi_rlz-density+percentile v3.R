#Distribution model

#run.detach = TRUE
run.detach = FALSE
if(run.detach){
detach(package:qAnalyst)
detach(package:car)
dev.off()
rm(list=ls(all=TRUE))
}

## Define lower and upper bound to calculate density
lb=0.0000001; ub=1
prob.use = c(0.01,0.02,0.05,0.1,0.2,0.25,0.5,0.75,0.95,0.99)

sub.fix = "climate"
#sub.fix = "stream"

## *** ###
## Plotting Density or Percentile, now plotting both
## *** ###
#density.plot = FALSE # if FALSE, then plot Percentile

## Realization show
seq.rlz.length = 100 #seq.show.rlz = seq(from=1,to=10, length.out=seq.rlz)
#input.file = "monthly-obs_filled1971-2006(4)+WetDay+WetRatio_V2.csv"
#input.file = "stream-monthly.csv"


#input.file = "conclude rain all-rlz all-sta obs1971-2006 cal1971-2006.csv"
#obs.file = "DailyPCP1971-2006 24sta.csv"
#ylabel = "precipitation (mm/day)"

#input.file = "conclude temp all-rlz all-sta obs1971-2006 cal1971-2006.csv"
#obs.file = "DailyTMP1971-2006.csv"
#ylabel = "temperature (deg C)"

#obs.file = "stream-monthly.csv"
#input.file = "DailyGen cal1971-2000 sim1971-2006 SWAT results 30 lrz only avaliable OBSyear.csv"
#ylabel = "monthly runoff (cms)"

#input.file = "Longterm optimal vrf 1986-1999.csv"
#obs.file = "monthly-obs_filled vrf 1986-1999+WetDay+WetRatio_V2.csv"
#ylabel = ""

input.file = "All SRES future as RLZ.csv"
obs.file = "monthly-obs_filled1971-2000_V2.csv"
ylabel = ""

dense.from.0 = TRUE # True when data is rainfall and runoff

#dir_name = "Dis-fit-Prob-multi_rlz PCP obs1971-2006 cal1971-2006"
#dir_name = "Dis-fit-Prob-multi_rlz TMP obs1971-2006 cal1971-2006"
#dir_name = "Dis-fit-Prob-multi_rlz FLW obs1971-2006 cal1971-2006"
#dir_name = "Dis-fit-Prob-v3 monthly downsacle vrf1985-1999 Density from 0"
dir_name = "Dis-fit-Prob-v3 future 3 SRES Density from 0"

#rlz.n = 1
#rlz.name = "simulation"

#rlz.n = 30
#rlz.name = "realizations"

rlz.n = 3
#rlz.name = c("A1B","A2","B1")
rlz.name = "SRES"
obs.name= "1971-1999"

hist.d = 100 # denstiy n of density plot
pt.size = 0.8 # size of dot plot for realization



maindir = "temp/accessory/R Dist models"
dir0=dirname(file.path("D:",paste(maindir),"dummy"))
L=file.exists(dir_name)  
if(!L) dirfile=dir.create(dir_name)
dirfile=(dir_name)
dir1=dirname(file.path("D:",paste(maindir),paste(dirfile),"dummy"))

# Reading input
cat("\nReding input")
m.org =  read.table(input.file, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
obs.org =  read.table(obs.file, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")


## Define season
define.3season = c(1,1,2,2,2,2,3,3,3,3,1,1) # define season for 1)winter, 2)summer and 3)rainy
define.4season = c(2,2,2,3,3,3,4,4,4,1,1,1) # define season each calendar month
define.12season = c(1,2,3,4,5,6,7,8,9,10,11,12) # define season each calendar month


library(MASS)
library(qualityTools)

n.row = dim(m.org)[1]
n.col = dim(m.org)[2]
n.col.obs = dim(obs.org)[2]
n.par = (n.col-3)/rlz.n

par.val = array(NA, c(n.col,12))
prb.val = array(NA, c(n.col,length(prob.use)+2))
prb.val3 = array(NA, c(n.col,(length(prob.use)+2)*length(unique(define.3season))))
prb.val4 = array(NA, c(n.col,(length(prob.use)+2)*length(unique(define.4season))))
prb.val12 = array(NA, c(n.col,(length(prob.use)+2)*length(unique(define.12season))))
par.val.obs = array(NA, c(n.col.obs,12))
prb.val.obs = array(NA, c(n.col.obs,length(prob.use)+2))
prb.val3.obs = array(NA, c(n.col.obs,(length(prob.use)+2)*length(unique(define.3season))))
prb.val4.obs = array(NA, c(n.col.obs,(length(prob.use)+2)*length(unique(define.4season))))
prb.val12.obs = array(NA, c(n.col.obs,(length(prob.use)+2)*length(unique(define.12season))))


#############################
# Section 1 for plotting denstiy
cat("\n***---****\n1)Plotting Density\n***---****")
density.plot = TRUE # if FALSE, then plot Percentile
#############################

setwd(dir1)
if(density.plot){
pdf(paste("Density plot.pdf",sep=""), width = 10 ,height = 8)
}else{
	pdf(paste("Percentile plot.pdf",sep=""), width = 10 ,height = 8)
	}
setwd(dir0)


for(col.i in 1:n.par){
cat("\nLoop NO season - Col :",col.i)
col.rang.rlz = ((col.i-1)*rlz.n+1+3):((col.i-1)*rlz.n+rlz.n+3)

obs.m = obs.org[,col.i+3]
data.obs = na.omit(obs.m)
x = data.obs[order(data.obs)]


#**########
# Change if member of data.m = 0, data.m -> 0.000001
#if(min(data.m, na.rm = TRUE) == 0){
#change.log = TRUE
#change.pos = which(data.m==0)
#data.m[change.pos] = 0.000001
#}else{change.log=FALSE}


par(mgp=c(2.5,1,0), ps =24, mfrow=c(1,1))
par(mai =  c(.8, .8, .8, .1))
par(oma =  c(.8, .8, 2, .8))

## Fitting Density or Percentile
xd = ecdf(x)
prob.res = quantile(x,probs = prob.use)
if(density.plot){
	# Density
	if(dense.from.0){
		xden = density(x,n=hist.d,from=0)
		}else{xden = density(x,n=hist.d)}
	y.lim.max = max(xden$y,density(na.omit(as.real(unlist(m.org[, col.rang.rlz]))))$y)+0.2
	plot(xden$x,xden$y,ylim=c(0,y.lim.max), type="l", xlab=ylabel, ylab="Density",main = paste(colnames(obs.org)[col.i+3],"- annual"),lwd = 4,col="red")
	legend("topright",c(rlz.name,"observation"),lwd=c(0,2),lty = c(0,1),pch=c(4,-1),cex = 1.2,col=c("black","red"))
	}else{
		# ECDF
		plot(x,xd(x),xlim=c(min(x),max(x)*1.1),ylim=c(min(xd(x))*0.8,1), type="l",xlab=ylabel, ylab="Percentile",main = paste(colnames(obs.org)[col.i+3],"- annual"),lwd=2,col="red")
		legend("bottomright",c(rlz.name,"observation"),lwd=c(0,2),lty = c(0,1),pch=c(4,-1),cex = 1.2,col=c("black","red"))
}



# Loop record multi-prob.
for(j in 1:length(prob.use)){
xval = prob.res[j]
xprb = xd(xval)
prb.val.obs[col.i+3,j] = xval
}
prb.val.obs[col.i+3,j+1] = mean(x)
prb.val.obs[col.i+3,j+2] = sd(x)

cat("\nRlz:")
####################
## Loop for rlz.i
for(rlz.i in 1:rlz.n){
cat(" ",rlz.i)
col.sel = (col.i-1)*rlz.n+rlz.i+3 

data.m = m.org[,col.sel]
data.m = na.omit(data.m)
x = data.m[order(data.m)]

## Fitting Density or Percentile
xd = ecdf(x)
prob.res = quantile(x,probs = prob.use)
if(density.plot){
	# Density
	if(dense.from.0){
		xden = density(x,n=hist.d,from=0)
		}else{xden = density(x,n=hist.d)}
	lines(xden$x,xden$y, type="p", pch = 4,cex = pt.size,col="black")
	}else{
		# ECDF
		seq.show.rlz = seq(from=1,to=length(x), length.out=seq.rlz.length)
		lines(x[seq.show.rlz],xd(x[seq.show.rlz]), type="p", pch = 4,cex = pt.size,col="black")
}

# Loop record multi-prob.
for(j in 1:length(prob.use)){
xval = prob.res[j]
xprb = xd(xval)
prb.val[col.sel,j] = xval
}
prb.val[col.sel,j+1] = mean(x)
prb.val[col.sel,j+2] = sd(x)

}
## END Loop for rlz.i

### END NO season #########



### 3 seasons #########
season.ch = define.3season
season.all = unique(season.ch)[order(unique(season.ch))]
season.n = length(season.all)

c.name = integer(0)
for(s.i in 1:season.n){c.name = c(c.name,paste(paste("S",s.i,"-",season.n,sep=""),c(paste(" Pr-",prob.use,sep=""),"mean","SD")))}
colnames(prb.val3.obs) = c.name
rownames(prb.val3.obs) = colnames(obs.org)
colnames(prb.val3) = c.name
rownames(prb.val3) = colnames(m.org)

cat("\nLoop 3 seasons - Col :",col.i)
# Loop s.i
for(s.i in 1:season.n){
data.m = integer(0)
all.m = which(season.ch == unique(season.ch)[s.i])
for(m.i in 1:length(all.m)){
	sel.m = which(obs.org$month == all.m[m.i])
	data.m = c(data.m,obs.org[sel.m,col.i+3])
}
data.m = na.omit(data.m)
x = data.m[order(data.m)]

## Fitting Density or Percentile
xd = ecdf(x)
prob.res = quantile(x,probs = prob.use)
if(density.plot){
	# Density
	if(dense.from.0){
		xden = density(x,n=hist.d,from=0)
		}else{xden = density(x,n=hist.d)}
	y.lim.max = max(xden$y,density(na.omit(as.real(unlist(m.org[, col.rang.rlz]))))$y)+0.2
	plot(xden$x,xden$y,ylim=c(0,y.lim.max), type="l", xlab=ylabel, ylab="Density",main = paste(colnames(obs.org)[col.i+3]," ss:",s.i,"/",season.n),lwd = 4, add=TRUE,col="red")
	legend("topright",c(rlz.name,"observation"),lwd=c(0,2),lty = c(0,1),pch=c(4,-1),cex = 1.2,col=c("black","red"))
	}else{
		# ECDF
		plot(x,xd(x),xlim=c(min(x),max(x)*1.1),ylim=c(min(xd(x))*0.8,1), type="l",xlab=ylabel, ylab="Percentile",main = paste(colnames(obs.org)[col.i+3]," ss:",s.i,"/",season.n),lwd=2,col="red")
		legend("bottomright",c(rlz.name,"observation"),lwd=c(0,2),lty = c(0,1),pch=c(4,-1),cex = 1.2,col=c("black","red"))
		}



# Loop plot multi-prob.
for(j in 1:length(prob.use)){
xval = prob.res[j]
xprb = xd(xval)
prb.val3.obs[col.i+3,j+(s.i-1)*(length(prob.use)+2)] = xval
}
prb.val3.obs[col.i+3,j+(s.i-1)*(length(prob.use)+2)+1] = mean(x)
prb.val3.obs[col.i+3,j+(s.i-1)*(length(prob.use)+2)+2] = sd(x)
#mtext(colnames(m.org)[col.i+3], outer = TRUE )


cat("\nRlz:")
####################
## Loop for rlz.i
for(rlz.i in 1:rlz.n){
cat(" ",rlz.i)
col.sel = (col.i-1)*rlz.n+rlz.i+3 

data.m = integer(0)
all.m = which(season.ch == unique(season.ch)[s.i])
for(m.i in 1:length(all.m)){
	sel.m = which(m.org$month == all.m[m.i])
	data.m = c(data.m,m.org[sel.m,col.sel])
}
data.m = na.omit(data.m)
x = data.m[order(data.m)]

## Fitting Density or Percentile
xd = ecdf(x)
prob.res = quantile(x,probs = prob.use)
if(density.plot){
	# Density
	if(dense.from.0){
		xden = density(x,n=hist.d,from=0)
		}else{xden = density(x,n=hist.d)}
	lines(xden$x,xden$y, type="p", pch = 4,cex = pt.size,col="black")
	}else{
		# ECDF
		seq.show.rlz = seq(from=1,to=length(x), length.out=seq.rlz.length)
		lines(x[seq.show.rlz],xd(x[seq.show.rlz]), type="p", pch = 4,cex = pt.size,col="black")
}


# Loop plot multi-prob.
for(j in 1:length(prob.use)){
xval = prob.res[j]
xprb = xd(xval)
prb.val3[col.sel,j+(s.i-1)*(length(prob.use)+2)] = xval
}
prb.val3[col.sel,j+(s.i-1)*(length(prob.use)+2)+1] = mean(x)
prb.val3[col.sel,j+(s.i-1)*(length(prob.use)+2)+2] = sd(x)
#mtext(colnames(m.org)[col.i+3], outer = TRUE )

}
## END Loop for rlz.i




} #END s.i
### END 3 seasons #########





### 4 seasons #########
season.ch = define.4season
season.all = unique(season.ch)[order(unique(season.ch))]
season.n = length(season.all)

c.name = integer(0)
for(s.i in 1:season.n){c.name = c(c.name,paste(paste("S",s.i,"-",season.n,sep=""),c(paste(" Pr-",prob.use,sep=""),"mean","SD")))}
colnames(prb.val4.obs) = c.name
rownames(prb.val4.obs) = colnames(obs.org)
colnames(prb.val4) = c.name
rownames(prb.val4) = colnames(m.org)

cat("\nLoop 4 seasons - Col :",col.i)

# Loop s.i
for(s.i in 1:season.n){
data.m = integer(0)
all.m = which(season.ch == unique(season.ch)[s.i])
for(m.i in 1:length(all.m)){
	sel.m = which(obs.org$month == all.m[m.i])
	data.m = c(data.m,obs.org[sel.m,col.i+3])
}
data.m = na.omit(data.m)
x = data.m[order(data.m)]

## Fitting Density or Percentile
xd = ecdf(x)
prob.res = quantile(x,probs = prob.use)
if(density.plot){
	# Density
	if(dense.from.0){
		xden = density(x,n=hist.d,from=0)
		}else{xden = density(x,n=hist.d)}
	y.lim.max = max(xden$y,density(na.omit(as.real(unlist(m.org[, col.rang.rlz]))))$y)+0.2
	plot(xden$x,xden$y,ylim=c(0,y.lim.max), type="l", xlab=ylabel, ylab="Density",main = paste(colnames(obs.org)[col.i+3]," ss:",s.i,"/",season.n),lwd = 4, add=TRUE,col="red")
	legend("topright",c(rlz.name,"observation"),lwd=c(0,2),lty = c(0,1),pch=c(4,-1),cex = 1.2,col=c("black","red"))
	}else{
		# ECDF
		plot(x,xd(x),xlim=c(min(x),max(x)*1.1),ylim=c(min(xd(x))*0.8,1), type="l",xlab=ylabel, ylab="Percentile",main = paste(colnames(obs.org)[col.i+3]," ss:",s.i,"/",season.n),lwd=2,col="red")
		legend("bottomright",c(rlz.name,"observation"),lwd=c(0,2),lty = c(0,1),pch=c(4,-1),cex = 1.2,col=c("black","red"))
		}




# Loop plot multi-prob.
for(j in 1:length(prob.use)){
xval = prob.res[j]
xprb = xd(xval)
prb.val4.obs[col.i+3,j+(s.i-1)*(length(prob.use)+2)] = xval
}
prb.val4.obs[col.i+3,j+(s.i-1)*(length(prob.use)+2)+1] = mean(x)
prb.val4.obs[col.i+3,j+(s.i-1)*(length(prob.use)+2)+2] = sd(x)
#mtext(colnames(m.org)[col.i+3], outer = TRUE )


cat("\nRlz:")
####################
## Loop for rlz.i
for(rlz.i in 1:rlz.n){
cat(" ",rlz.i)
col.sel = (col.i-1)*rlz.n+rlz.i+3 

data.m = integer(0)
all.m = which(season.ch == unique(season.ch)[s.i])
for(m.i in 1:length(all.m)){
	sel.m = which(m.org$month == all.m[m.i])
	data.m = c(data.m,m.org[sel.m,col.sel])
}
data.m = na.omit(data.m)
x = data.m[order(data.m)]

## Fitting Density or Percentile
xd = ecdf(x)
prob.res = quantile(x,probs = prob.use)
if(density.plot){
	# Density
	if(dense.from.0){
		xden = density(x,n=hist.d,from=0)
		}else{xden = density(x,n=hist.d)}
	lines(xden$x,xden$y, type="p", pch = 4,cex = pt.size,col="black")
	}else{
		# ECDF
		seq.show.rlz = seq(from=1,to=length(x), length.out=seq.rlz.length)
		lines(x[seq.show.rlz],xd(x[seq.show.rlz]), type="p", pch = 4,cex = pt.size,col="black")
}

# Loop plot multi-prob.
for(j in 1:length(prob.use)){
xval = prob.res[j]
xprb = xd(xval)
prb.val4[col.sel,j+(s.i-1)*(length(prob.use)+2)] = xval
}
prb.val4[col.sel,j+(s.i-1)*(length(prob.use)+2)+1] = mean(x)
prb.val4[col.sel,j+(s.i-1)*(length(prob.use)+2)+2] = sd(x)
#mtext(colnames(m.org)[col.i+3], outer = TRUE )

}
## END Loop for rlz.i

} #END s.i
### END 4 seasons #########



### 12 seasons #########
season.ch = define.12season
season.all = unique(season.ch)[order(unique(season.ch))]
season.n = length(season.all)

c.name = integer(0)
for(s.i in 1:season.n){c.name = c(c.name,paste(paste("S",s.i,"-",season.n,sep=""),c(paste(" Pr-",prob.use,sep=""),"mean","SD")))}
colnames(prb.val12.obs) = c.name
rownames(prb.val12.obs) = colnames(obs.org)
colnames(prb.val12) = c.name
rownames(prb.val12) = colnames(m.org)

cat("\nLoop 12 seasons - Col :",col.i)

# Loop s.i
for(s.i in 1:season.n){
data.m = integer(0)
all.m = which(season.ch == unique(season.ch)[s.i])
for(m.i in 1:length(all.m)){
	sel.m = which(obs.org$month == all.m[m.i])
	data.m = c(data.m,obs.org[sel.m,col.i+3])
}
data.m = na.omit(data.m)
x = data.m[order(data.m)]

## Fitting Density or Percentile
xd = ecdf(x)
prob.res = quantile(x,probs = prob.use)
if(density.plot){
	# Density
	if(dense.from.0){
		xden = density(x,n=hist.d,from=0)
		}else{xden = density(x,n=hist.d)}
	y.lim.max = max(xden$y,density(na.omit(as.real(unlist(m.org[, col.rang.rlz]))))$y)+0.2
	plot(xden$x,xden$y,ylim=c(0,y.lim.max), type="l", xlab=ylabel, ylab="Density",main = paste(colnames(obs.org)[col.i+3]," ss:",s.i,"/",season.n),lwd = 4, add=TRUE,col="red")
	legend("topright",c(rlz.name,"observation"),lwd=c(0,2),lty = c(0,1),pch=c(4,-1),cex = 1.2,col=c("black","red"))
	}else{
		# ECDF
		plot(x,xd(x),xlim=c(min(x),max(x)*1.1),ylim=c(min(xd(x))*0.8,1), type="l",xlab=ylabel, ylab="Percentile",main = paste(colnames(obs.org)[col.i+3]," ss:",s.i,"/",season.n),lwd=2,col="red")
		legend("bottomright",c(rlz.name,"observation"),lwd=c(0,2),lty = c(0,1),pch=c(4,-1),cex = 1.2,col=c("black","red"))
		}




# Loop plot multi-prob.
for(j in 1:length(prob.use)){
xval = prob.res[j]
xprb = xd(xval)
prb.val12.obs[col.i+3,j+(s.i-1)*(length(prob.use)+2)] = xval
}
prb.val12.obs[col.i+3,j+(s.i-1)*(length(prob.use)+2)+1] = mean(x)
prb.val12.obs[col.i+3,j+(s.i-1)*(length(prob.use)+2)+2] = sd(x)
#mtext(colnames(m.org)[col.i+3], outer = TRUE )


cat("\nRlz:")
####################
## Loop for rlz.i
for(rlz.i in 1:rlz.n){
cat(" ",rlz.i)
col.sel = (col.i-1)*rlz.n+rlz.i+3 

data.m = integer(0)
all.m = which(season.ch == unique(season.ch)[s.i])
for(m.i in 1:length(all.m)){
	sel.m = which(m.org$month == all.m[m.i])
	data.m = c(data.m,m.org[sel.m,col.sel])
}
data.m = na.omit(data.m)
x = data.m[order(data.m)]

## Fitting Density or Percentile
xd = ecdf(x)
prob.res = quantile(x,probs = prob.use)
if(density.plot){
	# Density
	if(dense.from.0){
		xden = density(x,n=hist.d,from=0)
		}else{xden = density(x,n=hist.d)}
	lines(xden$x,xden$y, type="p", pch = 4,cex = pt.size,col="black")
	}else{
		# ECDF
		seq.show.rlz = seq(from=1,to=length(x), length.out=seq.rlz.length)
		lines(x[seq.show.rlz],xd(x[seq.show.rlz]), type="p", pch = 4,cex = pt.size,col="black")
}


# Loop plot multi-prob.
for(j in 1:length(prob.use)){
xval = prob.res[j]
xprb = xd(xval)
prb.val12[col.sel,j+(s.i-1)*(length(prob.use)+2)] = xval
}
prb.val12[col.sel,j+(s.i-1)*(length(prob.use)+2)+1] = mean(x)
prb.val12[col.sel,j+(s.i-1)*(length(prob.use)+2)+2] = sd(x)
#mtext(colnames(m.org)[col.i+3], outer = TRUE )

}
## END Loop for rlz.i

} #END s.i
### END 12 seasons #########





# END if(min(data.m) > 0)
# Change back
#if(change.log){data.m[change.pos] = 0}
#**########

########
## END Drawing + Finding probability
########






}
# END loop col.i for plotting
#############################


dev.off()
#############################
# END Section 1 for plotting denstiy
#############################



rownames(prb.val.obs) = colnames(obs.org)
colnames(prb.val.obs) = c(paste("Pr-",prob.use,sep=""),"mean","SD")
colnames(prb.val) = c(paste("Pr-",prob.use,sep=""),"mean","SD")
rownames(prb.val) = colnames(m.org)

cat("\nWritting Records")
setwd(dir1)
write.csv(par.val.obs,file=paste("OBS p-values ",sub.fix,".csv",sep=""))
write.csv(prb.val.obs,file=paste("OBS ProbCal ",sub.fix,".csv",sep=""))
write.csv(prb.val3.obs,file=paste("OBS ProbCal3ss ",sub.fix,".csv",sep=""))
write.csv(prb.val4.obs,file=paste("OBS ProbCal4ss ",sub.fix,".csv",sep=""))
write.csv(prb.val12.obs,file=paste("OBS ProbCal12ss ",sub.fix,".csv",sep=""))
write.csv(par.val,file=paste("p-values ",sub.fix,".csv",sep=""))
write.csv(prb.val,file=paste("ProbCal ",sub.fix,".csv",sep=""))
write.csv(prb.val3,file=paste("ProbCal3ss ",sub.fix,".csv",sep=""))
write.csv(prb.val4,file=paste("ProbCal4ss ",sub.fix,".csv",sep=""))
write.csv(prb.val12,file=paste("ProbCal12ss ",sub.fix,".csv",sep=""))
setwd(dir0)



#############################
# Section 2 for plotting Percentile
cat("\n***---****\n2)Plotting Percentile\n***---****")
density.plot = FALSE # if FALSE, then plot Percentile
#############################
setwd(dir1)
if(density.plot){
pdf(paste("Density plot.pdf",sep=""), width = 10 ,height = 8)
}else{
	pdf(paste("Percentile plot.pdf",sep=""), width = 10 ,height = 8)
	}
setwd(dir0)

for(col.i in 1:n.par){
cat("\nLoop NO season - Col :",col.i)
col.rang.rlz = ((col.i-1)*rlz.n+1+3):((col.i-1)*rlz.n+rlz.n+3)

obs.m = obs.org[,col.i+3]
data.obs = na.omit(obs.m)
x = data.obs[order(data.obs)]


#**########
# Change if member of data.m = 0, data.m -> 0.000001
#if(min(data.m, na.rm = TRUE) == 0){
#change.log = TRUE
#change.pos = which(data.m==0)
#data.m[change.pos] = 0.000001
#}else{change.log=FALSE}


par(mgp=c(2.5,1,0), ps =24, mfrow=c(1,1))
par(mai =  c(.8, .8, .8, .1))
par(oma =  c(.8, .8, 2, .8))

## Fitting Density or Percentile
xd = ecdf(x)
prob.res = quantile(x,probs = prob.use)
if(density.plot){
	# Density
	if(dense.from.0){
		xden = density(x,n=hist.d,from=0)
		}else{xden = density(x,n=hist.d)}
	y.lim.max = max(xden$y,density(na.omit(as.real(unlist(m.org[, col.rang.rlz]))))$y)+0.2
	plot(xden$x,xden$y,ylim=c(0,y.lim.max), type="l", xlab=ylabel, ylab="Density",main = paste(colnames(obs.org)[col.i+3],"- annual"),lwd = 4,col="red")
	legend("topright",c(rlz.name,"observation"),lwd=c(0,2),lty = c(0,1),pch=c(4,-1),cex = 1.2,col=c("black","red"))
	}else{
		# ECDF
		plot(x,xd(x),xlim=c(min(x),max(x)*1.1),ylim=c(min(xd(x))*0.8,1), type="l",xlab=ylabel, ylab="Percentile",main = paste(colnames(obs.org)[col.i+3],"- annual"),lwd=2,col="red")
		legend("bottomright",c(rlz.name,"observation"),lwd=c(0,2),lty = c(0,1),pch=c(4,-1),cex = 1.2,col=c("black","red"))
}



# Loop record multi-prob.
for(j in 1:length(prob.use)){
xval = prob.res[j]
xprb = xd(xval)
prb.val.obs[col.i+3,j] = xval
}
prb.val.obs[col.i+3,j+1] = mean(x)
prb.val.obs[col.i+3,j+2] = sd(x)

cat("\nRlz:")
####################
## Loop for rlz.i
for(rlz.i in 1:rlz.n){
cat(" ",rlz.i)
col.sel = (col.i-1)*rlz.n+rlz.i+3 

data.m = m.org[,col.sel]
data.m = na.omit(data.m)
x = data.m[order(data.m)]

## Fitting Density or Percentile
xd = ecdf(x)
prob.res = quantile(x,probs = prob.use)
if(density.plot){
	# Density
	if(dense.from.0){
		xden = density(x,n=hist.d,from=0)
		}else{xden = density(x,n=hist.d)}
	lines(xden$x,xden$y, type="p", pch = 4,cex = pt.size,col="black")
	}else{
		# ECDF
		seq.show.rlz = seq(from=1,to=length(x), length.out=seq.rlz.length)
		lines(x[seq.show.rlz],xd(x[seq.show.rlz]), type="p", pch = 4,cex = pt.size,col="black")
}

# Loop record multi-prob.
for(j in 1:length(prob.use)){
xval = prob.res[j]
xprb = xd(xval)
prb.val[col.sel,j] = xval
}
prb.val[col.sel,j+1] = mean(x)
prb.val[col.sel,j+2] = sd(x)


}
## END Loop for rlz.i

### END NO season #########



### 3 seasons #########
season.ch = define.3season
season.all = unique(season.ch)[order(unique(season.ch))]
season.n = length(season.all)

c.name = integer(0)
for(s.i in 1:season.n){c.name = c(c.name,paste(paste("S",s.i,"-",season.n,sep=""),c(paste(" Pr-",prob.use,sep=""),"mean","SD")))}
colnames(prb.val3.obs) = c.name
rownames(prb.val3.obs) = colnames(obs.org)
colnames(prb.val3) = c.name
rownames(prb.val3) = colnames(m.org)

cat("\nLoop 3 seasons - Col :",col.i)
# Loop s.i
for(s.i in 1:season.n){
data.m = integer(0)
all.m = which(season.ch == unique(season.ch)[s.i])
for(m.i in 1:length(all.m)){
	sel.m = which(obs.org$month == all.m[m.i])
	data.m = c(data.m,obs.org[sel.m,col.i+3])
}
data.m = na.omit(data.m)
x = data.m[order(data.m)]

## Fitting Density or Percentile
xd = ecdf(x)
prob.res = quantile(x,probs = prob.use)
if(density.plot){
	# Density
	if(dense.from.0){
		xden = density(x,n=hist.d,from=0)
		}else{xden = density(x,n=hist.d)}
	y.lim.max = max(xden$y,density(na.omit(as.real(unlist(m.org[, col.rang.rlz]))))$y)+0.2
	plot(xden$x,xden$y,ylim=c(0,y.lim.max), type="l", xlab=ylabel, ylab="Density",main = paste(colnames(obs.org)[col.i+3]," ss:",s.i,"/",season.n),lwd = 4, add=TRUE,col="red")
	legend("topright",c(rlz.name,"observation"),lwd=c(0,2),lty = c(0,1),pch=c(4,-1),cex = 1.2,col=c("black","red"))
	}else{
		# ECDF
		plot(x,xd(x),xlim=c(min(x),max(x)*1.1),ylim=c(min(xd(x))*0.8,1), type="l",xlab=ylabel, ylab="Percentile",main = paste(colnames(obs.org)[col.i+3]," ss:",s.i,"/",season.n),lwd=2,col="red")
		legend("bottomright",c(rlz.name,"observation"),lwd=c(0,2),lty = c(0,1),pch=c(4,-1),cex = 1.2,col=c("black","red"))
		}



# Loop plot multi-prob.
for(j in 1:length(prob.use)){
xval = prob.res[j]
xprb = xd(xval)
prb.val3.obs[col.i+3,j+(s.i-1)*(length(prob.use)+2)] = xval
}
prb.val3.obs[col.i+3,j+(s.i-1)*(length(prob.use)+2)+1] = mean(x)
prb.val3.obs[col.i+3,j+(s.i-1)*(length(prob.use)+2)+2] = sd(x)
#mtext(colnames(m.org)[col.i+3], outer = TRUE )


cat("\nRlz:")
####################
## Loop for rlz.i
for(rlz.i in 1:rlz.n){
cat(" ",rlz.i)
col.sel = (col.i-1)*rlz.n+rlz.i+3 

data.m = integer(0)
all.m = which(season.ch == unique(season.ch)[s.i])
for(m.i in 1:length(all.m)){
	sel.m = which(m.org$month == all.m[m.i])
	data.m = c(data.m,m.org[sel.m,col.sel])
}
data.m = na.omit(data.m)
x = data.m[order(data.m)]

## Fitting Density or Percentile
xd = ecdf(x)
prob.res = quantile(x,probs = prob.use)
if(density.plot){
	# Density
	if(dense.from.0){
		xden = density(x,n=hist.d,from=0)
		}else{xden = density(x,n=hist.d)}
	lines(xden$x,xden$y, type="p", pch = 4,cex = pt.size,col="black")
	}else{
		# ECDF
		seq.show.rlz = seq(from=1,to=length(x), length.out=seq.rlz.length)
		lines(x[seq.show.rlz],xd(x[seq.show.rlz]), type="p", pch = 4,cex = pt.size,col="black")
}


# Loop plot multi-prob.
for(j in 1:length(prob.use)){
xval = prob.res[j]
xprb = xd(xval)
prb.val3[col.sel,j+(s.i-1)*(length(prob.use)+2)] = xval
}
prb.val3[col.sel,j+(s.i-1)*(length(prob.use)+2)+1] = mean(x)
prb.val3[col.sel,j+(s.i-1)*(length(prob.use)+2)+2] = sd(x)

#mtext(colnames(m.org)[col.i+3], outer = TRUE )

}
## END Loop for rlz.i




} #END s.i
### END 3 seasons #########





### 4 seasons #########
season.ch = define.4season
season.all = unique(season.ch)[order(unique(season.ch))]
season.n = length(season.all)

c.name = integer(0)
for(s.i in 1:season.n){c.name = c(c.name,paste(paste("S",s.i,"-",season.n,sep=""),c(paste(" Pr-",prob.use,sep=""),"mean","SD")))}
colnames(prb.val4.obs) = c.name
rownames(prb.val4.obs) = colnames(obs.org)
colnames(prb.val4) = c.name
rownames(prb.val4) = colnames(m.org)

cat("\nLoop 4 seasons - Col :",col.i)

# Loop s.i
for(s.i in 1:season.n){
data.m = integer(0)
all.m = which(season.ch == unique(season.ch)[s.i])
for(m.i in 1:length(all.m)){
	sel.m = which(obs.org$month == all.m[m.i])
	data.m = c(data.m,obs.org[sel.m,col.i+3])
}
data.m = na.omit(data.m)
x = data.m[order(data.m)]

## Fitting Density or Percentile
xd = ecdf(x)
prob.res = quantile(x,probs = prob.use)
if(density.plot){
	# Density
	if(dense.from.0){
		xden = density(x,n=hist.d,from=0)
		}else{xden = density(x,n=hist.d)}
	y.lim.max = max(xden$y,density(na.omit(as.real(unlist(m.org[, col.rang.rlz]))))$y)+0.2
	plot(xden$x,xden$y,ylim=c(0,y.lim.max), type="l", xlab=ylabel, ylab="Density",main = paste(colnames(obs.org)[col.i+3]," ss:",s.i,"/",season.n),lwd = 4, add=TRUE,col="red")
	legend("topright",c(rlz.name,"observation"),lwd=c(0,2),lty = c(0,1),pch=c(4,-1),cex = 1.2,col=c("black","red"))
	}else{
		# ECDF
		plot(x,xd(x),xlim=c(min(x),max(x)*1.1),ylim=c(min(xd(x))*0.8,1), type="l",xlab=ylabel, ylab="Percentile",main = paste(colnames(obs.org)[col.i+3]," ss:",s.i,"/",season.n),lwd=2,col="red")
		legend("bottomright",c(rlz.name,"observation"),lwd=c(0,2),lty = c(0,1),pch=c(4,-1),cex = 1.2,col=c("black","red"))
		}




# Loop plot multi-prob.
for(j in 1:length(prob.use)){
xval = prob.res[j]
xprb = xd(xval)
prb.val4.obs[col.i+3,j+(s.i-1)*(length(prob.use)+2)] = xval
}
prb.val4.obs[col.i+3,j+(s.i-1)*(length(prob.use)+2)+1] = mean(x)
prb.val4.obs[col.i+3,j+(s.i-1)*(length(prob.use)+2)+2] = sd(x)
#mtext(colnames(m.org)[col.i+3], outer = TRUE )


cat("\nRlz:")
####################
## Loop for rlz.i
for(rlz.i in 1:rlz.n){
cat(" ",rlz.i)
col.sel = (col.i-1)*rlz.n+rlz.i+3 

data.m = integer(0)
all.m = which(season.ch == unique(season.ch)[s.i])
for(m.i in 1:length(all.m)){
	sel.m = which(m.org$month == all.m[m.i])
	data.m = c(data.m,m.org[sel.m,col.sel])
}
data.m = na.omit(data.m)
x = data.m[order(data.m)]

## Fitting Density or Percentile
xd = ecdf(x)
prob.res = quantile(x,probs = prob.use)
if(density.plot){
	# Density
	if(dense.from.0){
		xden = density(x,n=hist.d,from=0)
		}else{xden = density(x,n=hist.d)}
	lines(xden$x,xden$y, type="p", pch = 4,cex = pt.size,col="black")
	}else{
		# ECDF
		seq.show.rlz = seq(from=1,to=length(x), length.out=seq.rlz.length)
		lines(x[seq.show.rlz],xd(x[seq.show.rlz]), type="p", pch = 4,cex = pt.size,col="black")
}

# Loop plot multi-prob.
for(j in 1:length(prob.use)){
xval = prob.res[j]
xprb = xd(xval)
prb.val4[col.sel,j+(s.i-1)*(length(prob.use)+2)] = xval
}
prb.val4[col.sel,j+(s.i-1)*(length(prob.use)+2)+1] = mean(x)
prb.val4[col.sel,j+(s.i-1)*(length(prob.use)+2)+2] = sd(x)
#mtext(colnames(m.org)[col.i+3], outer = TRUE )

}
## END Loop for rlz.i

} #END s.i
### END 4 seasons #########



### 12 seasons #########
season.ch = define.12season
season.all = unique(season.ch)[order(unique(season.ch))]
season.n = length(season.all)

c.name = integer(0)
for(s.i in 1:season.n){c.name = c(c.name,paste(paste("S",s.i,"-",season.n,sep=""),c(paste(" Pr-",prob.use,sep=""),"mean","SD")))}
colnames(prb.val12.obs) = c.name
rownames(prb.val12.obs) = colnames(obs.org)
colnames(prb.val12) = c.name
rownames(prb.val12) = colnames(m.org)

cat("\nLoop 12 seasons - Col :",col.i)

# Loop s.i
for(s.i in 1:season.n){
data.m = integer(0)
all.m = which(season.ch == unique(season.ch)[s.i])
for(m.i in 1:length(all.m)){
	sel.m = which(obs.org$month == all.m[m.i])
	data.m = c(data.m,obs.org[sel.m,col.i+3])
}
data.m = na.omit(data.m)
x = data.m[order(data.m)]

## Fitting Density or Percentile
xd = ecdf(x)
prob.res = quantile(x,probs = prob.use)
if(density.plot){
	# Density
	if(dense.from.0){
		xden = density(x,n=hist.d,from=0)
		}else{xden = density(x,n=hist.d)}
	y.lim.max = max(xden$y,density(na.omit(as.real(unlist(m.org[, col.rang.rlz]))))$y)+0.2
	plot(xden$x,xden$y,ylim=c(0,y.lim.max), type="l", xlab=ylabel, ylab="Density",main = paste(colnames(obs.org)[col.i+3]," ss:",s.i,"/",season.n),lwd = 4, add=TRUE,col="red")
	legend("topright",c(rlz.name,"observation"),lwd=c(0,2),lty = c(0,1),pch=c(4,-1),cex = 1.2,col=c("black","red"))
	}else{
		# ECDF
		plot(x,xd(x),xlim=c(min(x),max(x)*1.1),ylim=c(min(xd(x))*0.8,1), type="l",xlab=ylabel, ylab="Percentile",main = paste(colnames(obs.org)[col.i+3]," ss:",s.i,"/",season.n),lwd=2,col="red")
		legend("bottomright",c(rlz.name,"observation"),lwd=c(0,2),lty = c(0,1),pch=c(4,-1),cex = 1.2,col=c("black","red"))
		}




# Loop plot multi-prob.
for(j in 1:length(prob.use)){
xval = prob.res[j]
xprb = xd(xval)
prb.val12.obs[col.i+3,j+(s.i-1)*(length(prob.use)+2)] = xval
}
prb.val12.obs[col.i+3,j+(s.i-1)*(length(prob.use)+2)+1] = mean(x)
prb.val12.obs[col.i+3,j+(s.i-1)*(length(prob.use)+2)+2] = sd(x)
#mtext(colnames(m.org)[col.i+3], outer = TRUE )


cat("\nRlz:")
####################
## Loop for rlz.i
for(rlz.i in 1:rlz.n){
cat(" ",rlz.i)
col.sel = (col.i-1)*rlz.n+rlz.i+3 

data.m = integer(0)
all.m = which(season.ch == unique(season.ch)[s.i])
for(m.i in 1:length(all.m)){
	sel.m = which(m.org$month == all.m[m.i])
	data.m = c(data.m,m.org[sel.m,col.sel])
}
data.m = na.omit(data.m)
x = data.m[order(data.m)]

## Fitting Density or Percentile
xd = ecdf(x)
prob.res = quantile(x,probs = prob.use)
if(density.plot){
	# Density
	if(dense.from.0){
		xden = density(x,n=hist.d,from=0)
		}else{xden = density(x,n=hist.d)}
	lines(xden$x,xden$y, type="p", pch = 4,cex = pt.size,col="black")
	}else{
		# ECDF
		seq.show.rlz = seq(from=1,to=length(x), length.out=seq.rlz.length)
		lines(x[seq.show.rlz],xd(x[seq.show.rlz]), type="p", pch = 4,cex = pt.size,col="black")
}


# Loop plot multi-prob.
for(j in 1:length(prob.use)){
xval = prob.res[j]
xprb = xd(xval)
prb.val12[col.sel,j+(s.i-1)*(length(prob.use)+2)] = xval
}
prb.val12[col.sel,j+(s.i-1)*(length(prob.use)+2)+1] = mean(x)
prb.val12[col.sel,j+(s.i-1)*(length(prob.use)+2)+2] = sd(x)
#mtext(colnames(m.org)[col.i+3], outer = TRUE )

}
## END Loop for rlz.i

} #END s.i
### END 12 seasons #########





# END if(min(data.m) > 0)
# Change back
#if(change.log){data.m[change.pos] = 0}
#**########

########
## END Drawing + Finding probability
########

}
# END loop col.i for plotting
#############################


dev.off()
#############################
# END Section 2 for plotting denstiy
#############################


cat("\nEnd Script\n")





