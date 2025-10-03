# 1)
# Working directory and file
#source("clear.R")
#clear()

mdir = "temp/accessory/R Anomaly"
dir_name= "anomaly output"


L=file.exists(dir_name)  
if(!L) dir.create(dir_name)
dir1=dirname(file.path("D:",paste(mdir),paste(dir_name),"dummy"))
dir0=dirname(file.path("D:",paste(mdir),"dummy"))

# Input file1
#dataf1 = "monthly-obs_filled1971-2006(2100)+SST.csv"
#dataf1 = "monthly-obs_filled1971-1999+WetDay+WetRatio_V2.csv"
dataf1 = "monthly-obs_filled1971-2006(4)+WetDay+WetRatio+Stream_V2.csv"

########################define file for input / read #############@###################
data1 <- read.table(dataf1, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

cat("\n Finish reading data source \n")

#######################
## DATA for Observation
#######################
############# spatial info of obs #########################
monthly.time.obs = TRUE # if false then daily
first.unwantedcol.obs = 2
start0.obs=1971  #year start in table file
startyear.obs=1971 #year to analysis begins
startmonth.obs=1
startmonth0.obs=1

obs1 = data1[,-c(1:first.unwantedcol.obs)]
nobs1 = length(colnames(obs1))
month.list = data1[,2]

ts.obs = ts(obs1, frequency=12,start=c(startyear.obs,startmonth.obs))
anomaly.org = ts.obs
anomaly.out = ts.obs

setwd(dir1)
pdf(paste("anomaly-charts.pdf"), width=6 , height = 6)
setwd(dir0)
#par(mfrow = c(1,1))
par(mgp=c(1.8,0.8,0), ps =12)
par(mai =  c(0.8, .8, .8, .8))
#par(mar=c(4,3,2,2)+0.1)
#par(omd=c(0.5,0,0.5,0))

for (col.i in 1:nobs1){
	for (month.i in 1:12){
				row.i = which(month.list == month.i)
				mean.anomaly = mean(obs1[row.i,col.i], na.rm = TRUE)
				anomaly.out[row.i,col.i] = anomaly.org[row.i,col.i] - mean.anomaly
				station_name = colnames(ts.obs)[col.i]
				cat("\nAnomaly month",month.i,":",station_name)
	}
plot(anomaly.out[,col.i],main=paste("Anomaly",station_name),ylab="Anomaly",col="red")
abline(h = 0)
}
dev.off()

setwd(dir0)

# Set anomaly file
anomaly.print = data1
anomaly.print[,-c(1:first.unwantedcol.obs)] = anomaly.out
setwd(dir1)
write.csv(anomaly.print,file="anomaly-out.csv")
setwd(dir0)
cat("\n Finisch writing anomaly")



