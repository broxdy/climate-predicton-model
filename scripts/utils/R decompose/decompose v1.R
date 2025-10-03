# 1)
# Working directory and file
#source("clear.R")
#clear()

mdir = "temp/accessory/R decompose"
dir_name= "decompose output"


L=file.exists(dir_name)  
if(!L) dir.create(dir_name)
dir1=dirname(file.path("D:",paste(mdir),paste(dir_name),"dummy"))
dir0=dirname(file.path("D:",paste(mdir),"dummy"))

# Input file1
#dataf1 = "monthly-obs_filled1971-2006(2100)+SST.csv"
dataf1 = "monthly-obs_filled1971-2005.csv"

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

ts.obs = ts(obs1, frequency=12,start=c(startyear.obs,startmonth.obs))

setwd(dir1)
pdf(paste("decompose-charts.pdf"), width=6 , height = 6)
setwd(dir0)
#par(mfrow = c(1,1))
par(mgp=c(1.8,0.8,0), ps =12)
par(mai =  c(0.8, .8, .8, .8))
#par(mar=c(4,3,2,2)+0.1)
#par(omd=c(0.5,0,0.5,0))

for (i in 1:nobs1){
			station_name = colnames(ts.obs)[i]
			cat("\nDecompose",i,":",station_name)
			m = decompose(ts.obs[,i])
			#plot(m)
			plot(cbind(observed = m$random + m$trend * m$seasonal, trend = m$trend, seasonal = m$seasonal,
			random = m$random), main = paste("Addititive decomposition at station",station_name))
			#title(sub=paste("Decomposition of time series for station ", station_name))
}
dev.off()

setwd(dir0)

cat("\n Finisch plotting decomposition")



