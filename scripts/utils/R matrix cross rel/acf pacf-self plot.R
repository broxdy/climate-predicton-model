# 1)
# Working directory and file
source("clear.R")
clear()

mdir = "temp/accessory/R matrix cross rel"
dir_name= "ACF-Obs+SST 1971-2006"


L=file.exists(dir_name)  
if(!L) dir.create(dir_name)
dir1=dirname(file.path("D:",paste(mdir),paste(dir_name),"dummy"))
dir0=dirname(file.path("D:",paste(mdir),"dummy"))

# 2a) Input file1
dataf1 = "monthly-obs_filled1971-2006(2100)+SST.csv"
first.unwantedcol1 = 2

# 2a) Input file2
dataf2 = "GCM+SST 1971-19991.csv"
first.unwantedcol2 = 2

########################define file for input / read #############@###################
data1 <- read.table(dataf1, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
data2 <- read.table(dataf2, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")


cat("\n Finish reading data source \n")


#******** check correlation ***************
obs1 = data1[,-c(1:first.unwantedcol1)]
nobs1 = length(colnames(obs1))

rec.acf = matrix(NA,nrow = nobs1,ncol = 12)
rec.pacf = matrix(NA,nrow = nobs1,ncol = 12)

colnames(rec.acf) = paste("lag",c(1:12))
colnames(rec.pacf) = paste("lag",c(1:12))
rownames(rec.pacf) = colnames(obs1)
rownames(rec.acf) = colnames(obs1)



setwd(dir1)
pdf(paste("ACF-PACF-charts.pdf"), width=6 , height = 3)
setwd(dir0)
#par(ps = 12, mfrow = c(nobs1,1), mar = c(0.5,4,0.5,1))
par(mgp=c(2.5,1,0), ps =16)
par(mai =  c(.8, .8, .8, .8))

for (i in 1:nobs1){
			acf0 = acf(na.exclude(obs1)[i], drop.lag.0 = TRUE, ylim = c(-1,1),lag.max = 12,type = c("correlation"), plot = FALSE)
			pacf0 = acf(na.exclude(obs1)[i], ylim = c(-1,1),lag.max = 12,type = c("partial"),, plot = FALSE)

			# Plot
			#if(i == nobs1){
						#par(mar = c(2,4,1,1))
						#plot(abs(acf0$acf), type = "h",lwd=20, lend = 1, col="blue",ylab = "ACF/PACF", ylim = c(0,1), xlab = colnames(obs1)[i])
						#}else{
			plot(abs(acf0$acf), type = "h",lwd=20, lend = 1, col="blue",ylab = "abs. ACF / PACF", ylim = c(0,1), xlab = "lag (month)", main = colnames(obs1)[i])
							#}
			lines(pacf0$lag+0.2,abs(pacf0$acf), lwd=20, lend = 1, type = "h", col="red")

			legend(x = "top",  cex = 0.8, bty = "n",  lty = c(1,1), lwd = c(2,2), col = c("blue", "red"),  legend = c("ACF","PACF"))
			#text(5,10,colnames(obs1)[i])


			rec.acf[i,] = acf0$acf
			rec.pacf[i,] =  acf0$lag

			print(paste("matrix : ",i,",",j))
}
dev.off()

##Write correlation
rownames(obsccf) = colnames(obs1)
colnames(obsccf) = colnames(obs2)

##Write n.used
setwd(dir1)
write.csv(rec.acf,file= "ACF.csv")
setwd(dir0)

setwd(dir1)
write.csv(rec.pacf,file= "PACF.csv")
setwd(dir0)


cat("\n Finisch analysing cross-correlation")



