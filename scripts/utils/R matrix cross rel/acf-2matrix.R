# 1)
# Working directory and file

mdir = "temp/accessory/R matrix cross rel"
dir_name= "ACF vs GCM+SST 1971-19991"


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

obs2 = data2[,-c(1:first.unwantedcol2)]
nobs2 = length(colnames(obs2))

obsccf = matrix(NA,nrow = nobs1,ncol = nobs2)
nccf = matrix(NA,nrow = nobs1,ncol = nobs2)


setwd(dir1)
pdf(paste("ACF-PACF-charts.pdf"))
setwd(dir0)

for (i in 1:nobs1){
			for (j in 1:nobs2){

						# Adjust 2 matrix to same size
						obs.i = obs1[,i]
						obs.j = obs2[,j]
						max.nrow = max(length(obs.i),length(obs.j))
						length(obs.i) = max.nrow
						length(obs.j) = max.nrow

						if(nrow(na.exclude(cbind(obs.i,obs.j)))>1){
									#checkccf = ccf(obs.i,obs.j, type = c("correlation"),lag.max = 1,plot = FALSE, na.action = na.exclude)
									checkacf = acf(cbind(obs.i,obs.j), drop.lag.0 = FALSE, ylim = c(-1,1),lag.max = 12,type = c("correlation"))
									checkacf = acf(cbind(obs.i,obs.j), drop.lag.0 = FALSE, ylim = c(-1,1),lag.max = 12,type = c("partial"))
									obsccf[i,j] = checkccf$acf[2]
									nccf[i,j] =  checkccf$n.used
							}else{checkccf = NA
								obsccf[i,j] = NA
								nccf[i,j] = 0
								}
				
						}
			print(paste("matrix : ",i,",",j))
			}


##Write correlation
rownames(obsccf) = colnames(obs1)
colnames(obsccf) = colnames(obs2)
setwd(dir1)
write.csv(obsccf,file= "corr of all.csv")
setwd(dir0)

##Write n.used
rownames(nccf) = colnames(obs1)
colnames(nccf) = colnames(obs2)
setwd(dir1)
write.csv(nccf,file= "ncorr of all.csv")
setwd(dir0)


cat("\n Finisch analysing cross-correlation")



