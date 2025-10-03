# 1)
# Working directory and file
#ML-regression anlysis + correlation + line-charts of time series
# You have to define the time-series data in parameter below
# 1a) Current working folder
#mdir ="Temp/filling-NA"

mdir = "temp/accessory/R matrix cross rel"
dir_name= "self obs+SST"


L=file.exists(dir_name)  
if(!L) dir.create(dir_name)
dir1=dirname(file.path("D:",paste(mdir),paste(dir_name),"dummy"))
dir0=dirname(file.path("D:",paste(mdir),"dummy"))

# 1b) Input file
dataf = "monthly-obs_filled1971-2006(2100)v2.csv"
first.unwantedcol = 2

########################define file for input / read #############@###################
data <- read.table(dataf, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

cat("\n Finish reading data source \n")


#******** check correlation ***************
obs = data[,-c(1:first.unwantedcol)]
nobs = length(colnames(obs))
obsccf = matrix(NA,nrow = nobs,ncol = nobs)
nccf = matrix(NA,nrow = nobs,ncol = nobs)


for (i in 1:nobs){
			for (j in i:nobs){
						if(nrow(na.exclude(cbind(obs[i],obs[j])))>1){
									checkccf = ccf(obs[i],obs[j], type = c("correlation"),lag.max = 1,plot = FALSE, na.action = na.exclude)
									obsccf[i,j] = checkccf$acf[2]
									nccf[i,j] =  checkccf$n.used
							}else{checkccf = NA
								obsccf[i,j] = NA
								nccf[i,j] = 0
								}
				
						}
			print(paste("matrix : ",i,",",j))
			}
### Copy to lower diagonal of matrix ####
for (i in 1:nobs){
			for (j in i:nobs){obsccf[j,i] = obsccf[i,j]
						nccf[j,i] =	nccf[i,j]
						}
			}

##Write correlation
colnames(obsccf) = colnames(obs)
rownames(obsccf) = colnames(obs)
setwd(dir1)
write.csv(obsccf,file= "corr of all.csv")
setwd(dir0)

##Write n.used
colnames(nccf) = colnames(obs)
rownames(nccf) = colnames(obs)
setwd(dir1)
write.csv(nccf,file= "ncorr of all.csv")
setwd(dir0)


cat("\n Finisch analysing cross-correlation")



