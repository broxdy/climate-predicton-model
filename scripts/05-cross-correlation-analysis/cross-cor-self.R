# 1)
# Working directory and file
#ML-regression anlysis + correlation + line-charts of time series
# You have to define the time-series data in parameter below
# 1a) Current working folder
mdir ="Temp/filling-NA"


# 1b) Input file
dataf = "GCMs-real_mo-1971-1999v2b.csv"
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
sink("corr of all.csv")
write.table(obsccf, quote = FALSE, sep = ",")
sink()


##Write n.used
sink("ncorr of all.csv")
write.table(nccf, quote = FALSE, sep = ",")
sink()


cat("\n Finisch analysing cross-correlation")



