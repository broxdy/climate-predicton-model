# 1)
# Working directory and file

mdir = "temp/accessory/R matrix cross rel"
dir_name= "obs vs SST 1971-2006-with Lag"


L=file.exists(dir_name)  
if(!L) dir.create(dir_name)
dir1=dirname(file.path("D:",paste(mdir),paste(dir_name),"dummy"))
dir0=dirname(file.path("D:",paste(mdir),"dummy"))

# 2a) Input file1
dataf1 = "monthly-obs_filled1971-2006(2100)v2.csv"
first.unwantedcol1 = 2

# 2a) Input file2
#dataf2 = "GCM+SST 1971-19991.csv"
dataf2 = "ocean index.csv"
first.unwantedcol2 = 2

########################define file for input / read #############@###################
data1 <- read.table(dataf1, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
data2 <- read.table(dataf2, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")


cat("\n Finish reading data source \n")


#******** check correlation ***************
max.cor.lag = 11

obs1 = data1[,-c(1:first.unwantedcol1)]
nobs1 = length(colnames(obs1))

obs2 = data2[,-c(1:first.unwantedcol2)]
nobs2 = length(colnames(obs2))

obsccf = matrix(NA,nrow = nobs1*nobs2,ncol = (3+max.cor.lag*2+1))




for (i in 1:nobs1){
			for (j in 1:nobs2){

						# Adjust 2 matrix to same size
						obs.ii = obs1[,i]
						obs.jj = obs2[,j]
						min.nrow = min(length(obs.ii),length(obs.jj))
						cbind.obs = as.data.frame(na.omit(cbind(obs.ii[1:min.nrow],obs.jj[1:min.nrow])))
						if(nrow(na.exclude(cbind(obs.i,obs.j)))>1){
									#checkccf = ccf(obs.i,obs.j, type = c("correlation"),lag.max = max.cor.lag ,plot = FALSE, na.action = na.exclude)
									check.ccf = ccf(cbind.obs[,1],cbind.obs[,2], type = c("correlation"),lag.max = max.cor.lag ,plot = FALSE, na.action = na.exclude)
									obsccf[(i-1)*nobs2+j,1] = colnames(obs1)[i]
									obsccf[(i-1)*nobs2+j,2] = colnames(obs2)[j]
									obsccf[(i-1)*nobs2+j,3] = check.ccf$n.used

									obsccf[(i-1)*nobs2+j,4:(max.cor.lag*2+4)] = as.real(check.ccf$acf)
							}else{checkccf = NA
								obsccf[i,j] = NA
								nccf[i,j] = 0
								}
				
						}
			print(paste("matrix : ",i,",",j))
			}


##Write correlation
colnames(obsccf) = c(dataf1,dataf2,"n-used",paste("Lag",-max.cor.lag:max.cor.lag,sep =""))
setwd(dir1)
write.csv(obsccf,file= "corr of all.csv", row.names=FALSE)
setwd(dir0)

cat("\n Finisch analysing cross-correlation")



