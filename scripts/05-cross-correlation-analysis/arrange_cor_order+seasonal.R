#
#Program to rearrange matrix of cross-correlation and set order of predictor
#
#########################################
#### After running this script you need to run 
#### the prediction in ML-multiGCMv4.R (folder ML-regression-multiGCM)
#########################################

datafile = "monthly_seasonal-cor_GCMv4-OBS.csv"
dir_name="arrange_cor_order-monthly_seasonal-cor_GCMv4-OBS.csv"

L=file.exists(dir_name)  
if(!L) dir.create(dir_name)
dir0=dirname(file.path("D:","Temp","cross-correlation","dummy"))
dir1=dirname(file.path("D:","Temp","cross-correlation",paste(dir_name),"dummy"))

data <- read.table(datafile, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
first.unused.col = 5
model.col = 2
index.col = 4
obs.col = 5


# Set Obs name
obs.name = unique(data[,obs.col])
index.name = unique(data[,index.col])


order.all = array(NA,c(length(index.name),length(obs.name)))
model.all = array(NA,c(length(index.name),length(obs.name)))
index.all = array(NA,c(length(index.name),length(obs.name)))

colnames(order.all) = obs.name
colnames(model.all) = obs.name
colnames(index.all) = obs.name

# LOOP ###############################################
################ Start (obs) #########################
######################################################

for(i in 1:(length(colnames(data))-first.unused.col))	{
					for(j in 1:(length(obs.name))){
										selected.row = which(data[,obs.col]==obs.name[j])
										cor = abs(as.numeric(data[selected.row,i+first.unused.col]))
										cat(paste("\nCol:",colnames(data)[i+first.unused.col]," / Read obs:",obs.name[j]))
										rank.cor = order(cor,decreasing=TRUE)
										order.all[,j] = rank.cor
										#cat(paste("Prd:",colnames(data)[i+first.unused.col],"\n"))
										model.all[,j] = data[selected.row,model.col][rank.cor]
										index.all[,j] = data[selected.row,index.col][rank.cor]
										}

									setwd(dir1)
									write.csv(order.all, file=paste(colnames(data)[i+first.unused.col],"-order_col.csv"))
									write.csv(model.all, file=paste(colnames(data)[i+first.unused.col],"-order_model.csv"))
									write.csv(index.all, file=paste(colnames(data)[i+first.unused.col],"order_prd.csv"))
									setwd(dir0)
									cat(paste("\nOutputs:",colnames(data)[i+first.unused.col],"Written\n"))

									}

cat("\nScript END\n")








