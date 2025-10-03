#
#Program to rearrange matrix of cross-correlation and set order of predictor
#

datafile = "monthly-correlation_GCM-OBS.csv"
dir_name="arrange_cor_order-monthly-correlation_GCM-OBS.csv"

L=file.exists(dir_name)  
if(!L) dir.create(dir_name)
dir0=dirname(file.path("D:","Temp","cross-correlation","dummy"))
dir1=dirname(file.path("D:","Temp","cross-correlation",paste(dir_name),"dummy"))

data <- read.table(datafile, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
first.unused.col = 4
model.col = 2
par.col = 4

order.all = array(NA,c(length(rownames(data)),length(colnames(data))-first.unused.col))
model.all = array(NA,c(length(rownames(data)),length(colnames(data))-first.unused.col))
prd.all = array(NA,c(length(rownames(data)),length(colnames(data))-first.unused.col))

colnames(order.all) = colnames(data)[-c(1:first.unused.col)]
colnames(model.all) = colnames(data)[-c(1:first.unused.col)]
colnames(prd.all) = colnames(data)[-c(1:first.unused.col)]

# LOOP ###############################################
################ Start (obs) #########################
######################################################

for(i in 1:(length(colnames(data))-first.unused.col))	{
							cor = abs(data[,i+first.unused.col])
							rank.cor = order(cor,decreasing=TRUE)
							order.all[,i] = rank.cor
							cat(paste("Prd:",colnames(data)[i+first.unused.col],"\n"))
							model.all[,i] = data[rank.cor,model.col]
							prd.all[,i] = data[rank.cor,par.col]
							}

setwd(dir1)
write.csv(order.all, file="order_col.csv")
write.csv(model.all, file="order_model.csv")
write.csv(prd.all, file="order_prd.csv")
setwd(dir0)

cat("Script END\n")








