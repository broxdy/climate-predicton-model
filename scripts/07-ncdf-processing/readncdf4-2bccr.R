## Input have to be : file name , code (series), run (run number)
##

library(RNetCDF)
library(chron)
library(GenKern)
## Function to combine different-length vector "cbind.all"
cbind.all <- function(..., fill.with = NA) { 

   args <- list(...) 
   len <- sapply(args, NROW) 
   if(diff(rng <- range(len)) > 0) { 

     maxlen <- rng[2]
     pad <- function(x, n) c(x, rep(fill.with, n))
     for(j in seq(along = args)) {
       if(maxlen == len[j]) next
       if(is.data.frame(args[[j]])) {
         args[[j]] <- lapply(args[[j]], pad, maxlen - len[j])
         args[[j]] <- as.data.frame(args[[j]])
       } else if(is.matrix(args[[j]])) {
         args[[j]] <- apply(args[[j]], 2, pad, maxlen - len[j])
       } else if(is.vector(args[[j]])) {
         args[[j]] <- pad(args[[j]], maxlen - len[j])
       } else {
         stop("... must only contain data.frames or arrays.")
       }
     }

   } 
   do.call("cbind", args) 
} 
###




### INITIAL CONSTANT ####
# set initial constant
extend.dat = FALSE
first.record = TRUE
first.table1= TRUE
first.table2= TRUE
first.table3= TRUE
first.table4= TRUE


## Read input list sheet
sheetf = "BCCR-BCM2_g-20c3m-atm-da+file_run.csv"
sheet.list <- read.table(sheetf, stringsAsFactors=FALSE,header=FALSE, sep=",",dec=".")
# Set TRUE when memmory is not enough to whole entire one file for time-series analysis
largefile = FALSE # when TRUE it will not engage ts() funtion and the origin date have to be yyyy/1/1

datafile = sheet.list[,1]
codefile = sheet.list[,2]
runfile = sheet.list[,3]


# define run to keep in table by order etc c(1,4) run1=output.table1 , run4=output.table2
run.list = c(1,2,3,4)
#run.list = c(1)


# Output name
out.file = "output"
## specify output folder here
dir_name ="readncdf4-1"

# position start first id starts at 0
#mync.var.id = 6 # (default set to allvar.n -1) Position for variable to read
mync.time.id = 4 # Position for time dimension
mync.time.id4 = 5 # Position for time dimension for 4 dimensions
mync.long.id = 0 # Position for longtitude
mync.lat.id = 2 # Position for lat
mync.subvar.id = 4 # Position for sub variable, if there are 4 dimension in available

# Define for finding the focus grid cells
my.exact.long = 101.2
my.exact.lat = 12.7
#mync.grid.long = 1 # Position for selected lontitude
#mync.grid.lat = 1 # Position for selected latitude




# Set origin day for counting date
mync.time.start.day = 1
mync.time.start.month = 1
mync.time.start.year = 1800
#mync.time.calendarday = 365 # Day in a calendar year
mync.time.calendarday = 365.25 # Day in a calendar year


L=file.exists(dir_name)  
if(!L) dir.create(dir_name)
dir0=dirname(file.path("D:","CMIP3","BCCR","dummy"))
dir1=dirname(file.path("D:","CMIP3","BCCR",paste(dir_name),"dummy"))

##############################
## Read NCDF file start here #

nfile = length(datafile)

### START loop to change file ###
#for(i in 1:7){
for(i in 1:nfile){

nc = open.nc(datafile[i])
if(i==1){print.nc(nc)}

# Synchronize to disk
#sync.nc(nc)

# Define dimensions
mync.dim = file.inq.nc(nc)
allvar.n = mync.dim$nvars


# Find closest grid cell
mync.grid.long = nearest(var.get.nc(nc,mync.long.id),my.exact.long)
mync.grid.lat = nearest(var.get.nc(nc,mync.lat.id),my.exact.lat)

# Read Varidable properties
# at the last position of variables
var.info = var.inq.nc(nc, allvar.n-1)
var.name = var.info$name
cat(paste("\n\nn =",i,"  Run:",runfile[i]))


# Read Time
if(var.info$ndims == 3)	{var.time = var.get.nc(nc, mync.time.id)
				}else{var.time = var.get.nc(nc, mync.time.id4)} # time ID for 4 dimension



## Check dimension and read data of extracted variable
# if have 3 dimension
if(var.info$ndims == 3)	{
		var.data = var.get.nc(nc, allvar.n-1, c(mync.grid.long,mync.grid.lat,1),c(1,1,NA))
		# Convert variaable to table and put header
		var.table = as.data.frame(var.data)
		colnames(var.table) = var.name
		cat("/Dimension:3\n")
				}else	{


					# if have 4 dimension
					if(var.info$ndims == 4)	{
									subvar.n = length(var.get.nc(nc, allvar.n-1, c(mync.grid.long,mync.grid.lat,1,1),c(1,1,NA,1)))
									subvar.name = var.get.nc(nc, mync.subvar.id, 1,NA)
									# Loop for sub variable
									for(j in 1:subvar.n)	{
													var.data = var.get.nc(nc, allvar.n-1, c(mync.grid.long,mync.grid.lat,j,1),c(1,1,1,NA))
													subvar.table = as.data.frame(var.data)
													colnames(subvar.table) =  paste(var.name,format(subvar.name[j],scientific = FALSE))
													cat("/Dimension:4\n")
													# Put core for the first loop		
													if(j == 1)	{var.table = subvar.table
															}else{var.table = cbind(var.table,subvar.table)}
													}
									}else	{
										cat("variable dimension does not meet default")
										stop()
										}
					}


## END check dimension and read data of extracted variable

# Show file name
cat(paste("File:",datafile[i]))

# Record time dimension to output table
if(largefile==TRUE)	{
				time.table = as.data.frame(mync.time.start.year + var.time/mync.time.calendarday)
				}else	{
					var.ts = ts(var.data, frequency=mync.time.calendarday,start=(c(mync.time.start.year+var.time[1]/mync.time.calendarday,mync.time.start.month,mync.time.start.day)))
					time.table = as.data.frame(time(var.ts))
					}

colnames(time.table) = paste("Time",var.name)

# Combine columns of time and variable outputs
var.table = cbind(time.table,var.table)

# Combine varible output to former output table
if(i>1){
							if(extend.dat == TRUE)	{
											var.table = rbind(var.table.extend[1:length(var.table.extend[,1]),],var.table[1:length(var.table[,1]),])
											cat(paste("\n#Combined:"))
											}
	 }
# Return recorded values to var.table.extend
var.table.extend = var.table

# Check extendation of this series
if(i==nfile){extend.dat=FALSE
		}else	{if(codefile[i+1] == (codefile[i]+1))	{extend.dat = TRUE
										}else	{extend.dat = FALSE
											}
			}

# For normal case full data within one file
if(extend.dat == FALSE)	{
				if(first.record==TRUE){
								output.table = var.table[,1]
								first.record = FALSE
								output.table = cbind.all(output.table,var.table)[,-1]
							    }else	{
									output.table = cbind.all(output.table,var.table)
									}
				}
				


# show progress each loop
if(extend.dat == TRUE)	{cat(paste("\n#Extended:"))}
cat(paste("\n",colnames(var.table)[-c(1)]))

# check that data still on the same run number (runfile)
if(i < nfile){
	if(runfile[i+1]!=runfile[i])	{
						first.record = TRUE # to reset output.table
						if(runfile[i]==run.list[1])	{
											if(first.table1==TRUE)	{
															table1=output.table[,1]
															table1=cbind.all(table1,output.table)[,-1]
															first.table1=FALSE
															}else	{
																table1=cbind.all(table1,output.table)
																}
											}
						if(runfile[i]==run.list[2])	{
											if(first.table2==TRUE)	{
															table2=output.table[,1]
															table2=cbind.all(table2,output.table)[,-1]
															first.table2=FALSE
															}else	{
																table2=cbind.all(table2,output.table)
																}
											}
						if(runfile[i]==run.list[3])	{
											if(first.table3==TRUE)	{
															table3=output.table[,1]
															table3=cbind.all(table3,output.table)[,-1]
															first.table3=FALSE
															}else	{
																table3=cbind.all(table3,output.table)
																}
											}
						if(runfile[i]==run.list[4])	{
											if(first.table4==TRUE)	{
															table4=output.table[,1]
															table4=cbind.all(table4,output.table)[,-1]
															first.table4=FALSE
															}else	{
																table4=cbind.all(table4,output.table)
																}
											}

						}
# for last file
		}else{
						if(runfile[i]==run.list[1])	{
											if(first.table1==TRUE)	{
															table1=output.table[,1]
															table1=cbind.all(table1,output.table)[,-1]
															first.table1=FALSE
															}else	{
																table1=cbind.all(table1,output.table)
																}
											}
						if(runfile[i]==run.list[2])	{
											if(first.table2==TRUE)	{
															table2=output.table[,1]
															table2=cbind.all(table2,output.table)[,-1]
															first.table2=FALSE
															}else	{
																table2=cbind.all(table2,output.table)
																}
											}
						if(runfile[i]==run.list[3])	{
											if(first.table3==TRUE)	{
															table3=output.table[,1]
															table3=cbind.all(table3,output.table)[,-1]
															first.table3=FALSE
															}else	{
																table3=cbind.all(table3,output.table)
																}
											}
						if(runfile[i]==run.list[4])	{
											if(first.table4==TRUE)	{
															table4=output.table[,1]
															table4=cbind.all(table4,output.table)[,-1]
															first.table4=FALSE
															}else	{
																table4=cbind.all(table4,output.table)
																}
											}

			}
			
				

close.nc(nc)
}
################
# END of loop to change file

nc = open.nc(datafile[i])

cat(paste("\n\n#################\n#### Read:",i,"file(s) /",nfile,"variables(s)\n"))
cat(paste("GRID:\n lat  =",var.get.nc(nc,mync.long.id)[mync.grid.long],"(",mync.grid.long,")\n long =",var.get.nc(nc,mync.lat.id)[mync.grid.lat],"(",mync.grid.long,")\n")) 
if(largefile==FALSE)	{
				cat(paste("DAY/YEAR=",mync.time.calendarday,"\n start date=",time(var.ts)[1],"\n end date=",time(var.ts)[length(var.ts)], "\n total data =",length(var.ts),"\n##Varibales(s)##\n"))
				}else	{
					cat(paste("DAY/YEAR=",mync.time.calendarday,"\n start date=",mync.time.start.year + var.time[1]/mync.time.calendarday,"\n end date=",mync.time.start.year + var.time[length(var.time)]/mync.time.calendarday
					, "\n total data =",length(var.time),"\n##Varibales(s)##\n"))
					}
cat(paste("#Table",run.list[1],"\n"))
cat(paste(colnames(table1),"\n"))
if(length(run.list)>1)	{
				cat(paste("#Table",run.list[2],"\n"))
				cat(paste(colnames(table2),"\n"))
				}
if(length(run.list)>2)	{
				cat(paste("#Table",run.list[3],"\n"))
				cat(paste(colnames(table3),"\n"))
				}
if(length(run.list)>3)	{
				cat(paste("#Table",run.list[4],"\n"))
				cat(paste(colnames(table4),"\n"))
				}

		

close.nc(nc)

setwd(dir1)
write.table(table1, file=paste(out.file,run.list[1],".csv"), sep=',', col.names=TRUE)
if(length(run.list)>1){write.table(table2, file=paste(out.file,run.list[2],".csv"), sep=',', col.names=TRUE)}
if(length(run.list)>2){write.table(table3, file=paste(out.file,run.list[3],".csv"), sep=',', col.names=TRUE)}
if(length(run.list)>3){write.table(table4, file=paste(out.file,run.list[4],".csv"), sep=',', col.names=TRUE)}
setwd(dir0)
