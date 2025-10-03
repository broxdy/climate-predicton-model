library(RNetCDF)
library(chron)
library(GenKern)

### INITIAL CONSTANT ####
# set when file need to be bounded with another file
extend.dat = FALSE

## Read input list sheet
sheetf = "miub_echo_g-20c3m-atm-da-onlyfile.txt"
#sheetf = "short-list.txt"
sheet.list <- read.table(sheetf, stringsAsFactors=FALSE,header=FALSE, sep=",",dec=".",na.strings="-")

datafile = sheet.list[,1]
## or specify source file by table here below
#datafile = c("met-ee.csv","met-ss.csv","met-cc.csv","met-ne.csv","met-nn.csv")
#datafile = c("psl_A2_a42_0108-0147.nc")

nfile = length(datafile)

# Output name
out.file = "output.csv"

mync.time.id = 4 # Position for time dimension
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
mync.time.start.year = 1860
mync.time.calendarday = 360 # Day in a calendar year


## specify output folder here
dir_name ="readncdf2"

L=file.exists(dir_name)  
if(!L) dir.create(dir_name)
dir0=dirname(file.path("C:","Downloads","CMIP3","dummy"))
dir1=dirname(file.path("C:","Temp","CMIP3",paste(dir_name),"dummy"))


##############################
## Read NCDF file start here #

### START loop to change file ###
for(i in 1:nfile){

nc = open.nc(datafile[i])
print.nc(nc)

# Synchronize to disk
#sync.nc(nc)

# Define dimensions
mync.dim = file.inq.nc(nc)
allvar.n = mync.dim$nvars


# Read Time
var.time = var.get.nc(nc, mync.time.id)


# Find closest grid cell
mync.grid.long = nearest(var.get.nc(nc,mync.long.id),my.exact.long)
mync.grid.lat = nearest(var.get.nc(nc,mync.lat.id),my.exact.lat)




# Read Varidable properties
# at the last position of variables
var.info = var.inq.nc(nc, allvar.n-1)
var.name = var.info$name

## Check dimension and read data of extracted variable
# if have 3 dimension
if(var.info$ndims == 3)	{
		var.data = var.get.nc(nc, allvar.n-1, c(mync.grid.long,mync.grid.lat,1),c(1,1,NA))
		# Convert variaable to table and put header
		var.table = as.data.frame(var.data)
		colnames(var.table) = var.name
				}else	{


					# if have 4 dimension
					if(var.info$ndims == 4)	{
									subvar.n = length(var.get.nc(nc, allvar.n-1, c(mync.grid.long,mync.grid.lat,1,1),c(1,1,NA,1)))
									subvar.name = var.get.nc(nc, mync.subvar.id, 1,NA)
									# Loop for sub variable
									for(j in 1:subvar.n)	{
													var.data = var.get.nc(nc, allvar.n-1, c(mync.grid.long,mync.grid.lat,j,1),c(1,1,1,NA))
													subvar.table = as.data.frame(var.data)
													colnames(subvar.table) = paste(var.name,subvar.name[j])
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


# Record time dimension to output table
if(i == 1){
	var.ts = ts(var.data, frequency=360,start=(c(mync.time.start.year+var.time[1]/mync.time.calendarday,mync.time.start.month,mync.time.start.day)))
	output.table = as.data.frame(time(var.ts))
	colnames(output.table) = "Time"
}
# End of record time dimension

# Combine varible output to former output table
if(length(rownames(var.table)) < length(rownames(output.table))	{
											if(extend.dat == TRUE)	{
															output.table = rbind(var.table.extend,output.table)
															extend.dat = FALSE
															}else	{
																extend.dat = TRUE
																var.table.extend = var.table
																}
# For narmal case full data within one file
if(extend.dat == FALSE)	{
				output.table = cbind(output.table,var.table)
				}
				
				

close.nc(nc)
}
################
# END of loop to change file

nc = open.nc(datafile[i])

cat(paste("\n#####\n#**# Read",nfile,"file(s)\n"))
cat(paste("GRID:\n lat  =",var.get.nc(nc,mync.long.id)[mync.grid.long],"(",mync.grid.long,")\n long =",var.get.nc(nc,mync.lat.id)[mync.grid.lat],"(",mync.grid.long,")\n")) 
cat(paste("DAY/YEAR=",mync.time.calendarday,"\n start date=",time(var.ts)[1],"\n end date=",time(var.ts)[length(var.ts)], "\n total data =",length(var.ts),"\n"))
cat(paste(colnames(output.table),"\n"))

close.nc(nc)

setwd(dir1)
write.table(output.table, file=out.file, sep=',', col.names=TRUE)
setwd(dir0)
