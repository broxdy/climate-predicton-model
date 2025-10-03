library(RNetCDF)
library(chron)
library(GenKern)

## Read input list sheet
#sheetf = "miub_echo_g-20c3m-atm-da-onlyfile.txt"
sheetf = "short-list.txt"
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
dir0=dirname(file.path("D:","Temp","ncdf","dummy"))
dir1=dirname(file.path("D:","Temp","ncdf",paste(dir_name),"dummy"))


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
var.data = var.get.nc(nc, allvar.n-1, c(mync.grid.long,mync.grid.lat,1),c(1,1,NA))

# Convert variaable to time series
var.ts = ts(var.data, frequency=360,start=(c(mync.time.start.year+var.time[1]/mync.time.calendarday,mync.time.start.month,mync.time.start.day)))
var.table = as.data.frame(var.ts)
colnames(var.table) = var.name

# Record time dimension to output table
if(i == 1){
	output.table = as.data.frame(time(var.ts))
	colnames(output.table) = "Time"
}
# End of record time dimension

# Attach varible output to former output table
output.table = cbind(output.table,var.table)

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
