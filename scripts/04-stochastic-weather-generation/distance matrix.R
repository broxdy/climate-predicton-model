dir_name = "Weight Matrix"
maindir = "Temp/Stochastic"

# data file
data.f = "coordinate.csv"
# result file
result.f = "distance.csv"


data <- read.table(data.f, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

data.n = dim(data)[1]


# Check working directory
dir_name1 = dir_name
L=file.exists(dir_name1)  
if(!L) dirfile=dir.create(dir_name1)
dirfile=(dir_name1)

dir1=dirname(file.path("D:",paste(maindir),paste(dirfile),"dummy"))
dir0=dirname(file.path("D:",paste(maindir),"dummy"))

x.col = 3
y.col = 4

weight.m = array(NA,c(data.n,data.n))

for(i in 1:data.n){
			for(j in 1:data.n){
						w.ij = ((data[i,x.col]-data[j,x.col])^2+(data[i,y.col]-data[j,y.col])^2)^0.5
						weight.m[i,j] = w.ij
						}
			}

setwd(dir1)
write.csv(weight.m, file = result.f)
setwd(dir0)
