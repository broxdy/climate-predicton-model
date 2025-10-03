maindir = "Temp/accessory/R PlotLine"
#dir_name = "PlotLine SDSM"
dir_name = "PlotLine HiRes+GCM"
#par.name = "monthly rainfall (mm/day)"
par.name = "temperature (C)"
x.title = paste("observed",par.name)
y.title = paste("predicted",par.name)

limit.by.define = TRUE # IF not then by max min values themselves
limit.max = 30# max temp 40, min temp 30, PCP 15
limit.min = 15# max temp 25, min temp 15, PCP 0

# 1st data
#data.f = "qqPlot-GHpcp.csv"
#data.f = "qqPlot-GHtmax.csv"
#data.f = "qqPlot-GHtmin.csv"

#data.f = "qqPlot-Hpcp.csv"
#data.f = "qqPlot-Htmax.csv"
data.f = "qqPlot-Htmin.csv"


x.list.col = 1 # obs
y.list.col = 2 # realization
#y.list.col = 2:35 # realization
#legend.list = c("realization","avg. of rlz.","obs.")
#horizontal.rot = TRUE

# PDF file
pdffile = paste("LineQQPlot-",data.f,".pdf",sep="")

# Check working directory
dir_name1 = dir_name
L=file.exists(dir_name1)  
if(!L) dirfile=dir.create(dir_name1)
dirfile=(dir_name1)

dir1=dirname(file.path("D:",paste(maindir),paste(dirfile),"dummy"))
dir0=dirname(file.path("D:",paste(maindir),"dummy"))
#source("clear.R")
#clear()


############# 1st dialy files (obs)
cat("\nReading 1st data")
data.org =  read.table(data.f, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
x.list = data.org[,x.list.col]



## Plot line

cat("\n## Plot line")
setwd(dir1)
pdf(pdffile, width= 4, height = 4)
setwd(dir0)
par(ps =10,mgp=c(1.5,0.5,0),mai = c(0.8, 0.8, .8, .5))
#par(oma =  c(.8, .8, 2, .8))


if(limit.by.define){
	## definition
	bp.ylim.max.sta = limit.max
	bp.ylim.min.sta = limit.min
	}else{
		## by max min values
		bp.ylim.max.sta = max(data.org[,c(x.list.col,y.list.col)])
		bp.ylim.min.sta = min(data.org[,c(x.list.col,y.list.col)])
		}



##################
#### Plot box plot
cat("\n- Plot 1st daily line plot")
##########
y.list = data.org[,y.list.col[1]]
plot(x.list,y.list,xlim = c(bp.ylim.min.sta,bp.ylim.max.sta),ylim = c(bp.ylim.min.sta,bp.ylim.max.sta), type="n", xlab = x.title, ylab = y.title)

##dotted plot
col.use = y.list.col
for(i in 1:length(col.use)){
y.list = data.org[,col.use[i]]
cat("\nPlot line1", colnames(data.org)[col.use[i]])
points(x.list,y.list , main = paste(data.f), type= "p", pch = 4, cex = 0.8,col = "blue")
}

##diagonal plot
lines(c(-1000,1000),c(-1000,1000))




cat("\n-- END script --\n")
dev.off()
