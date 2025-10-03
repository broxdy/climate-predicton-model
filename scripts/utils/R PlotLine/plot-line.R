maindir = "Temp/accessory/R PlotLine"
dir_name = "PlotLine LARS-WG"
#dir_name = "PlotLine SDSM"
x.title = "Year"


# 1st data
#data.f = "SDSM-48478MAXcal+vrf.csv" #SDSM
#data.f = "SDSM-48478MIN.csv" #SDSM
#data.f = "SDSM-48092.csv" #SDSM
#data.f = "LARS-WG-48478MINcal+vrf.csv" # LARS-WG
data.f = "LARS-WG-48478MAXcal+vrf.csv" # LARS-WG
#data.f = "LARS-WG-48092cal+vrf.csv" # LARS-WG

x.list.col = 4
dot.list.col = 6:35 # list of realization column
#dot.list.col = 6:10 # realization
#line1.list.col = 36 # avg
line1.list.col = 11 # avg
line2.list.col = 5 # obs
legend.list = c("realization","avg. of rlz.","obs.")
#horizontal.rot = TRUE

## Y axis label
# definition rain
#y.title = "daily rainfall (mm)"
#bp.ylim.max.sta = 150
#bp.ylim.min.sta = 0

# definition temp
y.title = "temperature (C)"
bp.ylim.max.sta = 40
bp.ylim.min.sta = 20

# PDF file
fpdf = substr(data.f,1,nchar(data.f)-4)
pdffile = paste("LinePlot-",fpdf,".pdf",sep="")

#n.sta.1 = 1

n.row = 1
n.col = 1
#n.col = n.sta.1

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
pdf(pdffile, width= 6, height = 4)
setwd(dir0)
par(ps =10,mgp=c(1.5,0.5,0),mai = c(0.8, 0.8, .8, .5))
#par(oma =  c(.8, .8, 2, .8))

## by max min values
#bp.ylim.max.sta = max(data.org[,c(dot.list.col,line1.list.col,line2.list.col)])
#bp.ylim.min.sta = min(data.org[,c(dot.list.col,line1.list.col,line2.list.col)])




##################
#### Plot box plot
cat("\n- Plot 1st daily line plot")
##########
y.list = data.org[,dot.list.col[1]]
plot(x.list,y.list,ylim = c(bp.ylim.min.sta,bp.ylim.max.sta), type="n", xlab = x.title, ylab = y.title,xlim = c(min(x.list)-1,max(x.list)))

##dotted line
col.use = dot.list.col
for(i in 1:length(col.use)){
y.list = data.org[,col.use[i]]
cat("\nPlot line1", colnames(data.org)[col.use[i]])
points(x.list,y.list , main = paste(data.f), type= "p", pch = 16, cex = 0.1)
}


####################################### use this one or below one
##line1 line
#col.use = line1.list.col
#for(i in 1:length(col.use)){
#y.list = data.org[,col.use[i]]
#cat("\nPlot dot2", colnames(data.org)[col.use[i]])
#lines(x.list,y.list , main = paste(data.f), type= "l", lty=2, lwd=0.1, col = "red")
#}

####################################### use this one or upper one
##line1 line
col.use = line1.list.col
for(i in 1:length(col.use)){
y.list = data.org[,col.use[i]]
cat("\nPlot line2", colnames(data.org)[col.use[i]])
lines(x.list,y.list , main = paste(data.f), type= "p", lty=0, lwd=0.1, col = "red",pch = 4,cex = 0.5)
}

##line2 line
col.use = line2.list.col
for(i in 1:length(col.use)){
y.list = data.org[,col.use[i]]
cat("\nPlot dot", colnames(data.org)[col.use[i]])
lines(x.list,y.list , main = paste(data.f), type= "l", lty =1, lwd=0.2, col = "blue")
}


legend(x = "topleft",  cex = 0.5, pt.cex = c(0.3,0.8,1), pch=c(16,4,-1), lty = c(0,0,1),  lwd = c(0,0.5,1), col = c("black","red","blue"),  legend = legend.list)
#legend(x = "bottomright",  cex = 0.5, pt.cex = c(0.3,0.8,1), pch=c(16,4,-1), lty = c(0,0,1),  lwd = c(0,0.5,1), col = c("black","red","blue"),  legend = legend.list)

#legend(x = "bottomright",  cex = 0.5, pt.cex = c(0.5,1,1), pch=c(16,-1,-1), lty = c(0,2,1),  lwd = c(0,0.5,1), col = c("black","red","blue"),  legend = legend.list) # for two line-types





cat("\n-- END script --\n")
dev.off()
