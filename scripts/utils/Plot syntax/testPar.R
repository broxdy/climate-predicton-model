pdf("testPar plot.pdf", width = 20 ,height = 20)
par(ps =24, mfrow=c(2,2))
n.interval = 2

m = seq(from =1,to =3, length.out=n.interval)
for(k in 1:n.interval){
for(i in 1:n.interval){
for(j in 1:n.interval){


mgp.val = c(m[i],1,0)
mai.val = c(m[j],1,1,1)
oma.val =  c(m[k],1,1,1)

par(mgp=mgp.val)
par(mai = mai.val)
par(oma = oma.val)
plot(1:10,1:10,main=paste("POS:1 i =",i,"j =",j,"k =",k),xlab="X axis 123",ylab="Y axis 123")
text(2,8,labels=paste("mgp:",mgp.val[1],mgp.val[2],mgp.val[3],mgp.val[4],sep=","))
text(2,6,labels=paste("mai:",mai.val[1],mai.val[2],mai.val[3],mai.val[4],sep=","))
text(2,4,labels=paste("oma:",oma.val[1],oma.val[2],oma.val[3],oma.val[4],sep=","))


}
}
}


for(k in 1:n.interval){
for(i in 1:n.interval){
for(j in 1:n.interval){


mgp.val = c(1,m[i],0)
mai.val = c(1,m[j],1,1)
oma.val =  c(1,m[k],1,1)

par(mgp=mgp.val)
par(mai = mai.val)
par(oma = oma.val)
plot(1:10,1:10,main=paste("POS:2 i =",i,"j =",j,"k =",k),xlab="X axis 123",ylab="Y axis 123")
text(2,8,labels=paste("mgp:",mgp.val[1],mgp.val[2],mgp.val[3],mgp.val[4],sep=","))
text(2,6,labels=paste("mai:",mai.val[1],mai.val[2],mai.val[3],mai.val[4],sep=","))
text(2,4,labels=paste("oma:",oma.val[1],oma.val[2],oma.val[3],oma.val[4],sep=","))

}
}
}

for(k in 1:n.interval){
for(i in 1:n.interval){
for(j in 1:n.interval){


mgp.val = c(1,1,m[i])
mai.val = c(1,1,m[j],1)
oma.val =  c(1,1,m[k],1)

par(mgp=mgp.val)
par(mai = mai.val)
par(oma = oma.val)
plot(1:10,1:10,main=paste("POS:3 i =",i,"j =",j,"k =",k),xlab="X axis 123",ylab="Y axis 123")
text(2,8,labels=paste("mgp:",mgp.val[1],mgp.val[2],mgp.val[3],mgp.val[4],sep=","))
text(2,6,labels=paste("mai:",mai.val[1],mai.val[2],mai.val[3],mai.val[4],sep=","))
text(2,4,labels=paste("oma:",oma.val[1],oma.val[2],oma.val[3],oma.val[4],sep=","))


}
}
}

for(k in 1:n.interval){
for(i in 1:n.interval){
for(j in 1:n.interval){


mgp.val = c(1,1,m[i])
mai.val = c(1,1,1,m[j])
oma.val =  c(1,1,1,m[k])

par(mgp=mgp.val)
par(mai = mai.val)
par(oma = oma.val)
plot(1:10,1:10,main=paste("POS:4 i =",i,"j =",j,"k =",k),xlab="X axis 123",ylab="Y axis 123")
text(2,8,labels=paste("mgp:",mgp.val[1],mgp.val[2],mgp.val[3],mgp.val[4],sep=","))
text(2,6,labels=paste("mai:",mai.val[1],mai.val[2],mai.val[3],mai.val[4],sep=","))
text(2,4,labels=paste("oma:",oma.val[1],oma.val[2],oma.val[3],oma.val[4],sep=","))

}
}
}

dev.off()
cat("\nEND script")
#