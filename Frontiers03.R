Int<-read.csv2("C:/Users/ebairam/Documents/Thesis/LeavesSurfaces/Stats/Internodes2016.txt",sep="\t",header=TRUE,dec=",",row.names=1)

Int$NPos<-Int$Abs_Position/Int$Abs_Length
Int$NRank<-Int$IdInternodeAbs/Int$Abs_MaxInternode

#Fig06_chap2
plot(0,0,xlim=c(0,18),ylim=c(0,320),type="n",xlab="",ylab="",cex.axis=1.4)
for (i in (levels(Int$IdAbsShoot))){
  lines (Int$Abs_Position[Int$IdAbsShoot==i]~Int$IdInternodeAbs[Int$IdAbsShoot==i])
  points (Int$Abs_Position[Int$IdAbsShoot==i & Int$Shoot=="RO"]~Int$IdInternodeAbs[Int$IdAbsShoot==i & Int$Shoot=="RO"],col="dodgerblue3",pch=14,cex=0.8)
  points (Int$Abs_Position[Int$IdAbsShoot==i & Int$Shoot=="P1"]~Int$IdInternodeAbs[Int$IdAbsShoot==i & Int$Shoot=="P1"],col='#CD950C',pch=16,cex=0.8)
  points (Int$Abs_Position[Int$IdAbsShoot==i & Int$Shoot=="P2"]~Int$IdInternodeAbs[Int$IdAbsShoot==i & Int$Shoot=="P2"],col='#CD950C',pch=16,cex=0.8)
}
#Fig06_chap2

#fig15_chap2
plot(0,0,xlim=c(0,1),ylim=c(0,1),type="n",xlab="",ylab="",cex.axis=1.4)
points(Int$NRank,Int$NPos,pch=4,cex=1,col='#CD950C',lwd=2,xlab="",ylab="")
points(Int$NRank[Int$Shoot=="RO"],Int$NPos[Int$Shoot=="RO"],pch=4,cex=1,lwd=2,xlab="",ylab="",col="dodgerblue3")


x<-Int$NRank
y<-Int$NPos
model<-getInitial(y~SSlogis(x,NSL,xo,s),data=cbind.data.frame(x,y))
model

w<-seq(0,1,le=1000)
lines(w,SSlogis(w,model[1],model[2],model[3]),lwd=2)
#fig15_chap2

#RMSE
yy<-SSlogis(x,model[1],model[2],model[3])
rmse<-sqrt(mean((yy-y)^2))
rmse

#R²
cr<-1-(sum((y-yy)^2)/sum((y-mean(y))^2))
cr
