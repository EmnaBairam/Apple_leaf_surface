Pdb<-read.csv2("C:/Users/ebairam/Documents/Thesis/LeavesSurfaces/Stats/BS_inflo.txt",sep="\t",header=TRUE,dec=",",row.names=1)
Pdb<-subset(Pdb,Pdb$IdOrg!="Pt")

library(agricolae)
library(xlsx)
#_______________________________________________________________________________________________
#QFrom which stage of growth of the bourse shoot is the total leaf area
Gen="AR" #"AR"/FU"/"RB"
boxplot(Pdb$InfloTotalArea[Pdb$Genotype==Gen]~Pdb$GDD[Pdb$Genotype==Gen],xlab="GDD",ylab="TLA of Ariane bourse shoots")
#Q4.1. Leaf Surface
t<-table (Pdb$GDD[Pdb$Genotype==Gen & is.na(Pdb$InfloTotalArea)==FALSE])
write.xlsx(t,"C:/Users/ebairam/Documents/Thesis/LeavesSurfaces/Stats/SF/rst.xlsx")
##anova
a<-aov(Pdb$InfloTotalArea[Pdb$Genotype==Gen]~Pdb$GDD[Pdb$Genotype==Gen])
summary(a)
####residus####
res<-resid(a)
qqnorm(res)
shapiro.test(res) #si <<< pas de normalite
####anova####
an <- lm(Pdb$InfloTotalArea[Pdb$Genotype==Gen]~Pdb$GDD[Pdb$Genotype==Gen])
hist(resid(an),col="grey")
anova(an)
a<-SNK.test(an, "Pdb$GDD")
(a)
####kruskal####
#kru<-kruskal.test(Pdb$InfloTotalArea[Pdb$Genotype==Gen]~Pdb$GDD[Pdb$Genotype==Gen])
kru<-kruskal.test(Pdb$InfloTotalArea[Pdb$Genotype==Gen],Pdb$GDD[Pdb$Genotype==Gen])
print (kru)
#kru$statistics[2]
write.xlsx(a$groups,"C:/Users/ebairam/Documents/Thesis/LeavesSurfaces/Stats/SF/rst.xlsx")
#_______________________________________________________________________________________________
#R-Fig3
PdbL<-read.csv2("C:/Users/ebairam/Documents/Thesis/LeavesSurfaces/Stats/BS_leaves.txt",sep="\t",header=TRUE,dec=",",row.names=1)
PdbL<-subset(PdbL,(Genotype=="AR" & GDD>345)|(Genotype=="FU" & GDD>201)|(Genotype=="RB" & GDD>275))

PdbL$LMaxSq<-PdbL$LengthMax^2
PdbL$LengthSq<-PdbL$Length^2

library(agricolae)
library(xlsx)

#rank of the biggest leaves on BS
data<-subset(PdbL,AreaMax==Area)
t<-table(data$Genotype,data$Rank)
(t)
prop.table(table(data$Rank[data$Genotype=="RB"]))

barplot((table(data$Genotype,data$Rank)),ylim=c(0,100), beside=T,col=c('#A52A2A','#CD950C','#6E8B3D'),cex.axis=2,cex.names=2)
legend(18,98,legend=c("AR","FU","RB"),fill=c('#A52A2A','#CD950C','#6E8B3D'),cex=1.8)
#distribution of ILA
par(mfrow=c(2,2))
#BS
PdbL<-read.csv2("C:/Users/ebairam/Documents/Thesis/LeavesSurfaces/Stats/BS_leaves.txt",sep="\t",header=TRUE,dec=",",row.names=1)
PdbL<-subset(PdbL,(Genotype=="AR" & GDD>345)|(Genotype=="FU" & GDD>201)|(Genotype=="RB" & GDD>275))
for (Gen in levels(PdbL$Genotype)){
  data<-subset(PdbL,Genotype==Gen)
  data$Genotype<-factor(data$Genotype)
  plot(0,0,xlim=c(1,18),ylim=c(0,90),type="n",xlab="", ylab="",cex.axis=1.8,main=paste(Gen, " bourse shoot"), cex.main=1.8)
  interaction.plot(data$Rank,data$IdBS,data$Area,legend=F,lty=1,add=T,main=Gen,axes=F)
}
#RO
RoL<-read.csv2("C:/Users/ebairam/Documents/Thesis/LeavesSurfaces/Stats/RO_Leaves.txt",sep="\t",header=TRUE,dec=",",row.names=1)
RoL<-subset(RoL,RoL$Year==2015)
interaction.plot(RoL$IdObj,RoL$IdShoot,RoL$Area,legend=F,lty=1,xlab="",ylab="",cex.axis=1.8,cex.main=1.8,main="FU rosette")
#____Rank of the biggest leaf

#_______________________________________________________________________________________________
#R-Fig4
par(mfrow=c(2,2))

PdbL<-read.csv2("C:/Users/ebairam/Documents/Thesis/LeavesSurfaces/Stats/BS_leaves.txt",sep="\t",header=TRUE,dec=",",row.names=1)
PdbL<-subset(PdbL,(Genotype=="AR" & GDD>345)|(Genotype=="FU" & GDD>201)|(Genotype=="RB" & GDD>275))
PdbL<-subset(PdbL,NbrLeaves>1)
PdbL$NRank<-(PdbL$Rank-2)/PdbL$NbrLeaves
PdbL<-subset(PdbL,NRank>-0.4)
PdbL$NArea<-PdbL$Area/PdbL$AreaMax

for (Gen in levels(PdbL$Genotype)){
  data<-subset(PdbL,Genotype==Gen)
  data$Genotype<-factor(data$Genotype)
  plot(-0.4,0,xlim=c(-0.4,1),ylim=c(0,1),type="n", main=paste(Gen," bourse shoot"),cex.main=1.8,cex.axis=1.8,xlab="",ylab="")
  points(data$NRank,data$NArea,legend=F,pch=16,cex=0.4,axes=F)
  data$IdBS<-factor(data$IdBS)
  #   for (i in levels(data$IdBS)){
  #     lines(data$NRank[data$IdBS==i],data$NArea[data$IdBS==i],legend=F,pch=16,cex=0.4,axes=F)
  #   }
  #   points(data$NRank,data$NArea,legend=F,pch=16,cex=0.4,axes=F,col=data$NbrLeaves)
}


RoL<-read.csv2("C:/Users/ebairam/Documents/Thesis/LeavesSurfaces/Stats/RO_Leaves.txt",sep="\t",header=TRUE,dec=",",row.names=1)
RoL<-subset(RoL,RoL$Year==2015)

RoL$NRank<-(RoL$Rank)/RoL$NbrLeaves
RoL$NArea<-RoL$Area/RoL$AreaMax
plot(RoL$NRank,RoL$NArea,legend=F,main="FU rosette",xlab="",ylab="",pch=16,cex=0.4,cex.main=1.8,cex.axis=1.8)

par(mfrow=c(1,1))

