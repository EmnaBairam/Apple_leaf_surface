library(agricolae)
library(xlsx)

#__________________________________________
#Bourse shoot beta_________________________
#__________________________________________
#ANOVA, etc
#Comaprison between genotypes of the paraeter beta(f1)
Pdb<-read.csv2("C:/Users/ebairam/Documents/Thesis/LeavesSurfaces/Stats/BS_inflo.txt",sep="\t",header=TRUE,dec=",",row.names=1)
Pdb<-subset(Pdb,Pdb$IdOrg!="Pt")
PdbSelect<-subset(Pdb,(Genotype=="AR" & GDD>345)|(Genotype=="FU" & GDD>201)|(Genotype=="RB" & GDD>275))

PdbSelect$GDD<-factor(PdbSelect$GDD)
PdbSelect$f1<-PdbSelect$AreaMax*PdbSelect$NbrLeaves

#beta
PdbSelect$beta<-PdbSelect$InfloTotalArea/PdbSelect$f1
table (PdbSelect$Genotype[is.na(PdbSelect$InfloTotalArea)==FALSE])
##anova
a<-aov(PdbSelect$beta~PdbSelect$Genotype)
summary(a)
####residus####
res<-resid(a)
qqnorm(res)
shapiro.test(res) #si <<< pas de normalite
####anova####
an <- lm(PdbSelect$beta~PdbSelect$Genotype)
hist(resid(an),col="grey")
anova(an)
a<-SNK.test(an, "PdbSelect$Genotype")
(a) 
####kruskal####
kru<-kruskal.test(PdbSelect$beta,PdbSelect$Genotype)
print (kru)
#__________________________________________
#beta#___AR and FU___
Pdb<-read.csv2("C:/Users/ebairam/Documents/Thesis/LeavesSurfaces/Stats/BS_inflo.txt",sep="\t",header=TRUE,dec=",",row.names=1)
Pdb<-subset(Pdb,Pdb$IdOrg!="Pt")
PdbSelect<-subset(Pdb,(Genotype=="AR" & GDD>345)|(Genotype=="FU" & GDD>201)|(Genotype=="RB" & GDD>275))
PdbSelect$f1<-PdbSelect$AreaMax*PdbSelect$NbrLeaves
PdbSelect$Bin<-ifelse(as.numeric(substr(PdbSelect$IdInflo,3,5))%%3==0,0,1)
table(PdbSelect$Genotype[is.na(PdbSelect$InfloTotalArea)==FALSE],PdbSelect$Bin[is.na(PdbSelect$InfloTotalArea)==FALSE])
tset<-PdbSelect[PdbSelect$Genotype!="RB" & PdbSelect$Bin==1 & is.na(PdbSelect$InfloTotalArea)==FALSE,]
vset<-PdbSelect[PdbSelect$Genotype!="RB" & PdbSelect$Bin==0  & is.na(PdbSelect$InfloTotalArea)==FALSE,]

#construire le modele avec une ordonnee a l'origine = 0
y=tset$InfloTotalArea
x=tset$f1
mBS<-lm(tset$InfloTotalArea~tset$f1-1)
hist(resid(mBS),col="grey")
par(mfrow=c(3,2))
plot(mBS,which=1:6,ask=FALSE)
par(mfrow=c(1,1))
summary(mBS)
plot(0,0,xlim=c(0,900),ylim=c(0,600),type="n",xlab="",ylab="",cex.axis=1.8)
points(tset$InfloTotalArea~tset$f1,lwd=2,cex=1,
       col=ifelse(tset$Genotype=="AR",'#A52A2A',ifelse(tset$Genotype=="FU",'#CD950C','#6E8B3D')),
       pch=ifelse(tset$Genotype=="AR",16,ifelse(tset$Genotype=="FU",4,17)))
#legend("topleft",inset=.1,legend=c("AR","FU"),pch=c(16,4),col=c('#A52A2A','#CD950C'),lty=0,lwd=5,cex=2.4)
#legend("topleft",inset=.1,legend=c("AR","FU","RB"),pch=c(16,4,17),col=c('#A52A2A','#CD950C','#6E8B3D'),lty=0,lwd=5,cex=2.4)
#legend(500,120,legend=c(paste(beta,coefficients(mBS)),summary(mBS)$r.squared))
mini=min(x,na.rm=T)
maxi=max(x,na.rm=T)
w=seq(mini,maxi,le=1000)
lines(w,w*coefficients(mBS),col="black",lwd=2)

#validation
LT<-nrow(tset) #number of samples in training set
LV<-nrow(vset) #number of samples in validation set
x<-vset$f1
y<-vset$InfloTotalArea
(RSS.p <- sum(residuals(mBS)^2))  # Residual sum of squares
(TSS <- sum((y - mean(y))^2))  # Total sum of squares
Rs<-1 - (LV/LT*RSS.p/TSS)  # R-squared measure
Rs
p=1 #number of predictors
Rsa<-1-((1-Rs)*(length(y)-1)/(length(y)-1-1))
Rsa


y=vset$InfloTotalArea
x=vset$f1
plot(0,0,xlim=c(0,900),ylim=c(0,600),type="n",xlab="",ylab="",cex.axis=1.8)
points(vset$InfloTotalArea~vset$f1,lwd=2,cex=1,
       col=ifelse(vset$Genotype=="AR",'#A52A2A',ifelse(vset$Genotype=="FU",'#CD950C','#6E8B3D')),
       pch=ifelse(vset$Genotype=="AR",16,ifelse(vset$Genotype=="FU",4,17)))
#legend("topleft",inset=.1,legend=c("AR","FU"),pch=c(16,4),col=c('#A52A2A','#CD950C'),lty=0,lwd=5,cex=2.4)
#legend("topleft",inset=.1,legend=c("AR","FU","RB"),pch=c(16,4,17),col=c('#A52A2A','#CD950C','#6E8B3D'),lty=0,lwd=5,cex=2.4)
#legend(500,120,legend=c(paste(beta,coefficients(mBS)),summary(mBS)$r.squared))
mini=min(x,na.rm=T)
maxi=max(x,na.rm=T)
w=seq(mini,maxi,le=1000)
lines(w,w*coefficients(mBS),col="black",lwd=2)

#beta___RB___
Pdb<-read.csv2("C:/Users/ebairam/Documents/Thesis/LeavesSurfaces/Stats/BS_inflo.txt",sep="\t",header=TRUE,dec=",",row.names=1)
Pdb<-subset(Pdb,Pdb$IdOrg!="Pt")
PdbSelect<-subset(Pdb,(Genotype=="AR" & GDD>345)|(Genotype=="FU" & GDD>201)|(Genotype=="RB" & GDD>275))
PdbSelect$f1<-PdbSelect$AreaMax*PdbSelect$NbrLeaves
PdbSelect$Bin<-ifelse(as.numeric(substr(PdbSelect$IdInflo,3,5))%%3==0,0,1)
table(PdbSelect$Genotype[is.na(PdbSelect$InfloTotalArea)==FALSE],PdbSelect$Bin[is.na(PdbSelect$InfloTotalArea)==FALSE])
tset<-PdbSelect[PdbSelect$Genotype=="RB" & PdbSelect$Bin==1 & is.na(PdbSelect$InfloTotalArea)==FALSE,]
vset<-PdbSelect[PdbSelect$Genotype=="RB" & PdbSelect$Bin==0  & is.na(PdbSelect$InfloTotalArea)==FALSE,]

#construire le modele avec une ordonnee a l'origine = 0
mBS<-lm(tset$InfloTotalArea~tset$f1-1)
hist(resid(mBS),col="grey")
par(mfrow=c(3,2))
plot(mBS,which=1:6,ask=FALSE)
par(mfrow=c(1,1))
summary(mBS)

y=tset$InfloTotalArea
x=tset$f1
plot(0,0,xlim=c(0,1350),ylim=c(0,900),type="n",xlab="",ylab="",cex.axis=1.8)
points(tset$InfloTotalArea~tset$f1,cex=1,lwd=2,
       col=ifelse(tset$Genotype=="AR",'#A52A2A',ifelse(tset$Genotype=="FU",'#CD950C','#6E8B3D')),
       pch=ifelse(tset$Genotype=="AR",16,ifelse(tset$Genotype=="FU",4,17)))
mini=min(x,na.rm=T)
maxi=max(x,na.rm=T)
w=seq(mini,maxi,le=1350)
lines(w,w*coefficients(mBS),col="black",lwd=2)

#validation
LT<-nrow(tset) #number of samples in training set
LV<-nrow(vset) #number of samples in validation set
x<-vset$f1
y<-vset$InfloTotalArea
(RSS.p <- sum(residuals(mBS)^2))  # Residual sum of squares
(TSS <- sum((y - mean(y))^2))  # Total sum of squares
Rs<-1 - (LV/LT*RSS.p/TSS)  # R-squared measure
Rs
p=1 #number of predictors
Rsa<-1-((1-Rs)*(length(y)-1)/(length(y)-1-1))
Rsa

y=vset$InfloTotalArea
x=vset$f1
plot(0,0,xlim=c(0,1350),ylim=c(0,900),type="n",xlab="",ylab="",cex.axis=1.8)
points(vset$InfloTotalArea~vset$f1,cex=1,lwd=2,
       col=ifelse(vset$Genotype=="AR",'#A52A2A',ifelse(vset$Genotype=="FU",'#CD950C','#6E8B3D')),
       pch=ifelse(vset$Genotype=="AR",16,ifelse(vset$Genotype=="FU",4,17)))

mini=min(x,na.rm=T)
maxi=max(x,na.rm=T)
w=seq(mini,maxi,le=1000)
lines(w,w*coefficients(mBS),col="black",lwd=2)
#_______________________________________________________________________________________________
#__________________________________________
#Bourse shoot leaf shape : e and k_________
#__________________________________________
#the k factor
PdbL<-read.csv2("C:/Users/ebairam/Documents/Thesis/LeavesSurfaces/Stats/BS_leaves.txt",sep="\t",header=TRUE,dec=",",row.names=1)
PdbL<-subset(PdbL,(Genotype=="AR" & GDD>345)|(Genotype=="FU" & GDD>201)|(Genotype=="RB" & GDD>275))

PdbL$k<-PdbL$Width/PdbL$Length #k is defined as the ratio between the width and the length of the leaf
PdbL$e<-sqrt((PdbL$Length)^2-(PdbL$Width)^2)/PdbL$Length
#PdbL$k<-sqrt(1-PdbL$e)

library(agricolae)
library(xlsx)


#___________________________________________
#Visualisation
#___________________________________________
par(mfrow=c(2,2))
plot(Length~Width,data=PdbL,pch=16,cex=0.3,col=Genotype, main="All genotypes",xlab="Leaf Width", ylab="Leaf length")
Gen = levels (PdbL$Genotype)
for (i in Gen){
  plot(Length[PdbL$Genotype==i]~Width[PdbL$Genotype==i],data=PdbL,pch=16,cex=0.3,main=i,xlab="Leaf Width", ylab="Leaf length")
}
par(mfrow=c(1,1))

#___________________________________________
#Anova tests
#___________________________________________
#Q1. Is k constant among genotypes?
table (PdbL$Genotype)
##anova
a<-aov(PdbL$e~PdbL$Genotype)
summary(a)
####residus####
res<-resid(a)
qqnorm(res)
shapiro.test(res) #si <<< pas de normalite
####anova####
an <- lm(PdbL$e~PdbL$Genotype)
hist(resid(an),col="grey")
anova(an)
a<-SNK.test(an, "PdbL$Genotype")
(a)
#kruskal
kru<-kruskal.test(PdbL$e,PdbL$Genotype)
print (kru)
write.xlsx(a$groups,"C:/Users/ebairam/Documents/Thesis/LeavesSurfaces/Stats/SF/rst.xlsx")

#Q2. Is k constant among ranks for each genotype
Gen="AR" #"AR"/"FU"and"RB"
#PdbL<-subset(PdbL,PdbL$Genotype==Gen)
t<-table (PdbL$Rank[PdbL$Genotype==Gen])
t
write.xlsx(t,"C:/Users/ebairam/Documents/Thesis/LeavesSurfaces/Stats/SF/rst.xlsx")
#table(PdbL$NbrLeaves[PdbL$Genotype==Gen & is.na(PdbL$InfloTotalArea)==FALSE])
#hist(PdbL$NbrLeaves)
##anova
a<-aov(PdbL$e[PdbL$Genotype==Gen]~PdbL$Rank[PdbL$Genotype==Gen])
summary(a)
####residus####
res<-resid(a)
qqnorm(res)
shapiro.test(res) #si <<< pas de normalite
####anova####
an <- lm(PdbL$e[PdbL$Genotype==Gen]~PdbL$Rank[PdbL$Genotype==Gen])
hist(resid(an),col="grey")
anova(an)
a<-SNK.test(an, "PdbL$Rank")
(a)
write.xlsx(a$groups,"C:/Users/ebairam/Documents/Thesis/LeavesSurfaces/Stats/SF/rst.xlsx")

#Q3. Is k constant among years for the FU genotype
PdbL$Year<-factor(PdbL$Year)
Gen="FU" #"AR"/"FU"and"RB"
#PdbL<-subset(PdbL,PdbL$Genotype==Gen)
t<-table (PdbL$Year[PdbL$Genotype==Gen])
t
write.xlsx(t,"C:/Users/ebairam/Documents/Thesis/LeavesSurfaces/Stats/SF/rst.xlsx")
#table(PdbL$NbrLeaves[PdbL$Genotype==Gen & is.na(PdbL$InfloTotalArea)==FALSE])
#hist(PdbL$NbrLeaves)
##anova
a<-aov(PdbL$e[PdbL$Genotype==Gen]~PdbL$Year[PdbL$Genotype==Gen])
summary(a)
####residus####
res<-resid(a)
qqnorm(res)
shapiro.test(res) #si <<< pas de normalite
####anova####
an <- lm(PdbL$e[PdbL$Genotype==Gen]~PdbL$Year[PdbL$Genotype==Gen])
hist(resid(an),col="grey")
anova(an)
a<-SNK.test(an, "PdbL$Year")
(a)
kru<-kruskal.test(PdbL$e[PdbL$Genotype==Gen]~PdbL$Year[PdbL$Genotype==Gen])
kru
#___________________________________________
#Determining k factor
#___________________________________________
#the k factor
PdbL<-read.csv2("C:/Users/ebairam/Documents/Thesis/LeavesSurfaces/Stats/BS_leaves.txt",sep="\t",header=TRUE,dec=",",row.names=1)
PdbL<-subset(PdbL,(Genotype=="AR" & GDD>345)|(Genotype=="FU" & GDD>201)|(Genotype=="RB" & GDD>275))
PdbL$k<-PdbL$Width/PdbL$Length

PdbL$Bin<-ifelse(as.numeric(substr(PdbL$IdInflo,3,5))%%3==0,0,1)
table(PdbL$Genotype,PdbL$Bin)

Gen="RB" #"AR"/"FU"/"RB"
couleur<-ifelse(Gen=="AR",'#A52A2A',ifelse(Gen=="FU",'#CD950C','#6E8B3D'))
typepch<-ifelse(Gen=="AR",16,ifelse(Gen=="FU",4,17))

tset<-PdbL[PdbL$Genotype==Gen & PdbL$Bin==1,]
tset$Genotype<-factor(tset$Genotype)
vset<-PdbL[PdbL$Genotype==Gen & PdbL$Bin==0,]

#construire le modele avec une ordonnee a l'origine = 0
kmodel<-lm(tset$Width~tset$Length-1)
hist(resid(kmodel),col="grey")
par(mfrow=c(3,2))
plot(kmodel,which=1:6,ask=FALSE)
par(mfrow=c(1,1))
summary(kmodel)

#plot(tset$Width~tset$Length,cex=1.8,xlab="",ylab="",cex.axis=2.4,lwd=5,
#col=couleur,
#pch=typepch)

y=tset$Width
x=tset$Length

plot(0,0,xlim=c(0,14),ylim=c(0,14),type="n",xlab="",ylab="",cex.axis=1.8)
points(tset$Width~tset$Length,cex=1,lwd=2,
       col=couleur,
       pch=typepch)
mini=min(x,na.rm=T)
maxi=max(x,na.rm=T)
w=seq(mini,maxi,le=1000)
lines(w,w*coefficients(kmodel),col="black",lwd=2)

#validation
LT<-nrow(tset) #number of samples in training set
LV<-nrow(vset) #number of samples in validation set
x<-vset$Length
y<-vset$Width
(RSS.p <- sum(residuals(kmodel)^2))  # Residual sum of squares
(TSS <- sum((y - mean(y))^2))  # Total sum of squares
Rs<-1 - (LV/LT*RSS.p/TSS)  # R-squared measure
Rs
p=1 #number of predictors
Rsa<-1-((1-Rs)*(length(y)-1)/(length(y)-1-1))
Rsa

y=vset$Width
x=vset$Length

plot(0,0,xlim=c(0,14),ylim=c(0,14),type="n",xlab="",ylab="",cex.axis=1.8)
points(vset$Width~vset$Length,cex=1,cex.axis=1.8,lwd=2,
       col=couleur,
       pch=typepch)
mini=min(x,na.rm=T)
maxi=max(x,na.rm=T)
w=seq(mini,maxi,le=1000)
lines(w,w*coefficients(kmodel),col="black",lwd=2)

