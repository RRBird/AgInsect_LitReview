

library("arm")
library("AICcmodavg")
library("lme4")

#Analysis for Lit review

Litdata <- rawlit

head(Litdata,3);dim(Litdata)

table(Litdata$Spatial_Scale)





#QUESTION #: Geographic----

##Continents----
head(Litdata);dim(Litdata)

geo<-Litdata
geo<-geo[-c(which(geo$Continent=="Multiple")),]
#removed the two articles which had multiple continents
head(geo,3);dim(geo)

Con_Chi <- geo[sample(nrow(geo),150),]

Con_Chi1<- table(Con_Chi$Continent)
Con_Chi1<- Con_Chi1[-which(names(Con_Chi1)=='Multiple')]

table(geo$Continent)[-which(names(table(geo$Continent))=='Multiple')]

chisq.test(Con_Chi1,p = rep(1/length(Con_Chi1), length(Con_Chi1)))
#Reject Null - Number of studies are different in different continent 

dev.new(height=7,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(5.5,4,1,1))
plot(table(geo$Continent)[-which(names(table(geo$Continent))=='Multiple')], las=2,ylab = "Number of Articles",type = "p",cex = 2.5,xaxt="n",pch = 19,ylim = c(0,200))
axis(side=1,at=1:6,labels=c("Europe", "Asia","North \nAmerica","South \nAmerica","Africa","Oceania"),las = 2)
mtext(as.expression(bquote(chi^2~"= 87.76")),adj = 1,at = 4.5,line = -5)
mtext(text = 'df = 5',adj = 1,at = 4.08,line = -5.8)
mtext(text = 'p < 0.05',adj = 1,at = 4.4,line = -6.6)

##TO DO - Table of Countries ----

##Biome----

Bio_Chi <- Litdata[sample(nrow(Litdata),150),]
head(Bio_Chi,3)
Bio_Chi1<- table(Bio_Chi$Biome)
#to use in chi must have a count of at least 5 - remove montane, various and unspecified
Bio_Chi1<- Bio_Chi1[-which(names(Bio_Chi1)=='Montane')]
Bio_Chi1<- Bio_Chi1[-which(names(Bio_Chi1)=='Unspecified')]
Bio_Chi1<- Bio_Chi1[-which(names(Bio_Chi1)=='Various')]


chisq.test(Bio_Chi1,p = rep(1/length(Bio_Chi1), length(Bio_Chi1)))
#Reject Null - Number of studies are different in different Biomes 


dev.new(height=7,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(9,4,1,1))
plot(table(Litdata$Biome), ylim = c(0,200),ylab = "Number of Articles",type = "p",las = 2, cex = 2,pch = 19, xaxt="n")
axis(side=1,at=1:10,las = 2,labels = c("Temperate Forest", "Tropical/Subtropical \nForest","Tropical/Subtropical \nGrassland","Mediterrane","Temperate\n Grasslands","Boreal","Desert",'Montane','Various','Unspecified'))
mtext(as.expression(bquote(chi^2~"= 133.81")),adj = 1,at = 7.5,line = -5)
mtext(text = 'df = 6',adj = 1,at = 6.55,line = -5.8)
mtext(text = 'p < 0.05',adj = 1,at = 7.1,line = -6.6)

#Question #: Spatial----

##Spatial Scale----

Spat_Chi <- Litdata[sample(nrow(Litdata),150),]
Spat_Chi1<- table(Spat_Chi$Spatial_Scale)

chisq.test(Spat_Chi1,p = rep(1/length(Spat_Chi1), length(Spat_Chi1)))
#Reject Null - i.e the number of studies which use dpatial scales is different 


dev.new(height=7,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(8,4,1,1))
plot(table(Litdata$Spatial_Scale)[order(table(Litdata$Spatial_Scale),decreasing = T)], ylim = c(0,150),ylab = "Number of Articles",type = "p",las = 2, cex = 2,pch = 19, xaxt="n")
axis(side=1,at=1:4,las = 2,labels = c('No Environmental \n Variables','Site','Landscape','Both'))
mtext(as.expression(bquote(chi^2~"= 36.8127")),adj = 1,at = 3.5,line = -5)
mtext(text = 'df = 3',adj = 1,at = 3.1,line = -5.8)
mtext(text = 'p < 0.05',adj = 1,at = 3.26,line = -6.6)

##TO DO - Table of Environmental variables ----


#Question #: Functional Groups----

##TO DO - Table of Fun Groups ----

##Included Functional Groups----

Fun_Chi <- Litdata[sample(nrow(Litdata),150),]
Fun_Chi1<- table(Fun_Chi$Functional_Groups)

chisq.test(Fun_Chi1,p = rep(1/length(Fun_Chi1), length(Fun_Chi1)))
#Reject Null - i.e the number of studies which use spatial scales is different 

#Need better formatting for the things
dev.new(height=7,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(8,4,1,1))
plot(table(Litdata$Functional_Groups), ylim = c(0,250),ylab = "Number of Articles",type = "p",las = 2, cex = 2,pch = 19, xaxt="n")
axis(side=1,at=1:2,las = 2,labels = c('Not Included','Included'))
mtext(as.expression(bquote(chi^2~"= 22.427")),adj = 1,at = 1.5,line = -5)
mtext(text = 'df = 1',adj = 1,at = 1.1,line = -5.8)
mtext(text = 'p < 0.05',adj = 1,at = 1.26,line = -6.6)

