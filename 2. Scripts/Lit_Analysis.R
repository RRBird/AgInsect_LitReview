

library("arm")
library("AICcmodavg")
library("lme4")

#Analysis for Lit review

Litdata <- rawlit

head(Litdata,3);dim(Litdata)

table(Litdata$Spatial_Scale)







#Probability of Spatial Scale between Continents----
head(Litdata);dim(Litdata)
geo2<-table(Litdata$Continent)
geo3<-geo2[-which(names(geo2)=='Multiple')]

chisq.test(geo3)
random<-geo[sample(nrow(geo),100),]
random<-table(random$Continent)
random<-random[-which(names(random)=='Multiple')]
chisq.test(random)

geo<-Litdata
geo<-geo[-c(which(geo$Continent=="Multiple")),]
#removed the two articles which had multiple continents
head(geo,3);dim(geo)

Con_Chi <- geo[sample(nrow(geo),100),]

Con_Chi1<- table(Con_Chi$Continent)
Con_Chi1<- Con_Chi1[-which(names(Con_Chi1)=='Multiple')]

table(geo$Continent)[-which(names(table(geo$Continent))=='Multiple')]

chisq.test(Con_Chi1,p = rep(1/length(Con_Chi1), length(Con_Chi1)))
#Reject Null - Number of studies are different in different continent 

dev.new(height=7,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(5.5,4,1,1))
plot(table(geo$Continent)[-which(names(table(geo$Continent))=='Multiple')], las=2,ylab = "Number of Articles",type = "p",cex = 2.5,xaxt="n",pch = 19,ylim = c(0,200))
axis(side=1,at=1:6,labels=c("Europe", "Asia","North \nAmerica","South \nAmerica","Africa","Oceania"),las = 2)
mtext(as.expression(bquote(chi^2~"= 74.96")),adj = 1,at = 4.5,line = -5)
mtext(text = 'df = 5',adj = 1,at = 4.08,line = -5.8)
mtext(text = 'p = >0.05',adj = 1,at = 4.5,line = -6.6)




mtext(as.expression(bquote(Delta~"AICc ="~.(paste(round(m2.tab2$Delta_AICc[m2.tab2$Modnames=="fire"]-m2.tab2$Delta_AICc[m2.tab2$Modnames=="null"],2),sep="")))), side=3,line=0.1,adj=1,col="darkorange2", cex=0.75)
?mtext



