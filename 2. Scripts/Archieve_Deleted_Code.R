
Litdata$Both<-ifelse(Litdata$Spatial_Scale == "Both",1,0)
Litdata$Landscape<-ifelse(Litdata$Spatial_Scale == "Landscape",1,0)
Litdata$Site<-ifelse(Litdata$Spatial_Scale == "Site",1,0)
Litdata$None<-ifelse(Litdata$Spatial_Scale == "No_Enviro_Variable",1,0)

Litdata$Spatial_Scale<- sapply(Litdata$Spatial_Scale, factor, levels = c("Both", "Landscape","Site","No_Enviro_Variable"))

#Models with each continent as it's own binomial variable

geo$Eur<-ifelse(geo$Continent == "Europe",1,0)
geo$Asia<-ifelse(geo$Continent == "Asia",1,0)
geo$N_Am<-ifelse(geo$Continent == "North_America",1,0)
geo$S_Am<-ifelse(geo$Continent == "South_America",1,0)
geo$Afr<-ifelse(geo$Continent == "Africa",1,0)
geo$Oce<-ifelse(geo$Continent == "Oceania",1,0)

head(geo,3);dim(geo)

Con_null <- glm(Spatial_Scale~1, data = geo,family = binomial)
Con_Eur <- glm(Spatial_Scale~Eur, data = geo,family = binomial)
AICc(Con_null);AICc(Con_Eur)

Con_Asia <- glm(Spatial_Scale~Asia, data = geo,family = binomial)
AICc(Con_null);AICc(Con_Asia)

Con_N_Am <- glm(Spatial_Scale~N_Am, data = geo,family = binomial)
AICc(Con_null);AICc(Con_N_Am)

Con_S_Am <- glm(Spatial_Scale~S_Am, data = geo,family = binomial)
AICc(Con_null);AICc(Con_S_Am)

Con_Afr <- glm(Spatial_Scale~Afr, data = geo,family = binomial)
AICc(Con_null);AICc(Con_Afr)

Con_Oce <- glm(Spatial_Scale~Oce, data = geo,family = binomial)
AICc(Con_null);AICc(Con_Oce)


#Europe Predictions
Pred_Con_Eur<-data.frame(Eur = c(0,1,0,1,0,1,0,1),Spatial_Scale = c(1,2,3,4))

Pred_Con_Eur1<-predict(object=Con_Eur,newdata=Pred_Con_Eur,se.fit =T,type="link")

Pred_Con_Eur2<-data.frame(Pred_Con_Eur,fit.link=Pred_Con_Eur1$fit,se.link=Pred_Con_Eur1$se.fit)
Pred_Con_Eur2$lci.link<-Pred_Con_Eur2$fit.link-(1.96*Pred_Con_Eur2$se.link)
Pred_Con_Eur2$uci.link<-Pred_Con_Eur2$fit.link+(1.96*Pred_Con_Eur2$se.link)

Pred_Con_Eur2$fit<-invlogit(Pred_Con_Eur2$fit.link)
Pred_Con_Eur2$se<-invlogit(Pred_Con_Eur2$se.link)
Pred_Con_Eur2$lci<-invlogit(Pred_Con_Eur2$lci.link)
Pred_Con_Eur2$uci<-invlogit(Pred_Con_Eur2$uci.link)

dev.new(height=14,width=17,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(9,3,2,2))
plot(1:8,Pred_Con_Eur2$fit,type="p", ylim=c(min(0,min(Pred_Con_Eur2$lci)),max(1,max(Pred_Con_Eur2$uci))),xlim=c(min(0.5),max(8.5)),ylab="Probability",xlab =NA ,las=1,cex=2,pch=19,xaxt="n",font.main=1,cex.lab=1.2)
axis(side=1,at=1:8,labels=c("Other: Both \n Spatial Scales", "Europe: Both \n Spatial Scales",'Other: Landscape \nSpatial Scale',"Europe: Landscape \nSpatial Scale","Other: Site \n Spatial Scale","Europe: Site \n Spatial scale",'Other: No \n Enviro Variable','Europe: No \n Enviro Variable'),las=2)
arrows(x0=1:8, y0=Pred_Con_Eur2$lci,x1=1:8, y1=Pred_Con_Eur2$uci,angle=90,length=0.1, code=3, lwd=2)

#All categories for spatial scales and for continent

Con_null <- glm(Spatial_Scale~1, data = geo,family = binomial)
Con_All <- glm(Spatial_Scale~Continent, data = geo,family = binomial)
AICc(Con_null);AICc(Con_All)

#Each spatial scale as it's own binomial variable

Both_conNull <- glm(Both~1, data = geo,family = binomial)
Both_con <- glm(Both~Continent, data = geo,family = binomial)
AICc(Both_conNull);AICc(Both_con)

Land_conNull <- glm(Landscape~1, data = geo,family = binomial)
Land_con <- glm(Landscape~Continent, data = geo,family = binomial)
AICc(Land_conNull);AICc(Land_con)

Site_conNull <- glm(Site~1, data = geo,family = binomial)
Site_con <- glm(Site~Continent, data = geo,family = binomial)
AICc(Site_conNull);AICc(Site_conNull)

None_conNull <- glm(None~1, data = geo,family = binomial)
None_con <- glm(None~Continent, data = geo,family = binomial)
AICc(None_conNull);AICc(None_con)

#Both Predictions (within 2 AICc of Null)
Pred_Both_Con<-data.frame(Continent=c("Europe", "Asia",'North_America',"South_America","Africa","Oceania"))

Pred_Both_Con1<-predict(object=Both_con,newdata=Pred_Both_Con,se.fit =T,type="link")

Pred_Both_Con2<-data.frame(Pred_Both_Con,fit.link=Pred_Both_Con1$fit,se.link=Pred_Both_Con1$se.fit)
Pred_Both_Con2$lci.link<-Pred_Both_Con2$fit.link-(1.96*Pred_Both_Con2$se.link)
Pred_Both_Con2$uci.link<-Pred_Both_Con2$fit.link+(1.96*Pred_Both_Con2$se.link)

Pred_Both_Con2$fit<-invlogit(Pred_Both_Con2$fit.link)
Pred_Both_Con2$se<-invlogit(Pred_Both_Con2$se.link)
Pred_Both_Con2$lci<-invlogit(Pred_Both_Con2$lci.link)
Pred_Both_Con2$uci<-invlogit(Pred_Both_Con2$uci.link)

dev.new(height=14,width=14,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(8,4.5,2,2))
plot(1:6,Pred_Both_Con2$fit,type="p", ylim=c(min(0,min(Pred_Both_Con2$lci)),max(1,max(Pred_Both_Con2$uci))),xlim=c(min(0.5),max(6.5)),ylab="Probability of Both Spatial Level Variables",las=1,cex=2,pch=19,xaxt="n",font.main=1,cex.lab=1.2,xlab = NA)
axis(side=1,at=1:6,labels=c("Europe", "Asia",'North_America',"South_America","Africa","Oceania"), las = 2)
arrows(x0=1:6, y0=Pred_Both_Con2$lci,x1=1:6, y1=Pred_Both_Con2$uci,angle=90,length=0.1, code=3, lwd=2)

#Probability of Spatial Scale between Biomes----
BioNull <- glm(Spatial_Scale~1, data = Litdata,family = binomial)
BioAll <- glm(Spatial_Scale~Biome, data = Litdata,family = binomial)
AICc(BioNull);AICc(BioAll)


Both_BioNull <- glm(Both~1, data = Litdata,family = binomial)
Both_Bio <- glm(Both~Biome, data = Litdata,family = binomial)
AICc(Both_BioNull);AICc(Both_Bio)

Land_BioNull <- glm(Landscape~1, data = Litdata,family = binomial)
Land_Bio <- glm(Landscape~Biome, data = Litdata,family = binomial)
AICc(Land_BioNull);AICc(Land_Bio)

Site_BioNull <- glm(Site~1, data = Litdata,family = binomial)
Site_Bio <- glm(Site~Biome, data = Litdata,family = binomial)
AICc(Site_BioNull);AICc(Site_Bio)

None_BioNull <- glm(None~1, data = Litdata,family = binomial)
None_Bio <- glm(None~Biome, data = Litdata,family = binomial)
AICc(None_BioNull);AICc(None_Bio)


#Probability of Spatial Scale between Years----
YearNull <- glm(Spatial_Scale~1, data = Litdata,family = binomial)
YearAll <- glm(Spatial_Scale~Year, data = Litdata,family = binomial)
AICc(YearNull);AICc(YearAll)

Both_YearNull <- glm(Both~1, data = Litdata,family = binomial)
Both_Year <- glm(Both~Year, data = Litdata,family = binomial)
AICc(Both_YearNull);AICc(Both_Year)

Land_YearNull <- glm(Landscape~1, data = Litdata,family = binomial)
Land_Year <- glm(Landscape~Year, data = Litdata,family = binomial)
AICc(Land_YearNull);AICc(Land_Year)

Site_YearNull <- glm(Site~1, data = Litdata,family = binomial)
Site_Year <- glm(Site~Year, data = Litdata,family = binomial)
AICc(Site_YearNull);AICc(Site_Year)

None_YearNull <- glm(None~1, data = Litdata,family = binomial)
None_Year <- glm(None~Year, data = Litdata,family = binomial)
AICc(None_YearNull);AICc(None_Year)

#Probability of Spatial Scale with Functional Group Use----
FunNull <- glm(Spatial_Scale~1, data = Litdata,family = binomial)
FunAll <- glm(Spatial_Scale~Functional_Groups, data = Litdata,family = binomial)
AICc(FunNull);AICc(FunAll)

Both_FunNull <- glm(Both~1, data = Litdata,family = binomial)
Both_Fun <- glm(Both~Functional_Groups, data = Litdata,family = binomial)
AICc(Both_FunNull);AICc(Both_Fun)

Land_FunNull <- glm(Landscape~1, data = Litdata,family = binomial)
Land_Fun <- glm(Landscape~Functional_Groups, data = Litdata,family = binomial)
AICc(Land_FunNull);AICc(Land_Fun)

Site_FunNull <- glm(Site~1, data = Litdata,family = binomial)
Site_Fun <- glm(Site~Functional_Groups, data = Litdata,family = binomial)
AICc(Site_FunNull);AICc(Site_Fun)

None_FunNull <- glm(None~1, data = Litdata,family = binomial)
None_Fun <- glm(None~Functional_Groups, data = Litdata,family = binomial)
AICc(None_FunNull);AICc(None_Fun)

#Probability of Spatial Scale with Ecosystem service----

which(colnames(Litdata)=="Primary_Ecosystem_Service")
which(colnames(Litdata)=="Secondary_Ecosystem_Service")

#Adding primary and secondary ecosystem service columns into one data frame

ESLitData1 <- data.frame(Article_Number = Litdata$Article_Number,ES = Litdata$Primary_Ecosystem_Service)
ESLitData2 <- data.frame(Article_Number = Litdata$Article_Number,ES = Litdata$Secondary_Ecosystem_Service)
ESLitData2 <- na.omit(ESLitData2)

head(ESLitData1);dim(ESLitData1)
head(ESLitData2);dim(ESLitData2)

#article with category 'various' was only article that included more than two ecosystem services
#pollination, natural enemies, soil health and bio indicator species
#Seperated this out in database so no longer one that was various

ESLitData1[34,]
ESLitData1[34,2] <- "Pollination"
ESLitData1[355,2] <- "Natural_Enemies"
ESLitData1[355,1] <- 34
ESLitData1[356,2] <- "Decomp_Soil_Properties"
ESLitData1[356,1] <- 34
ESLitData1[357,2] <- "Bio_Indicators"
ESLitData1[357,1] <- 34

head(ESLitData1);dim(ESLitData1)

ESLitData <- rbind(ESLitData1,ESLitData2)

ESLitData <- merge(ESLitData,Litdata,by="Article_Number")

head(ESLitData);dim(ESLitData)
table(ESLitData1$ES)

#mixed models
ESNull <- glmer(Spatial_Scale~1 + (1 | Article_Number), data = ESLitData,family = binomial)
ESAll <- glmer(Spatial_Scale~ES + (1 | Article_Number), data = ESLitData,family = binomial) #MODEL FAILED TO CONVERGE
AICc(ESNull);AICc(ESAll)

Both_ESNull <- glmer(Both~1+ (1|Article_Number), data = ESLitData,family = binomial)
Both_ES <- glmer(Both~ES + (1 | Article_Number), data = ESLitData,family = binomial) 
AICc(Both_ESNull);AICc(Both_ES)

Land_ESNull <- glmer(Landscape~1 + (1|Article_Number), data = ESLitData,family = binomial)
Land_ES <- glmer(Landscape~ES + (1|Article_Number), data = ESLitData,family = binomial)
AICc(Land_ESNull);AICc(Land_ES)

Site_ESNull <- glmer(Site~1 + (1|Article_Number), data = ESLitData,family = binomial)
Site_ES <- glmer(Site~ES + (1|Article_Number), data = ESLitData,family = binomial) #MODEL FAILED TO CONVERGE
AICc(Site_ESNull);AICc(Site_ES)

None_ESNull <- glmer(None~1 + (1|Article_Number), data = ESLitData,family = binomial)
None_ES <- glmer(None~ES + (1|Article_Number), data = ESLitData,family = binomial) #MODEL FAILED TO CONVERGE
AICc(None_ESNull);AICc(None_ES)

#no random effect in these ones

ESNull1 <- glm(Spatial_Scale~1, data = ESLitData,family = binomial)
ESAll1 <- glm(Spatial_Scale~ES, data = ESLitData,family = binomial)
AICc(ESNull1);AICc(ESAll1)

Both_ESNull1 <- glm(Both~1, data = ESLitData,family = binomial)
Both_ES1 <- glm(Both~ES, data = ESLitData,family = binomial)
AICc(Both_ESNull1);AICc(Both_ES1)

Land_ESNull1 <- glm(Landscape~1, data = ESLitData,family = binomial)
Land_ES1 <- glm(Landscape~ES, data = ESLitData,family = binomial)
AICc(Land_ESNull1);AICc(Land_ES1)

Site_ESNull1 <- glm(Site~1, data = ESLitData,family = binomial)
Site_ES1 <- glm(Site~ES, data = ESLitData,family = binomial)
AICc(Site_ESNull1);AICc(Site_ES1)

None_ESNull1 <- glm(None~1, data = ESLitData,family = binomial)
None_ES1 <- glm(None~ES, data = ESLitData,family = binomial)
AICc(None_ESNull1);AICc(None_ES1)

table(ESLitData$ES)

#Landscape preds from the no random effect 

Pred_Land_ES<-data.frame(ES=c("Bio_Indicators", "Biodiversity_Ecosystem_Resilience",'Biological_Control',"Decomp_Soil_Properties","Ecosystem_Engineer","Food_Source","Natural_Enemies","Pests_Herbivory","Pollination"))

Pred_Land_ES1<-predict(object=Land_ES1,newdata=Pred_Land_ES,se.fit =T,type="link")

Pred_Land_ES2<-data.frame(Pred_Land_ES,fit.link=Pred_Land_ES1$fit,se.link=Pred_Land_ES1$se.fit)
Pred_Land_ES2$lci.link<-Pred_Land_ES2$fit.link-(1.96*Pred_Land_ES2$se.link)
Pred_Land_ES2$uci.link<-Pred_Land_ES2$fit.link+(1.96*Pred_Land_ES2$se.link)

Pred_Land_ES2$fit<-invlogit(Pred_Land_ES2$fit.link)
Pred_Land_ES2$se<-invlogit(Pred_Land_ES2$se.link)
Pred_Land_ES2$lci<-invlogit(Pred_Land_ES2$lci.link)
Pred_Land_ES2$uci<-invlogit(Pred_Land_ES2$uci.link)

dev.new(height=14,width=14,dpi=80,pointsize=14,noRStudioGD = T)
par(mar = c(9,4,2,2))
plot(1:9,Pred_Land_ES2$fit,type="p", ylim=c(min(0,min(Pred_Land_ES2$lci)),max(1,max(Pred_Land_ES2$uci))),xlim=c(min(0.5),max(9.5)),xlab=NA,ylab="Probability of Landscape Level Spatial Scale",las=1,cex=2,pch=19,xaxt="n",font.main=1,cex.lab=1.2)
axis(side=1,at=1:9,labels=c("Bio_Indicators", "Biodiversity_Ecosystem_Resilience",'Biological_Control',"Decomp_Soil_Properties","Ecosystem_Engineer","Food_Source","Natural_Enemies","Pests_Herbivory","Pollination "),las = 2)
arrows(x0=1:9, y0=Pred_Land_ES2$lci,x1=1:9, y1=Pred_Land_ES2$uci,angle=90,length=0.1, code=3, lwd=2)

table(ESLitData$ES)
# Pest/Herbivory and Ecosystem Engineers have a count
#could join them together into an 'other' category or alternatively could remove due to there not being enough data to evaluate these two ecosystem services
#Should there be a certain number of occurrences needed to be a category included in models?


#Both Preds from the no random effect 