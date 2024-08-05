
library('dplyr')
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

Con_Chi<- table(geo$Continent)
Con_Chi<- Con_Chi[-which(names(Con_Chi)=='Multiple')]

chisq.test(Con_Chi,p = rep(1/length(Con_Chi), length(Con_Chi)))
#Reject Null - Number of studies are different in different continent 

dev.new(height=7,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(5.5,4,1,1))
plot(Con_Chi, las=2,ylab = "Number of Articles",type = "p",cex = 2.5,xaxt="n",pch = 19,ylim = c(0,200))
axis(side=1,at=1:6,labels=c("Europe", "Asia","North \nAmerica","South \nAmerica","Africa","Oceania"),las = 2)
mtext(as.expression(bquote(chi^2~"= 194.72")),adj = 1,at = 4.6,line = -5)
mtext(text = 'df = 5',adj = 1,at = 4.08,line = -5.8)
mtext(text = 'p < 0.05',adj = 1,at = 4.4,line = -6.6)

##TO DO - Table of Countries ----

##Biome----

Bio_Chi<- table(Litdata$Biome)
#to use in chi must have a count of at least 5 - remove various and unspecified for analysis
Bio_Chi<- Bio_Chi[-which(names(Bio_Chi)=='Unspecified')]
Bio_Chi<- Bio_Chi[-which(names(Bio_Chi)=='Various')]

chisq.test(Bio_Chi,p = rep(1/length(Bio_Chi), length(Bio_Chi)))
#Reject Null - Number of studies are different in different Biomes 


dev.new(height=7,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(9,4,1,1))
plot(table(Litdata$Biome), ylim = c(0,200),ylab = "Number of Articles",type = "p",las = 2, cex = 2,pch = 19, xaxt="n")
axis(side=1,at=1:10,las = 2,labels = c("Temperate Forest", "Tropical/Subtropical \nForest","Tropical/Subtropical \nGrassland","Mediterrane","Temperate\n Grasslands","Boreal","Desert",'Montane','Various','Unspecified'))
mtext(as.expression(bquote(chi^2~"= 502.82")),adj = 1,at = 7.5,line = -5)
mtext(text = 'df = 7',adj = 1,at = 6.55,line = -5.8)
mtext(text = 'p < 0.05',adj = 1,at = 7.1,line = -6.6)
mtext(text = '*',adj = 1,at = 10.25,line = -16.5,cex = 1.5)
mtext(text = '*',adj = 1,at = 9.25,line = -16.3,cex = 1.5)



#Question #: Spatial----

##Spatial Scale----

Spat_Chi<- table(Spat_Chi$Spatial_Scale)

chisq.test(Spat_Chi,p = rep(1/length(Spat_Chi), length(Spat_Chi)))
#Reject Null - i.e the number of studies which use dpatial scales is different 


dev.new(height=7,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(8,4,1,1))
plot(table(Litdata$Spatial_Scale)[order(table(Litdata$Spatial_Scale),decreasing = T)], ylim = c(0,150),ylab = "Number of Articles",type = "p",las = 2, cex = 2,pch = 19, xaxt="n")
axis(side=1,at=1:4,las = 2,labels = c('No Environmental \n Variables','Site','Landscape','Both'))
mtext(as.expression(bquote(chi^2~"= 27.333")),adj = 1,at = 3.4,line = -5)
mtext(text = 'df = 3',adj = 1,at = 3.1,line = -5.8)
mtext(text = 'p < 0.05',adj = 1,at = 3.26,line = -6.6)

##TO DO - Table of Environmental variables ----

#Question #: Farm----

##Farm Management----

ManLitData1 <- data.frame(Article_Number = Litdata$Article_Number, Management = Litdata$Primary_Management_Type)
ManLitData2 <- data.frame(Article_Number = Litdata$Article_Number,Management = Litdata$Secondary_Management_Type)
ManLitData3 <- data.frame(Article_Number = Litdata$Article_Number,Management = Litdata$Third_Management_Type)
ManLitData2 <- na.omit(ManLitData2)
ManLitData3 <- na.omit(ManLitData3)

head(ManLitData1);dim(ManLitData1)
head(ManLitData2);dim(ManLitData2)
head(ManLitData3);dim(ManLitData3)

ManLitData <- rbind(ManLitData1,ManLitData2,ManLitData3)
head(ManLitData);dim(ManLitData)


#Function found online not written by me  https://www.r-bloggers.com/2013/01/randomly-deleting-duplicate-rows-from-a-dataframe/

duplicated.random = function(x, incomparables = FALSE, ...) 
{ 
  if ( is.vector(x) ) 
  { 
    permutation = sample(length(x)) 
    x.perm      = x[permutation] 
    result.perm = duplicated(x.perm, incomparables, ...) 
    result      = result.perm[order(permutation)] 
    return(result) 
  } 
  else if ( is.matrix(x) ) 
  { 
    permutation = sample(nrow(x)) 
    x.perm      = x[permutation,] 
    result.perm = duplicated(x.perm, incomparables, ...) 
    result      = result.perm[order(permutation)] 
    return(result) 
  } 
  else 
  { 
    stop(paste("duplicated.random() only supports vectors", 
               "matrices for now.")) 
  } 
} 

#Must take only one observation from each article to fit chi squared assumptions, above function used to randomly select a single observation per article number

ManLitData$Selected <- duplicated.random(ManLitData$Article_Number)
head(ManLitData);dim(ManLitData)
#FALSE records are taken as the record for each article

dim(ManLitData[which(ManLitData$Selected == "FALSE"),])
dim(Litdata)

Man_Chi <- ManLitData[which(ManLitData$Selected == "FALSE"),]

length(unique(Man_Chi$Article_Number)) 

Man_Chi1<- table(Man_Chi$Management)

chisq.test(Man_Chi1,p = rep(1/length(Man_Chi1), length(Man_Chi1)))
#Reject Null - i.e the number of studies which use different management types is different 


dev.new(height=7,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(8.5,4,1,1))
plot(table(ManLitData$Management), ylim = c(0,150),ylab = "Number of Articles",type = "p",las = 2, cex = 2,pch = 19, xaxt="n")
axis(side=1,at=1:12,las = 2,labels = c("Unspecified","Conventional","Organic","Intensity Gradient","Intensively \nManaged","Other","IPM","Low Input","Ag Enviro Scheme","Environmental","Commercial","Gradient Inputs"))
mtext(as.expression(bquote(chi^2~"= 559.08")),adj = 1,at = 7.9,line = -5)
mtext(text = 'df = 11',adj = 1,at = 7.0,line = -5.8)
mtext(text = 'p < 0.05',adj = 1,at = 7.4,line = -6.6)

##Farm Type----


FarmLitData1 <- data.frame(Article_Number = Litdata$Article_Number, Type = Litdata$Primary_Farm_Type)
FarmLitData2 <- data.frame(Article_Number = Litdata$Article_Number,Type = Litdata$Secondary_Farm_Type)
FarmLitData2 <- na.omit(FarmLitData2)

head(FarmLitData1);dim(FarmLitData1)
head(FarmLitData2);dim(FarmLitData2)


FarmLitData <- rbind(FarmLitData1,FarmLitData2)
head(FarmLitData);dim(FarmLitData)

FarmLitData$Type <- sub('Silvoarable','Silvo',FarmLitData$Type)
FarmLitData$Type <- sub('Silvopastoral','Silvo',FarmLitData$Type)
table(FarmLitData$Type)

FarmLitData$Selected <- duplicated.random(FarmLitData$Article_Number)
head(FarmLitData);dim(FarmLitData)
#FALSE records are taken as the record for each article

dim(FarmLitData[which(FarmLitData$Selected == "FALSE"),])
dim(Litdata)

Farm_Chi <- FarmLitData[which(FarmLitData$Selected == "FALSE"),]

length(unique(Farm_Chi$Article_Number)) 

Farm_Chi1<- table(Farm_Chi$Type)
#remove categories with less than 5

Farm_Chi1<- Farm_Chi1[-which(names(Farm_Chi1)=='Aquaculture')]
Farm_Chi1<- Farm_Chi1[-which(names(Farm_Chi1)=='Various')]


chisq.test(Farm_Chi1,p = rep(1/length(Farm_Chi1), length(Farm_Chi1)))


dev.new(height=7,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(8.5,4,1,1))
plot(table(FarmLitData$Type)[order(table(FarmLitData$Type),decreasing = T)], ylim = c(0,170),ylab = "Number of Articles",type = "p",las = 2, cex = 2,pch = 19, xaxt="n")
axis(side=1,at=1:9,las = 2,labels = c('Crops','Ground Fruit \n & Vegetables','Orchid','Livestock','Agroforestry','Unspecified','Silvoarable/\nSilvopastural','Various','Aquaculture'))
mtext(as.expression(bquote(chi^2~"= 268.13")),adj = 1,at = 7.57,line = -5)
mtext(text = 'df = 6',adj = 1,at = 6.7,line = -5.8)
mtext(text = 'p < 0.05',adj = 1,at = 7.2,line = -6.6)
mtext(text = '*',adj = 1,at = 9.25,line = -16.9,cex = 1.5)
mtext(text = '*',adj = 1,at = 8.25,line = -16.9,cex = 1.5)


#Question #: Insect Focus ----


##Ecosystem Service----

ESLitData1 <- data.frame(Article_Number = Litdata$Article_Number, ES = Litdata$Primary_Ecosystem_Service)
ESLitData2 <- data.frame(Article_Number = Litdata$Article_Number,ES = Litdata$Secondary_Ecosystem_Service)
ESLitData2 <- na.omit(ESLitData2)

head(ESLitData1);dim(ESLitData1)
head(ESLitData2);dim(ESLitData2)


ESLitData <- rbind(ESLitData1,ESLitData2)
head(ESLitData);dim(ESLitData)


ESLitData$Selected <- duplicated.random(ESLitData$Article_Number)
head(ESLitData);dim(ESLitData)
#FALSE records are taken as the record for each article

dim(ESLitData[which(ESLitData$Selected == "FALSE"),])
dim(Litdata)

ES_Chi <- ESLitData[which(ESLitData$Selected == "FALSE"),]

length(unique(ES_Chi$Article_Number)) 

ES_Chi1<- table(ES_Chi$ES)
#remove categories with less than 5

ES_Chi1<- ES_Chi1[-which(names(ES_Chi1)=='Various')]


chisq.test(ES_Chi1,p = rep(1/length(ES_Chi1), length(ES_Chi1)))


dev.new(height=7,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(8.5,4,1,1))
plot(table(ESLitData$ES)[order(table(ESLitData$ES),decreasing = T)], ylim = c(0,150),ylab = "Number of Articles",type = "p",las = 2, cex = 2,pch = 19, xaxt="n")
axis(side=1,at=1:10,las = 2,labels = c('Pollination',"Biologicol Control","Biodiversity/\nResilience","Natural Enemies","Soil","Bio Indicators","Pests/Herbivory","Ecosystem \nEngineer","Food Source", "Various"))
mtext(as.expression(bquote(chi^2~"= 285.8")),adj = 1,at = 7.65,line = -5)
mtext(text = 'df = 8',adj = 1,at = 6.9,line = -5.8)
mtext(text = 'p < 0.05',adj = 1,at = 7.4,line = -6.6)
mtext(text = '*',adj = 1,at = 10.25,line = -16.9,cex = 1.5)


##Taxon----

#would we be better off with a table?

TaxLitData1 <- data.frame(Article_Number = Litdata$Article_Number, Taxon = Litdata$Primary_Taxon)
TaxLitData2 <- data.frame(Article_Number = Litdata$Article_Number,Taxon = Litdata$Secondary_Taxon)
TaxLitData3 <- data.frame(Article_Number = Litdata$Article_Number,Taxon = Litdata$Third_Taxon)
TaxLitData2 <- na.omit(TaxLitData2)
TaxLitData3 <- na.omit(TaxLitData3)


head(TaxLitData1);dim(TaxLitData1)
head(TaxLitData2);dim(TaxLitData2)
head(TaxLitData3);dim(TaxLitData3)


TaxLitData <- rbind(TaxLitData1,TaxLitData2,TaxLitData3)
head(TaxLitData);dim(TaxLitData)


TaxLitData$Selected <- duplicated.random(TaxLitData$Article_Number)
head(TaxLitData);dim(TaxLitData)
#FALSE records are taken as the record for each article

dim(TaxLitData[which(TaxLitData$Selected == "FALSE"),])
dim(Litdata)

Tax_Chi <- TaxLitData[which(TaxLitData$Selected == "FALSE"),]

length(unique(Tax_Chi$Article_Number)) 

Tax_Chi1<- table(Tax_Chi$Taxon)
#remove categories with less than 5

Tax_Chi1[order(Tax_Chi1,decreasing = T)]

#ES_Chi1<- ES_Chi1[-which(names(ES_Chi1)=='NAME')]


#chisq.test(ES_Chi1,p = rep(1/length(ES_Chi1), length(ES_Chi1)))


#dev.new(height=7,width=7,dpi=80,pointsize=14,noRStudioGD = T)
#par(mar=c(8.5,4,1,1))
#plot(table(ESLitData$ES)[order(table(ESLitData$ES),decreasing = T)], ylim = c(0,150),ylab = "Number of Articles",type = "p",las = 2, cex = 2,pch = 19, xaxt="n")
#axis(side=1,at=1:10,las = 2,labels = c('Pollination',"Biologicol Control","Biodiversity/\nResilience","Natural Enemies","Soil","Bio Indicators","Pests/Herbivory","Ecosystem \nEngineer","Food Source", "Various"))
#mtext(as.expression(bquote(chi^2~"= 285.8")),adj = 1,at = 7.65,line = -5)
#mtext(text = 'df = 8',adj = 1,at = 6.9,line = -5.8)
#mtext(text = 'p < 0.05',adj = 1,at = 7.4,line = -6.6)

#Question #: Functional Groups----


##Included Functional Groups----


Fun_Chi<- table(Litdata$Functional_Groups)

chisq.test(Fun_Chi,p = rep(1/length(Fun_Chi), length(Fun_Chi)))
#Reject Null - i.e the number of studies which use spatial scales is different 

dev.new(height=7,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(7,4,1,1))
plot(c(NA, table(Litdata$Functional_Groups),NA), ylim = c(0,250),ylab = "Number of Articles",type = "p",las = 2, cex = 2,pch = 19, xaxt="n",xlab = NA,bty = 'n')
axis(side=1,at = 1:4,las = 2,labels = c(NA,'Functional\n Groups \nNot Included','Functional\n Groups \nIncluded',NA))
mtext(as.expression(bquote(chi^2~"= 46.282")),adj = 1,at = 3.43,line = -5)
mtext(text = 'df = 1',adj = 1,at = 3.1,line = -5.8)
mtext(text = 'p < 0.05',adj = 1,at = 3.27,line = -6.6)

##TO DO - Table of Fun Groups ----
