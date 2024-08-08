
library('dplyr')
library("rworldmap")

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

Conplot<-data.frame(Continent = unique(geo$Continent[order(geo$Continent)]), Articles = as.numeric(Con_Chi), Lat = c(54.919722,46.587472,50.301389,-19.624639,1.600889,-38.414056), Long = c(18.305611,94.334389,-109.222306,-58.773083,23.844111,160.639861))

##TO DO - Table of Countries ----

##Biome----

table(Litdata$Biome)
#to use in chi must have a count of at least 5 - add together those which have less then 5 (various and unspecified) into other
Biome<-Litdata
Biome$Biome<-sub('Unspecified',"Other",Biome$Biome)
Biome$Biome<-sub('Various',"Other",Biome$Biome)

Bio_Chi<- table(Biome$Biome)

chisq.test(Bio_Chi,p = rep(1/length(Bio_Chi), length(Bio_Chi)))
#Reject Null - Number of studies are different in different Biomes 

##Figure----

dev.new(height=14,width=14,dpi=80,pointsize=14,noRStudioGD = T)
layout(mat = matrix(c(1,1,1,1,
                      2,3,3,4),nrow = 2,byrow = T),heights = 1,1)
par(mar=c(0,0,0,0))
plot(countriesCoarseLessIslands,ylim = c(20,20),col = 'lightgrey')
points(Conplot$Long,Conplot$Lat,pch = 20, cex = c(Conplot$Articles/7),col = 'black')
text(Conplot$Long,Conplot$Lat,labels = Conplot$Articles,col = 'white',cex = c(Conplot$Articles/35))
mtext(text = 'df = 5',adj = 1,at = 170,line = -1.8)
mtext(as.expression(bquote(chi^2~"= 194.72")),adj = 1,at = 170,line = -3.2)
mtext(text = 'p < 0.001',adj = 1,at = 170,line = -4)
mtext(text = 'a)',adj = 1, at = -170, line = -2)
mtext(text = "*20",adj = 1, at = 168, line = -18.3,cex = 0.9)

plot.new()

par(mar=c(11,4,1,1))
barplot(Bio_Chi[order(Bio_Chi,decreasing = T)],ylim = c(0,200),las = 2,cex.axis = 1.3,names.arg = c("Temperate Forest", "Tropical Forest","Tropical Grassland","Mediterrane","Temperate Grassland","Boreal","Desert",'Montane','Other'),cex.names=1.2)
box(bty = 'l')
mtext(text = "Articles",side = 2, line = 3.2, cex =1)
mtext(text = 'df = 8',adj = 1,at = 11,line = -1,cex = 1)
mtext(as.expression(bquote(chi^2~"= 591.41")),adj = 1,at = 11,line = -2.3,cex = 1)
mtext(text = 'p < 0.001',adj = 1,at = 11,line = -3.1,cex = 1)
mtext(text = 'b)',adj = 1, at = -4, line = 0)

plot.new()


#Question #: Spatial----

##Spatial Scale----

Spat_Chi<- table(Spat_Chi$Spatial_Scale)

chisq.test(Spat_Chi,p = rep(1/length(Spat_Chi), length(Spat_Chi)))
#Reject Null - i.e the number of studies which use dpatial scales is different 


dev.new(height=7,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(9,4,1,1))
barplot(table(Litdata$Spatial_Scale)[order(table(Litdata$Spatial_Scale),decreasing = T)],names.arg = c('No Environmental \n Variables','Site','Landscape','Both'),las = 2,cex.axis = 1.2,cex.names = 1.1,)
box(bty='l')
mtext(text = "Articles",side = 2, line = 2.8, cex =1.2)
mtext(text = 'df = 3',adj = 1,at = 4.8,line = -1,cex = 1.1)
mtext(as.expression(bquote(chi^2~"= 27.33")),adj = 1,at = 4.8,line = -2,cex = 1.1)
mtext(text = 'p < 0.001',adj = 1,at = 4.8,line = -2.7,cex = 1.1)

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
barplot(table(ManLitData$Management), ylim = c(0,150),ylab = "Number of Articles",xaxt="n")
box(bty='l')
axis(side=1,at=1:12,las = 2,labels = c("Unspecified","Conventional","Organic","Intensity Gradient","Intensively \nManaged","Other","IPM","Low Input","Ag Enviro Scheme","Environmental","Commercial","Gradient Inputs"))
mtext(text = 'df = 11',adj = 1,at = 7.0,line = -5.8)
mtext(as.expression(bquote(chi^2~"= 559.08")),adj = 1,at = 7.9,line = -5)
mtext(text = 'p < 0.05',adj = 1,at = 7.4,line = -6.6)

##Farm Type----


FarmLitData1 <- data.frame(Article_Number = Litdata$Article_Number, Type = Litdata$Primary_Farm_Type)
FarmLitData2 <- data.frame(Article_Number = Litdata$Article_Number,Type = Litdata$Secondary_Farm_Type)
FarmLitData2 <- na.omit(FarmLitData2)

head(FarmLitData1);dim(FarmLitData1)
head(FarmLitData2);dim(FarmLitData2)


FarmLitData <- rbind(FarmLitData1,FarmLitData2)
head(FarmLitData);dim(FarmLitData)

FarmLitData$Type <- sub('Silvoarable','Other',FarmLitData$Type)
FarmLitData$Type <- sub('Silvopastoral','Other',FarmLitData$Type)
FarmLitData$Type <- sub('Aquaculture','Other',FarmLitData$Type)
FarmLitData$Type <- sub('Various','Other',FarmLitData$Type)

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

chisq.test(Farm_Chi1,p = rep(1/length(Farm_Chi1), length(Farm_Chi1)))


dev.new(height=7,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(8.5,4,1,1))
plot(table(FarmLitData$Type)[order(table(FarmLitData$Type),decreasing = T)], ylim = c(0,170),ylab = "Number of Articles",type = "p",las = 2, cex = 2,pch = 19, xaxt="n")
axis(side=1,at=1:9,las = 2,labels = c('Crops','Ground Fruit \n & Vegetables','Orchard','Livestock','Agroforestry','Unspecified','Silvoarable/\nSilvopastural','Various','Aquaculture'))
mtext(as.expression(bquote(chi^2~"= 268.13")),adj = 1,at = 7.57,line = -5)
mtext(text = 'df = 6',adj = 1,at = 6.7,line = -5.8)
mtext(text = 'p < 0.05',adj = 1,at = 7.2,line = -6.6)
mtext(text = '*',adj = 1,at = 9.25,line = -16.9,cex = 1.5)
mtext(text = '*',adj = 1,at = 8.25,line = -16.9,cex = 1.5)

#Figure

dev.new(height=7,width=14,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(9,4,1,1),mfrow = c(1,2))
barplot(table(ManLitData$Management), ylim = c(0,140),las = 2, names.arg = c("Unspecified","Conventional","Organic","Intensity Gradient","Intensively Managed","Other","IPM","Low Input","Ag Enviro Scheme","Environmental","Commercial","Gradient Inputs"))
box(bty='l')
mtext(text = "Articles",side = 2, line = 2.8, cex =1.2)
mtext(text = 'df = 11',adj = 1,at = 15.0,line = -1)
mtext(as.expression(bquote(chi^2~"= 559.08")),adj = 1,at = 15,line = -2)
mtext(text = 'p < 0.001',adj = 1,at = 15,line = -2.6)
mtext(text = 'a)',adj = 1,at = -2.5,line = 0,cex = 1.1)


par(mar=c(9,4,1,1))
barplot(table(FarmLitData$Type)[order(table(FarmLitData$Type),decreasing = T)],las = 2, ylim = c(0,170), names.arg = c('Crops','Ground Fruit \n & Vegetables','Orchard','Livestock','Agroforestry','Unspecified','Other') )
box(bty = 'l')
mtext(text = "Articles",side = 2, line = 2.8, cex =1.2)
mtext(text = 'df = 6',adj = 1,at = 8.5,line = -1)
mtext(as.expression(bquote(chi^2~"= 267.38")),adj = 1,at = 8.5,line = -2)
mtext(text = 'p < 0.001',adj = 1,at = 8.5,line = -2.6)
mtext(text = 'b)',adj = 1,at = -1.3,line = 0,cex = 1.1)


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

table(ESLitData$ES)
#categories with less than 5 added together

ESLitData$ES <- sub('Food_Source','Other',ESLitData$ES)
ESLitData$ES <- sub('Various','Other',ESLitData$ES)

ES_Chi1<- table(ES_Chi$ES)



chisq.test(ES_Chi1,p = rep(1/length(ES_Chi1), length(ES_Chi1)))


dev.new(height=7,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(8.5,4,1,1))
plot(table(ESLitData$ES)[order(table(ESLitData$ES),decreasing = T)], ylim = c(0,150),ylab = "Number of Articles",type = "p",las = 2, cex = 2,pch = 19, xaxt="n")
axis(side=1,at=1:10,las = 2,labels = c('Pollination',"Biologicol Control","Biodiversity/\nResilience","Natural Enemies","Soil","Bio Indicators","Pests/Herbivory","Ecosystem \nEngineer","Other"))
mtext(text = 'df = 8',adj = 1,at = 6.9,line = -5.8)
mtext(as.expression(bquote(chi^2~"= 277.78")),adj = 1,at = 7.65,line = -5)
mtext(text = 'p < 0.001',adj = 1,at = 7.4,line = -6.6)


##Taxon----

#did they use functional or phylum 
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
table(TaxLitData$Taxon)

TaxLitData$Type <- ifelse(test = TaxLitData$Taxon == 'Flying_Community'|TaxLitData$Taxon == 'Endogeic_Community'|TaxLitData$Taxon == 'Plant_Dwelling' | TaxLitData$Taxon == 'Surface_Active'|TaxLitData$Taxon == 'Water_Community',yes = 'FunArea', no = 'Phylo')

table(TaxLitData$Type)

TaxLitData$Selected <- duplicated.random(TaxLitData$Article_Number)

dim(TaxLitData[which(TaxLitData$Selected == "FALSE"),])
dim(Litdata)


Tax_Chi <- TaxLitData[which(TaxLitData$Selected == "FALSE"),]
#FALSE records are taken as the record for each article
table(TaxLitData$Taxon)

TaxLitData$Taxon <- sub('Neuroptera','Other',TaxLitData$Taxon)
TaxLitData$Taxon <- sub('Gastropoda','Other',TaxLitData$Taxon)
TaxLitData$Taxon <- sub('Acari','Other',TaxLitData$Taxon)
TaxLitData$Taxon <- sub('Collembola','Other',TaxLitData$Taxon)
TaxLitData$Taxon <- sub('Orthoptera','Other',TaxLitData$Taxon)
TaxLitData$Taxon <- sub('Odonata','Other',TaxLitData$Taxon)
TaxLitData$Taxon <- sub('Nematoda','Other',TaxLitData$Taxon)
TaxLitData$Taxon <- sub('Opisthopora','Other',TaxLitData$Taxon)
TaxLitData$Taxon <- sub('Isoptera','Other',TaxLitData$Taxon)
TaxLitData$Taxon <- sub('Dermaptera','Other',TaxLitData$Taxon)
TaxLitData$Taxon <- sub('Mites','Other',TaxLitData$Taxon)

TaxLitData$Taxon <- sub('Formicidae','Hymenoptera',TaxLitData$Taxon)
TaxLitData$Taxon <- sub('Aphididae','Hemiptera',TaxLitData$Taxon)
TaxLitData$Taxon <- sub('Apocrita','Hymenoptera',TaxLitData$Taxon)



table(TaxLitData$Taxon)

Tax_Chi <- TaxLitData[which(TaxLitData$Selected == "FALSE"),]
#FALSE records are taken as the record for each article

#Phylo vs Functional area

Tax_Chi1 <- table(Tax_Chi$Type)

chisq.test(Tax_Chi1,p = rep(1/length(Tax_Chi1), length(Tax_Chi1)))

head(Tax_Chi,4);dim(Tax_Chi)

#Functional group

TaxFUN_Chi<- table(Tax_Chi$Taxon[Tax_Chi$Type == 'FunArea'])
#remove water which is less than 5

TaxFUN_Chi <- TaxFUN_Chi[-which(names(TaxFUN_Chi)=='Water_Community')]
chisq.test(TaxFUN_Chi,p = rep(1/length(TaxFUN_Chi), length(TaxFUN_Chi)))

#PHylo

TaxPHY_Chi<- table(Tax_Chi$Taxon[Tax_Chi$Type == 'Phylo'])
chisq.test(TaxPHY_Chi,p = rep(1/length(TaxPHY_Chi), length(TaxPHY_Chi)))





TaxFUN_Chi

TaxPHY_Chi[order(TaxPHY_Chi,decreasing = T)]

##Figure----

dev.new(height=14,width=14,dpi=80,pointsize=14,noRStudioGD = T)
par(mfrow = c(2,2))
par(mar = c(9,4,1,1))
barplot(table(ESLitData$ES)[order(table(ESLitData$ES),decreasing = T)], ylim = c(0,140),las = 2,names.arg = c('Pollination',"Biologicol Control","Biodiversity","Natural Enemies","Soil","Bio Indicators","Pests/Herbivory","Ecosystem Engineer","Other"))
box(bty='l')
mtext(text = "Articles",side = 2, line = 2.8, cex =1)
mtext(text = 'df = 8',adj = 1,at = 11,line = -0.6,cex=0.9)
mtext(as.expression(bquote(chi^2~"= 277.78")),adj = 1,at = 11,line = -1.7,cex = 0.9)
mtext(text = 'p < 0.001',adj = 1,at = 11,line = -2.3,cex=0.9)
mtext(text = 'a)',at = -3.5,line = -0.1,cex =0.95)

  par(mar = c(9,4,1,1))
barplot(table(TaxLitData$Type)[order(table(TaxLitData$Type),decreasing = T)],las = 2,names.arg = c('Phylum',"Microhabitat"))
box(bty='l')
mtext(text = "Articles",side = 2, line = 2.8, cex =1)
mtext(text = 'df = 1',adj = 1,at = 2.4,line = -0.6,cex=0.9)
mtext(as.expression(bquote(chi^2~"= 52.249")),adj = 1,at = 2.4,line = -1.7,cex = 0.9)
mtext(text = 'p < 0.001',adj = 1,at = 2.4,line = -2.3,cex=0.9)
mtext(text = 'b)',at = -0.5,line = -0.1,cex =0.95)

par(mar = c(7,4,1,1))
barplot(table(TaxLitData$Taxon[TaxLitData$Type == "FunArea"])[order(table(TaxLitData$Taxon[TaxLitData$Type == "FunArea"]),decreasing = T)],las = 2,names.arg = c('Plant Dwelling',"Surface Active",'Flying','Endogeic','Water'))
box(bty='l')
mtext(text = "Articles",side = 2, line = 2.8, cex =1)
mtext(text = 'df = 3',adj = 1,at = 6,line = -0.8,cex=0.9)
mtext(as.expression(bquote(chi^2~"= 15.06")),adj = 1,at = 6,line = -1.9,cex = 0.9)
mtext(text = 'p = 0.002',adj = 1,at = 6,line = -2.5,cex=0.9)
mtext(text = 'c)',at = -1.6,line = -0.1,cex =0.95)

par(mar = c(7,4,1,1))
barplot(table(TaxLitData$Taxon[TaxLitData$Type == "Phylo"])[order(table(TaxLitData$Taxon[TaxLitData$Type == "Phylo"]),decreasing = T)],las = 2,names.arg = c('Hymenoptera',"Coleoptera",'Diptera','Lepidoptera','Other','Hemiptera','Arachnida'))
box(bty='l')
mtext(text = "Articles",side = 2, line = 2.8, cex =1)
mtext(text = 'df = 9',adj = 1,at = 8.5,line = -0.6,cex=0.9)
mtext(as.expression(bquote(chi^2~"= 177.09")),adj = 1,at = 8.5,line = -1.7,cex = 0.9)
mtext(text = 'p < 0.001',adj = 1,at = 8.5,line = -2.3,cex=0.9)
mtext(text = 'd)',at = -2.4,line = -0.1,cex =0.95)

