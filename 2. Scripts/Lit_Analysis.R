
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
#geo<-geo[-c(which(geo$Continent=="Multiple")),]
#did this in explore script now, -> removed the two articles which had multiple continents
head(geo,3);dim(geo)
table(geo$Continent)

Con_Chi<- table(geo$Continent)
#Con_Chi<- Con_Chi[-which(names(Con_Chi)=='Multiple')]

chisq.test(Con_Chi,p = rep(1/length(Con_Chi), length(Con_Chi)))
#Number of studies are different in different continent

Conplot<-data.frame(Continent = c('Europe','Asia','North_America',"South_America","Aftica","Oceania"), Articles = as.numeric(Con_Chi), Lat = c(54.919722,46.587472,50.301389,-19.624639,1.600889,-38.414056), Long = c(18.305611,94.334389,-109.222306,-58.773083,23.844111,160.639861))

##Table of Countries ----
table(geo$Country)[order(table(geo$Country),decreasing = T)]

write.table(table(geo$Country)[order(table(geo$Country),decreasing = T)], file = '3. Results/Countries.txt', col.names = TRUE,
            row.names = FALSE, sep = "\t")

##Biome----

table(Litdata$Biome)
#to use in chi must have a count of at least 5 - add together those which have less then 5 (various and unspecified) into other
Biome<-Litdata
Biome$Biome<-sub('Unspecified',"Other",Biome$Biome)
Biome$Biome<-sub('Various',"Other",Biome$Biome)
table(Biome$Biome)

Bio_Chi<- table(Biome$Biome)
Bio_Chi[order(Bio_Chi, decreasing = T)]

chisq.test(Bio_Chi,p = rep(1/length(Bio_Chi), length(Bio_Chi)))
#Number of studies are different in different Biomes 

##Figure----
str(countriesCoarseLessIslands)

dev.new(height=17,width=14,dpi=80,pointsize=14,noRStudioGD = T)
layout(mat = matrix(c(1,1,1,1,
                      2,2,3,4),nrow = 2,byrow = T),heights = 1,1)
par(mar=c(0,0,0,0))
plot(countriesCoarseLessIslands,col = 'lightgrey',ylim = c(-10,80))
points(Conplot$Long,Conplot$Lat,pch = 20, cex = c(Conplot$Articles/7),col = 'black')
text(Conplot$Long,Conplot$Lat,labels = Conplot$Articles,col = 'white',cex = c(Conplot$Articles/35))
points(Conplot$Long[6],Conplot$Lat[6],cex = c(Conplot$Articles[6]/7),pch = 20, col='black')
mtext(text = 'df = 5',adj = 1,at = 170,line = -1.8,cex = 0.9)
mtext(as.expression(bquote(chi^2~"= 194.72")),adj = 1,at = 170,line = -3.2,cex = 0.9)
mtext(text = expression(italic('p')< 0.001),adj = 1,at = 170,line = -4.2 ,cex = 0.9)
mtext(text = 'a)',adj = 1, at = -170, line = -2,cex = 0.95)
mtext(text = "20",adj = 1, at = 179, line = -13.5,cex = 0.9)
arrows(x0 = 172.5,y0 = 4,x1 = 161.5,y1 = -33,length = 0.1,angle = 40,lwd = 2)
arrows(x0 = 160.5,y0 = -37,x1 = 150,y1 = -30,length = 0.1,angle = 20,lwd = 2)
arrows(x0 = 160,y0 = -37,x1 = 175.5,y1 = -39,length = 0.1,angle = 20,lwd = 2)

par(mar=c(11,4,1,1))
barplot(Bio_Chi[order(Bio_Chi,decreasing = T)],ylim = c(0,200),las = 2,cex.axis = 1.3,names.arg = c("Temperate", "Tropical/Subtropical","Mediterranean","Boreal","Desert",'Montane','Other'),cex.names=1.2)
box(bty = 'l')
mtext(text = "Articles",side = 2, line = 2.7, cex =1)
mtext(text = 'df = 6',adj = 1,at = 8,line = -1,cex = 0.9)
mtext(as.expression(bquote(chi^2~"= 629.56")),adj = 1,at = 8,line = -2.3,cex = 0.9)
mtext(text = expression(italic('p')< 0.001),adj = 1,at = 8,line = -3.3,cex = 0.9)
mtext(text = 'b)',adj = 1, at = -2.2, line = 0,cex = 0.95)

plot.new()
plot.new()


#Question #: Spatial----

##Spatial Scale----

Spatial <- Litdata

table(Spatial$Spatial_Scale)

Spat_Chi<- table(Spatial$Spatial_Scale)
Spat_Chi[order(Spat_Chi, decreasing = T)]


chisq.test(Spat_Chi,p = rep(1/length(Spat_Chi), length(Spat_Chi)))
#number of studies which use the various spatial scales is different 

##Figure----

dev.new(height=4.5,width=3.5,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(9,4,1,1))
barplot(table(Litdata$Spatial_Scale)[order(table(Litdata$Spatial_Scale),decreasing = T)],names.arg = c('No Environmental \n Variables','Site','Landscape','Both'),las = 2,cex.axis = 1.1,cex.names = 1.1,)
box(bty='l')
mtext(text = "Articles",side = 2, line = 2.8, cex =1)
mtext(text = 'df = 3',adj = 1,at = 4.8,line = -1,cex = 0.9)
mtext(as.expression(bquote(chi^2~"= 53.39")),adj = 1,at = 4.8,line = -2,cex = 0.9)
mtext(text = expression(italic('p')< 0.001),adj = 1,at = 4.8,line = -2.7,cex = 0.9)

#Question #: Farm----

##Farm Management----

dim(Litdata)
tail(Litdata) #has three extra empty rows removing those
Litdata<-Litdata[-c(355:357),]

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

table(ManLitData$Management)
ManLitData$Management <- sub('Intensively_Managed',"Conventional",ManLitData$Management)
table(ManLitData$Management)



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



Man_Chi <- ManLitData[which(ManLitData$Selected == "FALSE"),]

length(unique(Man_Chi$Article_Number)) 

Man_Chi1<- table(Man_Chi$Management)
Man_Chi1[order(Man_Chi1, decreasing = T)]


chisq.test(Man_Chi1,p = rep(1/length(Man_Chi1), length(Man_Chi1)))
#the number of studies which use different management types is different 


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
FarmLitData$Type <- sub('Pasture','Other',FarmLitData$Type)


table(FarmLitData$Type)

FarmLitData$Selected <- duplicated.random(FarmLitData$Article_Number)
head(FarmLitData);dim(FarmLitData)
#FALSE records are taken as the record for each article

dim(FarmLitData[which(FarmLitData$Selected == "FALSE"),])
dim(Litdata)

Farm_Chi <- FarmLitData[which(FarmLitData$Selected == "FALSE"),]

length(unique(Farm_Chi$Article_Number)) 

Farm_Chi1<- table(Farm_Chi$Type)
Farm_Chi1[order(Farm_Chi1, decreasing = T)]

chisq.test(Farm_Chi1,p = rep(1/length(Farm_Chi1), length(Farm_Chi1)))


##Figure----

dev.new(height=4.5,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(9,4,1,1),mfrow = c(1,2))
barplot(Man_Chi1[order(Man_Chi1, decreasing = T)], ylim = c(0,140),las = 2, names.arg = c("Unspecified","Conventional","Organic","Intensity Gradient","Other","IPM","Low Input","Agri-Enviro Scheme","Environmental"),cex.names = 0.9)
box(bty='l')
mtext(text = "Articles",side = 2, line = 2.8, cex =1)
mtext(text = 'df = 8',adj = 1,at = 10,line = -1, cex = 0.9)
mtext(as.expression(bquote(chi^2~"= 427.47")),adj = 1,at = 10,line = -2,cex = 0.9)
mtext(text = expression(italic('p')< 0.001),adj = 1,at = 10,line = -2.6,cex = 0.9)
mtext(text = 'a)',adj = 1,at = -3.8,line = 0, cex = 0.95)


par(mar=c(9,4,1,1))
barplot(Farm_Chi1[order(Farm_Chi1, decreasing = T)],las = 2, names.arg = c('Crops','Low Fruit/Vegetables','Orchard','Livestock','Agroforestry','Unspecified','Other'), cex.names = 0.9)
box(bty = 'l')
mtext(text = "Articles",side = 2, line = 2.8, cex =1)
mtext(text = 'df = 6',adj = 1,at = 8.5,line = -1,cex = 0.9)
mtext(as.expression(bquote(chi^2~"= 269.87")),adj = 1,at = 8.5,line = -2,cex = 0.9)
mtext(text = expression(italic('p')< 0.001),adj = 1,at = 8.5,line = -2.6,cex = 0.9)
mtext(text = 'b)',adj = 1,at = -2.4,line = 0,cex = 0.95)


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
ES_Chi1[order(ES_Chi1, decreasing = T)]



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
Tax_Chi1[order(Tax_Chi1, decreasing = T)]


chisq.test(Tax_Chi1,p = rep(1/length(Tax_Chi1), length(Tax_Chi1)))

head(Tax_Chi,4);dim(Tax_Chi)

#PHylo

TaxPHY_Chi<- table(Tax_Chi$Taxon[Tax_Chi$Type == 'Phylo'])
chisq.test(TaxPHY_Chi,p = rep(1/length(TaxPHY_Chi), length(TaxPHY_Chi)))


TaxPHY_Chi[order(TaxPHY_Chi,decreasing = T)]

#Functional group

TaxFUN_Chi<- table(Tax_Chi$Taxon[Tax_Chi$Type == 'FunArea'])
#remove water which is less than 5

TaxFUN_Chi <- TaxFUN_Chi[-which(names(TaxFUN_Chi)=='Water_Community')]
chisq.test(TaxFUN_Chi,p = rep(1/length(TaxFUN_Chi), length(TaxFUN_Chi)))

TaxFUN_Chi[order(TaxFUN_Chi, decreasing = T)]


##Figure----

FUN_tab <- table(TaxLitData$Taxon[TaxLitData$Type == "FunArea"])[order(table(TaxLitData$Taxon[TaxLitData$Type == "FunArea"]),decreasing = T)]
FUN_tab[-5]
FUN_tab <- FUN_tab[-5]

dev.new(height=17,width=14,dpi=80,pointsize=14,noRStudioGD = T)
par(mfrow = c(2,2))
par(mar = c(9,4,1,1))
barplot(table(ESLitData$ES)[order(table(ESLitData$ES),decreasing = T)], ylim = c(0,140),las = 2,names.arg = c('Pollination',"Biologicol Control","Biodiversity","Natural Enemies","Soil","Bio Indicators","Pests/Herbivory","Ecosystem Engineer","Other"))
box(bty='l')
mtext(text = "Articles",side = 2, line = 2.8, cex =1)
mtext(text = 'df = 9',adj = 1,at = 11,line = -0.6,cex=0.9)
mtext(as.expression(bquote(chi^2~"= 372.95")),adj = 1,at = 11,line = -1.7,cex = 0.9)
mtext(text = expression(italic('p')< 0.001),adj = 1,at = 11,line = -2.5,cex=0.9)
mtext(text = 'a)',at = -4.5,line = -0.1,cex =0.95)

  par(mar = c(9,4,1,1))
barplot(table(TaxLitData$Type)[order(table(TaxLitData$Type),decreasing = T)],las = 2,names.arg = c('Taxon',"Functional Group"))
box(bty='l')
mtext(text = "Articles",side = 2, line = 2.8, cex =1)
mtext(text = 'df = 1',adj = 1,at = 2.6,line = -0.6,cex=0.9)
mtext(as.expression(bquote(chi^2~"= 50.72")),adj = 1,at = 2.6,line = -1.7,cex = 0.9)
mtext(text = expression(italic('p')< 0.001),adj = 1,at = 2.6,line = -2.5,cex=0.9)
mtext(text = 'b)',at = -0.8,line = -0.1,cex =0.95)

par(mar = c(9,4,1,1))
barplot(table(TaxLitData$Taxon[TaxLitData$Type == "Phylo"])[order(table(TaxLitData$Taxon[TaxLitData$Type == "Phylo"]),decreasing = T)],las = 2,names.arg = c('Hymenoptera',"Coleoptera",'Diptera','Lepidoptera','Other','Hemiptera','Arachnida'))
box(bty='l')
mtext(text = "Articles",side = 2, line = 2.8, cex =1)
mtext(text = 'df = 6',adj = 1,at = 8.5,line = -0.6,cex=0.9)
mtext(as.expression(bquote(chi^2~"= 209.22")),adj = 1,at = 8.5,line = -1.7,cex = 0.9)
mtext(text = expression(italic('p')< 0.001),adj = 1,at = 8.5,line = -2.5,cex=0.9)
mtext(text = 'c)',at = -3.2,line = -0.1,cex =0.95)

par(mar = c(9,4,1,1))
barplot(FUN_tab,las = 2,names.arg = c('Plant Dwelling',"Surface Active",'Flying','Endogeic'))
box(bty='l')
mtext(text = "Articles",side = 2, line = 2.8, cex =1)
mtext(text = 'df = 3',adj = 1,at = 5,line = -0.6,cex=0.9)
mtext(as.expression(bquote(chi^2~"= 14.68")),adj = 1,at = 5,line = -1.7,cex = 0.9)
mtext(text = expression(italic('p') == 0.002),adj = 1,at = 5,line = -2.5,cex=0.9)
mtext(text = 'd)',at = -2,line = -0.1,cex =0.95)




