#Packages----
install.packages("stringi")
install.packages('patchwork')

library("stringi")
library("dplyr")
library('patchwork')
library('grid')

#Raw data----

getwd()

rawlit <- read.csv("1. Data/LitData.csv")

head(rawlit);dim(rawlit)
str(rawlit)
rawlit[rawlit == ""] <- NA

#Farm Management----

#putting minor management types into categories
table(c(rawlit$Primary_Management_Type,rawlit$Secondary_Management_Type,rawlit$Third_Management_Type))

rawlit$Primary_Management_Type<-sub("Intensive_vs_Extensive","Intensively_Managed",rawlit$Primary_Management_Type)

replace <- list("Climate_Smart", "Conservation_Agriculture","Ecological_Compensation_Areas","Sustainable_Stratergies","Agroecological","Ecological_Cultivation")

for (i in replace){
  rawlit$Primary_Management_Type <- sub(i,"Environmental",rawlit$Primary_Management_Type)
  rawlit$Secondary_Management_Type <- sub(i,"Environmental",rawlit$Secondary_Management_Type)
  rawlit$Third_Management_Type <- sub(i,"Environmental",rawlit$Third_Management_Type)
}

replace2<- list("IFPMD", "Minimal_Management","Palmette","Small_Scale_Farm","Abandoned","Homegardens","Integrated_Production","Mixed_Input","QSMAS","IFP","RBAPS")


for (i in replace2){
  rawlit$Primary_Management_Type <- sub(i,"Other",rawlit$Primary_Management_Type)
  rawlit$Secondary_Management_Type <- sub(i,"Other",rawlit$Secondary_Management_Type)
  rawlit$Third_Management_Type <- sub(i,"Other",rawlit$Third_Management_Type)
}

table(c(rawlit$Primary_Management_Type,rawlit$Secondary_Management_Type,rawlit$Third_Management_Type))

#Farm Management Type Graph

cols <- c("Primary_Management_Type","Secondary_Management_Type","Third_Management_Type")

rawlit[cols] <- lapply(rawlit[cols], factor, levels =c("Unspecified","Conventional","Organic","Intensity_Gradient","Intensively_Managed","Other","IPM","Low_Input","AgEnviroScheme","Environmental","Commercial","Gradient_Inputs"))
str(rawlit)

dev.new(height=7,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(9,4,1,1))
barplot(table(c(rawlit$Primary_Management_Type,rawlit$Secondary_Management_Type,rawlit$Third_Management_Type)),names.arg = c("Unspecified","Conventional","Organic","Intensity Gradient","Intensively Managed","Other","IPM","Low Input","Ag Enviro Scheme","Environmental","Commercial","Gradient Inputs"),las = 2,axis.lty =1)
text(x = -2.3,
     y = 110,
     labels = "Number of Articles",
     xpd = NA,
     srt = 90,
     adj = 1.3,
     cex = 1)


#Geography (Country, Continent)----

head(rawlit)

length(unique(rawlit$Country))
table(rawlit$Country[rawlit$Country=="Australia"])
table(rawlit$Country)[order(table(rawlit$Country),decreasing = T)]

table(rawlit$Country)[table(rawlit$Country)>5]

#61 countries (plus category of 'multiple') and most common country is USA
#10 studies where in Australia

#Plotting of only countries with occurrences >5

dev.new(height=8,width=8,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(8,4,1,1))
barplot(table(rawlit$Country)[order(table(rawlit$Country),decreasing = T)][1:21],las = 2, axis.lty = 1,names.arg = c('USA','Brazil','China','Germany','England','Sweden','France','Multiple Countries','South Africa','Spain','Indonesia','Italy','Australia','New Zealand','Argentina','Ireland','Tanzania','United Kingdom',"Hungry",'Kenya','Malaysia'))
mtext(text = "Number of Articles",
      side = 2,line = 2.5)

#Continent Graph

rawlit$Continent <- sapply(rawlit$Continent, factor, levels = c("Europe", "Asia","North_America","South_America","Africa","Oceania","Multiple"))
str(rawlit)

dev.new(height=7,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(5.5,4,1,1))
barplot(table(rawlit$Continent), las=2,axis.lty=1,names.arg = c("Europe", "Asia","North \nAmerica","South \nAmerica","Africa","Oceania","Multiple \nContinents"))
text(x = -1.3,
     y = 110,
     labels = "Number of Articles",
     xpd = NA,
     srt = 90,
     adj = 1.3,
     cex = 1)

#Biome----

table(rawlit$Biome)

#Biome Graph

rawlit$Biome <- sapply(rawlit$Biome, factor, levels = c("Temperate_Forest", "Tropical_Subtropical_Forest","Tropical_Subtropical_Grasslands","Mediterrane","Temperate_Grasslands","Boreal","Desert","Montane","Various","Unspecified"))
str(rawlit)

dev.new(height=7,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(10,4,1,1))
barplot(table(rawlit$Biome), ylim = c(0,200),las = 2, axis.lty = 1, names.arg = c("Temperate Forest", "Tropical/Subtropical \nForest","Tropical/Subtropical \nGrassland","Mediterrane","Temperate Grasslands","Boreal","Desert","Montane","Various","Unspecified"))
text(x = -2,
     y = 160,
     labels = "Number of Articles",
     xpd = NA,
     srt = 90,
     adj = 1.3,
     cex = 1)


#Ecosystem Service----

table(c(rawlit$Primary_Ecosystem_Service,rawlit$Secondary_Ecosystem_Service))
#article with category 'various' was only article that included more than two ecosystem services
#pollination, natural enemies, soil health and bio indicator species

#below is creating a table of ecosystem services with above various added to article counts

ESdata <- matrix(data = c(134,80,45,43,35,31,10,5,5), nrow = 1, ncol = 9)
colnames(ESdata) <- c('Pollination',"Biologicol_Control","Biodiversity_Ecosystem_Resilience","Natural Enemies","Soil","Bio_Indicators","Pests_Herbivory","Ecosystem_Engineer","Food_Source")

dev.new(height=7,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(8,4,1,1))
barplot(ESdata, ylim = c(0,140), col = 'grey',las = 2, axis.lty = 1, names.arg = c('Pollination',"Biologicol Control","Biodiversity/\nResilience","Natural Enemies","Soil","Bio Indicators","Pests/Herbivory","Ecosystem \nEngineer","Food Source"))
text(x = -1.9,
     y = 120,
     labels = "Number of Articles",
     xpd = NA,
     srt = 90,
     adj = 1.3,
     cex = 1)

?barplot

#Taxon Studied ----

table(c(rawlit$Primary_Taxon,rawlit$Secondary_Taxon,rawlit$Third_Taxon))

rawlit$Primary_Taxon <- sub("Mites","Acari",rawlit$Primary_Taxon)

table(c(rawlit$Primary_Taxon,rawlit$Secondary_Taxon,rawlit$Third_Taxon))[order(table(c(rawlit$Primary_Taxon,rawlit$Secondary_Taxon,rawlit$Third_Taxon)),decreasing = T)]


dev.new(height=7,width=9,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(11,4,1,1))
barplot(table(c(rawlit$Primary_Taxon,rawlit$Secondary_Taxon,rawlit$Third_Taxon))[order(table(c(rawlit$Primary_Taxon,rawlit$Secondary_Taxon,rawlit$Third_Taxon)),decreasing = T)]
        ,las = 2,axis.lty = 1,
        names.arg = c('Hymenoptera','Plant Dwelling Species','Coleoptera','Diptera','Ground Dwelling Species','Flying Species',"Lepidoptera","Arachnida","Aphididae",'Formicidae','Parasitoid','Soil Dwelling Species','Hemiptera','Opisthopora','Odonata','Orthoptera','Isoptera','Acari','Nematoda','Water Dwelling Species','Collembola','Gastropoda','Apocrita','Dermaptera','Neuroptera'))
text(x = -3.9,
     y = 105,
     labels = "Number of Articles",
     xpd = NA,
     srt = 90,
     adj = 1.3,
     cex = 1)

#Functional Groups----

table(rawlit$Functional_Groups)

#Reducing The Number of Categories for Plotting

rawlit$Secondary_Functional <- sub("Habitat_Preference","Microhabitat_Preference",rawlit$Secondary_Functional)
rawlit[,33:35] <- sub("Service_or_Disservice","Ecosystem_Service",c(rawlit$Primary_Functional,rawlit$Secondary_Functional,rawlit$Third_Functional))
rawlit[,33:35] <- sub("Dispersal_Method","Dispersal_Capacity",c(rawlit$Primary_Functional,rawlit$Secondary_Functional,rawlit$Third_Functional))
rawlit<-rawlit

fungroup<-table(c(rawlit$Primary_Functional,rawlit$Secondary_Functional,rawlit$Third_Functional))[order(table(c(rawlit$Primary_Functional,rawlit$Secondary_Functional,rawlit$Third_Functional)),decreasing = T)]

#graph which excludes categories that = 1
dev.new(height=7,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(10,4,1,1))
barplot(fungroup[1:17],
        las = 2, axis.lty = 1,names.arg = c('Tropic Group','Size','Diet','Social Behaviour','Nesting Style','Ecosystem Service','Hunting Style','Specialisation','Body Length','Dispersal Capacity','Feeding Style','Microhabitat','Time Active',"Yearly Generations","Pollen Collection Style",'Tongue Length','Wing Size'))

#graph including all categories
dev.new(height=7,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(10,4,1,1))
barplot(fungroup,
        las = 2, axis.lty = 1,names.arg = c('Tropic Group','Size','Diet','Social Behaviour','Nesting Style','Ecosystem Service','Hunting Style','Specialisation','Body Length','Dispersal Capacity','Feeding Style','Microhabitat','Time Active',"Yearly Generations","Pollen Collection Style",'Tongue Length','Wing Size','Dung Removal Style','Pres/Abs of Eyes','Flight Length','Foraging Style','Larval Host Specificity','Leg Body Ratio',"No. Larval Stages","Wing Asymmetry"))

dev.new(height=7,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,1,1))
barplot(table(rawlit$Functional_Groups),las = 2, axis.lty = 1)


#Plotting together - with all categories

dev.new(height=7,width=10,dpi=80,pointsize=14,noRStudioGD = T)
layout(mat = matrix(c(1,2,2,3,
                      4,4,4,4),nrow = 2,byrow = T), heights = c(1,2))
plot.new()
par(mar=c(3,2,1,1))
barplot(table(rawlit$Functional_Groups),las = 2, axis.lty = 1)
mtext(text = "Number of Articles",
      side = 2,line = 2.5,cex = 0.9)
mtext(text = "(a)",at = c(0,180),cex = 0.9)
plot.new()
par(mar=c(10,4,0,1))
barplot(fungroup,
        las = 2, axis.lty = 1,names.arg = c('Tropic Group','Size','Diet','Social Behaviour','Nesting Style','Ecosystem Service','Hunting Style','Specialisation','Body Length','Dispersal Capacity','Feeding Style','Microhabitat','Time Active',"Yearly Generations","Pollen Collection Style",'Tongue Length','Wing Size','Dung Removal Style','Pres/Abs of Eyes','Flight Length','Foraging Style','Larval Host Specificity','Leg Body Ratio',"No. Larval Stages","Wing Asymmetry"),cex.names = 1.1)
mtext(text = "Number of Articles",
      side = 2,line = 2.2,cex = 0.9)
text(labels = "(b)", x = -2, y = 55, xpd = NA,cex = 1.3)

#plotting together - graph which excludes categories that = 1

dev.new(height=7,width=10,dpi=80,pointsize=14,noRStudioGD = T)
layout(mat = matrix(c(1,2,2,3,
                      4,4,4,4),nrow = 2,byrow = T), heights = c(1,2))
plot.new()
par(mar=c(3,2,1,1))
barplot(table(rawlit$Functional_Groups),las = 2, axis.lty = 1)
mtext(text = "Number of Articles",
      side = 2,line = 2.5,cex = 0.9)
mtext(text = "(a)",at = c(0,180),cex = 0.9)
plot.new()
par(mar=c(10,4,0,1))
barplot(fungroup[1:17],
        las = 2, axis.lty = 1,names.arg = c('Tropic Group','Size','Diet','Social Behaviour','Nesting Style','Ecosystem Service','Hunting Style','Specialisation','Body Length','Dispersal Capacity','Feeding Style','Microhabitat','Time Active',"Yearly Generations","Pollen Collection Style",'Tongue Length','Wing Size'),cex.names = 1.1)
mtext(text = "Number of Articles",
      side = 2,line = 2.2,cex = 0.9)
text(labels = "(b)", x = -1, y = 55, xpd = NA,cex = 1.3)

#Habitat Types in Study Areas----

table(c(rawlit$Primary_Habitat,rawlit$Secondary_Habitat))[order(table(c(rawlit$Primary_Habitat,rawlit$Secondary_Habitat)),decreasing = T)]
head(rawlit[,18:19])

#Reducing The Number of Categories for Plotting

replace3 <- list("Moist_Deciduous_Forest", "Conifer_Forests","Evergreen_Forest","Lowland_Dipterocarp_Forest","Moist_Montane_Forest","Premontane_Lowland_Forest",'Sclerophyllous_Forest','Tropical_Forest',"Eucalypt")

for (i in replace3){
  rawlit$Primary_Habitat <- sub(i,"Forest",rawlit$Primary_Habitat)
  rawlit$Secondary_Habitat <- sub(i,"Forest",rawlit$Secondary_Habitat)
}
rawlit$Primary_Habitat <- sub('Lowland_Rainforest',"Rainforest",rawlit$Primary_Habitat)
rawlit[,18:19] <- sub('Mountains',"Alpine",c(rawlit$Primary_Habitat,rawlit$Secondary_Habitat))
rawlit$Secondary_Habitat <- sub('River',"Riparian",rawlit$Secondary_Habitat)
rawlit$Primary_Habitat <- sub('Mediterranean_scrubland',"Shrublands",rawlit$Primary_Habitat)
rawlit[,18:19] <- sub('Prairie',"Grassland",c(rawlit$Primary_Habitat,rawlit$Secondary_Habitat))
rawlit[,18:19] <- sub('Meadow',"Grassland",c(rawlit$Primary_Habitat,rawlit$Secondary_Habitat))
rawlit[,18:19] <- sub('Albany_Thicket',"Thicket",c(rawlit$Primary_Habitat,rawlit$Secondary_Habitat))
rawlit[,18:19] <- sub('Woody_Thicket',"Thicket",c(rawlit$Primary_Habitat,rawlit$Secondary_Habitat))
rawlit[,18:19] <- sub('Mallee_Woodland',"Woodland",c(rawlit$Primary_Habitat,rawlit$Secondary_Habitat))
rawlit[,18:19] <- sub('Savannah_Woodlands',"Woodland",c(rawlit$Primary_Habitat,rawlit$Secondary_Habitat))
rawlit[,18:19] <- sub('Open_Savannah',"Savannah",c(rawlit$Primary_Habitat,rawlit$Secondary_Habitat))


dev.new(height=7,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(6,4,1,1))
barplot(table(c(rawlit$Primary_Habitat,rawlit$Secondary_Habitat))[order(table(c(rawlit$Primary_Habitat,rawlit$Secondary_Habitat)),decreasing = T)],las = 2,axis.lty = 1)
text(x = -2,
     y = 93,
     labels = "Number of Articles",
     xpd = NA,
     srt = 90,
     adj = 1.3,
     cex = 1)

#Methods----
head(rawlit[25:27])
table(c(rawlit$Primary_Method,rawlit$Secondary_Method,rawlit$Third_Method))

#Reducing The Number of Categories for Plotting

rawlit[,25:27] <- sub('Pan_Trap',"Pan_trap",c(rawlit$Primary_Method,rawlit$Secondary_Method,rawlit$Third_Method))
rawlit[,25:27] <- sub('Baited_Pitfall',"Pitfall_trap",c(rawlit$Primary_Method,rawlit$Secondary_Method,rawlit$Third_Method))
rawlit[,25:27] <- sub('Na',NA,c(rawlit$Primary_Method,rawlit$Secondary_Method,rawlit$Third_Method))
rawlit[,25:27] <- sub('Hand_Search',"Visual",c(rawlit$Primary_Method,rawlit$Secondary_Method,rawlit$Third_Method))
rawlit[,25:27] <- sub('Transect_Search',"Visual",c(rawlit$Primary_Method,rawlit$Secondary_Method,rawlit$Third_Method))
rawlit[,25:27] <- sub('Pond_Dipping_Net',"Water_Sample",c(rawlit$Primary_Method,rawlit$Secondary_Method,rawlit$Third_Method))
rawlit[,25:27] <- sub('Disturbance_Downstream_Net',"Water_Sample",c(rawlit$Primary_Method,rawlit$Secondary_Method,rawlit$Third_Method))
rawlit[,25:27] <- sub('Branch_Tapping',"Beat_Sheet",c(rawlit$Primary_Method,rawlit$Secondary_Method,rawlit$Third_Method))
rawlit[,25:27] <- sub('Photo_Time_Lapse',"Camera",c(rawlit$Primary_Method,rawlit$Secondary_Method,rawlit$Third_Method))
rawlit[,25:27] <- sub('Camera_Trap',"Camera",c(rawlit$Primary_Method,rawlit$Secondary_Method,rawlit$Third_Method))
rawlit[,25:27] <- sub('Artifical_Nest_Caverties',"Trap_Nests",c(rawlit$Primary_Method,rawlit$Secondary_Method,rawlit$Third_Method))
rawlit[,25:27] <- sub('Soil_Monoliths',"Soil_Sample",c(rawlit$Primary_Method,rawlit$Secondary_Method,rawlit$Third_Method))
rawlit[,25:27] <- sub('eDNA_Gut_Analysis',"eDNA",c(rawlit$Primary_Method,rawlit$Secondary_Method,rawlit$Third_Method))
rawlit[,25:27] <- sub('Metabarcoding',"eDNA",c(rawlit$Primary_Method,rawlit$Secondary_Method,rawlit$Third_Method))



dev.new(height=7,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(10,4,1,1))
barplot(table(c(rawlit$Primary_Method,rawlit$Secondary_Method,rawlit$Third_Method))[order(table(c(rawlit$Primary_Method,rawlit$Secondary_Method,rawlit$Third_Method)),decreasing = T)],las = 2, axis.lty = 1,names.arg = c('Visual Count','Pitfall',"Sweep Net",'Pan Trap','Soil Sample','Vaccum Suction','Sticky Traps','Beat Sheeting','Malaise Traps','Exclusion','Sentinel/Artifical Prey','Leaf Litter Sample','Trap Nests','Bottle Trap','eDNA','Light Trap','Vane Trap','Water Sample','Camera Trap','Flight Interception','Collected Vegetation','Artifical Dung','Subterranean Trap','Pheromone Line Trap','Photoeclector Trap','Soil Emergence Tents','Thermal Fogging','Toothpicks'))
text(x = -4.5,
     y = 130,
     labels = "Number of Articles",
     xpd = NA,
     srt = 90,
     adj = 1.3,
     cex = 1)

#Farm Type----

head(rawlit[8:9])

table(c(rawlit$Primary_Farm_Type,rawlit$Secondary_Farm_Type))[order(table(c(rawlit$Primary_Farm_Type,rawlit$Secondary_Farm_Type)),decreasing = T)]

dev.new(height=7,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(8,4,1,1))
barplot(table(c(rawlit$Primary_Farm_Type,rawlit$Secondary_Farm_Type))[order(table(c(rawlit$Primary_Farm_Type,rawlit$Secondary_Farm_Type)),decreasing = T)],las = 2, axis.lty = 1,names.arg = c('Crops','Ground Fruit \n & Vegetables','Orchid','Livestock','Agroforestry','Unspecified','Silvoarable','Silvopastoral','Various','Aquaculture'))
mtext(text = "Number of Articles",side = 2,line = 2.5)

length(table(c(rawlit$Primary_Farm_Specific,rawlit$Secondary_Farm_Specific)))
#96 specific categories of farms - 9 different 'various' categories, and unspecified -- therefore 86 specific farm types in articles

table(c(rawlit$Primary_Farm_Specific[rawlit$Primary_Farm_Specific=="Potato"],rawlit$Secondary_Farm_Specific[rawlit$Secondary_Farm_Specific=="Potato"]))
#only one occurrence of potato fields

#graphing of farm specific that have over 5 instances

dev.new(height=7,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(7,4,1,1))
barplot(table(c(rawlit$Primary_Farm_Specific,rawlit$Secondary_Farm_Specific))[order(table(c(rawlit$Primary_Farm_Specific,rawlit$Secondary_Farm_Specific)),decreasing = T)][1:20],las = 2, axis.lty = 1,names.arg = c('Wheat','Cattle Grazing','Vineyard','Crops (Various)','Maize','Apple','Unspecified','Cereal','Oilseed Rape','Rice','Soybean','Palm Oil','Coffee','Cattle Dairy','Veg (Various)','Beans','Cotton','Tomato','Barely','Mango'))
mtext(text = "Number of Articles",side = 2,line = 2.5)

#together on panels

dev.new(height=7,width=14,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(8,4,1,1),mfrow = c(1,2))
barplot(table(c(rawlit$Primary_Farm_Type,rawlit$Secondary_Farm_Type))[order(table(c(rawlit$Primary_Farm_Type,rawlit$Secondary_Farm_Type)),decreasing = T)],las = 2, axis.lty = 1,names.arg = c('Crops','Ground Fruit \n & Vegetables','Orchid','Livestock','Agroforestry','Unspecified','Silvoarable','Silvopastoral','Various','Aquaculture'))
mtext(text = "Number of Articles",side = 2,line = 2.5)
text(x = -2.3,y = 165,labels = "(a)",xpd = NA,)

par(mar=c(8,4,1,1))
barplot(table(c(rawlit$Primary_Farm_Specific,rawlit$Secondary_Farm_Specific))[order(table(c(rawlit$Primary_Farm_Specific,rawlit$Secondary_Farm_Specific)),decreasing = T)][1:20],las = 2, axis.lty = 1,names.arg = c('Wheat','Cattle Grazing','Vineyard','Crops (Various)','Maize','Apple','Unspecified','Cereal','Oilseed Rape','Rice','Soybean','Palm Oil','Coffee','Cattle Dairy','Veg (Various)','Beans','Cotton','Tomato','Barely','Mango'))
mtext(text = "Number of Articles",side = 2,line = 2.5)
text(x = -4.3,y = 32,labels = "(b)",xpd = NA)



#Results Presented----

head(rawlit[28:31])
which(colnames(rawlit)=="Primary_Result")
which(colnames(rawlit)=="Fourth_Result")

table(c(rawlit$Primary_Result,rawlit$Secondary_Result,rawlit$Third_Result,rawlit$Fourth_Result))[order(table(c(rawlit$Primary_Result,rawlit$Secondary_Result,rawlit$Third_Result,rawlit$Fourth_Result)),decreasing = T)]

#Spatial aggregation = 1

#including in plot if more than five occurances 

dev.new(height=7,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(8,4,1,1))
barplot(table(c(rawlit$Primary_Result,rawlit$Secondary_Result,rawlit$Third_Result,rawlit$Fourth_Result))[order(table(c(rawlit$Primary_Result,rawlit$Secondary_Result,rawlit$Third_Result,rawlit$Fourth_Result)),decreasing = T)][1:11],las = 2, axis.lty = 1,names.arg = c('Abundance','Richness','Diversity','Composition','Insect & Flower\n Interactions','Density','Predation Rate','Parasitism','Evenness','Activity','Occurrence'))
mtext(text = "Number of Articles",side = 2,line = 2.5)

#Spatial Scale----

head(rawlit);dim(rawlit)

table(rawlit$Spatial_Scale)[order(table(rawlit$Spatial_Scale),decreasing = T)]

table(c(rawlit$Primary_Enviro_Variable,rawlit$Secondary_Enviro_Variable))[order(table(c(rawlit$Primary_Enviro_Variable,rawlit$Secondary_Enviro_Variable)),decreasing = T)]

#Reducing The Number of Categories for Plotting

rawlit[,3:4] <- sub("Water_Properties","Water",c(rawlit$Primary_Enviro_Variable,rawlit$Secondary_Enviro_Variable))
rawlit[,3:4] <- sub("Proportion_Soil_Elements","Soil_Properties",c(rawlit$Primary_Enviro_Variable,rawlit$Secondary_Enviro_Variable))
rawlit[,3:4] <- sub("Wooded_Landscape_Features","Trees",c(rawlit$Primary_Enviro_Variable,rawlit$Secondary_Enviro_Variable))
rawlit[,3:4] <- sub("Tree_Density","Trees",c(rawlit$Primary_Enviro_Variable,rawlit$Secondary_Enviro_Variable))
rawlit[,3:4] <- sub("Tree_Richness","Trees",c(rawlit$Primary_Enviro_Variable,rawlit$Secondary_Enviro_Variable))
rawlit[,3:4] <- sub("Isolation","Habitat_Connectivity",c(rawlit$Primary_Enviro_Variable,rawlit$Secondary_Enviro_Variable))
rawlit[,3:4] <- sub("MEM","Landscape_Complexity",c(rawlit$Primary_Enviro_Variable,rawlit$Secondary_Enviro_Variable))
rawlit[,3:4] <- sub("Patch_Complexity","Habitat Patches",c(rawlit$Primary_Enviro_Variable,rawlit$Secondary_Enviro_Variable))
rawlit[,3:4] <- sub("Patch_Size","Habitat Patches",c(rawlit$Primary_Enviro_Variable,rawlit$Secondary_Enviro_Variable))
rawlit[,3:4] <- sub("Patch_Density","Habitat Patches",c(rawlit$Primary_Enviro_Variable,rawlit$Secondary_Enviro_Variable))

replace4 <- list("Inflorescence", "Flower_Avalibility","Proportion_Flowers","Flower_Species_Rich","Flower_Abundance","Flower_Species",'Flower_Diversity','Flower_Margin_Age','Distance_To_Flowers')

for (i in replace4){
  rawlit$Primary_Enviro_Variable <- sub(i,"Flowers Variables",rawlit$Primary_Enviro_Variable)
  rawlit$Secondary_Enviro_Variable <- sub(i,"Flowers Variables",rawlit$Secondary_Enviro_Variable)
}

replace5 <- list("Proportion_Veg", "Veg_Height","Veg_Species_Rich","Veg_Complexity","Veg_Diversity",'Leaf_Hairiness','Veg_Composition','Veg_Chemical_Make_Up','Veg_Species_Abundance','Veg_Type','Shrub_Richness',"Veg_Species")

for (i in replace5){
  rawlit$Primary_Enviro_Variable <- sub(i,"Vegetation Variables",rawlit$Primary_Enviro_Variable)
  rawlit$Secondary_Enviro_Variable <- sub(i,"Vegetation Variables",rawlit$Secondary_Enviro_Variable)
}

dev.new(height=7,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(7,4,1,1))
barplot(table(rawlit$Spatial_Scale)[order(table(rawlit$Spatial_Scale),decreasing = T)],las = 2, axis.lty = 1,names.arg = c('No Environmental \n Variables','Site','Landscape','Both'))
mtext(text = "Number of Articles",side = 2,line = 2.5)

dev.new(height=7,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(10,4,1,1))
barplot(table(c(rawlit$Primary_Enviro_Variable,rawlit$Secondary_Enviro_Variable))[order(table(c(rawlit$Primary_Enviro_Variable,rawlit$Secondary_Enviro_Variable)),decreasing = T)],las = 2, axis.lty = 1,names.arg = c('Vegetation Variables', '% Habitat',"Flower Variables",'Landscape Complexity',"% Farming",'Soil Properties','Distance to Habitat','Tree Variables','Water Properties','Patches of Habitat','Habitat Connectivity','Elevation','% Bare Ground','Canopy Cover','Edge Density','Habitat Heterogeneity','Habitat Quality','Landscape Intensity','Leaf Litter Depth','% Artifical Surfaces','Time Since Fire','Weed Richness'))
mtext(text = "Number of Articles",side = 2,line = 2.5)


#Graphing together

dev.new(height=7,width=10,dpi=80,pointsize=14,noRStudioGD = T)
layout(mat = matrix(c(1,2,2,3,
                      4,4,4,4),nrow = 2,byrow = T), heights = c(1,2))

plot.new()
par(mar=c(4,4,1,1))
barplot(table(rawlit$Spatial_Scale)[order(table(rawlit$Spatial_Scale),decreasing = T)],las = 2, axis.lty = 1,names.arg = c('No Environmental \n Variables','Site','Landscape','Both'))
mtext(text = "Number of Articles",
      side = 2,line = 2.5,cex = 0.7)
text(labels = "(a)", x = -0.7, y = 140, xpd = NA,cex = 1)
plot.new()

par(mar=c(10,4,1,1))
barplot(table(c(rawlit$Primary_Enviro_Variable,rawlit$Secondary_Enviro_Variable))[order(table(c(rawlit$Primary_Enviro_Variable,rawlit$Secondary_Enviro_Variable)),decreasing = T)],las = 2, axis.lty = 1,names.arg = c('Vegetation Variables', '% Habitat',"Flower Variables",'Landscape Complexity',"% Farming",'Soil Properties','Distance to Habitat','Tree Variables','Water Properties','Patches of Habitat','Habitat Connectivity','Elevation','% Bare Ground','Canopy Cover','Edge Density','Habitat Heterogeneity','Habitat Quality','Landscape Intensity','Leaf Litter Depth','% Artifical Surfaces','Time Since Fire','Weed Richness'))
mtext(text = "Number of Articles",
      side = 2,line = 2.5,cex = 0.7)
text(labels = "(b)", x = -2.5, y = 71, xpd = NA,cex = 1)
