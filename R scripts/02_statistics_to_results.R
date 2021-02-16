#Systematic map of herbivory in Arctic tundra
# script for calculating statistics for the results chapter

### NOTES FROM EEVA TO JAMES --------------------------------------------------------
# I here take in a file that has 
# 1) filtered away all redundant datapoints
# 2) filtered away points outside arctic study area
# 3) added all GIS-context variables (or NA for evidence points that cannot have them for various reasons)

# I chose file "AllCodedData_withGIScontext.csv" 

# this file is missing variables that area already in the Shiny App
# bioclimatic zone
# distance to treeline
# permafrost

# this file is also missing variables that seem not to be in the Shiny App either but were on our original variable list
# 1) growing_season_productivity (i.e. current NDVI), we had said we take it from the data layers from Taejins paper
# 2) duration_of_growing_season (i.e. current growing season length),we had said we take it from the data layers from Taejins paper
# 3) recent_warming; we had listed changes in NDVI, changes in growing season length, and changes in temperature. 
# we planned to use this: GISTEMP Team, 2016: GISS Surface Temperature Analysis (GISTEMP). NASA Goddard Institute for Space Studies.     Hansen, J., R. Ruedy, M. Sato, and K. Lo, 2010: Global surface temperature change, Rev. Geophys., 48, RG4004, doi:10.1029/2010RG000345.
# from https://data.giss.nasa.gov/gistemp/maps/

# is there another file than "AllCodedData_withGIScontext.csv" that has these? 
# (i.e. file that has rows= evidence points, columns = variables)
# if not, can you add these and make such a file?


# load packages ---------------------------------------------------------

library(tidyverse) #data wrangling


# Load data  -----------------------------------------------
# Take in data that has redundant evidence points removed, spatial filtering done, and spatial variables added

alldata<-read.csv("Data/AllCodedData_withGIScontext.csv")
alldata<-read.delim("Data/AllCodedData_downloaded_fromEviAtlas_17022021.txt")
dim(alldata)

# data for plant functional groups
plants<-read.delim("Data/PFTs_Systematic_Herbivory_Map_16022021.txt")
#dim(plants)
#head(plants)  

# filter plant data so only evidence points in alldata are considered 
plants<-plants[plants$evidence_point_ID %in% alldata$evidence_point_ID,]

# Approaches, study designs, and methods  -----------------------------------------------

table(alldata$study_method)
table(alldata$study_design)
table(alldata$study_design_comments)
table(alldata$study_method_comments)

tito<-filter(alldata, study_method_comments=="phytotron") 
tito$study_method # all phytothron studies are counted in the greenhouse studies

tito<-filter(alldata, study_method_comments=="dendrochronology") 
tito$study_method # dendrochronology study is in the group "other" 

# no. experimental field studies 
tito<-filter(alldata, study_design=="experimental") 
sum(grepl("field",c(tito$study_method, tito$study_method_comments)), na.rm=TRUE) # 306

# no. observational field studies
# first combine obs & quasi-exp
alldata$study_design[alldata$study_design=="quasi-experimental"]<-"observational"
tito<-filter(alldata, study_design=="observational") 
sum(grepl("field",c(tito$study_method, tito$study_method_comments)), na.rm=TRUE) # 313

# no other methods
sum(grepl("remote sensing",c(alldata$study_method, alldata$study_method_comments)), na.rm=TRUE) # 29
sum(grepl("greenhouse",c(alldata$study_method, alldata$study_method_comments)), na.rm=TRUE) # 13
sum(grepl("modelling",c(alldata$study_method, alldata$study_method_comments)), na.rm=TRUE) # 19


## additional exposures
table(alldata$additional_exposures)
levels(as.factor(alldata$additional_treatments_comments))

tito<-filter(alldata, study_method=="field") 
tito<-filter(tito, study_design=="experimental") 
sum(grepl("warming",c(tito$additional_exposures, tito$additional_treatments_comments)), na.rm=TRUE) # 19
sum(grepl("nutrient manipulation",c(tito$additional_exposures, tito$additional_treatments_comments)), na.rm=TRUE) # 23


## temporal aspects
table(alldata$temporal_resolution)
table(alldata$extent_of_temporal_scale)
alldata$extent_of_temporal_scale<-as.numeric(alldata$extent_of_temporal_scale)


tito<-filter(alldata, temporal_resolution=="once") 
dim(tito)[1]

tito<-filter(alldata, extent_of_temporal_scale==1) 
dim(tito)[1]

tito<-alldata[alldata$extent_of_temporal_scale>10,]
dim(tito)[1]



# Arctic plants and plant communities (population)  -----------------------------------------------

table(alldata$biological_organization_level_reported)
table(alldata$biological_organization_level_reported_comments)

sum(grepl("population/species",c(alldata$biological_organization_level_recorded, alldata$biological_organization_recorded_comments)), na.rm=TRUE) 
sum(grepl("individual",c(alldata$biological_organization_level_recorded, alldata$biological_organization_recorded_comments)), na.rm=TRUE) 
sum(grepl("groups of species",c(alldata$biological_organization_level_recorded, alldata$biological_organization_recorded_comments)), na.rm=TRUE) 
sum(grepl("community",c(alldata$biological_organization_level_recorded, alldata$biological_organization_recorded_comments)), na.rm=TRUE) 

sum(grepl("population/species",c(alldata$biological_organization_level_reported, alldata$biological_organization_reported_comments)), na.rm=TRUE) 
sum(grepl("individual",c(alldata$biological_organization_level_reported, alldata$biological_organization_reported_comments)), na.rm=TRUE) 
sum(grepl("groups of species",c(alldata$biological_organization_level_reported, alldata$biological_organization_reported_comments)), na.rm=TRUE) 
sum(grepl("community",c(alldata$biological_organization_level_reported, alldata$biological_organization_reported_comments)), na.rm=TRUE) 

# no evidence points looking at different plant groups
names(plants)
sum(grepl("yes",plants$graminoids), na.rm=TRUE) 
sum(grepl("yes",plants$decidious_dwarf_shrubs), na.rm=TRUE) 
sum(grepl("yes",plants$evergreen_dwarf_shrubs), na.rm=TRUE)
sum(grepl("yes",plants$forbs), na.rm=TRUE) 
sum(grepl("yes",plants$bryophytes), na.rm=TRUE)
sum(grepl("yes",plants$lichens), na.rm=TRUE) 
sum(grepl("yes",plants$decidious_trees), na.rm=TRUE)
sum(grepl("yes",plants$vascular_plant_community), na.rm=TRUE) 
sum(grepl("yes",plants$ferns_and_allies), na.rm=TRUE)
sum(grepl("yes",plants$evergreen_trees), na.rm=TRUE) 
sum(grepl("yes",plants$evergreen_tall_shrubs), na.rm=TRUE) 
sum(grepl("yes",plants$decidious_tall_shrubs), na.rm=TRUE) 

# Herbivores (exposure) -----------------------------------------------

table(alldata$herbivore_type)
levels(as.factor(alldata$herbivore_type_comments))

## no. points per group
sum(grepl("other vertebrates",c(alldata$herbivore_type, alldata$herbivore_type_comments)), na.rm=TRUE) # 405
sum(grepl("waterfowl",c(alldata$herbivore_type, alldata$herbivore_type_comments)), na.rm=TRUE) # 135
sum(grepl("small rodents and pikas",c(alldata$herbivore_type, alldata$herbivore_type_comments)), na.rm=TRUE) # 161
sum(grepl("defoliating invertebrates",c(alldata$herbivore_type, alldata$herbivore_type_comments)), na.rm=TRUE) # 71
sum(grepl("galling invertebrates",c(alldata$herbivore_type, alldata$herbivore_type_comments)), na.rm=TRUE) # 3
sum(grepl("invertebrates feeding on reproductive structures",c(alldata$herbivore_type, alldata$herbivore_type_comments)), na.rm=TRUE) # 2
sum(grepl("phloem feeders",c(alldata$herbivore_type, alldata$herbivore_type_comments)), na.rm=TRUE) # 1
sum(grepl("root feeding invertebrates",c(alldata$herbivore_type, alldata$herbivore_type_comments)), na.rm=TRUE) # 2
sum(grepl("unknown",c(alldata$herbivore_type, alldata$herbivore_type_comments)), na.rm=TRUE) # 14
sum(grepl("several",c(alldata$herbivore_type, alldata$herbivore_type_comments)), na.rm=TRUE) # 14


# in total vertebrates
sum(grepl("other vertebrates|waterfowl|small rodents and pikas",c(alldata$herbivore_type, alldata$herbivore_type_comments)), na.rm=TRUE) # 602

# in total invertebrates
sum(grepl("invertebrates|insects|feeders|defoliating",c(alldata$herbivore_type, alldata$herbivore_type_comments)), na.rm=TRUE) # 82


## vertebrate herbivore genera
sum(grepl("Rangifer", alldata$herbivore_identity), na.rm=TRUE) 
sum(grepl("Lagopus", alldata$herbivore_identity), na.rm=TRUE) 
sum(grepl("Lepus", alldata$herbivore_identity), na.rm=TRUE) 
sum(grepl("Alces", alldata$herbivore_identity), na.rm=TRUE) 
sum(grepl("Ovibos", alldata$herbivore_identity), na.rm=TRUE) 
sum(grepl("Ovis", alldata$herbivore_identity), na.rm=TRUE) 

sum(grepl("Microtus", alldata$herbivore_identity), na.rm=TRUE) 
sum(grepl("Clethrionomus|Clethrionomys|Myodes", alldata$herbivore_identity), na.rm=TRUE) 
sum(grepl("Lemmus", alldata$herbivore_identity), na.rm=TRUE) 
sum(grepl("Dicrostonyx", alldata$herbivore_identity), na.rm=TRUE) 
sum(grepl("Spermophilus", alldata$herbivore_identity), na.rm=TRUE) 
sum(grepl("Urocitellus", alldata$herbivore_identity), na.rm=TRUE) 

sum(grepl("Chen", alldata$herbivore_identity), na.rm=TRUE) 
sum(grepl("Branta", alldata$herbivore_identity), na.rm=TRUE) 
sum(grepl("Anser", alldata$herbivore_identity), na.rm=TRUE) 

## invertebrate herbivore genera
table(alldata$herbivore_identity)
sum(grepl("Operophtera|Operopthera", alldata$herbivore_identity), na.rm=TRUE) 
sum(grepl("Epirrita", alldata$herbivore_identity), na.rm=TRUE) 


## approaches to study different types of herbivores 
vert<-filter(alldata, grepl("other vertebrates|waterfowl|small rodents and pikas", alldata$herbivore_type))
vert2<-filter(alldata, grepl("other vertebrates|waterfowl|small rodents and pikas", alldata$herbivore_type_comments))
#vert$evidence_point_ID %in% vert2$evidence_point_ID
#vert2$evidence_point_ID %in% vert$evidence_point_ID
vert<-rbind.data.frame(vert, vert2)

table(vert$study_method)
table(vert$study_design)
table(vert$biological_organization_level_reported)

invert<-filter(alldata, grepl("invertebrates|insects|feeders|defoliating", alldata$herbivore_type))
invert2<-filter(alldata, grepl("invertebrates|insects|feeders|defoliating", alldata$herbivore_type_comments))
invert<-rbind.data.frame(invert, invert2)

table(invert$study_method)
table(invert$study_design) # nb quasi-experimental and observational pooled, so total = 56
table(invert$biological_organization_level_reported)

sev<-filter(alldata, grepl("several", alldata$herbivore_type))
table(sev$study_method)
table(sev$study_design) # nb quasi-experimental and observational pooled, so total = 56

# Comparison between levels of herbivore impact (comparator) -----------------------------------------------
# get experimental field studies
levels(as.factor(alldata$exposure_quantification))
tito<-filter(alldata, study_method=="field")
tito<-filter(tito, study_design=="experimental")

# get observational field studies
alldata$study_design[alldata$study_design=="quasi-experimental"]<-"observational"
tito<-filter(alldata, study_method=="field")
tito<-filter(tito, study_design=="observational") 


sum(grepl("exclosure", c(tito$exposure_quantification, tito$exposure_quantification_comments)), na.rm=TRUE) 
sum(grepl("simulated herbivory", c(tito$exposure_quantification, tito$exposure_quantification_comments)), na.rm=TRUE) 
sum(grepl("enclosure", c(tito$exposure_quantification, tito$exposure_quantification_comments)), na.rm=TRUE) 
sum(grepl("fence", c(tito$exposure_quantification, tito$exposure_quantification_comments)), na.rm=TRUE) 
sum(grepl("herbivore outbreak", c(tito$exposure_quantification, tito$exposure_quantification_comments)), na.rm=TRUE) 
sum(grepl("herbivore population up", c(tito$exposure_quantification, tito$exposure_quantification_comments)), na.rm=TRUE) 
sum(grepl("herbivore population down", c(tito$exposure_quantification, tito$exposure_quantification_comments)), na.rm=TRUE) 
sum(grepl("spatial contrast/gradient", c(tito$exposure_quantification, tito$exposure_quantification_comments)), na.rm=TRUE) 

table(alldata$experimental_design)
table(tito$experimental_design)
294/dim(tito)[1] # % of exp field studies using control-impact studies
112/dim(tito)[1]# % of obs field studies using control-impact studies

# Response types to herbivory (outcome) ---------------------------------------

sum(grepl("biomass", c(alldata$ measured_response_variable, alldata$ measured_response_comments)), na.rm=TRUE) 
sum(grepl("cover", c(alldata$ measured_response_variable, alldata$ measured_response_comments)), na.rm=TRUE) 
sum(grepl("diversity", c(alldata$ measured_response_variable, alldata$ measured_response_comments)), na.rm=TRUE) 
sum(grepl("physiological response", c(alldata$ measured_response_variable, alldata$ measured_response_comments)), na.rm=TRUE) 
sum(grepl("morphological measure", c(alldata$ measured_response_variable, alldata$ measured_response_comments)), na.rm=TRUE) 
sum(grepl("several", c(alldata$ measured_response_variable)), na.rm=TRUE) 

dim(alldata)[1]
328/678

# Ecological contexts covered by the evidence base: countries and zones ----------------------------
table(alldata$country)
sum(grepl("Norway|Sweden|Finland", c(alldata$country, alldata$country_comments)), na.rm=TRUE) 
277/dim(alldata)[1]

# add here the arctic zones vs subarctic
table(alldata$Subzone)
5+43+106+51+120


# Ecological contexts covered by the evidence base: geographic space  ----------------------------

boxplot(alldata$elevation_DEM)
quantile(alldata$elevation_DEM, c(.25, .50, .98), na.rm=TRUE) 
summary(alldata$elevation_DEM)

boxplot(alldata$distance_from_coast)
quantile(alldata$distance_from_coast, c(.25, .50, .90), na.rm=TRUE) 
summary(alldata$distance_from_coast)
dim(alldata[as.numeric(alldata$distance_from_coast)>=150,])[1]/dim(alldata)[1]

# treeline
names(alldata)
summary(alldata$DistanceToTreeline/1000)

# all  points have an arctic zone
ggplot(alldata, aes(x=coordinates_E, y=coordinates_N, color=Subzone)) + 
  geom_point(size=6) 

# many sites have NA for distance to treeline
ggplot(alldata, aes(x=coordinates_E, y=coordinates_N, color=DistanceToTreeline)) + 
  geom_point(size=6) 

# Svalbard has very many of these NAs
tito<-alldata[is.na(alldata$DistanceToTreeline),]
table(tito$country)

boxplot(alldata$DistanceToTreeline/1000)
ggplot(alldata, aes(x=country, y=DistanceToTreeline/1000, fill=country)) +
#geom_point()
geom_violin()
#geom_boxplot()
 
# how big % is less then 100km? of the treeline?
new<-abs(alldata$DistanceToTreeline/1000)
boxplot(new)
close<-new<=200
far<-new>=900
sum(close, na.rm = TRUE)
sum(far, na.rm = TRUE)

# permafrost
table(alldata$Permafrost)
table(alldata$Permafrost, alldata$country)


# Soil type     
summary(as.factor(alldata$soil_type.))
162/678
155/678
87/678
153/678

tito<-alldata[is.na(alldata$soil_type.),]
table(tito$country)

# Disturbance 
table(alldata$disturbance)
481/678 # not reported


sum(grepl("fire",c(alldata$disturbance_comment, alldata$disturbance)), na.rm=TRUE)
sum(grepl("flooding",c(alldata$disturbance_comment, alldata$disturbance)), na.rm=TRUE)
sum(grepl("geomorphological issues",c(alldata$disturbance_comment, alldata$disturbance)), na.rm=TRUE)
sum(grepl("human infrastructure",c(alldata$disturbance_comment, alldata$disturbance)), na.rm=TRUE)
sum(grepl("pollution",c(alldata$disturbance_comment, alldata$disturbance)), na.rm=TRUE)
sum(grepl("outbreaks",c(alldata$disturbance_comment, alldata$disturbance)), na.rm=TRUE) +sum(grepl("herbivory",c(alldata$disturbance_comment, alldata$disturbance)), na.rm=TRUE) 




# Ecological contexts covered by the evidence base: current climate context   ----------------------------

names(alldata)
boxplot(alldata$Annual_Mean_Temperature)
quantile(alldata$Annual_Mean_Temperature, c(.25, .50, .90), na.rm=TRUE) 


boxplot(alldata$Annual_Precipitation)
quantile(alldata$Annual_Precipitation, c(.25, .50, .90), na.rm=TRUE) 


boxplot(alldata$Max_Temperature_of_Warmest_Month)
quantile(alldata$Max_Temperature_of_Warmest_Month, c(.25, .50, .90), na.rm=TRUE) 


boxplot(alldata$Temperature_Annual_Range)
quantile(alldata$Temperature_Annual_Range, c(.25, .50, .90), na.rm=TRUE) 



# Ecological contexts covered by the evidence base: climate change context   ----------------------------

names(alldata)
summary(alldata$GrowingSeasonLength.trend)

#  food web context   ----------------------------

table(alldata$food_web_context_other_herbivores)
table(alldata$food_web_context_predators)

# Ecological contexts covered by the evidence base: humans  -----------------------------------------------
table(alldata$management)
table(alldata$management_area)
table(alldata$management_plant)
table(alldata$management_herbivore)
table(alldata$management_herbivore_comments)

table(alldata$conservation_study_area)
table(alldata$conservation_study_area_comment)
table(alldata$conservation_plant)
table(alldata$conservation_plant_comments)
table(alldata$conservation_herbivore)
table(alldata$conservation_herbivore_comments)

table(alldata$conservation_focus)



sum(grepl("historical|current|historical and current",alldata$management_area), na.rm=TRUE)
sum(grepl("protected area",c(alldata$conservation_study_area, alldata$conservation_study_area_comment)), na.rm=TRUE)


# Ecological contexts covered by the evidence base: differences between herbivores?  -----------------------------------------------

table(alldata$herbivore_type)

# Geese
geese<-filter(alldata, grepl("waterfowl", alldata$herbivore_type))
geese2<-filter(alldata, grepl("waterfowl", alldata$herbivore_type_comments))
waterfowl<-rbind.data.frame(geese, geese2)
length(unique(waterfowl$evidence_point_ID))
table(waterfowl$country)

summary(waterfowl$Elevation)
boxplot(waterfowl$Elevation)
# how many are max 150m altitude
quantile(waterfowl$Elevation, c(.25, .50, .90), na.rm=TRUE) 
dim(waterfowl[waterfowl$Elevation<=150,])[1]

boxplot(waterfowl$DistancetoCoast)
dim(waterfowl[waterfowl$DistancetoCoast<=50,])[1]

table(waterfowl$Subzone)

boxplot(waterfowl$Annual_Precipitation)


# small rodents and pikas
sma<-filter(alldata, herbivore_type=="small rodents and pikas") 
sma_2<-filter(alldata, herbivore_type_comments=="small rodents and pikas") 
sma$evidence_point_ID %in% sma_2$evidence_point_ID; sma_2$evidence_point_ID %in% sma$evidence_point_ID
sma<-rbind.data.frame(sma, sma_2)

boxplot(sma$DistancetoCoast)
dim(sma[sma$DistancetoCoast<=50,])[1]


table(sma$country)
(3+27+32+2)
dim(sma)[1]

# other vertebrates
oth<-filter(alldata, grepl("other vertebrates", alldata$herbivore_type))
oth2<-filter(alldata, grepl("other vertebrates", alldata$herbivore_type_comments))
oth$evidence_point_ID %in% oth2$evidence_point_ID; oth2$evidence_point_ID %in% oth$evidence_point_ID
oth<-rbind.data.frame(oth, oth2)
dim(oth)

table(oth$country)
boxplot(oth$Annual_Mean_Temperature)
quantile(oth$Annual_Mean_Temperature, c(.25, .50, .90), na.rm=TRUE) 

boxplot(oth$DistancetoCoast)
dim(oth[oth$DistancetoCoast<=50,])[1]

table(oth$Subzone)
226+83

# defoliation invertebrates
invert<-filter(alldata, grepl("defoliating", alldata$herbivore_type))
invert2<-filter(alldata, grepl("defoliating", alldata$herbivore_type_comments))
invert$evidence_point_ID %in% invert2$evidence_point_ID; invert2$evidence_point_ID %in% invert$evidence_point_ID
invert<-rbind.data.frame(invert, invert2)
length(unique(invert$evidence_point_ID))

table(invert$temporal_resolution)
table(invert$exposure_quantification)
boxplot(invert$Elevation)
dim(invert[invert$Elevation<=300,])[1]
table(invert$Subzone)
boxplot(invert$Annual_Precipitation)
dim(invert[invert$Annual_Precipitation>=400,])[1]
boxplot(invert$Annual_Mean_Temperature)
dim(invert[invert$Annual_Mean_Temperature>=-5,])[1]
table(invert$country)
22+12+12
canada<-invert[invert$country=="Canada",]
dim(canada[canada$coordinates_E>-100,])[1]

boxplot(canada$coordinates_E)

# Rangifer   
levels(alldata$herbivore_identity)
sum(na.omit(str_count(alldata$herbivore_identity, "Rangifer"))) #366
dim(reindeer)[1]/dim(alldata)[1] #52%

reindeer<-filter(alldata, grepl("Rangifer", alldata$herbivore_identity))
table(reindeer$country)
table(reindeer$country_comments)
# Fennoscandia, Svalbard, Yamal 
65+85+45+37+22+2+2+5+1
264/349

#nearctic
(39+35)/dim(reindeer)[1] #20%
#scandinavia
(65+101+24+11)/dim(reindeer)[1]


# Mapping the quality of included studies ----------------------------------

table(alldata$experimental_design)

tito<-filter(alldata, study_design=="experimental")
table(tito$experimental_design)

table(alldata$year_start)
table(alldata$temporal_resolution)
table(alldata$extent_of_spatial_scale) # not reported and not relevant pooled in data from shinyapp. see raw data
table(alldata$spatial_resolution_recorded)
table(alldata$spatial_resolution_reported)


# extent of spatial scale not relevant?
tito<-filter(alldata, extent_of_spatial_scale=="not relevant") 
tito<-filter(tito, study_method!="greenhouse") 
tito<-filter(tito, study_method!="modelling") 

tito$evidence_point_ID
View(tito)

## study type 
tito<-filter(alldata, study_design=="experimental")
table(tito$experimental_design)


tito<-filter(alldata, biological_organization_level_reported=="individual") 
table(tito$spatial_resolution_reported, tito$extent_of_spatial_scale)






## spatial scale of sampling  -----------------------------------------------
tito<-filter(alldata, spatial_resolution_reported=="up to  1x1 m (including 1x1 m)") 
tito<-filter(alldata, spatial_resolution_reported=="from  1x1 m to 10x10 m (including 10x10m)") 
dim(tito)[1]/dim(alldata)[1]

levels(tito$extent_of_spatial_scale)
tito<-filter(alldata, extent_of_spatial_scale=="1x1 km or less") 
tito<-filter(alldata, extent_of_spatial_scale=="from 1x1 km to 10x10 km (including 10x10 km)") 
tito<-filter(alldata, extent_of_spatial_scale=="from 10x10 km to 100x100 km (including 100x100 km)") 
tito<-filter(alldata, extent_of_spatial_scale=="not reported") 
tito<-filter(alldata, extent_of_spatial_scale=="not relevant") 
dim(tito)[1]/dim(alldata)[1]

# mistakes in temporal extent
alldata$extent_of_temporal_scale<-as.numeric(alldata$extent_of_temporal_scale)
tito<-filter(alldata, !is.na(extent_of_temporal_scale))
tito$evidence_point_ID
View(tito)
str(alldata)









