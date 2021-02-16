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
# if not, can you add these?


# load packages ---------------------------------------------------------

library(tidyverse) #data wrangling


# Load data  -----------------------------------------------
# Take in data that has redundant evidence points removed, spatial filtering done, and spatial variables added

alldata<-read.csv("Data/AllCodedData_withGIScontext.csv")
dim(alldata)
names(alldata)

# data for plant functional groups
plants<-read.delim("Data/PFTs_Systematic_Herbivory_Map_16022021.txt")
dim(plants)
head(plants)  

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

# Herbivores(exposure) -----------------------------------------------

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

## done until here 16.2. Eeva

















tito<-filter(alldata, study_method=="field")
tito<-filter(tito, study_design=="experimental")
table(tito$exposure_quantification)
tito<-filter(tito, exposure_quantification=="several")
tito$exposure_quantification_comments # 8 additional exclosures, 2 additional simulated herbivory

## approaches to herbivory in observational field studies
tito<-filter(alldata, study_method=="field")
tito<-filter(tito, study_design=="observational")
table(tito$exposure_quantification)
tito<-filter(tito, exposure_quantification=="several")
sum(grepl("outbreak",tito$exposure_quantification_comments), na.rm=TRUE) #6
sum(grepl("spatial contrast/gradient",tito$exposure_quantification_comments), na.rm=TRUE) #4
sum(grepl("fence",tito$exposure_quantification_comments), na.rm=TRUE) #3



# Rangifer   -----------------------------------------------
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

# Geese -----------------------------------------------
waterfowl<-droplevels(filter(alldata, herbivore_type=="waterfowl") )
table(waterfowl$herbivore_identity)

anser<-filter(alldata, grepl("Anser", alldata$herbivore_identity))
branta<-filter(alldata, grepl("Branta", alldata$herbivore_identity))
chen<-filter(alldata, grepl("Chen", alldata$herbivore_identity))
geese<-rbind.data.frame(anser, branta, chen)

table(geese$country)
dim(geese)[1]

# small rodents and pikas -----------------------------------------------
sma<-filter(alldata, herbivore_type=="small rodents and pikas") 
sma_2<-filter(alldata, herbivore_type_comments=="small rodents and pikas") 
sma<-rbind.data.frame(sma, sma_2)

boxplot(sma$distance_from_coast)
quantile(sma$distance_from_coast, c(.25, .82, .90), na.rm=TRUE) 

table(sma$country)
(3+27+32+1+2)/dim(sma)[1]












## approaches to herbivory in experimental field studies
tito<-filter(alldata, study_method=="field")
tito<-filter(tito, study_design=="experimental")
table(tito$exposure_quantification)
tito<-filter(tito, exposure_quantification=="several")
tito$exposure_quantification_comments # 8 additional exclosures, 2 additional simulated herbivory

## approaches to herbivory in observational field studies
tito<-filter(alldata, study_method=="field")
tito<-filter(tito, study_design=="observational")
table(tito$exposure_quantification)
tito<-filter(tito, exposure_quantification=="several")
sum(grepl("outbreak",tito$exposure_quantification_comments), na.rm=TRUE) #6
sum(grepl("spatial contrast/gradient",tito$exposure_quantification_comments), na.rm=TRUE) #4
sum(grepl("fence",tito$exposure_quantification_comments), na.rm=TRUE) #3



## approaches to herbivory in quasi-exp field studies
# tito<-filter(alldata, study_method=="field")
# tito<-filter(tito, study_design=="quasi-experimental")
# table(tito$exposure_quantification)
# tito<-filter(tito, exposure_quantification=="several")
# tito$exposure_quantification_comments # 2 additional herbivore outbreaks, 6 additional spatial contrasts

## design of field experiments
tito<-filter(alldata, study_method=="field")
tito<-filter(tito, study_design=="experimental")
CI<-filter(tito, experimental_design=="control-impact")
dim(CI)[1]/dim(tito)[1]

## design of quasi-experiments
tito<-filter(alldata, study_method=="field")
tito<-filter(tito, study_design=="observational")
CI<-filter(tito, experimental_design=="control-impact")
dim(CI)[1]/dim(tito)[1]

table(tito$experimental_design)
tito<-filter(tito, exposure_quantification=="several")
tito$exposure_quantification_comments # 2 additional herbivore outbreaks, 6 additional spatial contrasts


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

# less clear info on study design -----------------------------------------------
tito<-filter(alldata, year_start=="not reported") 
tito<-filter(alldata, temporal_resolution=="not reported") 
tito<-filter(alldata, extent_of_spatial_scale=="not reported") 
tito<-filter(alldata, spatial_resolution_recorded=="not reported") 
tito<-filter(alldata, spatial_resolution_reported=="not reported") 

summary(tito$year)
boxplot(tito$year)
dim(tito)




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

## response types to herbivory 
bio<-filter(alldata, measured_response_variable=="biomass") 
cov<-filter(alldata, measured_response_variable=="cover") 
div<-filter(alldata, measured_response_variable=="diversity") 
phy<-filter(alldata, measured_response_variable=="physiological response") 
mor<-filter(alldata, measured_response_variable=="morphological measure") 


sev<-filter(alldata, measured_response_variable=="several") 
sum(str_count(sev$measured_response_comments, "biomass")) #167
sum(str_count(sev$measured_response_comments, "cover")) #114
sum(str_count(sev$measured_response_comments, "diversity")) #85
sum(str_count(sev$measured_response_comments, "physiological response")) #65
sum(str_count(sev$measured_response_comments, "morphological measure")) #165


(dim(bio)[1]+167)/dim(alldata)[1] #36%
(dim(cov)[1]+114)/dim(alldata)[1] #24%
(dim(div)[1]+85)/dim(alldata)[1] #21%
(dim(phy)[1]+65)/dim(alldata)[1] #18%
(dim(mor)[1]+165)/dim(alldata)[1] #28%





# Take in filtered data with GIS variables  -----------------------------------------------


# Elevation   -----------------------------------------------

boxplot(alldata$elevation_DEM)
quantile(alldata$elevation_DEM, c(.25, .50, .98), na.rm=TRUE) 
summary(alldata$elevation_DEM)


# Distance from coast    -----------------------------------------------

boxplot(alldata$distance_from_coast)
quantile(alldata$distance_from_coast, c(.25, .50, .90), na.rm=TRUE) 
summary(alldata$distance_from_coast)
dim(alldata[as.numeric(alldata$distance_from_coast)>=150,])[1]/dim(alldata)[1]

# Distance from treeline    -----------------------------------------------
names(alldata)

# Permafrost    -----------------------------------------------
names(alldata)


# Soil type     -----------------------------------------------
summary(as.factor(alldata$soil_type.))
162/678
155/678
87/678
153/678


# Disturbance     -----------------------------------------------
table(alldata$disturbance)
481/678 # not reported

levels(as.factor(alldata$disturbance))

sum(grepl("fire",c(alldata$disturbance_comment, alldata$disturbance)), na.rm=TRUE)/dim(alldata)[1] ##33
sum(grepl("flooding",c(alldata$disturbance_comment, alldata$disturbance)), na.rm=TRUE)/dim(alldata)[1] ## 18
sum(grepl("geomorphological issues",c(alldata$disturbance_comment, alldata$disturbance)), na.rm=TRUE)/dim(alldata)[1] ## 14
sum(grepl("human infrastructure",c(alldata$disturbance_comment, alldata$disturbance)), na.rm=TRUE)/dim(alldata)[1] ## 31
sum(grepl("pollution",c(alldata$disturbance_comment, alldata$disturbance)), na.rm=TRUE)/dim(alldata)[1] ## 8

sum(grepl("outbreaks",c(alldata$disturbance_comment, alldata$disturbance)), na.rm=TRUE) # 22
sum(grepl("herbivory",c(alldata$disturbance_comment, alldata$disturbance)), na.rm=TRUE) # 57
(22+57)/dim(alldata)[1]


# Climate -----------------------------------------------
names(alldata)
# https://www.worldclim.org/data/bioclim.html
# data are in celcius degrees*10 and mm for precipitation
boxplot(alldata$bio1/10)
quantile(alldata$bio1/10, c(.25, .50, .90), na.rm=TRUE) 
sum(alldata$bio1/10>(-5), na.rm=TRUE)/dim(alldata)[1]
sum(alldata$bio1/10>(-4), na.rm=TRUE)/dim(alldata)[1]
sum(alldata$bio1/10>(-3.8), na.rm=TRUE)/dim(alldata)[1]


boxplot(alldata$bio12)
quantile(alldata$bio12, c(.25, .50, .90), na.rm=TRUE) 


# Food web context -----------------------------------------------
table(alldata$food_web_context_other_herbivores)
table(alldata$food_web_context_predators)
490/678
631/678


# Human context -----------------------------------------------
names(alldata)
table(alldata$management_area)
table(alldata$management)

table(alldata$conservation_herbivore)
table(alldata$conservation_herbivore_comments)

# historical or current area management
dim(alldata[alldata$management_area=="current"|alldata$management_area=="historical"|alldata$management_area=="historical and current",])[1]/dim(alldata)[1]

# conservation area 
dim(alldata[alldata$conservation_study_area=="protected area",])[1]/dim(alldata)[1]
#
table(alldata$conservation_study_area)
#table(alldata$conservation_study_area_comment)

# conservbation focus
dim(alldata[alldata$conservation_focus=="mentioned",])[1]/dim(alldata)[1]

table(alldata$conservation_focus)




