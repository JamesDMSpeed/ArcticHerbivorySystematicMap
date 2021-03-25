#Systematic map of herbivory in Arctic tundra
# script for calculating statistics for the results chapter

# The script takes in a data file that has 
# 1) filtered away all redundant datapoints
# 2) filtered away points outside arctic study area
# 3) added all GIS-context variables (or NA for evidence points that cannot have them for various reasons)

# the script also takes in a separatae data file on plant functional groups

rm(list=ls())
objects()


# load packages ---------------------------------------------------------

library(tidyverse) #data wrangling
library(ggplot2) # exploring "study quality" across ecol contexts

# Load data  -----------------------------------------------
# Take in data that has redundant evidence points removed, spatial filtering done, and spatial variables added

alldata<-read.csv("Data/AllCodedData_withGIScontext.csv")
dim(alldata)

# data for plant functional groups
plants<-read.delim("Data/PFTs_Systematic_Herbivory_Map_23032021.txt")
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
sum(grepl("field",c(tito$study_method, tito$study_method_comments)), na.rm=TRUE) 

# no. observational field studies
# first combine obs & quasi-exp
alldata$study_design[alldata$study_design=="quasi-experimental"]<-"observational"
tito<-filter(alldata, study_design=="observational") 
sum(grepl("field",c(tito$study_method, tito$study_method_comments)), na.rm=TRUE) 

# no other methods
sum(grepl("remote sensing",c(alldata$study_method, alldata$study_method_comments)), na.rm=TRUE) 
sum(grepl("greenhouse",c(alldata$study_method, alldata$study_method_comments)), na.rm=TRUE) 
sum(grepl("modelling",c(alldata$study_method, alldata$study_method_comments)), na.rm=TRUE) 


## additional exposures
table(alldata$additional_exposures)
levels(as.factor(alldata$additional_treatments_comments))

tito<-filter(alldata, study_method=="field") 
tito<-filter(tito, study_design=="experimental") 
sum(grepl("warming",c(tito$additional_exposures, tito$additional_treatments_comments)), na.rm=TRUE) 
sum(grepl("nutrient manipulation",c(tito$additional_exposures, tito$additional_treatments_comments)), na.rm=TRUE) 


## temporal aspects
table(alldata$temporal_resolution)
table(alldata$extent_of_temporal_scale)
alldata$extent_of_temporal_scale<-as.numeric(alldata$extent_of_temporal_scale)


tito<-filter(alldata, temporal_resolution=="once") 
dim(tito)[1]

tito<-filter(alldata, extent_of_temporal_scale==1) 
dim(tito)[1]

alldata$extent_of_temporal_scale
tito<-filter(alldata, extent_of_temporal_scale>=10)
tito$extent_of_temporal_scale
length(tito$extent_of_temporal_scale)

## spatial resolution
table(alldata$spatial_resolution_reported)
table(alldata$extent_of_spatial_scale)


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

dim(alldata)[1]
table(alldata$herbivore_type)
levels(as.factor(alldata$herbivore_type_comments))

## no. points per group
sum(grepl("other vertebrates",c(alldata$herbivore_type, alldata$herbivore_type_comments)), na.rm=TRUE) # 405
sum(grepl("waterfowl",c(alldata$herbivore_type, alldata$herbivore_type_comments)), na.rm=TRUE) # 135
sum(grepl("small rodents and pikas",c(alldata$herbivore_type, alldata$herbivore_type_comments)), na.rm=TRUE) # 161
sum(grepl("defoliating",c(alldata$herbivore_type, alldata$herbivore_type_comments)), na.rm=TRUE) # 71
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
table(sev$study_design) # nb quasi-experimental and observational pooled,


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
407+13
table(tito$experimental_design)
(287+8)/dim(tito)[1] # % of exp field studies using control-impact studies
(105+5)/dim(tito)[1]# % of obs field studies using control-impact studies

# Response types to herbivory (outcome) ---------------------------------------

sum(grepl("biomass", c(alldata$ measured_response_variable, alldata$ measured_response_comments)), na.rm=TRUE) 
sum(grepl("cover", c(alldata$ measured_response_variable, alldata$ measured_response_comments)), na.rm=TRUE) 
sum(grepl("diversity", c(alldata$ measured_response_variable, alldata$ measured_response_comments)), na.rm=TRUE) 
sum(grepl("physiological response", c(alldata$ measured_response_variable, alldata$ measured_response_comments)), na.rm=TRUE) 
sum(grepl("morphological measure", c(alldata$ measured_response_variable, alldata$ measured_response_comments)), na.rm=TRUE) 
sum(grepl("several", c(alldata$ measured_response_variable)), na.rm=TRUE) 

dim(alldata)[1]
317/662

# Ecological contexts covered by the evidence base: countries and zones ----------------------------
table(alldata$country)
sum(grepl("Norway|Sweden|Finland", c(alldata$country, alldata$country_comments)), na.rm=TRUE) 
263/dim(alldata)[1]

# add here the arctic zones vs subarctic
names(alldata)
table(alldata$Subzone)
4+30+102+45+110


# Ecological contexts covered by the evidence base: geographic space  ----------------------------
str(alldata$elevation_DEM)
alldata$elevation_DEM<-as.numeric(alldata$elevation_DEM)
boxplot(alldata$elevation_DEM)
quantile(alldata$elevation_DEM, c(.25, .50, .98), na.rm=TRUE) 
summary(alldata$elevation_DEM)
toto<-alldata[alldata$elevation_DEM<=100,]
length(na.omit(toto$elevation_DEM))/dim(alldata)[1]

# subarctic studies done at higher elevations than arctic studies
suba<-filter(alldata, north_of_treeline<0)
summary(suba$elevation_DEM)
arct<-filter(alldata, north_of_treeline>0)
summary(arct$elevation_DEM)

boxplot(alldata$distance_from_coast)
quantile(alldata$distance_from_coast, c(.25, .50, .90), na.rm=TRUE) 
summary(alldata$distance_from_coast)
dim(alldata[as.numeric(alldata$distance_from_coast)>=100,])[1]/dim(alldata)[1]

# treeline
names(alldata)
summary(alldata$north_of_treeline)


# not all  points have an arctic zone - how can that be? - effect of coast!
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
 
# how big % is close to the treeline?
new<-abs(alldata$north_of_treeline)
boxplot(new)
close<-new<=200
far<-new>=900
sum(close, na.rm = TRUE)
sum(far, na.rm = TRUE)

# permafrost
table(alldata$permafrost)
table(alldata$permafrost, alldata$country)


# Soil type     
summary(as.factor(alldata$soil_type.))
162/678
155/678
87/678
153/678

tito<-alldata[is.na(alldata$soil_type.),]
table(tito$country)

ggplot(alldata, aes(x=coordinates_E, y=coordinates_N, color=soil_type.)) + 
  geom_point(size=6) 

table(alldata$soil_type., alldata$country)
table(alldata$country)

# Disturbance 
table(alldata$disturbance)
470/662 # not reported


sum(grepl("fire",c(alldata$disturbance_comment, alldata$disturbance)), na.rm=TRUE)
sum(grepl("flooding",c(alldata$disturbance_comment, alldata$disturbance)), na.rm=TRUE)
sum(grepl("geomorphological issues",c(alldata$disturbance_comment, alldata$disturbance)), na.rm=TRUE)
sum(grepl("human infrastructure",c(alldata$disturbance_comment, alldata$disturbance)), na.rm=TRUE)
sum(grepl("pollution",c(alldata$disturbance_comment, alldata$disturbance)), na.rm=TRUE)
sum(grepl("outbreaks",c(alldata$disturbance_comment, alldata$disturbance)), na.rm=TRUE) +sum(grepl("herbivory",c(alldata$disturbance_comment, alldata$disturbance)), na.rm=TRUE) 


# Ecological contexts covered by the evidence base: current climate context   ----------------------------

names(alldata)
boxplot(alldata$bio1/10)
quantile(alldata$bio1/10, c(.25, .50, .90), na.rm=TRUE) 

## mean annual temp across study area
mean(na.omit(data_eco_cont$bio1/10))

## mean annual precipitation 
mean(na.omit(data_eco_cont$bio12))
boxplot(na.omit(data_eco_cont$bio12))
#
mean(na.omit(alldata$bio12))
boxplot(na.omit(alldata$bio12))
quantile(alldata$bio12, c(.25, .50, .90), na.rm=TRUE) 
quantile(data_eco_cont$bio12, c(.25, .50, .90), na.rm=TRUE) 


## annual range of temprature
mean(na.omit(data_eco_cont$bio7/10)) # 48
## 90% of evidence points had less than 49C annual range. 
quantile(alldata$bio7/10, c(.25, .50, .90), na.rm=TRUE) 
quantile(data_eco_cont$bio7/10, c(.25, .50, .90), na.rm=TRUE) 

## how large part of the study area had larger annual range than this
## BIO7 = Temperature Annual Range (BIO5-BIO6)
boxplot(data_eco_cont$bio7/10)
hist(data_eco_cont$bio7/10)
toto<-c(na.omit(data_eco_cont$bio7/10))
test<-toto<49
test<-as.numeric(test)
sum(test)/length(test) 


boxplot(alldata$Annual_Precipitation)
quantile(alldata$Annual_Precipitation, c(.25, .50, .90), na.rm=TRUE) 



boxplot(alldata$Max_Temperature_of_Warmest_Month)
quantile(alldata$Max_Temperature_of_Warmest_Month, c(.25, .50, .90), na.rm=TRUE) 


boxplot(alldata$Temperature_Annual_Range)
quantile(alldata$Temperature_Annual_Range, c(.25, .50, .90), na.rm=TRUE) 



# Ecological contexts covered by the evidence base: climate change context   ----------------------------

names(alldata)
summary(alldata$GrowingSeasonLength.trend)
summary(alldata$NDVI.trend)
summary(alldata$Temperature.anomaly)

temp_data<-cbind.data.frame("data"= c(alldata$Temperature.anomaly, data_eco_cont$Temperature.anomaly), "group"=c(rep("used", times=length(alldata$Temperature.anomaly)), rep("available", times=length(data_eco_cont$Temperature.anomaly))))
temp_data<-cbind.data.frame("data"= c(alldata$NDVI.trend, data_eco_cont$NDVI.trend), "group"=c(rep("used", times=length(alldata$NDVI.trend)), rep("available", times=length(data_eco_cont$NDVI.trend))))
temp_data<-cbind.data.frame("data"= c(alldata$GrowingSeasonLength.trend, data_eco_cont$GrowingSeasonLength.trend), "group"=c(rep("used", times=length(alldata$GrowingSeasonLength.trend)), rep("available", times=length(data_eco_cont$GrowingSeasonLength.trend))))

toto<-alldata[!(is.na(alldata$NDVI.trend)),]
temp_data<-cbind.data.frame("data"= c(toto$Temperature.anomaly, data_eco_cont$Temperature.anomaly), "group"=c(rep("used", times=length(toto$Temperature.anomaly)), rep("available", times=length(data_eco_cont$Temperature.anomaly))))

ggplot(temp_data, aes(x = data, group = group)) +
  geom_density(aes(fill = group), alpha = 0.4)

## conclusion: the histograms in Fig 5 give all datapoints, not only those that are in the xyplot

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

summary(waterfowl$elevation_DEM)
boxplot(waterfowl$elevation_DEM)
# how many are max 150m altitude
quantile(waterfowl$elevation_DEM, c(.25, .50, .90), na.rm=TRUE) 
dim(waterfowl[waterfowl$elevation_DEM<=150,])[1]

boxplot(waterfowl$distance_from_coast)
dim(waterfowl[waterfowl$distance_from_coast<=50,])[1]

table(waterfowl$Subzone)

boxplot(waterfowl$Annual_Precipitation)

table(waterfowl$permafrost)


# small rodents and pikas
sma<-filter(alldata, herbivore_type=="small rodents and pikas") 
sma_2<-filter(alldata, herbivore_type_comments=="small rodents and pikas") 
sma$evidence_point_ID %in% sma_2$evidence_point_ID; sma_2$evidence_point_ID %in% sma$evidence_point_ID
sma<-rbind.data.frame(sma, sma_2)

boxplot(sma$distance_from_coast)
dim(sma[sma$distance_from_coast<=50,])[1]


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
oth_rus<-oth[oth$country=="Russia",]
ggplot(oth_rus, aes(x=coordinates_E, y=coordinates_N, color="herbivore_type")) + 
  geom_point(size=6) 

names(oth_rus)

boxplot(oth$bio1/10)
quantile(oth$bio1/10, c(.25, .50, .90), na.rm=TRUE) 

boxplot(oth$distance_from_coast)
dim(oth[oth$distance_from_coast<=50,])[1]

table(oth$Subzone)
197+75

# defoliation invertebrates
invert<-filter(alldata, grepl("defoliating", alldata$herbivore_type))
invert2<-filter(alldata, grepl("defoliating", alldata$herbivore_type_comments))
invert$evidence_point_ID %in% invert2$evidence_point_ID; invert2$evidence_point_ID %in% invert$evidence_point_ID
invert<-rbind.data.frame(invert, invert2)
length(unique(invert$evidence_point_ID))

table(invert$temporal_resolution)
table(invert$exposure_quantification)
boxplot(invert$elevation_DEM)
dim(invert[invert$elevation_DEM<=300,])[1]
table(invert$Subzone)
boxplot(invert$bio12)
dim(invert[invert$bio12>=400,])[1]
boxplot(invert$bio1/10)
dim(invert[invert$bio1/10>=-5,])[1]
table(invert$country)
22+15+10
canada<-invert[invert$country=="Canada",]
dim(canada[canada$coordinates_E>-100,])[1]

boxplot(canada$coordinates_E)


# Mapping the quality of included studies ----------------------------------

# study type
names(alldata)
table(alldata$study_design)
#211+113

## experimental design types
tito<-filter(alldata, study_design=="experimental")
table(tito$experimental_design)

# missing information
table(alldata$year_start)
table(alldata$temporal_resolution)
table(alldata$extent_of_spatial_scale) # not reported and not relevant pooled in data from shinyapp. see raw data
table(alldata$spatial_resolution_recorded)
table(alldata$spatial_resolution_reported)

# temporal trend in  misisng information of start year
toto<-cbind.data.frame("year"=levels(as.factor(alldata$year)), "evi_points"=as.vector(table(alldata$year)))
tito<-filter(alldata, year_start=="not reported") 
tito<-as.data.frame(table(tito$year))
names(tito)<-c("year", "evi_points_missing")
prop_table<-left_join(toto, tito)
prop_table[is.na(prop_table)] <- 0
prop_table$evi_points_missing_prop<-prop_table$evi_points_missing/prop_table$evi_points
plot(prop_table$year, prop_table$evi_points_missing_prop)
summary(lm(prop_table$evi_points_missing_prop~as.numeric(prop_table$year)))


# temporal trend in  misisng information of temporal resolution
toto<-cbind.data.frame("year"=levels(as.factor(alldata$year)), "evi_points"=as.vector(table(alldata$year)))
tito<-filter(alldata, temporal_resolution=="not reported") 
tito<-as.data.frame(table(tito$year))
names(tito)<-c("year", "evi_points_missing")
prop_table<-left_join(toto, tito)
prop_table[is.na(prop_table)] <- 0
prop_table$evi_points_missing_prop<-prop_table$evi_points_missing/prop_table$evi_points
plot(prop_table$year, prop_table$evi_points_missing_prop)
summary(lm(prop_table$evi_points_missing_prop~as.numeric(prop_table$year)))


# temporal trend in  misisng information spatial_resolution_recorded
toto<-cbind.data.frame("year"=levels(as.factor(alldata$year)), "evi_points"=as.vector(table(alldata$year)))
tito<-filter(alldata, spatial_resolution_recorded=="not reported") 
tito<-as.data.frame(table(tito$year))
names(tito)<-c("year", "evi_points_missing")
prop_table<-left_join(toto, tito)
prop_table[is.na(prop_table)] <- 0
prop_table$evi_points_missing_prop<-prop_table$evi_points_missing/prop_table$evi_points
plot(prop_table$year, prop_table$evi_points_missing_prop)
summary(lm(prop_table$evi_points_missing_prop~as.numeric(prop_table$year)))


# temporal trend in  misisng information spatial_resolution_reported
toto<-cbind.data.frame("year"=levels(as.factor(alldata$year)), "evi_points"=as.vector(table(alldata$year)))
tito<-filter(alldata, spatial_resolution_reported=="not reported") 
tito<-as.data.frame(table(tito$year))
names(tito)<-c("year", "evi_points_missing")
prop_table<-left_join(toto, tito)
prop_table[is.na(prop_table)] <- 0
prop_table$evi_points_missing_prop<-prop_table$evi_points_missing/prop_table$evi_points
plot(prop_table$year, prop_table$evi_points_missing_prop)
summary(lm(prop_table$evi_points_missing_prop~as.numeric(prop_table$year)))


# temporal trend in  misisng information extent_of_spatial_scale
toto<-cbind.data.frame("year"=levels(as.factor(alldata$year)), "evi_points"=as.vector(table(alldata$year)))
tito<-filter(alldata, extent_of_spatial_scale=="not reported") 
tito<-as.data.frame(table(tito$year))
names(tito)<-c("year", "evi_points_missing")
prop_table<-left_join(toto, tito)
prop_table[is.na(prop_table)] <- 0
prop_table$evi_points_missing_prop<-prop_table$evi_points_missing/prop_table$evi_points
plot(prop_table$year, prop_table$evi_points_missing_prop)
summary(lm(prop_table$evi_points_missing_prop~as.numeric(prop_table$year)))
confint(lm(prop_table$evi_points_missing_prop~as.numeric(prop_table$year)))

# Mapping the quality of included studies, study types across ecological contexts ----------------------------------
## extent of spatial scale; are there ecol-contexts that 
toto<-alldata
names(toto)

toto$grouped_spat_ext<-toto$extent_of_spatial_scale
levels(as.factor(toto$grouped_spat_ext))

library(plyr)
toto$grouped_spat_ext<- revalue(toto$extent_of_spatial_scale, c("1x1 km or less"="small", "from 1x1 km to 10x10 km"="local", "from 10x10 km to 100x100 km"="large",
             "from 100x100 km to 1000x1000 km" = "large", "larger than 1000x1000 km" ="large"))

ggplot(data=toto, aes(x=grouped_spat_ext, y=bio1/10))+
  geom_boxplot()
ggplot(data=toto, aes(x=grouped_spat_ext, y=bio7))+
  geom_boxplot()
ggplot(data=toto, aes(x=grouped_spat_ext, y=bio12))+
  geom_boxplot()





# 
# ## short-term studies over MAT
# toto<-cbind.data.frame("MAT"=levels(as.factor(alldata$bio1)), "evi_points"=as.vector(table(alldata$bio1)))
# tito<-filter(alldata, extent_of_spatial_scale=="1x1 km or less"|extent_of_spatial_scale=="from 1x1 km to 10x10 km") 
# tito<-as.data.frame(table(tito$bio1))
# names(tito)<-c("MAT", "evi_points_local")
# prop_table<-left_join(toto, tito)
# prop_table[is.na(prop_table)] <- 0
# prop_table$evi_points_local_prop<-prop_table$evi_points_local/prop_table$evi_points
# plot(prop_table$MAT, prop_table$evi_points_local_prop)
# 
# 
# 
# 
# # extent of spatial scale not relevant?
# tito<-filter(alldata, extent_of_spatial_scale=="not relevant") 
# tito<-filter(tito, study_method!="greenhouse") 
# tito<-filter(tito, study_method!="modelling") 
# 
# tito$evidence_point_ID
# View(tito)
# 
# ## study type 
# tito<-filter(alldata, study_design=="experimental")
# table(tito$experimental_design)
# 
# 
# tito<-filter(alldata, biological_organization_level_reported=="individual") 
# table(tito$spatial_resolution_reported, tito$extent_of_spatial_scale)
# 
# 
# 
# 
# 
# 
# ## spatial scale of sampling 
# table(alldata$spatial_resolution_reported)
# table(alldata$extent_of_spatial_scale) # data from shinyapp has no category from 10 to 100km. 
# 
# 
# ## areas where long-term studies needed?
# toto<-alldata
# # the study with length = zero is now corrected in the raw data. it was a study with start year = end year, and should therefore have had one as study length
# toto$extent_of_temporal_scale<-plyr::mapvalues(toto$extent_of_temporal_scale, 0, 1)
# toto$grouped_temp_ext<-toto$extent_of_temporal_scale
# toto$grouped_temp_ext<-plyr::mapvalues(toto$grouped_temp_ext, c(2:5), rep("2-5", times=length(c(2:5))))
# toto$grouped_temp_ext<-plyr::mapvalues(toto$grouped_temp_ext, c(6:10), rep("6-10", times=length(c(6:10))))
# toto$grouped_temp_ext<-plyr::mapvalues(toto$grouped_temp_ext, c(11:20), rep("11-20", times=length(c(11:20))))
# toto$grouped_temp_ext<-plyr::mapvalues(toto$grouped_temp_ext, c(21:45), rep("21-45", times=length(c(21:45))))
# toto$grouped_temp_ext<-plyr::mapvalues(toto$grouped_temp_ext, c(21:45), rep("21-45", times=length(c(21:45))))
# 
# toto<-toto %>% mutate(grouped_temp_ext= na_if(grouped_temp_ext, "not relevant"))
# toto<-toto %>% mutate(grouped_temp_ext= na_if(grouped_temp_ext, "not reported"))
# toto<-toto %>% mutate(grouped_temp_ext= na_if(grouped_temp_ext, "I15-I14+1"))
# toto<-toto %>% mutate(grouped_temp_ext= na_if(grouped_temp_ext, "AH15-AH14+1"))
# 
# toto<-toto %>%
#   mutate(grouped_temp_ext = fct_relevel(grouped_temp_ext, 
#                                         "1", "2-5", "6-10", 
#                                         "11-20", "21-45", "longer than 45"))
# 
#                               
# ggplot(toto, aes(x=country, fill=grouped_temp_ext)) + 
#   geom_bar(position="fill")
# 
# ggplot(toto, aes(x=country, fill=grouped_temp_ext)) + 
#   geom_bar()
# 
# ggplot(toto, aes(x=Subzone, fill=grouped_temp_ext)) + 
#   geom_bar(position="fill")
# 
# ggplot(toto, aes(x=Subzone, fill=grouped_temp_ext)) + 
#   geom_bar()
# 
# ## nb the datafile is missing one category
# ggplot(toto, aes(x=Subzone, fill=extent_of_spatial_scale)) + 
#   geom_bar()
# 
# 
# 
# # Rangifer   
# levels(alldata$herbivore_identity)
# sum(na.omit(str_count(alldata$herbivore_identity, "Rangifer"))) #366
# dim(reindeer)[1]/dim(alldata)[1] #52%
# 
# reindeer<-filter(alldata, grepl("Rangifer", alldata$herbivore_identity))
# table(reindeer$country)
# table(reindeer$country_comments)
# # Fennoscandia, Svalbard, Yamal 
# 65+85+45+37+22+2+2+5+1
# 264/349
# 
# #nearctic
# (39+35)/dim(reindeer)[1] #20%
# #scandinavia
# (65+101+24+11)/dim(reindeer)[1]
# 
# 
# 
# 
# 
# 
