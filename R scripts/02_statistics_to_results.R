#Systematic map of herbivory in Arctic tundra
# script for calculating statistics for the results chapter

# The script takes in a data files for 
# 1) evidence points
# 2) plant functional groups grouped for evidence points
# 3) ecological context variables for study area

rm(list=ls())
objects()


# load packages ---------------------------------------------------------

library(tidyverse) #data wrangling
library(ggplot2) # exploring "study quality" across ecol contexts
library(plyr) #data wrangling (revalue)


 # Load data  -----------------------------------------------
# Take in data that has redundant evidence points removed, spatial filtering done, and spatial variables added

alldata<-read.csv("Data/AllCodedData_withGIScontext.csv")
dim(alldata)
names(alldata)

# data for plant functional groups
plants<-read.delim("Data/PFTs_Systematic_Herbivory_Map_23032021.txt")
#dim(plants)
#head(plants)  

# filter plant data so only evidence points in alldata are considered 
plants<-plants[plants$evidence_point_ID %in% alldata$evidence_point_ID,]

# Take in pre-processed ecological context data
data_eco_cont<-read.csv("Data/RangeofEcoContexts.csv")
names(data_eco_cont)


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

## were there many different Branta species
grepl("Branta", alldata$herbivore_identity)
Branta<-filter(alldata, grepl("Branta", alldata$herbivore_identity))
Branta$herbivore_identity

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
summary(abs(alldata$north_of_treeline))
summary(alldata$north_of_treeline)

boxplot(alldata$north_of_treeline)
hist(abs(alldata$north_of_treeline), breaks=30)

# number of evidence points decreases with distance from treeline


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
table(alldata$permafrost, alldata$Subzone, useNA="ifany")
3+15+20

sum(grepl("spatial contrast/gradient", c(tito$exposure_quantification, tito$exposure_quantification_comments)), na.rm=TRUE) 


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
mean(na.omit(data_eco_cont$bio7/10)) # 47
mean(na.omit(alldata$bio7/10)) # 34

median(na.omit(data_eco_cont$bio7/10)) # 47
median(na.omit(alldata$bio7/10)) # 34


## 90% of evidence points had less than 49C annual range. 
quantile(alldata$bio7/10, c(.25, .50, .90), na.rm=TRUE) 
quantile(data_eco_cont$bio7/10, c(.25, .50, .90), na.rm=TRUE) 
mean(data_eco_cont$bio7/10, na.rm=TRUE)
summary(data_eco_cont$bio7/10)

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
table(alldata$extent_of_spatial_scale) 
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

### missing elevation and distance to coast!!!

palette = c("#00AFBB", "#E7B800")

toto<-alldata
names(toto)

toto$grouped_spat_ext<-toto$extent_of_spatial_scale
levels(as.factor(toto$grouped_spat_ext))
toto$grouped_spat_ext<- revalue(toto$extent_of_spatial_scale, c("1x1 km or less"="small", "from 1x1 km to 10x10 km"="small", "from 10x10 km to 100x100 km"="large",
             "from 100x100 km to 1000x1000 km" = "large", "larger than 1000x1000 km" ="large"))
toto<-toto[!(toto$grouped_spat_ext=="not relevant"),]
toto<-toto[!(toto$grouped_spat_ext=="not reported"),]

a<-ggplot(toto, aes(x = bio1/10, group = grouped_spat_ext)) +
  geom_density(aes(fill = grouped_spat_ext), alpha = 0.3)+theme_light()+
  theme(legend.position = c(0.1, 0.9), legend.text=element_text(size=15),legend.title = element_blank(), axis.title.y=element_blank())+
  xlab("Mean annual temperature")+scale_fill_manual(values=palette)
a
b<-ggplot(toto, aes(x = bio12, group = grouped_spat_ext)) +
  geom_density(aes(fill = grouped_spat_ext), alpha = 0.3, show.legend=FALSE)+theme_light()+
  theme(axis.title.y=element_blank())+xlab("Annual precipitation")+scale_fill_manual(values=palette)
c<-ggplot(toto, aes(x = bio7/10, group = grouped_spat_ext)) +
  geom_density(aes(fill = grouped_spat_ext), alpha = 0.3, show.legend=FALSE)+theme_light()+
  theme(axis.title.y=element_blank())+xlab("Temperature annual range")+scale_fill_manual(values=palette)
d<-ggplot(toto, aes(x = Subzone, group = grouped_spat_ext)) +
  geom_density(aes(fill = grouped_spat_ext), alpha = 0.3, show.legend=FALSE) +theme_light()+
  theme(axis.title.y=element_blank())+xlab("Subzone")+scale_fill_manual(values=palette)
e<-ggplot(toto, aes(x = north_of_treeline, group = grouped_spat_ext)) +
  geom_density(aes(fill = grouped_spat_ext), alpha = 0.3, show.legend=FALSE) +theme_light()+
  theme(axis.title.y=element_blank())+xlab("Distance to treeline (km)")+scale_fill_manual(values=palette)
f<-ggplot(toto, aes(x = ArcticHerbivore_Species.richness, group = grouped_spat_ext)) +
  geom_density(aes(fill = grouped_spat_ext), alpha = 0.3, show.legend=FALSE) +theme_light()+
  theme(axis.title.y=element_blank())+xlab("Arctic herbivore species richness")+scale_fill_manual(values=palette)
g<-ggplot(toto, aes(x = ArcticHerbivore_Phylogenetic.diversity, group = grouped_spat_ext)) +
  geom_density(aes(fill = grouped_spat_ext), alpha = 0.3, show.legend=FALSE) +theme_light()+
  theme(axis.title.y=element_blank())+xlab("Arctic herbivore phylogenetic diversity")+scale_fill_manual(values=palette)
h<-ggplot(toto, aes(x = ArcticHerbivore_Functional.diversity, group = grouped_spat_ext)) +
  geom_density(aes(fill = grouped_spat_ext), alpha = 0.3, show.legend=FALSE) +theme_light()+
  theme(axis.title.y=element_blank())+xlab("Arctic herbivore functional diversity")+scale_fill_manual(values=palette)
i<-ggplot(toto, aes(x = GPW, group = grouped_spat_ext)) +
  geom_density(aes(fill = grouped_spat_ext), alpha = 0.3, show.legend=FALSE) +theme_light()+
  theme(axis.title.y=element_blank())+xlab("Human population density")+scale_fill_manual(values=palette)
j<-ggplot(toto, aes(x = Footprint, group = grouped_spat_ext)) +
  geom_density(aes(fill = grouped_spat_ext), alpha = 0.3, show.legend=FALSE) +theme_light()+
  theme(axis.title.y=element_blank())+xlab("Human footprint")+scale_fill_manual(values=palette)
k<-ggplot(toto, aes(x = Current.NDVI, group = grouped_spat_ext)) +
  geom_density(aes(fill = grouped_spat_ext), alpha = 0.3, show.legend=FALSE)  +theme_light()+
  theme(axis.title.y=element_blank())+xlab("NDVI")+scale_fill_manual(values=palette)
l<-ggplot(toto, aes(x = CurrentGrowingSeasonLength, group = grouped_spat_ext)) +
  geom_density(aes(fill = grouped_spat_ext), alpha = 0.3, show.legend=FALSE) +theme_light()+
  theme(axis.title.y=element_blank())+xlab("Growing season length")+scale_fill_manual(values=palette)
m<-ggplot(toto, aes(x = NDVI.trend, group = grouped_spat_ext)) +
  geom_density(aes(fill = grouped_spat_ext), alpha = 0.3, show.legend=FALSE) +theme_light()+
  theme(axis.title.y=element_blank())+xlab("NDVI trend")+scale_fill_manual(values=palette)
n<-ggplot(toto, aes(x = GrowingSeasonLength.trend, group = grouped_spat_ext)) +
  geom_density(aes(fill = grouped_spat_ext), alpha = 0.3, show.legend=FALSE) +theme_light()+
  theme(axis.title.y=element_blank())+xlab("Growing season length trend")+scale_fill_manual(values=palette)
o<-ggplot(toto, aes(x = Temperature.anomaly, group = grouped_spat_ext)) +
  geom_density(aes(fill = grouped_spat_ext), alpha = 0.3, show.legend=FALSE) +theme_light()+
  theme(axis.title.y=element_blank())+xlab("Temperature anomaly")+scale_fill_manual(values=palette)
p<-ggplot(toto, aes(x = permafrost, group = grouped_spat_ext)) +
  geom_density(aes(fill = grouped_spat_ext), alpha = 0.3, show.legend=FALSE) +theme_light()+
  theme(axis.title.y=element_blank())+xlab("Permafrost")+scale_fill_manual(values=palette)
q<-ggplot(toto, aes(x = soil_type., group = grouped_spat_ext)) +
  geom_density(aes(fill = grouped_spat_ext), alpha = 0.3, show.legend=FALSE) +theme_light()+
  theme(axis.title.y=element_blank())+xlab("Soil type")+scale_fill_manual(values=palette)


tiff('Figures/supplement_spatial.tif',height=10,width=9,units = 'in',res=150)


print(grid.arrange(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,
                   ncol=3))

dev.off()
dev.off()

######### temporal scales

toto<-alldata
names(toto)

toto$grouped_temp<-toto$extent_of_temporal_scale
toto<-toto[!(toto$grouped_temp=="not relevant"),]
toto<-toto[!(toto$grouped_temp=="not reported"),]
toto$grouped_temp<-as.numeric(toto$grouped_temp)
toto$grouped_temp<-base::cut(toto$grouped_temp, breaks=c(0,1,1500))
toto$grouped_temp<- revalue(toto$grouped_temp, c("(0,1]"="one", "(1,1.5e+03]"="longer"))
 
a<-ggplot(toto, aes(x = bio1/10, group = grouped_temp)) +
  geom_density(aes(fill = grouped_temp), alpha = 0.3)+theme_light()+
   theme(legend.position = c(0.1, 0.9), legend.text=element_text(size=15),legend.title = element_blank(), axis.title.y=element_blank())+
  xlab("Mean annual temperature")+scale_fill_manual(values=palette)
a
b<-ggplot(toto, aes(x = bio12, group = grouped_temp)) +
  geom_density(aes(fill = grouped_temp), alpha = 0.3, show.legend=FALSE)+theme_light()+
  theme(axis.title.y=element_blank())+xlab("Annual precipitation")+scale_fill_manual(values=palette)
c<-ggplot(toto, aes(x = bio7/10, group = grouped_temp)) +
  geom_density(aes(fill = grouped_temp), alpha = 0.3, show.legend=FALSE)+theme_light()+
  theme(axis.title.y=element_blank())+xlab("Temperature annual range")+scale_fill_manual(values=palette)
d<-ggplot(toto, aes(x = Subzone, group = grouped_temp)) +
  geom_density(aes(fill = grouped_temp), alpha = 0.3, show.legend=FALSE) +theme_light()+
  theme(axis.title.y=element_blank())+xlab("Subzone")+scale_fill_manual(values=palette)
e<-ggplot(toto, aes(x = north_of_treeline, group = grouped_temp)) +
  geom_density(aes(fill = grouped_temp), alpha = 0.3, show.legend=FALSE) +theme_light()+
  theme(axis.title.y=element_blank())+xlab("Distance to treeline (km)")+scale_fill_manual(values=palette)
f<-ggplot(toto, aes(x = ArcticHerbivore_Species.richness, group = grouped_temp)) +
  geom_density(aes(fill = grouped_temp), alpha = 0.3, show.legend=FALSE) +theme_light()+
  theme(axis.title.y=element_blank())+xlab("Arctic herbivore species richness")+scale_fill_manual(values=palette)
g<-ggplot(toto, aes(x = ArcticHerbivore_Phylogenetic.diversity, group = grouped_temp)) +
  geom_density(aes(fill = grouped_temp), alpha = 0.3, show.legend=FALSE) +theme_light()+
  theme(axis.title.y=element_blank())+xlab("Arctic herbivore phylogenetic diversity")+scale_fill_manual(values=palette)
h<-ggplot(toto, aes(x = ArcticHerbivore_Functional.diversity, group = grouped_temp)) +
  geom_density(aes(fill = grouped_temp), alpha = 0.3, show.legend=FALSE) +theme_light()+
  theme(axis.title.y=element_blank())+xlab("Arctic herbivore functional diversity")+scale_fill_manual(values=palette)
i<-ggplot(toto, aes(x = GPW, group = grouped_temp)) +
  geom_density(aes(fill = grouped_temp), alpha = 0.3, show.legend=FALSE) +theme_light()+
  theme(axis.title.y=element_blank())+xlab("Human population density")+scale_fill_manual(values=palette)
j<-ggplot(toto, aes(x = Footprint, group = grouped_temp)) +
  geom_density(aes(fill = grouped_temp), alpha = 0.3, show.legend=FALSE) +theme_light()+
  theme(axis.title.y=element_blank())+xlab("Human footprint")+scale_fill_manual(values=palette)
k<-ggplot(toto, aes(x = Current.NDVI, group = grouped_temp)) +
  geom_density(aes(fill = grouped_temp), alpha = 0.3, show.legend=FALSE)  +theme_light()+
  theme(axis.title.y=element_blank())+xlab("NDVI")+scale_fill_manual(values=palette)
l<-ggplot(toto, aes(x = CurrentGrowingSeasonLength, group = grouped_temp)) +
  geom_density(aes(fill = grouped_temp), alpha = 0.3, show.legend=FALSE) +theme_light()+
  theme(axis.title.y=element_blank())+xlab("Growing season length")+scale_fill_manual(values=palette)
m<-ggplot(toto, aes(x = NDVI.trend, group = grouped_temp)) +
  geom_density(aes(fill = grouped_temp), alpha = 0.3, show.legend=FALSE) +theme_light()+
  theme(axis.title.y=element_blank())+xlab("NDVI trend")+scale_fill_manual(values=palette)
n<-ggplot(toto, aes(x = GrowingSeasonLength.trend, group = grouped_temp)) +
  geom_density(aes(fill = grouped_temp), alpha = 0.3, show.legend=FALSE) +theme_light()+
  theme(axis.title.y=element_blank())+xlab("Growing season length trend")+scale_fill_manual(values=palette)
o<-ggplot(toto, aes(x = Temperature.anomaly, group = grouped_temp)) +
  geom_density(aes(fill = grouped_temp), alpha = 0.3, show.legend=FALSE) +theme_light()+
  theme(axis.title.y=element_blank())+xlab("Temperature anomaly")+scale_fill_manual(values=palette)
p<-ggplot(toto, aes(x = permafrost, group = grouped_temp)) +
  geom_density(aes(fill = grouped_temp), alpha = 0.3, show.legend=FALSE) +theme_light()+
  theme(axis.title.y=element_blank())+xlab("Permafrost")+scale_fill_manual(values=palette)
q<-ggplot(toto, aes(x = soil_type., group = grouped_temp)) +
  geom_density(aes(fill = grouped_temp), alpha = 0.3, show.legend=FALSE) +theme_light()+
  theme(axis.title.y=element_blank())+xlab("Soil type")+scale_fill_manual(values=palette)

tiff('Figures/supplement_temporal.tif',height=10,width=9,units = 'in',res=150)


print(grid.arrange(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,
                   ncol=3))

dev.off()
dev.off()


## additional check arctic zones
table(alldata$Subzone)