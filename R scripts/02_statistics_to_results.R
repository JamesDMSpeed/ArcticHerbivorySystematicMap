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


# Approaches  -----------------------------------------------

# proportion of experimental field studies 
tito<-filter(alldata, study_design=="experimental") 
tito<-filter(tito, study_method=="field") 
dim(tito)[1]/dim(alldata)[1]

# proportion of observational field studies
# first combine obs & quasi-exp
alldata$study_design[alldata$study_design=="quasi-experimental"]<-"observational"
tito<-filter(alldata, study_design=="observational") 
tito<-filter(tito, study_method=="field") 
dim(tito)[1]/dim(alldata)[1]

#tito<-filter(alldata, study_design=="quasi-experimental") 
#tito<-filter(tito, study_method=="field") 
#dim(tito)[1]/dim(alldata)[1]

tito<-filter(alldata, study_method=="greenhouse") 
dim(tito)[1]/dim(alldata)[1]

tito<-filter(alldata, study_method=="remote sensing") 
dim(tito)[1]/dim(alldata)[1]

tito<-filter(alldata, study_method=="modelling") 
dim(tito)[1]/dim(alldata)[1]

## additional exposures
tito<-filter(alldata, study_method=="field") 
tito<-filter(tito, study_design=="experimental") 
tito<-filter(tito, additional_exposures!="none") 
tito<-filter(tito, additional_exposures!="not relevant") 
warming<-filter(tito, additional_exposures=="warming"); dim(warming)[1]
nut<-filter(tito, additional_exposures=="nutrient manipulation"); dim(nut)[1]
sev<-filter(tito, additional_exposures=="several")
other<-filter(tito, additional_exposures=="other")

sev$additional_treatments_comments
# additional 7 evidence points to warming
# additional 4 nutrient manipulation
18+7
19+4

other$additional_treatments_comments
# additional 2 nutrient removal

dim(warming)[1]+7
dim(nut)[1]+4+2

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

## temporal scale of sampling  -----------------------------------------------
tito<-filter(alldata, temporal_resolution=="once") 
dim(tito)[1]/dim(alldata)[1]

tito<-alldata[alldata$extent_of_temporal_scale>10,]
dim(tito)[1]/dim(alldata)[1]
str(tito)

## temporal scale of sampling  -----------------------------------------------


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

# plants recorded  -----------------------------------------------
pop<-filter(alldata, biological_organization_level_recorded=="population/species") 
ind<-filter(alldata, biological_organization_level_recorded=="individual") 
com<-filter(alldata, biological_organization_level_recorded=="community") 
gru<-filter(alldata, biological_organization_level_recorded=="groups of species") 


sev<-filter(alldata, biological_organization_level_recorded=="several") 
sum(na.omit(str_count(sev$biological_organization_recorded_comments, "population/species"))) #25
sum(na.omit(str_count(sev$biological_organization_recorded_comments, "individual"))) #9
sum(na.omit(str_count(sev$biological_organization_recorded_comments, "community"))) #5
sum(na.omit(str_count(sev$biological_organization_recorded_comments, "groups of species"))) #16


(dim(pop)[1]+25)/dim(alldata)[1] #49%
(dim(ind)[1]+9)/dim(alldata)[1] #29%
(dim(com)[1]+5)/dim(alldata)[1] #11%
(dim(gru)[1]+16)/dim(alldata)[1] #13%

# plants reported  -----------------------------------------------
table(alldata$biological_organization_level_reported)

pop<-filter(alldata, biological_organization_level_reported=="population/species") 
ind<-filter(alldata, biological_organization_level_reported=="individual") 
com<-filter(alldata, biological_organization_level_reported=="community") 
gru<-filter(alldata, biological_organization_level_reported=="groups of species") 


sev<-filter(alldata, biological_organization_level_reported=="several") 
sum(na.omit(str_count(sev$biological_organization_reported_comments, "population/species"))) #68
sum(na.omit(str_count(sev$biological_organization_reported_comments, "individual"))) #2
sum(na.omit(str_count(sev$biological_organization_reported_comments, "community"))) #50
sum(na.omit(str_count(sev$biological_organization_reported_comments, "groups of species"))) #101


(dim(pop)[1]+68)/dim(alldata)[1] #49%
(dim(ind)[1]+2)/dim(alldata)[1] #9%
(dim(com)[1]+50)/dim(alldata)[1] #25%
(dim(gru)[1]+101)/dim(alldata)[1] #29%

# herbivores  -----------------------------------------------
table(alldata$herbivore_type)
oth<-filter(alldata, herbivore_type=="other vertebrates") 
wat<-filter(alldata, herbivore_type=="waterfowl") 
sma<-filter(alldata, herbivore_type=="small rodents and pikas") 
def<-filter(alldata, herbivore_type=="defoliating invertebrates") 


sev<-filter(alldata, herbivore_type=="several") 
sum(na.omit(str_count(sev$herbivore_type_comments, "other vertebrates"))) #93
sum(na.omit(str_count(sev$herbivore_type_comments, "waterfowl"))) #22
sum(na.omit(str_count(sev$herbivore_type_comments, "small rodents and pikas"))) #84
sum(na.omit(str_count(sev$herbivore_type_comments, "defoliating invertebrates"))) #20


(dim(oth)[1]+93)/dim(alldata)[1] #60%
(dim(wat)[1]+22)/dim(alldata)[1] #19%
(dim(sma)[1]+84)/dim(alldata)[1] #23%
(dim(def)[1]+20)/dim(alldata)[1] #10%


# total vertebrate herbivore %; thhis many evidence points included vertebrate herbivory
((dim(oth)[1]+dim(wat)[1]+dim(sma)[1])/dim(alldata))[1]  # 74

# among the "several", how many have vertebates?
table(sev$herbivore_type_comments)
1+1+4+42+1+3+8+8+1+23+3+2+1+1

## vertebrate herbivore genera  ---------------------
herb<-alldata
levels(as.factor(herb$herbivore_identity))

# explore how to split "other vertebrates"; these were the only genera present, so we use genera 
herb$Rangifer <- ifelse(grepl("Rangifer" , herb$herbivore_identity) , 1 , 0);sum(herb$Rangifer)
herb$Lagopus <- ifelse(grepl("Lagopus" , herb$herbivore_identity) , 1 , 0) ; sum(herb$Lagopus)
herb$Lepus <- ifelse(grepl("Lepus" , herb$herbivore_identity) , 1 , 0); sum(herb$Lepus)
herb$Alces <- ifelse(grepl("Alces" , herb$herbivore_identity) , 1 , 0); sum(herb$Alces)
herb$Ovibos <- ifelse(grepl("Ovibos" , herb$herbivore_identity) , 1 , 0); sum(herb$Ovibos)
herb$Ovis <- ifelse(grepl("Ovis" , herb$herbivore_identity) , 1 , 0); sum(herb$Ovis)

sma<-filter(alldata, herbivore_type=="small rodents and pikas") 
sma_2<-filter(alldata, herbivore_type_comments=="small rodents and pikas") 
sma<-rbind.data.frame(sma, sma_2)
herb<-sma
herb$Microtus <- ifelse(grepl("Microtus" , herb$herbivore_identity) , 1 , 0); sum(herb$Microtus)
herb$Myodes <- ifelse(grepl("Myodes" , herb$herbivore_identity) , 1 , 0) 
herb$Myodes <- ifelse(grepl("Clethrionomus" , herb$herbivore_identity) , 1 , herb$Myodes) 
herb$Myodes <- ifelse(grepl("Clethrionomys" , herb$herbivore_identity) , 1 , herb$Myodes) ; sum(herb$Myodes)
herb$Lemmus <- ifelse(grepl("Lemmus" , herb$herbivore_identity) , 1 , 0); sum(herb$Lemmus)

herb$Dicrostonyx <- ifelse(grepl("Dicrostonyx" , herb$herbivore_identity) , 1 , 0); sum(herb$Dicrostonyx)
herb$Spermophilus <- ifelse(grepl("Spermophilus" , herb$herbivore_identity) , 1 , 0); sum(herb$Spermophilus)
herb$Marmota <- ifelse(grepl("Marmota" , herb$herbivore_identity) , 1 , 0); sum(herb$Marmota)
herb$Urocitellus <- ifelse(grepl("Urocitellus" , herb$herbivore_identity) , 1 , 0); sum(herb$Urocitellus)


herb$Chen <- ifelse(grepl("Chen" , herb$herbivore_identity) , 1 , 0); sum(herb$Chen)
herb$Branta <- ifelse(grepl("Branta" , herb$herbivore_identity) , 1 , 0); sum(herb$Branta)
herb$Anser <- ifelse(grepl("Anser" , herb$herbivore_identity) , 1 , 0); sum(herb$Anser)

sum(herb$Myodes)/dim(alldata)[1]
sum(herb$Lemmus)/dim(alldata)[1]
sum(herb$Chen)/dim(alldata)[1]


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




