#Systematic map of herbivory in Arctic tundra

#Script to import coded data from excel spreadsheet.
#Data taken from Eeva's UiT Box account and read in using a direct download link
#Data is spread between multiple sheets with 1000 evidence points per sheet


library(readxl)#Read in xls from box
library(mapdata)#World map
library(maptools)#Playing with maps
library(sp)#Spatial data
library(rgdal)#Spatial data
library(raster)#Climate data
library(rasterVis)#Visualisations
library(ggplot2)
library(gridExtra)
library(rgeos)
library(RColorBrewer)#Colours 
library(tidyverse) #data wrangling
library(networkD3) # for sankey diagrams
library(harrypotter)# color palettes



# Data Import and wrangling -----------------------------------------------

#Download coded data from box
link2coded_data <- paste(tempfile(),".xlsx",sep = "")
download.file("https://uitno.box.com/shared/static/4s5aet6g7qnpk4mpsa64a1v75t7ksbla.xlsx", link2coded_data, mode = "wb")

#Read in sheets with coded data
codeddata1 <- read_excel(link2coded_data,sheet = 'CODING_template_1_1000')
codeddata2 <- read_excel(link2coded_data,sheet = 'CODING_template_1001_2000')
codeddata3 <- read_excel(link2coded_data,sheet = 'CODING_template_2001_3000')
codeddata4 <- read_excel(link2coded_data,sheet = 'CODING_template_3001_')

#Check
View(codeddata1)
View(codeddata2)
View(codeddata3)
View(codeddata4)

#Transpose coded data so that each evidence point is a row (rather than column as coded in excel template)
t1<-data.frame(t(codeddata1[,c(7:ncol(codeddata1))]),check.names = F,stringsAsFactors = F)
names(t1)<-codeddata1$`Coding variable`
t2<-data.frame(t(codeddata2[,c(7:ncol(codeddata2))]),check.names = F,stringsAsFactors = F)
names(t2)<-codeddata2$`Coding variable`
t3<-data.frame(t(codeddata3[,c(7:ncol(codeddata3))]),check.names=F,stringsAsFactors = F)
names(t3)<-codeddata3$`Coding variable`
t4<-data.frame(t(codeddata4[,c(8:ncol(codeddata4))]),check.names = F,stringsAsFactors = F)#NB extra meta column in this sheet
names(t4)<-codeddata4$`Coding variable`


#Rbind these together into a single dataset
alldata<-do.call(rbind,list(t1,t2,t3,t4))
#Remove hidden line breaks in character
alldata<-apply(alldata,2,function(x)(gsub("\r\r\n", "", x)))
#Convert to a dataframe but supress changing characters to vectors
alldata<-data.frame(alldata,stringsAsFactors = F)
#Convert columns to guessed format
alldata<-type.convert(alldata)
#Checking
str(alldata)
head(alldata)
View(alldata)#Looks good!


#Number of studies
length(levels(as.factor(alldata$title)))  #328
#Number of evidence points
length(levels(as.factor(alldata$evidence_point_ID)))  #705

#Fix issue with different levels for extent of spatial scale
levels(as.factor(alldata$extent_of_spatial_scale))
alldata$extent_of_spatial_scale<-as.factor(alldata$extent_of_spatial_scale)
levels(alldata$extent_of_spatial_scale)<-
  c("1x1 km or less",                                          
    "from 100x100 km to 1000x1000 km",                         
    "from 100x100 km to 1000x1000 km",
    "from 10x10 km to 100x100 km"     ,                        
    "from 10x10 km to 100x100 km"      ,
    "from 1x1 km to 10x10 km"           ,                      
    "from 1x1 km to 10x10 km"            ,
    "larger than 1000x1000 km"            ,                    
    "not relevant"                         ,                   
    "not reported"  )
levels(alldata$extent_of_spatial_scale)
alldata$extent_of_spatial_scale<-factor(alldata$extent_of_spatial_scale,ordered = T)

# Mapping -----------------------------------------------------------------


# Mapping in space --------------------------------------------------------

#Evidence points

#By country
eps_country<-tapply(alldata$country,alldata$country,length)
pdf('Figures/Countries.pdf')
ggplot(data=alldata,aes(x=country))+geom_bar()+  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Countries")+ theme(axis.title.x = element_blank()) 
dev.off()

#Across biome
#World map
polarproj<-CRS('+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ')
boundaries <- maps::map('worldHires', fill=TRUE,plot=FALSE,ylim=c(40,90))
IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
bPols <- map2SpatialPolygons(boundaries, IDs=IDs,
                             proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
bPolslaea<-spTransform(bPols,polarproj)
plot(bPolslaea,ylim=c(55,90))

#CAVM arctic subzones
subzones<-readOGR('Data/cp_biozone_la_shp','cavm_all polygon')
subzones
#spplot(subzones,zcol=subzones$ZONE)

#Spatial evidence points
alldatasp1<-alldata
#Change to spatial points df
alldata_sp<-SpatialPointsDataFrame(coords=cbind(alldatasp1$coordinates_E,alldatasp1$coordinates_N),data=alldatasp1,proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
alldata_splaea<-spTransform(alldata_sp,polarproj)
#obs<-alldata_splaea[alldata_splaea$evidence_point_ID%in%alldata_sp$evidence_point_ID==F,]
#obsRa<-alldata_splaea[alldata_sp$evidence_point_ID%in%alldata_splaea$evidence_point_ID==F,]
#Get CAFF boundaries to add
arczones<-readOGR('Data/ABA-Boundaries','Arctic_Zones')
arczones_laea<-spTransform(arczones,polarproj)
subarcbound<-arczones_laea[arczones_laea@data$Zone=='Sub arctic',]

#Figure
plot(bPolslaea,ylim=c(55,90),main='Spatial distribution of evidence points')
points(alldata_splaea,pch=16,col='darkgreen',cex=0.5)
plot(subarcbound,border='red',lwd=2,lty=2,add=T)#Some coordinates outside the CAFF limit
dev.off()


#Remove redundant studies
alldata_splaea_removeredundant<-alldata_splaea[alldata_splaea$redundancy!='redundant',]


#Remove evidence points outside of arctic
#Buffer the Arctic polygons by 10000m to get sites with coordinate inaccuracies offshore
arczones_buffer<-gBuffer(arczones_laea,100000,byid=T,id=c('a','b','c'))
plot(arczones_laea)
plot(arczones_buffer,border=2,add=T)
points(alldata_splaea,col=3,pch=16,cex=0.4)
#Remove evidence points outside of buffered polygon
alldata_splaea_removeoutsidearctic<-alldata_splaea_removeredundant[arczones_buffer,]
dim(alldata_splaea)
dim(alldata_splaea_removeoutsidearctic)

#List removed studies
removedstudies<-alldata_splaea[alldata_splaea$evidence_point_ID%in%alldata_splaea_removeoutsidearctic$evidence_point_ID==F,]
write.csv(removedstudies@data,'Data/StudiesOutsideCAFFBound.csv')

plot(bPolslaea,ylim=c(55,90),main='Spatial distribution of evidence points')
points(alldata_splaea,pch=16,col='red',cex=0.5)
points(alldata_splaea_removeoutsidearctic,pch=16,col='darkgreen',cex=0.5)
plot(subarcbound,border='blue',lwd=2,lty=2,add=T)#Seems better - may have included some non arctic sites in N. Fennoscandia...



#Save filtered data
#Write data
write.table(alldata_splaea_removeoutsidearctic,'Data/AllCodedData.txt',row.names = F,sep=';',quote=F,dec='.')

#Write data to be imported to EviAtlas
#Need to solve more link break and quote issues
alldataW<-as.data.frame(sapply(alldata_splaea_removeoutsidearctic@data,function(x)(gsub("\r\n", " ", x))))
alldataW1<-as.data.frame(sapply(alldataW,function(x)(gsub("\"","",x))))
write.table(alldataW1,'Data/AllCodedDataW.txt',row.names = F,sep=';',dec='.')
#Open this in excel - set coordinates to be imported as text. Replace ; with ..  Save as UTF AllCodedDataEncoded.csv.


# Mapping in time ---------------------------------------------------------


#Evidence points - year of publication
pub<-ggplot(alldata_splaea_removeoutsidearctic@data,aes(x=as.numeric(year)))+geom_histogram()+ggtitle("Publication year")+xlab('Publication year')
#Evidence points - year of study start
alldata_splaea_removeoutsidearctic$year_start[alldata_splaea_removeoutsidearctic$year_start=="not available"]<-NA
alldata_splaea_removeoutsidearctic$year_start[alldata_splaea_removeoutsidearctic$year_start=="not relevant"]<-NA
alldata_splaea_removeoutsidearctic$year_start[alldata_splaea_removeoutsidearctic$year_start=="not reported" ]<-NA
alldata_splaea_removeoutsidearctic$year_start<-droplevels(alldata_splaea_removeoutsidearctic$year_start)

startyr<-ggplot(alldata_splaea_removeoutsidearctic@data,aes(x=as.numeric(as.character(year_start))))+geom_histogram()+ggtitle("Study start year")+xlab('Study start year')

pdf('Figures/Time.pdf')
tiff('Figures/Time.tif',width=6,height=4,units='in',res=150)
grid.arrange(pub,startyr,ncol=2)
dev.off()

# Figure 2 ---------------------------------------------------------


#Year of publication - panel A
pub<-ggplot(alldata_splaea_removeoutsidearctic@data,aes(x=as.numeric(year)))+geom_histogram()+ggtitle("A) Publication year")+
      theme(axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x  = element_text(size=16),
      axis.text.y =element_text(size=16)) + theme(plot.title = element_text(size=16))+
      theme(plot.margin=unit(c(0.2,0.4,0.4,0.1),"cm"))

pub

#Year of study start - panel B
alldata_splaea_removeoutsidearctic$year_start[alldata_splaea_removeoutsidearctic$year_start=="not available"]<-NA
alldata_splaea_removeoutsidearctic$year_start[alldata_splaea_removeoutsidearctic$year_start=="not relevant"]<-NA
alldata_splaea_removeoutsidearctic$year_start[alldata_splaea_removeoutsidearctic$year_start=="not reported" ]<-NA
alldata_splaea_removeoutsidearctic$year_start<-droplevels(alldata_splaea_removeoutsidearctic$year_start)

startyr<-ggplot(alldata_splaea_removeoutsidearctic@data,aes(x=as.numeric(as.character(year_start))))+geom_histogram()+ggtitle("B) First year")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x  = element_text(size=16),
        axis.text.y =element_text(size=16)) + theme(plot.title = element_text(size=16))+
  theme(plot.margin=unit(c(0.2,0.2,0.4,0.4),"cm"))
startyr

#Extent of temporal scale - panel C
# the study with more than 1000 years temporal scale is a modeling study that simulates current (and future) climate for 1500 years
# the study with length = zero is now corrected in the raw data. it was a study with start year = end year, and should therefore have had one as study length
levels(alldata_splaea_removeoutsidearctic$extent_of_temporal_scale)[levels(alldata_splaea_removeoutsidearctic$extent_of_temporal_scale)=="0"] <- "1"

tr1<-ggplot(alldata_splaea_removeoutsidearctic@data,aes(x=as.numeric(as.character(extent_of_temporal_scale))))+geom_histogram()+scale_x_continuous(trans='log10')+
  ggtitle("C) Extent of temporal scale") + xlab("Temporal scale (years)")  +ylab("")+ theme(axis.title.x = element_text(size=16),
                 axis.text.x  = element_text(size=16),
                 axis.text.y =element_text(size=16)) + theme(plot.title = element_text(size=16))+
  theme(plot.margin=unit(c(0.4,0.4,0.2,0.1),"cm"))

tr1

#Temporal resolution  - panel D
# the study with temporal_resolution  = "twice" is now corrected in the raw data. it was a mistake, we have no level "twice"
# the edit below is therefore redundant
levels(alldata_splaea_removeoutsidearctic$temporal_resolution)[levels(alldata_splaea_removeoutsidearctic$temporal_resolution)=="twice"] <- "twice, interval longer than one year"

# rename factor levels shorter to read
levels(alldata_splaea_removeoutsidearctic$temporal_resolution)[levels(alldata_splaea_removeoutsidearctic$temporal_resolution)=="twice, interval one year or shorter"] <- "twice <= 1 yr"
levels(alldata_splaea_removeoutsidearctic$temporal_resolution)[levels(alldata_splaea_removeoutsidearctic$temporal_resolution)=="twice, interval longer than one year"] <- "twice > 1 yr"
levels(alldata_splaea_removeoutsidearctic$temporal_resolution)[levels(alldata_splaea_removeoutsidearctic$temporal_resolution)=="regular with intervals shorter than a year"] <- "regular < 1 yr"
levels(alldata_splaea_removeoutsidearctic$temporal_resolution)[levels(alldata_splaea_removeoutsidearctic$temporal_resolution)=="regular with intervals longer than a year"] <- "regular > 1 yr"
levels(alldata_splaea_removeoutsidearctic$temporal_resolution)[levels(alldata_splaea_removeoutsidearctic$temporal_resolution)=="irregular with intervals shorter than a year"] <- "irregular < 1 yr"
levels(alldata_splaea_removeoutsidearctic$temporal_resolution)[levels(alldata_splaea_removeoutsidearctic$temporal_resolution)=="irregular with intervals longer than a year"] <- "irregular > 1 yr"

# relevel factor
alldata_splaea_removeoutsidearctic$temporal_resolution<-fct_relevel(alldata_splaea_removeoutsidearctic$temporal_resolution, "once", 
            "twice <= 1 yr", "regular < 1 yr", "irregular < 1 yr",
            "twice > 1 yr", "regular > 1 yr", "irregular > 1 yr",
             "annual","not reported")

tr2<-ggplot(alldata_splaea_removeoutsidearctic@data,aes(x=temporal_resolution))+geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ ggtitle("D) Temporal resolution") +
     theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
                 axis.text.x  = element_text(size=16),
                 axis.text.y =element_text(size=16)) + theme(plot.title = element_text(size=16))+
  theme(plot.margin=unit(c(0.4,0.2,0.2,0.4),"cm"))

tr2

pdf('Figures/Figure_2.pdf',width=8,height=8)
tiff('Figures/Figure_2.tif',height=8,width=8,units = 'in',res=150)
grid.arrange(pub, startyr, tr1,tr2,ncol=2)
dev.off()


# Figure 3A ---------------------------------------------------------

# data for herbivores
herb<-cbind.data.frame(alldata_splaea_removeoutsidearctic$evidence_point_ID,
                   alldata_splaea_removeoutsidearctic$herbivore_type,
                   alldata_splaea_removeoutsidearctic$herbivore_type_comments,
                   alldata_splaea_removeoutsidearctic$herbivore_identity)
names(herb)<-c("evidence_point_ID","herbivore_type","herbivore_type_comments", "herbivore_identity")

# group very small invertebrate groups together
levels(herb$herbivore_type)[levels(herb$herbivore_type)=="galling invertebrates"] <- "other invertebrates"
levels(herb$herbivore_type)[levels(herb$herbivore_type)=="invertebrates feeding on reproductive structures"] <- "other invertebrates"
levels(herb$herbivore_type)[levels(herb$herbivore_type)=="phloem feeders"] <- "other invertebrates"
levels(herb$herbivore_type)[levels(herb$herbivore_type)=="root feeding invertebrates"] <- "other invertebrates"

# create new variables per herbivore by combining data across columns
herb$other_invertebrates <- ifelse(grepl("other invertebrates" , herb$herbivore_type) , 1 , 0)
herb$other_invertebrates <- ifelse(grepl("galling invertebrates" , herb$herbivore_type_comments) , 1 , herb$other_invertebrates)
herb$other_invertebrates <- ifelse(grepl("invertebrates feeding on reproductive structures" , herb$herbivore_type_comments) , 1 , herb$other_invertebrates)
herb$other_invertebrates <- ifelse(grepl("phloem feeders" , herb$herbivore_type_comments) , 1 , herb$other_invertebrates)
herb$other_invertebrates <- ifelse(grepl("root feeding invertebrates" , herb$herbivore_type_comments) , 1 , herb$other_invertebrates)
#
herb$waterfowl <- ifelse(grepl("waterfowl" , herb$herbivore_type_comments) , 1 , 0)
herb$waterfowl <- ifelse(grepl("waterfowl" , herb$herbivore_type) , 1 , herb$waterfowl)
#
herb$small_rodents_and_pikas <- ifelse(grepl("small rodents and pikas" , herb$herbivore_type_comments) , 1 , 0)
herb$small_rodents_and_pikas <- ifelse(grepl("small rodents and pikas" , herb$herbivore_type) , 1 , herb$small_rodents_and_pikas)
#
herb$other_vertebrates <- ifelse(grepl("other vertebrates" , herb$herbivore_type_comments) , 1 , 0)
herb$other_vertebrates <- ifelse(grepl("other vertebrates" , herb$herbivore_type) , 1 , herb$other_vertebrates)
#
herb$defoliating_invertebrates <- ifelse(grepl("defoliating invertebrates" , herb$herbivore_type_comments) , 1 , 0)
herb$defoliating_invertebrates <- ifelse(grepl("defoliating invertebrates" , herb$herbivore_type) , 1 , herb$defoliating_invertebrates)
#

# explore how to split "other vertebrates"; these were the only genera present, so we use genera 
herb$Rangifer <- ifelse(grepl("Rangifer" , herb$herbivore_identity) , 1 , 0);sum(herb$Rangifer)
herb$Lagopus <- ifelse(grepl("Lagopus" , herb$herbivore_identity) , 1 , 0) ; sum(herb$Lagopus)
herb$Lepus <- ifelse(grepl("Lepus" , herb$herbivore_identity) , 1 , 0); sum(herb$Lepus)
herb$Alces <- ifelse(grepl("Alces" , herb$herbivore_identity) , 1 , 0); sum(herb$Alces)
herb$Ovibos <- ifelse(grepl("Ovibos" , herb$herbivore_identity) , 1 , 0); sum(herb$Ovibos)
herb$Ovis <- ifelse(grepl("Ovis" , herb$herbivore_identity) , 1 , 0); sum(herb$Ovis)
# keep category unknown
herb$unknown <- ifelse(grepl("unknown" , herb$herbivore_type) , 1 , 0)

head(herb)
herb_data<-herb %>% select(evidence_point_ID, other_invertebrates, waterfowl, small_rodents_and_pikas, defoliating_invertebrates:unknown); head(herb_data)

herbL<-
  herb_data %>%
  pivot_longer(!c("evidence_point_ID"), names_to = "herbivores", values_to = "count_herbivores")
head(herbL)

# remove zero rows from herbivore data to make this work with plants for snakey diagram
herbLnew<-filter(herbL, count_herbivores==1)
head(herbLnew)

# data for plants
plants<-read.delim("Data/PFTs_Systematic_Herbivory_Map_03022021.txt")
head(plants)  


# combine plant and herbivore data
data_sankey<-inner_join(herbLnew, plants, by="evidence_point_ID"); head(data_sankey)
data_sankey<-data_sankey %>% select(-c(article_checked, original_scored_data, comments, count_herbivores)); head(data_sankey)

#change yes and no to zero and one
data_sankey <-
  mutate(data_sankey, 
         evergreen_dwarf_shrubs = if_else(evergreen_dwarf_shrubs == "yes", 1L, 0L),
         decidious_dwarf_shrubs = if_else(decidious_dwarf_shrubs == "yes", 1L , 0L),
         decidious_tall_shrubs = if_else(decidious_tall_shrubs == "yes", 1L , 0L),
         evergreen_tall_shrubs = if_else(evergreen_tall_shrubs == "yes", 1L , 0L),
         decidious_trees = if_else(decidious_trees == "yes", 1L , 0L),
         evergreen_trees = if_else(evergreen_trees == "yes", 1L , 0L),
         graminoids = if_else(graminoids == "yes", 1L , 0L),
         forbs = if_else(forbs == "yes", 1L , 0L),
         ferns_and_allies = if_else(ferns_and_allies == "yes", 1L , 0L),
         bryophytes = if_else(bryophytes == "yes", 1L , 0L),
         vascular_plant_community = if_else(vascular_plant_community == "yes", 1L , 0L),
         lichens = if_else(lichens == "yes", 1L , 0L)
  )


# pivot to even longer data  
data_sankeyL<-
  data_sankey %>%
  pivot_longer(!c("evidence_point_ID", "herbivores"), names_to = "plants", values_to = "count_plants")
head(data_sankeyL)

# summerize so that count is sum across the whole dataset, get rid of evid
data_sankeyL<-data_sankeyL %>% select(c(!evidence_point_ID))
links<-aggregate(data_sankeyL$count_plants,list(herbivore = data_sankeyL$herbivores, plant = data_sankeyL$plants),sum)
names(links)<-c("source", "target", "value")
head(links)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% 
    unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# prepare color scale: I give one specific color for each node.
my_color <- 'd3.scaleOrdinal() .domain(["group_A", "group_B","group_C", "group_D", "group_E", "group_F", "group_G", "group_H"]) .range(["blue", "blue" , "blue", "red", "red", "yellow", "purple", "purple"])'

# Make the Network. I call my colour scale with the colourScale argument
p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name", 
                   fontSize = 15, nodeWidth=40, nodePadding = 10)
                   #fontFamily = "serif")
                   #colourScale=my_color)
p
# does not save automatically without extra packages

# Figures 3B and 3C ---------------------------------------------------------
# use same grouping for herbivores as done above 

data_fig_3<-as.data.frame(alldata_splaea_removeoutsidearctic) %>% select(evidence_point_ID, study_design, experimental_design, study_method, exposure_quantification, biological_organization_level_reported)
head(data_fig_3)  

# merge and edit data
data_fig_3<-merge(herbLnew, data_fig_3)
levels(data_fig_3$study_design)[levels(data_fig_3$study_design)=="quasi-experimental"] <- "observational"
data_fig_3$study_type<-paste(data_fig_3$study_design, data_fig_3$study_method, sep=", ")
data_fig_3$study_type<-as.factor(data_fig_3$study_type)
data_fig_3$herbivores<-as.factor(data_fig_3$herbivores)
levels(data_fig_3$study_type)[levels(data_fig_3$study_type)=="modelling, modelling"] <- "modelling"
levels(data_fig_3$herbivores)[levels(data_fig_3$herbivores)=="defoliating_invertebrates"] <- "defoliating inv."
levels(data_fig_3$herbivores)[levels(data_fig_3$herbivores)=="small_rodents_and_pikas"] <- "small rodents"
levels(data_fig_3$herbivores)[levels(data_fig_3$herbivores)=="other_invertebrates"] <- "other inv."


levels(data_fig_3$herbivores)
table(data_fig_3$herbivores)

# re-order variables  
data_fig_3$herbivores<-fct_relevel(data_fig_3$herbivores, 
                                   "Rangifer", "small rodents", "waterfowl", "defoliating inv.","Lagopus", "Ovibos", 
                                   "Lepus", "Ovis", "Alces","other inv.","unknown")
data_fig_3$study_type<-fct_relevel(data_fig_3$study_type, 
                                   "experimental, field", "experimental, remote sensing", "experimental, greenhouse" ,
                                   "modelling",                  
                                   "observational, field","observational, remote sensing","observational, other","observational, several")
data_fig_3$biological_organization_level_reported<-fct_relevel(data_fig_3$biological_organization_level_reported, "individual", 
                                                               "population/species", "groups of species", "community",
                                                               "several", "other")

# corrected one factor level in raw data, this line will be redundant
levels(data_fig_3$biological_organization_level_reported)[levels(data_fig_3$biological_organization_level_reported)=="population/species, community"] <- "several"

# color palette for panel B
hp_color_manual <- c( "#006699FF", "#177CAFFF" ,"#369BCEFF", "#72B6D9FF",  "#B35900FF",  "#D99E66FF")

ct2<-ggplot(data_fig_3, aes(fill = biological_organization_level_reported, x = herbivores)) + geom_bar(aes(fill=biological_organization_level_reported))+
  scale_fill_manual("Plant level", values=hp_color_manual)+
  ggtitle("B) Herbivore type and plant level")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=16),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y =element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text( size = 14))
ct2

# color palette for panel C
hp_color_manual <- c( "#B35900FF", "#D3771CFF", "#D99E66FF" , "#B3B8B3FF" ,"#006699FF", "#177CAFFF" ,"#369BCEFF", "#72B6D9FF")

ct1<-ggplot(data_fig_3, aes(fill = study_type, x = herbivores)) + geom_bar(aes(fill=study_type))+
  scale_fill_manual("Study type", values=hp_color_manual)+
  ggtitle("C) Herbivore type and study type")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=16),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y =element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text( size = 14)
  )
#  theme(plot.margin=unit(c(0.4,0.2,0.2,0.4),"cm"))
ct1



pdf('Figures/Figure3_BC.pdf',height=6,width=14)
tiff('Figures/Figure3_BC.tif',height=6,width=14,units = 'in',res=150)
grid.arrange(ct2,ct1,ncol=2)
dev.off()


# Mapping study types -----------------------------------------------------

#Study design

studdes<-ggplot(alldata_splaea_removeoutsidearctic@data,aes(x=study_design))+geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Study design")+ theme(axis.title.x = element_blank()) 
exdes<-ggplot(alldata_splaea_removeoutsidearctic@data,aes(x=experimental_design))+geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Experimental design")+ theme(axis.title.x = element_blank()) 
studmeth<-ggplot(alldata_splaea_removeoutsidearctic@data,aes(x=study_method))+geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Study method")+ theme(axis.title.x = element_blank()) 
expquant<-ggplot(alldata_splaea_removeoutsidearctic@data,aes(x=exposure_quantification))+geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Exposure quantification")+ theme(axis.title.x = element_blank()) 

pdf('Figures/StudyDesign.pdf',width=6,height=10)
grid.arrange(studdes,studmeth,exdes,expquant,ncol=2)
dev.off()

#Herbivore type
pdf('Figures/HerbivoreType.pdf')
ggplot(alldata_splaea_removeoutsidearctic@data,aes(x=herbivore_type))+geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Herbivore type")+ theme(axis.title.x = element_blank()) 
dev.off()


# Mapping temporal and spatial resolutions & extents --------------------------------

tr1<-ggplot(alldata_splaea_removeoutsidearctic@data,aes(x=as.numeric(extent_of_temporal_scale)))+geom_histogram()+scale_x_log10()+
  ggtitle("Extent of temporal scale") +
  xlab("Temporal scale (years)")

tr2<-ggplot(alldata_splaea_removeoutsidearctic@data,aes(x=temporal_resolution))+geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Temporal resolution")+ theme(axis.title.x = element_blank()) 

pdf('Figures/TempRes.pdf',width=8,height=6)
grid.arrange(tr1,tr2,ncol=2)
dev.off()

#Contingency tables

ct1<-ggplot(alldata_splaea_removeoutsidearctic@data, aes(fill = study_design, x = herbivore_type)) + geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Herbivore type and study design")+ theme(axis.title.x = element_blank()) 


ct2<-ggplot(alldata_splaea_removeoutsidearctic@data, aes(fill = biological_organization_level_reported, x = herbivore_type)) + geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Herbivore type and plant level")+ theme(axis.title.x = element_blank()) 

pdf('Figures/ContingencyFigs.pdf',height=6,width=10)
tiff('Figures/ContingencyFigs.tif',height=6,width=10,units = 'in',res=150)
grid.arrange(ct1,ct2,ncol=2)
dev.off()


#Simplified herbivores
alldata_splaea_removeoutsidearctic$SimpleHerbivore<-alldata_splaea_removeoutsidearctic$herbivore_type
levels(alldata_splaea_removeoutsidearctic$SimpleHerbivore)<-c(
  rep('Invertebrate',times=3),
  'Vertebrate','Invertebrate','Invertebrate','Multiple','Vertebrate','Unknown','Vertebrate')

ct1a<-ggplot(alldata_splaea_removeoutsidearctic@data, aes(fill = study_design, x = SimpleHerbivore)) + geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position=c(0.8,0.8))+ theme(legend.text=element_text(size=10))+labs(fill='Study design')+
  ggtitle("Herbivore type and study design")+ theme(axis.title.x = element_blank()) 

ct2a<-ggplot(alldata_splaea_removeoutsidearctic@data, aes(fill = biological_organization_level_reported, x = SimpleHerbivore)) + geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position=c(0.8,0.8))+ theme(legend.text=element_text(size=10))+labs(fill='Plant level')+
  ggtitle("Herbivore type and plant level")+ theme(axis.title.x = element_blank()) 

tiff('Figures/AltGroups.tif',width=6,height=10,units='in',res=150)
grid.arrange(ct1a,ct2a,ncol=1)
dev.off()
#NB: Identity of biology organization unit too many levels

# Temperature and precipitation axes --------------------------------------

#Downloading worldclim bioclimate data at 2.5deg resolution
bioclimdat<-getData('worldclim',var='bio',res=2.5)

#Using spatial data to extract
bioclim_ex<-extract(bioclimdat,alldata_sp)
#Bind to spatial data
alldata_sp<-cbind(alldata_sp,bioclim_ex)

# MAT and MAP are bio1 and bio12 respectively

climatespace<-ggplot(data=alldata_sp@data,aes(x=bio12, y=bio1/10,colour=coordinates_N))+geom_point()+
  ggtitle("Climatic space") + scale_y_reverse()+
  xlab("MAP (mm)")+ ylab(expression('MAT ' (degree~C)))
pdf('Figures/ClimateSpace.pdf')
climatespace
dev.off()

#Adding points for total Arctic climate space
arcclim<-extract(bioclimdat,arczones)
arcclim_all<-data.frame(do.call(rbind,arcclim))
arcclim_all$zone<-c(rep(1,times=nrow(arcclim[[1]])),rep(2,times=nrow(arcclim[[2]])),rep(3,times=nrow(arcclim[[3]])))

#Make this a png due to very many points = large file!
climatespace2<-ggplot(data=arcclim_all,mapping=aes(x=bio12,y=bio1/10))+geom_point(alpha=1/100,size=0.001)+
  ggtitle("Climatic space") + scale_y_reverse()+
  xlab("MAP (mm)")+ ylab(expression('MAT ' (degree~C)))+
  #geom_point(data=arcclim_all,mapping=aes(x=bio12,y=bio1/10,colour=grey(0.5),size=0.1))+
  geom_point(data=alldata_sp@data,aes(x=bio12, y=bio1/10,colour=coordinates_N))
png('Figures/ClimateSpace_Available.png')
climatespace2
dev.off()

#Elevation
#DTM from Guille
#dtmurl<-'https://uitno.box.com/shared/static/gw986nzxvif3cx6hhsqryjk1xzsdie5c.rrd'
#download.file(dtmurl,'Data/GIS layeres/DTM.rrd')
#dtm<-raster('Data/GIS layeres/DTM.rrd')#Canæ't open

#Use raster::getData instead
charcount <- c('NO', 'SE','FI','CA','IS','GL','SJ') 
allac2 <- do.call("merge", lapply(charcount, function(x)  raster::getData('alt', country=x)))
#Issues with USA and Russia (crossing 180deg?)
rusalt<-getData('alt',country='RU')
rusalt2<-merge(rusalt[[1]],rusalt[[2]])
usalt<-getData('alt',country='USA')
usalt2<-do.call("merge",list(usalt[[1]],usalt[[2]],usalt[[3]],usalt[[4]]))
plot(usalt2)
arcelev<-merge(allac2,rusalt2,usalt2)
arcelev<-crop(arcelev,alldata_sp)
plot(arcelev)

arcelev_laea<-projectRaster(arcelev,vertherb_sr)
arcelev_laea
arcelev_laea<-mask(arcelev_laea,vertherb_sr)
plot(arcelev_laea)

#Distance from coast
distancefromcoast<-raster('Data/GIS_layers/DistancetoCoast.tif')
distancefromcoast
plot(distancefromcoast)


#Geographic stack
geographicstack<-stack(crop(arcelev_laea,distancefromcoast),crop(distancefromcoast,arcelev_laea))
names(geographicstack)[1]<-'Elevation'
geographicstack$DistancetoCoast<-mask(geographicstack$DistancetoCoast,geographicstack$Elevation)
plot(geographicstack)

#Distance to treeline
treelineurl<-'https://uitno.box.com/shared/static/09fxunzc49qg5oqtg1uannk4exmr8eh4.zip'
download.file(treelineurl,'Data/GIS_layers/Treeline.zip')
unzip('Data/GIS_layers/Treeline.zip',exdir = 'Data/GIS_layers/Treeline')
treelineshp<-readOGR('Data/GIS_layers/Treeline','cp_treeline_la')

#Calculate distance from treeline (change lines to points for ease)
arczonesT<-rasterize(spTransform(arczones,distancefromcoast@crs),distancefromcoast,field='ZONE_')
plot(arczonesT)
treelinepts<-as(treelineshp,'SpatialPoints')
treelinedist<-mask(distanceFromPoints(arczonesT,treelinepts),arczonesT)
plot(treelinedist)

#Set subarctic (S of treeline) to negative 
northoftreeline<-treelinedist
northoftreeline[arczonesT==0]<-0-treelinedist[arczonesT==0] 
plot(northoftreeline)
diverge0 <- function(p, ramp) {
  require(RColorBrewer)
  require(rasterVis)
  if(length(ramp)==1 && is.character(ramp) && ramp %in% 
     row.names(brewer.pal.info)) {
    ramp <- suppressWarnings(colorRampPalette(rev(brewer.pal(11, ramp))))
  } else if(length(ramp) > 1 && is.character(ramp) && all(ramp %in% colors())) {
    ramp <- colorRampPalette(ramp)
  } else if(!is.function(ramp)) 
    stop('ramp should be either the name of a RColorBrewer palette, ', 
         'a vector of colours to be interpolated, or a colorRampPalette.')
  rng <- range(p$legend[[1]]$args$key$at)
  s <- seq(-max(abs(rng)), max(abs(rng)), len=1001)
  i <- findInterval(rng[which.min(abs(rng))], s)
  zlim <- switch(which.min(abs(rng)), `1`=i:(1000+1), `2`=1:(i+1))
  p$legend[[1]]$args$key$at <- s[zlim]
  p$par.settings$regions$col <- ramp(1000)[zlim[-length(zlim)]]
  p
}
treelinelp<-levelplot(northoftreeline,margin=F,scales=list(draw=F))+
  layer(sp.lines(treelineshp))
diverge0(treelinelp,'RdBu')

#Herbivore diversity layers
vertherb_sr<-raster('Data/GIS_layers/ArcticHerbivore_Species.richness.tif')
vertherb_pd<-raster('Data/GIS_layers/ArcticHerbivore_Phylogenetic.diversity.tif')
vertherb_fd<-raster('Data/GIS_layers/ArcticHerbivore_Functional.diversity.tif')
vertherb_div<-stack(vertherb_sr,vertherb_pd,vertherb_fd)

#Permafrost
# permafrosturl<-'https://uitno.box.com/shared/static/mftidvyo8z2tkyqq1aivhbbg6y2339hz.zip'
# download.file(permafrosturl,'Data/GIS_layers/Permafrost.zip')
# unzip('Data/GIS_layers/Permafrost.zip',exdir='Data/GIS_layers/Permafrost')
# 
# permafrost<-readOGR('Data/GIS_layers/Permafrost','permaice')
# permafrost
# plot(permafrost)
# 
# permrast<-rasterize(permafrost,arczonesT,field='EXTENT',method='ngb')
# plot(permrast)
# 

#Instead using 12.5km grid dataset
pm<-raster('Data/GIS_layers/nhipa.byte')
crs(pm)<-polarproj
permafrostcode<-raster(pm)
values(permafrostcode)<-NA
permafrostcode[pm%in%c(1,5,9,13,17)]<-4 #Continuous
permafrostcode[pm%in%c(2,6,10,14,18)]<-3 #Discontinuous
permafrostcode[pm%in%c(3,7,11,15,19)]<-2 #Sporadic
permafrostcode[pm%in%c(4,8,12,16,20)]<-1 #Isolated
plot(permafrostcode)
permrast<-permafrostcode
levelplot(permrast,margin=F)+
  latticeExtra::layer(sp.polygons(bPolslaea))

#perm2<-resample(permrast,vertherb_div,method='ngb')

#Soils
#soilras<-raster('Data/GIS_layers/Soils/sq1.asc')
#hswd<-stack('Data/GIS_layers/Soils/HWSD_RASTER/hwsd')

soilurl<-'https://ntnu.box.com/shared/static/rr5yqlplu3lwhu19kc855a69rbtaj1as.zip'
download.file(soilurl,'Data/GIS_layers/Soils/DSMW.zip')
unzip('Data/GIS_layers/DSMW.zip',exdir='Data/GIS_layers/DSMW')
dsmw<-readOGR('Data/GIS_layers/Soils/DSMW','DSMW')#http://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/faounesco-soil-map-of-the-world/en/
dsmw$SimpleSoilUnit<-substr(dsmw$DOMSOI,1,1)
#Legend http://www.fao.org/fileadmin/user_upload/soils/docs/Soil_map_FAOUNESCO/images/Legend_I.jpg 
levels(as.factor(dsmw$SimpleSoilUnit))
#spplot(dsmw,'SimpleSoilUnit')

dsmw_arc<-crop(dsmw,alldata_sp)



#Human context
#GPW:Center for International Earth Science Information Network - CIESIN - Columbia University. 2018. Gridded Population of the World, Version 4 (GPWv4): Population Density, Revision 11. Palisades, NY: NASA Socioeconomic Data and Applications Center (SEDAC). https://doi.org/10.7927/H49C6VHW. Accessed 21.10.2020. 
#2015 human pop density

gpwurl<-'https://uitno.box.com/shared/static/u2t8wrffagosabv4k498dh856shzkqyi.zip'
download.file(gpwurl,'Data/GIS_layers/GPW2015.zip',mode='wb')
unzip('Data/GIS_layers/GPW2015.zip',exdir='Data/GIS_layers/GPW')

gpw<-raster('Data/GIS_layers/GPW/gpw_v4_population_density_rev11_2015_2pt5_min.tif')
levelplot(gpw+1,zscaleLog=T,margin=F,scales=list(draw=F))

#Human footprint
#Venter, O., E. W. Sanderson, A. Magrach, J. R. Allan, J. Beher, K. R. Jones, H. P. Possingham, W. F. Laurance, P. Wood, B. M. Fekete, M. A. Levy, and J. E. Watson. 2018. Last of the Wild Project, Version 3 (LWP-3): 2009 Human Footprint, 2018 Release. Palisades, NY: NASA Socioeconomic Data and Applications Center (SEDAC). https://doi.org/10.7927/H46T0JQ4. Accessed 21.10.2020. 
humfooturl<-'https://uitno.box.com/shared/static/1bjcuidtjis8456locp1rio2ul6a7zi4.zip'
download.file(humfooturl,'Data/GIS_layers/Humanfootprint.zip')
unzip('Data/GIS_layers/Humanfootprint.zip',exdir='Data/GIS_layers/HumanFootprint')

humanfoot<-raster('Data/GIS_layers/HumanFootprint/wildareas-v3-2009-human-footprint.tif')
levelplot(humanfoot,margin=F)

humanstack1<-stack(gpw,projectRaster(humanfoot,gpw))
humanstack<-aggregate(mask(crop(humanstack1,spTransform(arczones,gpw@crs)),spTransform(arczones,gpw@crs)),50)
names(humanstack)<-c('GPW','Footprint')

#NDVI trends
ndvitrend_url<-'https://uitno.box.com/shared/static/2vw9e99myxjzj08t8p2rkc1mmxxopmno.nc'
download.file(ndvitrend_url,'Data/GIS_layers/NDVItrend.nc',mode='wb')
ndvitrend<-raster('Data/GIS_layers/NDVItrend.nc',varname='gssndvi_trend')
lstrend<-raster('Data/GIS_layers/NDVItrend.nc',varname='los_trend')

#Current 1982-2014
currentndvi<-raster('Data/GIS_layers/NDVItrend.nc',varname='gssndvi')
currentlos<- raster('Data/GIS_layers/NDVItrend.nc',varname='los')

plot(ndvitrend)#Need to do some gymnastics here to get this correct orientation
ndvitrend<-t(flip(ndvitrend,direction=2))
plot(ndvitrend)
lostrend<-t(flip(lstrend,direction=2))
plot(lostrend)
plot(currentndvi)
ndvi<-t(flip(currentndvi,direction=2))
plot(ndvi)
plot(currentlos)
losc<-t(flip(currentlos,direction=2))
plot(losc)

ndvitrend@crs<-bioclimdat@crs
lostrend@crs<-bioclimdat@crs
ndvi@crs<-bioclimdat@crs
losc@crs<-bioclimdat@crs
ndvitrend_laea<-projectRaster(ndvitrend,crs=arczones)
lostrend_laea<-projectRaster(lostrend,crs=arczones)
ndvitrend_laea<-projectRaster(ndvi,crs=arczones)
losc_laea<-projectRaster(losc,crs=arczones)
plot(ndvitrend_laea)
plot(lostrend_laea)
points(alldata_splaea_removeoutsidearctic,pch=16,cex=0.1,col=2)
plot(arczones,add=T)

#Temperature change 
#GISTEMP Team, 2016: GISS Surface Temperature Analysis (GISTEMP). NASA Goddard Institute for Space Studies.     Hansen, J., R. Ruedy, M. Sato, and K. Lo, 2010: Global surface temperature change, Rev. Geophys., 48, RG4004, doi:10.1029/2010RG000345. https://data.giss.nasa.gov/gistemp/maps/)
tempdiff<-raster('Data/GIS_layers/amaps.nc')
tempdiffpp<-resample(projectRaster(tempdiff,crs=ndvitrend_laea),ndvitrend_laea,method='bilinear')

climatechangestack<-stack(ndvitrend_laea,losc_laea,ndvitrend_laea,lostrend_laea,tempdiffpp)
climatechangestack<-mask(climatechangestack,arczones)
names(climatechangestack)[1:4]<-c('Current NDVI','CurrentGrowingSeasonLength','NDVI trend','GrowingSeasonLength trend')

#Zones
subzonesR<-rasterize(subzones,bioclimdat_laea,field='ZONE')
#Simplify CAVM zones
agzones<-aggregate(subzones,by=list(subzones$ZONE),dissolve=T,FUN='mean')
#Set ice to NA
agzones$ZONE[agzones$ZONE==0]<-NA

agzone1<-spTransform(agzones,alldata_splaea@proj4string)
names(agzone1)[7]<-'ZONE_'
allzones<-rbind(agzone1[,7],subarcbound[,2],makeUniqueIDs = TRUE)
allzones$ZONE_<-as.factor(allzones$ZONE_)
levels(allzones$ZONE_)<-c('Subarctic','A','B','C','D','E','Subarctic')
spplot(allzones)+
latticeExtra::layer(sp.points(alldata_splaea_removeoutsidearctic))

#allzonesR<-rasterize(allzones,arcelev,field='ZONE_')
#allzonesR[allzonesR==0]<-7
#plot(allzonesR)+
 
# GIS data extraction -----------------------------------------------------


#Extract variables
alldata_final<-read.csv('Data/AllCodedDataEncoded.csv',header=T,sep=';')
alldata_final_sp<-SpatialPointsDataFrame(cbind(alldata_final$coordinates_E,alldata_final$coordinates_N),alldata_final)

alldata_final_sp1<-cbind(alldata_final_sp,raster::extract(bioclimdat,alldata_final_sp))
alldata_final_sp1$elevation_DEM<-raster::extract(arcelev,alldata_final_sp1)
alldata_final_sp1$distance_from_coast<-raster::extract(projectRaster(distancefromcoast,crs=crs(bioclimdat)),alldata_final_sp1)
alldata_final_sp1$soil_type.<-raster::extract(dsmw_arc,alldata_final_sp1)$SimpleSoilUnit
#alldata_final_sp1$permafrost<-raster::extract(projectRaster(perm2,crs=crs(bioclimdat),method='ngb'),alldata_final_sp1)
alldata_final_sp1$permafrost<-raster::extract(permafrostcode,alldata_splaea_removeoutsidearctic)
alldata_final_sp1$Subzone<-over(alldata_splaea_removeoutsidearctic,allzones[,'ZONE_'])$ZONE_
alldata_final_sp1$north_of_treeline<-raster::extract(northoftreeline,alldata_final_sp1)
alldata_final_sp2<-cbind(alldata_final_sp1,raster::extract(projectRaster(vertherb_div,crs = crs(bioclimdat)),alldata_final_sp1))
alldata_final_sp2a<-cbind(alldata_final_sp2,raster::extract(humanstack,alldata_final_sp2))
alldata_final_sp3a<-cbind(alldata_final_sp2a,raster::extract(projectRaster(climatechangestack,crs=crs(bioclimdat)),alldata_final_sp2a))
head(alldata_final_sp3a)
names(alldata_final_sp3a)

#Remove empty columns
na_count <-sapply(alldata_final_sp3@data, function(y) sum(length(which(is.na(y))))/length(y))
alldata_final_sp3<-alldata_final_sp3a[,na_count<1]

write.csv(alldata_final_sp3,'Data/AllCodedData_withGIScontext.csv')
write.csv(alldata_final_sp3,'shiny/AllCodedData_withGIScontext.csv')

#Context GIS layers
bioclimdat_laea<-projectRaster(bioclimdat,vertherb_sr)
bioclimdat_laea<-mask(crop(bioclimdat_laea,vertherb_sr),vertherb_sr)

crs(dsmw_arc)<-crs(bioclimdat)
dsmw_arc$simplesoilnum<-as.numeric(as.factor(dsmw_arc$SimpleSoilUnit))
soiltyperast<-rasterize(dsmw_arc,bioclimdat,field='simplesoilnum',fun=function(x, ...) modal(x,na.rm=T))
plot(soiltyperast)
context_stack<-stack(vertherb_div,bioclimdat_laea,arcelev_laea,projectRaster(climatechangestack,bioclimdat_laea),projectRaster(humanstack,bioclimdat_laea),projectRaster(soiltyperast,bioclimdat_laea,method='ngb'),projectRaster(permrast,bioclimdat_laea))
names(context_stack)[c(23,29:32)]<-c('Elevation','HumanPopulationDensity','Human footprint','Soil Type','Permafrost')

context_range<-extract(context_stack,1:ncell(context_stack),df=T)
write.csv(context_range,'Data/RangeofEcoContexts.csv')
write.csv(context_range,'shiny/RangeofEcoContexts.csv')

#Soil Legend
soilleg<-data.frame(Letter=levels(as.factor(dsmw_arc$SimpleSoilUnit)),Number=levels(as.factor(dsmw_arc$simplesoilnum)),
  SoilType=c('Cambisols','Chernozems','Podsoluvisols','Rendzinas','Gleysols','Phaeozems',
         'Lithosols','Fluvisols','Kastanozems','Luvisols','Greyzems','Nitosols','Histosols',
         'Podzols','Arenosols','Regosols','Solonetz','Andosols','Rankers','Planosols','Xerosols'))
write.csv(soilleg,'Data/SoilLegend.csv')

#Herbivore diversity space figure
alldata_final_sp2$SimpleHerbivore<-as.factor(alldata_final_sp2$herbivore_type)
levels(alldata_final_sp2$SimpleHerbivore)<-c(
  rep('Invertebrate',times=3),
                                             'Vertebrate',
                                             'Invertebrate','Invertebrate','Multiple','Vertebrate','Unknown','Vertebrate')


vertherbdat<-extract(vertherb_div,1:ncell(vertherb_div),df=T)
herbivorespace<-ggplot(data=vertherbdat,mapping=aes(x=ArcticHerbivore_Species.richness*70,y=ArcticHerbivore_Functional.diversity))+
  geom_point(size=0.01)+
  ggtitle("Herbivore space") +
  xlab("Vertebrate herbivore species richness")+ ylab('Vertebrate herbivore functional diversity')+
  geom_point(data=alldata_final_sp2@data,aes(x=ArcticHerbivore_Species.richness*70, y=ArcticHerbivore_Functional.diversity,
                                             colour=SimpleHerbivore))
png('Figures/HerbivoreSpace_Available.png')
herbivorespace
dev.off()


#Geographic space figure
geogcontextdat<-extract(geographicstack,1:ncell(geographicstack),df=T)
geospace<-ggplot(data=geogcontextdat,mapping=aes(x=DistancetoCoast,y=Elevation))+
  geom_point(alpha=2/10,size=0.2)+
  ggtitle("Geographic space") +
  xlab("Distance to coast (km)")+ ylab('Elevation (m)')+
  geom_point(data=alldata_final_sp3@data,aes(x=distance_from_coast, y=elevation_DEM,
                                             colour=coordinates_N
                                            ))
geospace+theme(legend.position=(c(0.9,0.8)))
png('Figures/GeographicSpace_Available.png')
geospace
dev.off()


#Climate change figure
climcontextdat<-extract(climatechangestack,1:ncell(climatechangestack),df=T)
climchangespace<-ggplot(data=climcontextdat,mapping=aes(x=GrowingSeasonLength.trend,y=NDVI.trend))+
  geom_point(alpha=1/10,size=0.1)+
  ggtitle("Climate change space") +
  xlab("Change in growing season length (days per decade")+ ylab('Change in NDVI (% per decade)')+
  geom_point(data=alldata_final_sp3@data,aes(x=GrowingSeasonLength.trend, y=NDVI.trend,
                                              colour=coordinates_N
  ))
png('Figures/ClimateChangeSpace_Available.png')
climchangespace
dev.off()

legtit<- "Latitude (°)"
png('Figures/4contexts.png')
tiff('Figures/4contexts.tif')
grid.arrange(climatespace2+theme(legend.position = c(0.8,0.8))+labs(color=legtit),
             climchangespace+theme(legend.position="none"),
             geospace+theme(legend.position="none"),
             herbivorespace+theme(legend.position=c(0.8,0.2))+theme(legend.title=element_blank()),
             ncol=2)
dev.off()
dev.off()


#Human context
humancontextdat<-extract(humanstack,1:ncell(humanstack),df=T)
humanspace<-ggplot(data=humancontextdat,mapping=aes(x=GPW,y=Footprint))+
  geom_point(size=0.3)+
  ggtitle("Human space") +
  xlab(expression("Human population density" ~(km^-2)))+ ylab('Human footprint')+
  geom_point(data=alldata_final_sp3@data,aes(x=GPW, y=Footprint,
                                             colour=coordinates_N))

png('Figures/HumanSpace_Available.png')
tiff('Figures/HumanSpace_Available.tif',width=6,height=4,units='in',res=150)
humanspace+scale_x_log10()+scale_y_log10(breaks=c(0.0001,100),labels=c('Low','High')) 
dev.off()

#Distribution figure
subzonesR<-rasterize(subzones,bioclimdat_laea,field='ZONE')
#Simplify CAVM zones
agzones<-aggregate(subzones,by=list(subzones$ZONE),dissolve=T,FUN='mean')
#Set ice to NA
agzones$ZONE[agzones$ZONE==0]<-NA

pdf('Figures/SpatialDistribution.pdf')
tiff('Figures/SpatialDistribution.tif')
colzones<-brewer.pal(6,'YlGn')

myColorkey <- list(space='right',
                  col=colzones,
                  at=seq(0.5,6.5,by=1), title='Arctic subzone',
                  labels=list(labels=c('Subarctic','E','D','C','B','A'),at=1:6))
blankras<-vertherb_sr*0

levelplot(projectRaster(blankras,crs=alldata_splaea@proj4string),
          margin=F,scales=list(draw=F),colorkey=myColorkey,col.regions=list(col='trasparent'))+
   layer(sp.polygons(subarcbound,fill=colzones[1],col=NA))+
   layer(sp.polygons(spTransform(agzones,alldata_splaea@proj4string),
                     fill=colzones[6:2][agzones$ZONE],col=NA,colorkey=myColorkey))+
  layer(sp.polygons(bPolslaea,col=grey(0.5),lwd=0.5))+ 
  layer(sp.points(alldata_splaea_removeoutsidearctic,col=1,pch=16,cex=0.4))
dev.off()
dev.off()

#Summarize by subzone

agzone1<-spTransform(agzones,alldata_splaea@proj4string)
names(agzone1)[7]<-'ZONE_'
allzones<-rbind(agzone1[,7],subarcbound[,2],makeUniqueIDs = TRUE)
plot(allzones)

allzonesR<-rasterize(allzones,bioclimdat_laea,field='ZONE_')
allzonesR[allzonesR==0]<-7
plot(allzonesR)
#a<-extract(spTransform(allzones,crs(bioclimdat)),alldata_final_sp3)
#tapply(a$ZONE_,a$ZONE_,length)

