#Systematic map of herbivory in Arctic tundra
#Script to import coded data from excel spreadsheet.
#Data taken from Eeva's UiT Box account and read in using a direct download link
#Data is spread between multiple sheets with 1000 evidence points per sheet


### NOTES FROM EEVA TO JAMES --------------------------------------------------------

# my idea was to collect here only the stuff  that is needed to produce the datafile to-be-used in further analyses
# so I have made separate scripts for calculating statistics for results (script 02), and for making figures (scripts 03 and 04)
# I have not deleted anything from here yet.

# can you clean this script so that its only producing and saving the datafile that has:
# 1) filtered away all redundant datapoints
# 2) filtered away points outside arctic study area
# 3) added all GIS-context variables (or NA for evidence points that cannot have them for various reasons)


# there is something funny with the treeline-variable in the "AllCodedData_withGIScontext.csv" -file
# quite many sites have NA, should not be possible?


# read in libraries -----------------------------------------------


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
boundaries <- map('worldHires', fill=TRUE,plot=FALSE,ylim=c(40,90))
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
#Open this in excel - set coordinates to be imported as text. Replace ; with ..  Save as UTF csv.


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

#Permafrost
permafrosturl<-'https://uitno.box.com/shared/static/mftidvyo8z2tkyqq1aivhbbg6y2339hz.zip'
download.file(permafrosturl,'Data/GIS_layers/Permafrost.zip')
unzip('Data/GIS_layers/Permafrost.zip',exdir='Data/GIS_layers/Permafrost')

permafrost<-readOGR('Data/GIS_layers/Permafrost','permaice')
permafrost
plot(permafrost)

permrast<-rasterize(permafrost,arczonesT,field='EXTENT',method='ngb')
plot(permrast)

#Soils
#soilras<-raster('Data/GIS_layers/Soils/sq1.asc')
#hswd<-stack('Data/GIS_layers/Soils/HWSD_RASTER/hwsd')
dsmw<-readOGR('Data/GIS_layers/Soils/DSMW','DSMW')#http://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/faounesco-soil-map-of-the-world/en/
dsmw$SimpleSoilUnit<-substr(dsmw$DOMSOI,1,1)
#Legend http://www.fao.org/fileadmin/user_upload/soils/docs/Soil_map_FAOUNESCO/images/Legend_I.jpg 
levels(as.factor(dsmw$SimpleSoilUnit))
#spplot(dsmw,'SimpleSoilUnit')

dsmw_arc<-crop(dsmw,alldata_sp)


#Herbivore diversity layers
vertherb_sr<-raster('Data/GIS_layers/ArcticHerbivore_Species.richness.tif')
vertherb_pd<-raster('Data/GIS_layers/ArcticHerbivore_Phylogenetic.diversity.tif')
vertherb_fd<-raster('Data/GIS_layers/ArcticHerbivore_Functional.diversity.tif')
vertherb_div<-stack(vertherb_sr,vertherb_pd,vertherb_fd)

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
ndvitrend<-raster('Data/GIS_layers/NDVItrend.nc',varname='gssndvi_trend')#Can't open
lstrend<-raster('Data/GIS_layers/NDVItrend.nc',varname='los_trend')

plot(ndvitrend)#Need to do some gymnastics here to get this correct orientation
ndvitrend<-t(flip(ndvitrend,direction=2))
plot(ndvitrend)
lostrend<-(flip(lstrend,direction=1))
plot(lostrend)

ndvitrend@crs<-bioclimdat@crs
lostrend@crs<-bioclimdat@crs
ndvitrend_laea<-projectRaster(ndvitrend,crs=arczones)
lostrend_laea<-projectRaster(lostrend,crs=arczones)
plot(ndvitrend_laea)
plot(lostrend_laea)
points(alldata_splaea_removeoutsidearctic,pch=16,cex=0.1,col=2)
plot(arczones,add=T)

climatechangestack<-stack(ndvitrend_laea,lostrend_laea)
climatechangestack<-mask(climatechangestack,arczones)
names(climatechangestack)<-c('NDVI trend','GrowingSeasonLength trend')

#Extract variables
alldata_final<-read.csv('Data/AllCodedDataEncoded.csv',header=T,sep=';')
alldata_final_sp<-SpatialPointsDataFrame(cbind(alldata_final$coordinates_E,alldata_final$coordinates_N),alldata_final)

alldata_final_sp1<-cbind(alldata_final_sp,extract(bioclimdat,alldata_final_sp))
alldata_final_sp1$elevation_DEM<-extract(arcelev,alldata_final_sp1)
alldata_final_sp1$distance_from_coast<-extract(projectRaster(distancefromcoast,crs=crs(bioclimdat)),alldata_final_sp1)
alldata_final_sp1$soil_type.<-extract(dsmw_arc,alldata_final_sp1)$SimpleSoilUnit
alldata_final_sp2<-cbind(alldata_final_sp1,extract(projectRaster(vertherb_div,crs = crs(bioclimdat)),alldata_final_sp1))
alldata_final_sp2a<-cbind(alldata_final_sp2,extract(humanstack,alldata_final_sp2))
alldata_final_sp3a<-cbind(alldata_final_sp2a,extract(projectRaster(climatechangestack,crs=crs(bioclimdat)),alldata_final_sp2a))
head(alldata_final_sp3)

#Remove empty columns
na_count <-sapply(alldata_final_sp3a@data, function(y) sum(length(which(is.na(y))))/length(y))
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
context_stack<-stack(vertherb_div,bioclimdat_laea,arcelev_laea,projectRaster(climatechangestack,bioclimdat_laea),projectRaster(humanstack,bioclimdat_laea),projectRaster(soiltyperast,bioclimdat_laea,method='ngb'))
names(context_stack)[c(23,26,27,28)]<-c('Elevation','HumanPopulationDensity','Human footprint','Soil Type')

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


a<-extract(spTransform(allzones,crs(bioclimdat)),alldata_final_sp3)
tapply(a$ZONE_,a$ZONE_,length)

