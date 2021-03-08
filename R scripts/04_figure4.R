#Systematic map of herbivory in Arctic tundra
# script for making figure 4


# The script takes in a data file that has 
# 1) filtered away all redundant datapoints
# 2) filtered away points outside arctic study area
# 3) added all GIS-context variables (or NA for evidence points that cannot have them for various reasons)

# the script also takes in GIS-data layers for:
# world map 
# CAVM arctic subzones
# worldclim climate data
# CAFF boundaries
# elevation
# human footprint
# human population density
# herbivore species richness
# herbivore functional diversity
# change in NDVI
# change in growing season length

# and the layers missing from earlier versions (see notes on script 02)
# 1) growing_season_productivity
# 2) duration_of_growing_season 
# 3) recent_warming


### NOTES FROM EEVA TO JAMES --------------------------------------------------------



# load packages ---------------------------------------------------------

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
library(cowplot) # form plotrring kewrnesl and histograms
library(magrittr) # for pipes and %<>%
library(ncdf4) ## needed for NDVI rasters?
library(ks)

# Take in filtered evidence point data and preprocess --------------------------------

alldata<-read.csv("Data/AllCodedData_withGIScontext.csv")
dim(alldata)

#make spatial version of evidence points
alldatasp1<-alldata
polarproj<-CRS('+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ')
alldata_sp<-SpatialPointsDataFrame(coords=cbind(alldatasp1$coordinates_E,alldatasp1$coordinates_N),data=alldatasp1,proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
alldata_splaea<-spTransform(alldata_sp,polarproj)
#obs<-alldata_splaea[alldata_splaea$evidence_point_ID%in%alldata_sp$evidence_point_ID==F,]
#obsRa<-alldata_splaea[alldata_sp$evidence_point_ID%in%alldata_splaea$evidence_point_ID==F,]

# Take in pre-processed ecological context data
#data_eco_cont<-read.csv("Data/RangeofEcoContexts.csv")


# Get GIS data needed for figure 4 --------------------------------------

# human footprint
# human population density
# change in NDVI
# change in growing season length

#World map --- seems to work
boundaries <- maps::map('worldHires', fill=TRUE,plot=FALSE,ylim=c(40,90))
IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
bPols <- map2SpatialPolygons(boundaries, IDs=IDs,
                             proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
bPolslaea<-spTransform(bPols,polarproj)
plot(bPolslaea,ylim=c(55,90))


#CAVM arctic subzones -- seems to work
subzones<-readOGR('Data/cp_biozone_la_shp','cavm_all polygon')
subzones
## spplot(subzones,zcol=subzones$ZONE) takes forever but appears to work

#Get CAFF boundaries to add -- seems to work
arczones<-readOGR('Data/ABA-Boundaries','Arctic_Zones')
arczones_laea<-spTransform(arczones,polarproj)
subarcbound<-arczones_laea[arczones_laea@data$Zone=='Sub arctic',]
subarcbound
plot(subarcbound)

#Downloading worldclim data at 2.5deg resolution -- seems to work
bioclimdat<-getData('worldclim',var='bio',res=2.5)


#Herbivore diversity layers --- use later as raster background
vertherb_sr<-raster('Data/GIS_layers/ArcticHerbivore_Species.richness.tif')

#Context GIS layers -- making a rasterstack of all layers
bioclimdat_laea<-projectRaster(bioclimdat,vertherb_sr)
bioclimdat_laea<-mask(crop(bioclimdat_laea,vertherb_sr),vertherb_sr)


# #Distance from coast -- seems to work
# distancefromcoast<-raster('Data/GIS_layers/DistancetoCoast.tif')
# distancefromcoast
# plot(distancefromcoast)
# 
# 
# #Elevation --- seems to work
# #Use raster::getData instead
# charcount <- c('NO', 'SE','FI','CA','IS','GL','SJ') 
# allac2 <- do.call("merge", lapply(charcount, function(x)  raster::getData('alt', country=x)))
# #Issues with USA and Russia (crossing 180deg?)
# rusalt<-getData('alt',country='RU')
# rusalt2<-merge(rusalt[[1]],rusalt[[2]])
# usalt<-getData('alt',country='USA')
# usalt2<-do.call("merge",list(usalt[[1]],usalt[[2]],usalt[[3]],usalt[[4]]))
# plot(usalt2)
# arcelev<-merge(allac2,rusalt2,usalt2)
# arcelev<-crop(arcelev,alldata_sp)
# plot(arcelev)
# 
# #Herbivore diversity layers --- seems to work
# vertherb_sr<-raster('Data/GIS_layers/ArcticHerbivore_Species.richness.tif')
# #vertherb_pd<-raster('Data/GIS_layers/ArcticHerbivore_Phylogenetic.diversity.tif')
# vertherb_fd<-raster('Data/GIS_layers/ArcticHerbivore_Functional.diversity.tif')
# #vertherb_div<-stack(vertherb_sr,vertherb_pd,vertherb_fd)
# vertherb_div<-stack(vertherb_sr,vertherb_fd)
# 
# #Human population density --- seems to work
# #GPW:Center for International Earth Science Information Network - CIESIN - Columbia University. 2018. Gridded Population of the World, Version 4 (GPWv4): Population Density, Revision 11. Palisades, NY: NASA Socioeconomic Data and Applications Center (SEDAC). https://doi.org/10.7927/H49C6VHW. Accessed 21.10.2020. 
# #2015 human pop density
# #gpwurl<-'https://uitno.box.com/shared/static/u2t8wrffagosabv4k498dh856shzkqyi.zip'
# #download.file(gpwurl,'Data/GIS_layers/GPW2015.zip',mode='wb')
# #unzip('Data/GIS_layers/GPW2015.zip',exdir='Data/GIS_layers/GPW')
# ##
# gpw<-raster('Data/GIS_layers/GPW/gpw_v4_population_density_rev11_2015_2pt5_min.tif')
# levelplot(gpw+1,zscaleLog=T,margin=F,scales=list(draw=F))
# 
# #Human footprint --- seems to work
# #Venter, O., E. W. Sanderson, A. Magrach, J. R. Allan, J. Beher, K. R. Jones, H. P. Possingham, W. F. Laurance, P. Wood, B. M. Fekete, M. A. Levy, and J. E. Watson. 2018. Last of the Wild Project, Version 3 (LWP-3): 2009 Human Footprint, 2018 Release. Palisades, NY: NASA Socioeconomic Data and Applications Center (SEDAC). https://doi.org/10.7927/H46T0JQ4. Accessed 21.10.2020. 
# #humfooturl<-'https://uitno.box.com/shared/static/1bjcuidtjis8456locp1rio2ul6a7zi4.zip'
# #download.file(humfooturl,'Data/GIS_layers/Humanfootprint.zip')
# #unzip('Data/GIS_layers/Humanfootprint.zip',exdir='Data/GIS_layers/HumanFootprint')
# ##
# humanfoot<-raster('Data/GIS_layers/HumanFootprint/wildareas-v3-2009-human-footprint.tif')
# levelplot(humanfoot,margin=F)
# 
# ## stack human context together for Fig 5e --- seems to work
# humanstack1<-stack(gpw,projectRaster(humanfoot,gpw))
# humanstack<-aggregate(mask(crop(humanstack1,spTransform(arczones,gpw@crs)),spTransform(arczones,gpw@crs)),50) # NOTE arczones needed first
# names(humanstack)<-c('GPW','Footprint')
# 
# #NDVI trends
# #ndvitrend_url<-'https://uitno.box.com/shared/static/2vw9e99myxjzj08t8p2rkc1mmxxopmno.nc'
# #download.file(ndvitrend_url,'Data/GIS_layers/NDVItrend.nc',mode='wb')
# ndvitrend<-raster('Data/GIS_layers/NDVItrend.nc',varname='gssndvi_trend')
# lstrend<-raster('Data/GIS_layers/NDVItrend.nc',varname='los_trend')
# 
# #Current 1982-2014
# currentndvi<-raster('Data/GIS_layers/NDVItrend.nc',varname='gssndvi')
# currentlos<- raster('Data/GIS_layers/NDVItrend.nc',varname='los')
# 
# plot(ndvitrend)#Need to do some gymnastics here to get this correct orientation
# ndvitrend<-t(flip(ndvitrend,direction=2))
# plot(ndvitrend)
# lostrend<-t(flip(lstrend,direction=2))
# plot(lostrend)
# plot(currentndvi)
# ndvi<-t(flip(currentndvi,direction=2))
# plot(ndvi)
# plot(currentlos)
# losc<-t(flip(currentlos,direction=2))
# plot(losc)
# 
# ndvitrend@crs<-bioclimdat@crs
# lostrend@crs<-bioclimdat@crs
# ndvi@crs<-bioclimdat@crs
# losc@crs<-bioclimdat@crs
# ndvitrend_laea<-projectRaster(ndvitrend,crs=arczones)
# lostrend_laea<-projectRaster(lostrend,crs=arczones)
# ndvitrend_laea<-projectRaster(ndvi,crs=arczones)
# losc_laea<-projectRaster(losc,crs=arczones)
# plot(ndvitrend_laea)
# plot(lostrend_laea)
# #points(alldata_splaea_removeoutsidearctic,pch=16,cex=0.1,col=2)
# points(alldata_splaea,pch=16,cex=0.1,col=2)
# plot(arczones,add=T)
# 
# #Temperature change 
# #GISTEMP Team, 2016: GISS Surface Temperature Analysis (GISTEMP). NASA Goddard Institute for Space Studies.     Hansen, J., R. Ruedy, M. Sato, and K. Lo, 2010: Global surface temperature change, Rev. Geophys., 48, RG4004, doi:10.1029/2010RG000345. https://data.giss.nasa.gov/gistemp/maps/)
# tempdiff<-raster('Data/GIS_layers/amaps.nc')
# tempdiffpp<-resample(projectRaster(tempdiff,crs=ndvitrend_laea),ndvitrend_laea,method='bilinear')
# 
# climatechangestack<-stack(ndvitrend_laea,losc_laea,ndvitrend_laea,lostrend_laea,tempdiffpp)
# climatechangestack<-mask(climatechangestack,arczones)
# names(climatechangestack)[1:4]<-c('Current NDVI','CurrentGrowingSeasonLength','NDVI trend','GrowingSeasonLength trend')
# 
# # #Soils
# # soilurl<-'https://ntnu.box.com/shared/static/rr5yqlplu3lwhu19kc855a69rbtaj1as.zip'
# # download.file(soilurl,'Data/GIS_layers/Soils/DSMW.zip')
# # unzip('Data/GIS_layers/DSMW.zip',exdir='Data/GIS_layers/DSMW')
# # dsmw<-readOGR('Data/GIS_layers/Soils/DSMW','DSMW')#http://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/faounesco-soil-map-of-the-world/en/
# # dsmw$SimpleSoilUnit<-substr(dsmw$DOMSOI,1,1)
# # #Legend http://www.fao.org/fileadmin/user_upload/soils/docs/Soil_map_FAOUNESCO/images/Legend_I.jpg 
# # levels(as.factor(dsmw$SimpleSoilUnit))
# # #spplot(dsmw,'SimpleSoilUnit')
# # dsmw_arc<-crop(dsmw,alldata_sp)
# # crs(dsmw_arc)<-crs(bioclimdat)
# # dsmw_arc$simplesoilnum<-as.numeric(as.factor(dsmw_arc$SimpleSoilUnit))
# # soiltyperast<-rasterize(dsmw_arc,bioclimdat,field='simplesoilnum',fun=function(x, ...) modal(x,na.rm=T))
# # plot(soiltyperast)
# 
# # # exploring permafrost
# # #Permafrost
# # #Instead using 12.5km grid dataset
# # pm<-raster('Data/GIS_layers/nhipa.byte')
# # crs(pm)<-polarproj
# # permafrostcode<-raster(pm)
# # values(permafrostcode)<-NA
# # permafrostcode[pm%in%c(1,5,9,13,17)]<-4 #Continuous
# # permafrostcode[pm%in%c(2,6,10,14,18)]<-3 #Discontinuous
# # permafrostcode[pm%in%c(3,7,11,15,19)]<-2 #Sporadic
# # permafrostcode[pm%in%c(4,8,12,16,20)]<-1 #Isolated
# # permrast<-permafrostcode
# #   plot(permafrostcode)
# #   latticeExtra::layer(sp.polygons(bPolslaea))
# 
# 
# #Context GIS layers -- making a rasterstack of all layers
# bioclimdat_laea<-projectRaster(bioclimdat,vertherb_sr)
# bioclimdat_laea<-mask(crop(bioclimdat_laea,vertherb_sr),vertherb_sr)
# 
# 
# arcelev_laea<-projectRaster(arcelev,vertherb_sr)
# arcelev_laea
# arcelev_laea<-mask(arcelev_laea,vertherb_sr)
# plot(arcelev_laea)
# 
# context_stack<-stack(vertherb_div,bioclimdat_laea,arcelev_laea,projectRaster(climatechangestack,bioclimdat_laea),projectRaster(humanstack,bioclimdat_laea),projectRaster(soiltyperast,bioclimdat_laea,method='ngb'),projectRaster(permrast,bioclimdat_laea))
# names(context_stack)[c(23,29:32)]<-c('Elevation','HumanPopulationDensity','Human footprint','Soil Type','Permafrost')
# 
# context_range<-extract(context_stack,1:ncell(context_stack),df=T)
# write.csv(context_range,'Data/RangeofEcoContexts.csv')
# write.csv(context_range,'shiny/RangeofEcoContexts.csv')




# Figure 4    --------------------------------------


# plot(bPolslaea,ylim=c(55,90),main='Spatial distribution of evidence points')
# points(alldata_splaea,pch=16,col='darkgreen',cex=0.5)
# plot(subarcbound,border='red',lwd=2,lty=2,add=T)#Some coordinates outside the CAFF limit
# dev.off()

# plot(bPolslaea,ylim=c(55,90),main='Spatial distribution of evidence points')
# points(alldata_splaea,pch=16,col='red',cex=0.5)
# points(alldata_splaea_removeoutsidearctic,pch=16,col='darkgreen',cex=0.5)
# plot(subarcbound,border='blue',lwd=2,lty=2,add=T) 

#Distribution figure
subzonesR<-rasterize(subzones,bioclimdat_laea,field='ZONE')
#Simplify CAVM zones
agzones<-aggregate(subzones,by=list(subzones$ZONE),dissolve=T,FUN='mean')
#Set ice to NA
agzones$ZONE[agzones$ZONE==0]<-NA

pdf('Figures/SpatialDistribution.pdf')
tiff('Figures/SpatialDistribution.tif')
#
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
  layer(sp.points(alldata_splaea,col=1,pch=16,cex=0.4))

dev.off()
dev.off()




#Summarize by subzone

agzone1<-spTransform(agzones,alldata_splaea@proj4string)
names(agzone1)[7]<-'ZONE_'
allzones<-rbind(agzone1[,7],subarcbound[,2],makeUniqueIDs = TRUE)
plot(allzones)


a<-extract(spTransform(allzones,crs(bioclimdat)),alldata_final_sp3)
tapply(a$ZONE_,a$ZONE_,length)

