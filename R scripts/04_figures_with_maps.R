#Systematic map of herbivory in Arctic tundra
# script for making figures 4 and 5


### NOTES FROM EEVA TO JAMES --------------------------------------------------------
# I here take in a file that has 
# 1) filtered away all redundant datapoints
# 2) filtered away points outside arctic study area
# 3) added all GIS-context variables (or NA for evidence points that cannot have them for various reasons)

# I chose file "AllCodedData_withGIScontext.csv" (see notes in script 02 what it seems to be missing)

# I have sorted this script so that first one reads in the necessary GIS data to make the bakground points
# and then does the plotting

# but I did not really manage to extract from your script which were the GIS data used in Fig 4 and 5
# can you give it a go?


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

# Load data  -----------------------------------------------
# Take in data that has redundant evidence points removed, spatial filtering done, and spatial variables added

alldata<-read.csv("Data/AllCodedData_withGIScontext.csv")
dim(alldataGIS)

# Get additional data needed for figure 5 and preprocess it   --------------------------------------


## Climate data

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

# Figure 4    --------------------------------------


# Figure 5    --------------------------------------



# Trying kernels and marginal histograms  --------------------------------------

toto<-alldata
group_length<-length(toto$year)/2
toto$group<-as.factor(c(rep(1, times=group_length), rep(2, times=group_length)))

# create plot with kernels density distribution
first_plot<-toto %>%
  ggplot(aes(coordinates_E, coordinates_N, color=group)) +
  geom_point() +
  #stat_density2d(aes(fill = ..level..), alpha = 0.3, geom = "polygon")+
  #stat_density_2d(geom = "polygon", aes(alpha = 0.3, fill = group)) + 
  #scale_alpha_continuous(range = c(0, 1))
  stat_density_2d(geom = "polygon",
                  aes(alpha = ..level.., fill = group),
                  bins = 20) 

first_plot

#create y-axis histogram
y_density <- axis_canvas(first_plot, axis = "y", coord_flip = TRUE) +
  geom_density(data = toto, aes(x = coordinates_N,fill = group), color = NA, alpha = 0.5) +
  coord_flip()

#create x-axis histogram
x_density <- axis_canvas(first_plot, axis = "x", coord_flip = TRUE) +
  geom_density(data = toto, aes(y = coordinates_E,fill = group), color = NA, alpha = 0.5) +
  coord_flip()


# create the combined plot
combined_plot <- insert_yaxis_grob(first_plot, y_density, position = "right")
combined_plot %<>% insert_xaxis_grob(., x_density, position = "top")


# show the result
ggdraw(combined_plot)
