#Systematic map of herbivory in Arctic tundra
# script for making figures 4 and 5


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

# I chose file "AllCodedData_withGIScontext.csv". It works here, but see comment in script 02.

## for the rest... I did not quite manage to pick the right parts from your original script, and there are many things I was unable to run
## can you:
# add clean the section " Get all GIS data and preprocess" so that it takes in all data layers needed for figures 4 and 5?
# some of these seem to be misisng form the "Data" folder, like the distance to coast
# 


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
library(spatialEco)#For hexbin mappoing
library(hexbin)

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
data_eco_cont<-read.csv("Data/RangeofEcoContexts.csv")

source("Functions.R") 

# Figure 4    --------------------------------------


#Make an empty raster
blankras<-raster(nrows=74,ncols=81,xmn=-3400000,xmx=4600000,ymn=-4400000,ymx=3000000,
                 crs='+proj=longlat +datum=WGS84 +no_defs', 
                 res=100000)

blankras<-raster(nrows=74,ncols=81,xmn=-4300000,xmx=2920000,ymn=-2880000,ymx=4300000,
                 crs="+proj=laea +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
                 res=100000)

#Distribution figure
subzones<-readOGR('Data/cp_biozone_la_shp','cavm_all polygon')
subzones
subzonesR<-rasterize(subzones,blankras,field='ZONE')

#Simplify CAVM zones
agzones<-aggregate(subzones,by=list(subzones$ZONE),dissolve=T,FUN='mean')
#Set ice to NA
agzones$ZONE[agzones$ZONE==0]<-NA

#Get CAFF boundaries to add
arczones<-readOGR('Data/ABA-Boundaries','Arctic_Zones')
arczones_laea<-spTransform(arczones,polarproj)
subarcbound<-arczones_laea[arczones_laea@data$Zone=='Sub arctic',]

#CAFF+CAVM
# CAFF boundary 
caff<-readOGR("Data/CAFF_boundary", "CAFF_Boundary_Polygon_4326")
caff_line<-readOGR("Data/CAFF_boundary", "CAFF_Boundary_Line_4326")
caff_laea<-spTransform(caff,polarproj)

caffbuff<-gBuffer(caff_laea,width=0)#Apply a 0 buffer to fix that point
caffbuff
plot(caffbuff)
cavm_caff<-gIntersection(caffbuff,allzones,byid = F)


#Country boundaries
boundaries <- maps::map('worldHires', fill=TRUE,plot=FALSE,ylim=c(40,90))
IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
bPols <- map2SpatialPolygons(boundaries, IDs=IDs,
                             proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
bPolslaea<-spTransform(bPols,polarproj)

#Temperature anomaly
tempdiff<-raster('Data/GIS_layers/amaps.nc')
tempdiffpp<-projectRaster(tempdiff,blankras)
tempdiffppm<-mask(tempdiffpp,bPolslaea)
plot(tempdiffppm)

colzones<-rev(brewer.pal(6,'YlGn'))

myColorkey <- list(space='right',
                   col=colzones,
                   at=seq(0.5,6.5,by=1), title='Arctic subzone',
                   labels=list(labels=c('Subarctic','E','D','C','B','A'),at=1:6))

#blankras<-vertherb_sr*0

pts=SpatialPoints(rbind(c(-180,35),c(0,35),c(180,90),c(180,90)), CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
gl = gridlines(pts, easts = seq(-180,180,90),
               norths = c(66.7), ndiscr = 100)
glp<-spTransform(gl,polarproj)

values(blankras)<-0

#pdf('Figures/SpatialDistribution.pdf')
tiff('Figures/SpatialDistribution.tif',units='in',width=6,height=5,res=200)

levelplot(blankras,
          margin=F,scales=list(draw=F),colorkey=myColorkey,col.regions=list(col='trasparent'))+
  latticeExtra::layer(sp.polygons(subarcbound,fill=colzones[1],col=NA))+
  latticeExtra::layer(sp.polygons(spTransform(agzones,alldata_splaea@proj4string),
                    fill=colzones[6:2][agzones$ZONE],col=NA,colorkey=myColorkey))+
  latticeExtra::layer(sp.polygons(bPolslaea,col=grey(0.5),lwd=0.5))+
  latticeExtra::layer(sp.lines(glp,col=grey(0.7),lwd=0.5))+
  latticeExtra::layer(sp.points(alldata_splaea,col=1,pch=16,cex=0.4))

dev.off()
#dev.off()

#Summarising point density
#Hexbin
hb<-hexbin(alldata_splaea@coords,xbins=60)
hb
plot(hb)

points<-cbind.data.frame("xcoord"=hb@xcm, "ycoord"=hb@ycm, "count"=hb@count)
sppoints<-SpatialPointsDataFrame(points[,1:2],points,proj4string = crs(alldata_splaea))

k <- list(x = 1.2, y = 0.4, corner = c(0, 0), points=list(cex=c(10/5,0,25/5,0,50/5), col='darkorange',pch=16,alpha=0.2), 
          text=list(c('10','','25','','50')))

tiff('Figures/SpatialDistribution_HexBins.tif',units='in',width=6,height=5,res=200)
levelplot(blankras,
          margin=F,scales=list(draw=F),colorkey=myColorkey,key=k,col.regions=list(col='trasparent'))+
    latticeExtra::layer(sp.polygons(subarcbound,fill=colzones[1],col=NA))+
  latticeExtra::layer(sp.polygons(spTransform(agzones,alldata_splaea@proj4string),
                                  fill=colzones[6:2][agzones$ZONE],col=NA,colorkey=myColorkey))+
  latticeExtra::layer(sp.polygons(bPolslaea,col=grey(0.5),lwd=0.5))+
  latticeExtra::layer(sp.lines(glp,col=grey(0.7),lwd=0.5))+
  #latticeExtra::layer(sp.points(sppoints[sppoints$count==1,],pch=1))+
  #latticeExtra::layer(sp.points(sppoints[sppoints$count>1 &sppoints$count<11,],cex=2,pch=1))+
  #latticeExtra::layer(sp.points(sppoints[sppoints$count>10 &sppoints$count<21,],cex=3,pch=1))+
  #latticeExtra::layer(sp.points(sppoints[sppoints$count>20 &sppoints$count<31,],cex=4,pch=1))+
  #latticeExtra::layer(sp.points(sppoints[sppoints$count>30 ,],cex=5,pch=1))
    latticeExtra::layer(sp.polygons(caffbuff,col=grey(0.4),lwd=0.5))+
  latticeExtra::layer(sp.points(sppoints,cex=((sppoints$count)/5),pch=16,alpha=0.2,col='darkorange3'))+
  latticeExtra::layer(sp.points(alldata_splaea,cex=0.3,col=1,pch=16))
dev.off()


#Sample plot but with delta temperature as background
tiff('Figures/SpatialDistribution_tempdiff.tif',units='in',width=6,height=5,res=200)
l1<-levelplot(tempdiffppm,
          margin=F,scales=list(draw=F),colorkey=list(title=expression("Temperature anomaly ("*~degree*C*")")))+
  #latticeExtra::layer(sp.polygons(subarcbound,fill=colzones[1],col=NA))+
  #latticeExtra::layer(sp.polygons(spTransform(agzones,alldata_splaea@proj4string),
  #                                fill=colzones[6:2][agzones$ZONE],col=NA,colorkey=myColorkey))+
  latticeExtra::layer(sp.polygons(bPolslaea,col=grey(0.5),lwd=0.5))+
  latticeExtra::layer(sp.lines(glp,col=grey(0.7),lwd=0.5))+
  #latticeExtra::layer(sp.points(sppoints[sppoints$count==1,],pch=1))+
  #latticeExtra::layer(sp.points(sppoints[sppoints$count>1 &sppoints$count<11,],cex=2,pch=1))+
  #latticeExtra::layer(sp.points(sppoints[sppoints$count>10 &sppoints$count<21,],cex=3,pch=1))+
  #latticeExtra::layer(sp.points(sppoints[sppoints$count>20 &sppoints$count<31,],cex=4,pch=1))+
  #latticeExtra::layer(sp.points(sppoints[sppoints$count>30 ,],cex=5,pch=1))
  latticeExtra::layer(sp.polygons(caffbuff,col=grey(0.4),lwd=0.5))+
  #latticeExtra::layer(sp.points(sppoints,cex=((sppoints$count)/5),pch=16,alpha=0.2,col='darkorange3'))+
  latticeExtra::layer(sp.points(alldata_splaea,cex=0.3,col=1,pch=16))

diverge0(l1,'RdBu')
  dev.off()





#Simple number of evidence points per raster cell
#Points per cell
pointdens<-rasterize(alldata_splaea,blankras,field=1,fun='count')
levelplot(pointdens,scales=list(draw=F),margin=F,par.settings='YlOrRdTheme')+
 # latticeExtra::layer(sp.polygons(subarcbound,fill=colzones[1],col=NA))+
 # latticeExtra::layer(sp.polygons(spTransform(agzones,alldata_splaea@proj4string),
 #                               fill=NA,col=1,colorkey=myColorkey))+
  latticeExtra::layer(sp.polygons(bPolslaea,col=grey(0.5),lwd=0.5))+
  latticeExtra::layer(sp.lines(glp,col=grey(0.7),lwd=0.5))#+
 # latticeExtra::layer(sp.points(alldata_splaea,col=1,pch=16,cex=0.4))




#Trying  hexagons in SpatialEco package
a<-sp.kde(alldata_splaea)
plot(a)
h<-hexagons(alldata_splaea,res=100000)
hex.pts <- (over(alldata_splaea,h))
h$count<-rep(0,times=dim(h)[1])
pinp<-data.frame(table(hex.pts[,1]))
h$count[h$HEXID%in%pinp$Var1]<-pinp$Freq

my.palette <- c('white',brewer.pal(n = 5, name = "OrRd"))
breaks<-c(0,1,2,5,10,20,50)
spplot(h, "count", col.regions = my.palette, cuts = 5,at=breaks, col = "transparent")+
  latticeExtra::layer(sp.polygons(bPolslaea,col=grey(0.5),lwd=0.5))


# Figure 5    --------------------------------------


####################### geographic space

names(alldata) 
names(data_eco_cont)

used<-cbind.data.frame("Dist"=alldata$distance_from_coast, "Elevation" = alldata$elevation_DEM, "group" =rep("used", times=length(alldata$bio12)))
head(used)
available<-cbind.data.frame("MAP"=data_eco_cont$bio12, "Elevation" = data_eco_cont$Elevation, "group" =rep("available", times=length(data_eco_cont$bio12)))
head(available)

data<-rbind.data.frame(used, available)

# create plot with kernels density distribution
cbPalette <- c("#999999", "#E69F00")

pal <- hp(n = 7, house = "Ravenclaw")
image(volcano, col = pal)
pal[7]

palette_points<-c("#006699FF", "#B35900FF")

col_points_used<-c("#B35900FF")
col_points_available<-c("#006699FF")

#col_kernel_used<-c("#D9AC82FF")
#col_kernel_available<-c("#98C2D9FF")

first_plot<-data %>%
  ggplot(aes(MAP, MAT)) +
  #stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = group), bins = 100, show.legend=FALSE) +
  stat_hpd_2d(aes(fill = group), prob = 0.8, alpha = 0.2, linetype = "22", size = 1) +
  #scale_alpha_continuous(range = c(0, 1))+
  scale_fill_manual(values=c(col_points_available,col_points_used), name = "Data", labels = c("The Arctic", "Evidence points"))+
  geom_point(data=subset(data, group == "available"), alpha = 0.3, color=col_points_available)+
  geom_point(data=subset(data, group == "used"), alpha = 0.3, color=col_points_used)+
  xlab("MAP (mm)")+ ylab(expression('MAT ' (degree~C)))+
  theme_light()
first_plot


#create y-axis histogram
y_density <- axis_canvas(first_plot, axis = "y", coord_flip = TRUE) +
  geom_density(data = data, aes(x = MAT,fill = group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=palette_points)+
  coord_flip()

#create x-axis histogram
x_density <- axis_canvas(first_plot, axis = "x", coord_flip = TRUE) +
  geom_density(data = data, aes(y = MAP,fill = group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=palette_points)+
  coord_flip()

# create the combined plot
combined_plot <- insert_yaxis_grob(first_plot, y_density, position = "right")
combined_plot %<>% insert_xaxis_grob(., x_density, position = "top")

# show the result
ggdraw(combined_plot_geo_space)












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



####################### climate space

## need to create data frame where all datapoints are combined?
## four columns coordinate_E, coordinate_N, evidence_point, background

names(alldata) 
names(data_eco_cont)
#x=bio12, y=bio1/10

used<-cbind.data.frame("MAP"=alldata$bio12, "MAT" = alldata$bio1/10, "group" =rep("used", times=length(alldata$bio12)))
head(used)
available<-cbind.data.frame("MAP"=data_eco_cont$bio12, "MAT" = data_eco_cont$bio1/10, "group" =rep("available", times=length(data_eco_cont$bio12)))
head(available)

data<-rbind.data.frame(used, available)

# create plot with kernels density distribution
cbPalette <- c("#999999", "#E69F00")

pal <- hp(n = 7, house = "Ravenclaw")
image(volcano, col = pal)
pal[7]

palette_points<-c("#006699FF", "#B35900FF")

col_points_used<-c("#B35900FF")
col_points_available<-c("#006699FF")

#col_kernel_used<-c("#D9924DFF")
#col_kernel_available<-c("#0F75A8FF")

col_kernel_used<-c("#D9AC82FF")
col_kernel_available<-c("#98C2D9FF")

## kernel on background
first_plot<-data %>%
  ggplot(aes(MAP, MAT)) +
  stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = group), bins = 100, show.legend=FALSE) +
 scale_alpha_continuous(range = c(0, 1))+
 scale_fill_manual(values=c(col_kernel_available,col_kernel_used))+
  geom_point(data=subset(data, group == "available"), alpha = 3/10, color=col_points_available)+
  geom_point(data=subset(data, group == "used"), alpha = 3/10, color=col_points_used)+
  xlab("MAP (mm)")+ ylab(expression('MAT ' (degree~C)))+
  theme_light()
first_plot


first_plot<-data %>%
  ggplot(aes(MAP, MAT)) +
  #stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = group), bins = 100, show.legend=FALSE) +
  stat_hpd_2d(aes(fill = group), prob = 0.8, alpha = 0.2, linetype = "22", size = 1) +
  #scale_alpha_continuous(range = c(0, 1))+
  scale_fill_manual(values=c(col_points_available,col_points_used), name = "Data", labels = c("The Arctic", "Evidence points"))+
  geom_point(data=subset(data, group == "available"), alpha = 0.3, color=col_points_available)+
  geom_point(data=subset(data, group == "used"), alpha = 0.3, color=col_points_used)+
  xlab("MAP (mm)")+ ylab(expression('MAT ' (degree~C)))+
  #labs(fill = "Dose (mg)")+
  #scale_fill_discrete(name = "Data", labels = c("The Arctic", "Evidence points"))+
  theme_light()
first_plot


# ## kernel on background
# first_plot<-data %>%
#   #ggplot(aes(MAP, MAT, color=group)) +
#   ggplot(aes(MAP, MAT)) +
#   stat_density_2d(data=subset(data, group == "available"), geom = "polygon", aes(fill = ..level..), show.legend=FALSE) +
#   stat_density_2d(data=subset(data, group == "used"), geom = "polygon", aes(fill = ..level..),show.legend=FALSE) +
#   #   scale_fill_manual(values=cbPalette)+
#   geom_point(data=subset(data, group == "available"), alpha = 5/10, color="#999999")+
#   geom_point(data=subset(data, group == "used"), alpha = 5/10, color="#E69F00")+
#   theme_light()
# #geom_point() +
# 
# first_plot
  
#stat_density_2d(data = subset(data, group == "used"), geom = "raster", aes(alpha = ..density..), fill = "#E69F00" , contour = FALSE) +
#stat_density_2d(data = subset(data, group == "available"), geom = "raster", aes(alpha = ..density..), fill = "#999999" , contour = FALSE) +
#scale_alpha(range = c(0, 1)) 
  
# ## kernel on foreground
# first_plot<-data %>%
#   #ggplot(aes(MAP, MAT, color=group)) +
#   ggplot(aes(MAP, MAT)) +
#   # scale_alpha_continuous(range = c(0, 1))+
#   scale_fill_manual(values=cbPalette)+
#   geom_point(data=subset(data, group == "available"), alpha = 5/10, color="#999999")+
#   geom_point(data=subset(data, group == "used"), alpha = 5/10, color="#E69F00")+
#   stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = group)) 
# #geom_point() +
# #stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = group), bins = 20) +

# ## kernel on background
# first_plot<-data %>%
#   #ggplot(aes(MAP, MAT, color=group)) +
#   ggplot(aes(MAP, MAT)) +
#   stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = group), bins=20) +
#   # scale_alpha_continuous(range = c(0, 1))+
#   scale_fill_manual(values=cbPalette)+
#   geom_point(data=subset(data, group == "available"), alpha = 5/10, color=col_points_available)+
#   geom_point(data=subset(data, group == "used"), alpha = 5/10, color=col_points_used)+
#   theme_light()
# first_plot

#create y-axis histogram
y_density <- axis_canvas(first_plot, axis = "y", coord_flip = TRUE) +
  geom_density(data = data, aes(x = MAT,fill = group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=palette_points)+
  coord_flip()

#create x-axis histogram
x_density <- axis_canvas(first_plot, axis = "x", coord_flip = TRUE) +
  geom_density(data = data, aes(y = MAP,fill = group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=palette_points)+
  coord_flip()

# create the combined plot
combined_plot <- insert_yaxis_grob(first_plot, y_density, position = "right")
combined_plot %<>% insert_xaxis_grob(., x_density, position = "top")

# show the result
ggdraw(combined_plot)






climatespace<-ggplot(data=alldata_sp@data,aes(x=bio12, y=bio1/10,colour=coordinates_N))+geom_point()+
  ggtitle("Climatic space") + scale_y_reverse()+
  xlab("MAP (mm)")+ ylab(expression('MAT ' (degree~C)))
climatespace

pdf('Figures/ClimateSpace_toto.pdf')
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

####################### climate change space


#Climate change figure ---??? does not work
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



####################### herbivore space


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

####################### human space

## need to create data frame where all datapoints are combined?
## four columns coordinate_E, coordinate_N, evidence_point, background

names(alldata) 
names(data_eco_cont)


used<-cbind.data.frame("GPW"=alldata$GPW, "Footprint" = alldata$Footprint, "group" =rep("used", times=length(alldata$GPW)))
head(used)
#used<-used[1:50,]; dim(used)
available<-cbind.data.frame("GPW"=data_eco_cont$HumanPopulationDensity, "Footprint" = data_eco_cont$Human.footprint, "group" =rep("available", times=length(data_eco_cont$Human.footprint)))
head(available)
#available<-na.omit(available)
#available<-available[1:50,]; dim(available)

data<-rbind.data.frame(used, available)


# create plot with kernels density distribution
cbPalette <- c("#999999", "#E69F00")


first_plot<-data %>%
  ggplot(aes(GPW, Footprint, color=group)) +
  geom_point() +
  #stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = group), bins = 20) +
  stat_density_2d(data = subset(data, group == "used"), geom = "raster", aes(alpha = ..density..), fill = "#E69F00" , contour = FALSE) +
  stat_density_2d(data = subset(data, group == "available"), geom = "raster", aes(alpha = ..density..), fill = "#999999" , contour = FALSE) +
  scale_alpha(range = c(0, 1)) +
      scale_x_log10()+
  scale_y_log10(breaks=c(0.0001,100),labels=c('Low','High'))+
  scale_color_manual(values=cbPalette)


first_plot

#create y-axis histogram
y_density <- axis_canvas(first_plot, axis = "y", coord_flip = TRUE) +
  geom_density(data = data, aes(x = Footprint,fill = group), color = NA, alpha = 0.5) +
  #scale_fill_manual(values=cbPalette)+
  coord_flip()

#create x-axis histogram
x_density <- axis_canvas(first_plot, axis = "x", coord_flip = TRUE) +
  geom_density(data = data, aes(y = GPW,fill = group), color = NA, alpha = 0.5) +
  #scale_fill_manual(values=cbPalette)+
  coord_flip()


# create the combined plot
combined_plot <- insert_yaxis_grob(first_plot, y_density, position = "right")
combined_plot %<>% insert_xaxis_grob(., x_density, position = "top")


# show the result
ggdraw(combined_plot)







#Human context old
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



####################### combine to one figure 5

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
