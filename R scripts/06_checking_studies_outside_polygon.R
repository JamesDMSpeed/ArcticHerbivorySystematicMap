# checking studies that are outside study area polygon

rm(list=ls())
objects()


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
#library(networkD3) # for sankey diagrams
#library(harrypotter)# color palettes
#library(cowplot) # form plotrring kewrnesl and histograms
#library(magrittr) # for pipes and %<>%
#library(ncdf4) ## needed for NDVI rasters?
#library(ks)

# Take in data  --------------------------------

check_these<-read.csv("Data/EviPointsOutsideCAFFCAVMintersection.csv", sep=";")
view(check_these)
summary(check_these)
names(check_these)
str(check_these)

### if coordinates are comma separated instead of dor
mysub <- function(x) {sub(",",".",x)}
check_these$coordinates_E <- as.numeric(mysub(check_these$coordinates_E))
check_these$coordinates_N <- as.numeric(mysub(check_these$coordinates_N))


#make spatial version of evidence points
alldatasp1<-check_these
polarproj<-CRS('+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ')
alldata_sp<-SpatialPointsDataFrame(coords=cbind(alldatasp1$coordinates_E,alldatasp1$coordinates_N),data=alldatasp1,proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
alldata_splaea<-spTransform(alldata_sp,polarproj)


# Plot of subzones   --------------------------------


blankras<-raster(nrows=74,ncols=81,xmn=-4300000,xmx=2920000,ymn=-2880000,ymx=4300000,
                 crs="+proj=laea +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
                 res=100000)


# CAVM zones
subzones<-readOGR('Data/cp_biozone_la_shp','cavm_all polygon')
#subzones
#subzonesR<-rasterize(subzones,blankras,field='ZONE')
#Simplify CAVM zones
agzones<-aggregate(subzones,by=list(subzones$ZONE),dissolve=T,FUN='mean')
#Set ice to NA
agzones$ZONE[agzones$ZONE==0]<-NA

#Get subarctic boundary CAVM 
arczones<-readOGR('Data/ABA-Boundaries','Arctic_Zones')
arczones_laea<-spTransform(arczones,polarproj)
subarcbound<-arczones_laea[arczones_laea@data$Zone=='Sub arctic',]
#plot(subarcbound, add=TRUE, col="red")

# CAFF boundary 
#caff<-readOGR("Data/CAFF_boundary", "CAFF_Boundary_Polygon_4326")
caff_line<-readOGR("Data/CAFF_boundary", "CAFF_Boundary_Line_4326")
#caff_laea<-spTransform(caff,polarproj)
caff_spline<-spTransform(caff_line,polarproj)
#plot(caff_laea)

#Country boundaries
boundaries <- maps::map('worldHires', fill=TRUE,plot=FALSE,ylim=c(40,90))
IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
bPols <- map2SpatialPolygons(boundaries, IDs=IDs,
                             proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
bPolslaea<-spTransform(bPols,polarproj)


colzones<-brewer.pal(6,'YlGn')

myColorkey <- list(space='right',
                   col=colzones,
                   at=seq(0.5,6.5,by=1), title='Arctic subzone',
                   labels=list(labels=c('Subarctic','E','D','C','B','A'),at=1:6))

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
  latticeExtra::layer(sp.lines(caff_spline,col="black",lwd=0.5))+
  #latticeExtra::layer(sp.lines(glp,col=grey(0.7),lwd=0.5))+
  #latticeExtra::layer(sp.points(alldata_splaea,col="red",pch=16,cex=1))
  latticeExtra::layer(sp.points(toto,col="red",pch=16,cex=1))


# Make variable for inclusion  --------------------------------

check_these$include<-rep("yes", times=length(check_these$evidence_point_ID))

## all evidence points on Svalbard are included
## all evidence points on Greenland are included
# excluded form Russia 
check_these$include[check_these$locality == "Ossora"] <- "no"
check_these$include[check_these$locality == "Esso"] <- "no"
check_these$include[check_these$locality == "Ust'Kamchatsk"] <- "no"
# excluded from Canada 
check_these$include[check_these$locality == "Ruby Range"] <- "no"
# All sites in Alaska are in
# From Fennoscandia exclude sites location ==Dovre, Hol "AmmarnÃ¤s" , "BjÃ¶rkfjÃ¤llet", "Lappi", "NÃ¤kkÃ¤lÃ¤"  
#, "Padjelanta"  , "Pallastunturi", "Ritsem" , "Sallivaara"   ,  "Teuravuoma" , #"JÃ¡vrreÅ¡duottar"
check_these$include[check_these$locality == "Dovre"] <- "no"
check_these$include[check_these$locality == "Hol"] <- "no"
check_these$include[check_these$locality == "AmmarnÃ¤s" ] <- "no"
check_these$include[check_these$locality == "BjÃ¶rkfjÃ¤llet"] <- "no"
check_these$include[check_these$locality =="Lappi"] <- "no"
check_these$include[check_these$locality ==  "NÃ¤kkÃ¤lÃ¤"] <- "no"
check_these$include[check_these$locality == "Padjelanta"] <- "no"
check_these$include[check_these$locality == "Pallastunturi"] <- "no"
check_these$include[check_these$locality == "Ritsem"] <- "no"
check_these$include[check_these$locality == "Sallivaara"] <- "no"
check_these$include[check_these$locality == "Teuravuoma"] <- "no"
check_these$include[check_these$locality == "JÃ¡vrreÅ¡duottar"] <- "no"
#
check_these$include[check_these$locality == "Kautokeino_NÃ¤kkÃ¤lÃ¤"] <- "no"

toto<-cbind.data.frame(check_these$include, check_these$locality, check_these$country, check_these$evidence_point_ID)


write.table(check_these,'Data/EviPointsOutsideCAFFCAVMintersection_checked.txt', sep=";")


# which points to plot    --------------------------------


summary(alldata_splaea)
toto<-alldata_splaea[alldata_splaea$country=="Russia",]
toto<-alldata_splaea[alldata_splaea$locality=="Kharasavey 2",]
toto<-alldata_splaea[alldata_splaea$locality=="Kharasavey",]
toto<-alldata_splaea[alldata_splaea$locality=="N Yamal Peninsula",]
toto<-alldata_splaea[alldata_splaea$locality=="NW Taymyr Peninsula",]
toto<-alldata_splaea[alldata_splaea$locality=="Kotelny Island",]
toto<-alldata_splaea[alldata_splaea$locality=="Provideniya",]
toto<-alldata_splaea[alldata_splaea$locality=="Novo Chaplino",]
toto<-alldata_splaea[alldata_splaea$locality=="Ossora",]
toto<-alldata_splaea[alldata_splaea$locality=="Esso",]
toto<-alldata_splaea[alldata_splaea$locality=="Ust'Kamchatsk",]
#
toto<-alldata_splaea[alldata_splaea$country=="Canada",]
toto<-alldata_splaea[alldata_splaea$locality=="Alexandra Fiord",]
toto<-alldata_splaea[alldata_splaea$locality=="Lumsden River",]
toto<-alldata_splaea[alldata_splaea$locality=="Stitt River",]
toto<-alldata_splaea[alldata_splaea$locality=="Akimiski Island",]
toto<-alldata_splaea[alldata_splaea$locality=="Ellesmere Island",]
toto<-alldata_splaea[alldata_splaea$locality=="La PÃ©rouse Bay",]
toto<-alldata_splaea[alldata_splaea$locality=="Mealy Mountains",]
toto<-alldata_splaea[alldata_splaea$locality=="Minstik River",]
toto<-alldata_splaea[alldata_splaea$locality=="Rideout Island",]
toto<-alldata_splaea[alldata_splaea$locality=="RiviÃ¨re George",]
toto<-alldata_splaea[alldata_splaea$locality=="Ruby Range",]
toto<-alldata_splaea[alldata_splaea$locality=="Thomson River",]
#
toto<-alldata_splaea[alldata_splaea$country=="Alaska",]
toto<-toto[toto$locality!="Aleutian archipelago",] # Aleuts are inside the study area
toto<-toto[toto$locality!="Atkasook",] # Atkasoon and Meade river are inside the study area
toto<-toto[toto$locality!="Meade River",]
toto<-toto[toto$locality!="Kokechik Bay",]


plot(toto, col="red",pch=16,cex=1)
plot(bPolslaea, add=TRUE, col="gray", lwd=0.5)
plot(caff_spline, add=TRUE, col="green", lwd=0.5)
points(toto, col="red",pch=16,cex=1)


toto<-alldata_splaea[alldata_splaea$country=="Alaska",]

View(check_these[check_these$country=="Alaska",])

          
toto<-alldata_splaea[alldata_splaea$country=="Finland"|alldata_splaea$country=="Sweden"|alldata_splaea$country=="Norway"|alldata_splaea$country=="several",]
## two evidence points south of CAFF border
toto<-toto[toto$locality!="Dovre",] # 
toto<-toto[toto$locality!="Hol",] 
toto<-toto[toto$locality=="JÃ¡vrreÅ¡duottar"|toto$locality=="Pallastunturi",] 

          
plot(toto, col="red",pch=16,cex=1)
#plot(bPolslaea, add=TRUE, col="gray", lwd=0.5)
plot(subarcbound, add=TRUE, col="lightgray")
plot(caff_spline, add=TRUE, col="green", lwd=0.5)
#points(toto, col="red",pch=16,cex=1)
points(tito, col="blue",pch=16,cex=1)


tito<-toto[toto$locality== "Kautokeino_NÃ¤kkÃ¤lÃ¤",]

levels(as.factor(toto$locality))
View(as.data.frame(toto))

#YES "HÃ¥kÃ¸ya" , "Ifjordfjellet" , "Komagdalen" ,  "Stokkvikeidet" ,  "TromsÃ¸" 
# NO"AmmarnÃ¤s" , "BjÃ¶rkfjÃ¤llet"    , "Lappi", "NÃ¤kkÃ¤lÃ¤"  , "Padjelanta"  , "Pallastunturi", "Ritsem" , "Sallivaara"   ,  "Teuravuoma" , #"JÃ¡vrreÅ¡duottar"

toto<-alldata_splaea[alldata_splaea$country=="several",]

             
                          
                     
