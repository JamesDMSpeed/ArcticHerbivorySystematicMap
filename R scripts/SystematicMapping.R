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
library(ggplot2)
library(gridExtra)

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
length(levels(as.factor(alldata$title)))  #329
#Number of evidence points
length(levels(as.factor(alldata$evidence_point_ID)))  #715


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

#Spatial evidence points
alldatasp1<-alldata
#Change to spatial points df
alldata_sp<-SpatialPointsDataFrame(coords=cbind(alldatasp1$coordinates_E,alldatasp1$coordinates_N),data=alldatasp1,proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
alldata_splaea<-spTransform(alldata_sp,polarproj)

#Get CAFF boundaries to add
arczones<-readOGR('Data/ABA-Boundaries','Arctic_Zones')
arczones_laea<-spTransform(arczones,polarproj)
subarcbound<-arczones_laea[arczones_laea@data$Zone=='Sub arctic',]

#Figure
pdf('Figures/SpatialDistribution.pdf')
plot(bPolslaea,ylim=c(55,90),main='Spatial distribution of evidence points')
points(alldata_splaea,pch=16,col='darkgreen',cex=0.5)
plot(subarcbound,border='red',lwd=2,lty=2,add=T)#Some coordinates outside the CAFF limit
dev.off()


#Remove evidence points outside of arctic
#Buffer the Arctic polygons by 10000m to get sites with coordinate inaccuracies offshore
arczones_buffer<-gBuffer(arczones_laea,100000,byid=T,id=c('a','b','c'))
plot(arczones_laea)
plot(arczones_buffer,border=2,add=T)

#Remove evidence points outside of buffered polygon
alldata_splaea_removeoutsidearctic<-alldata_splaea[arczones_buffer,]
dim(alldata_splaea)
dim(alldata_splaea_removeoutsidearctic)

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
#Open this in excel - set coordinates to be imported as text. Replace ; with ..  Save as UTF csv. Open in EviAtlas with UTF-8 encoding. ; sep and " quote



# Mapping in time ---------------------------------------------------------
#Evidence points - year of publication
pub<-ggplot(alldata,aes(x=as.numeric(year)))+geom_histogram()+ggtitle("Publication year")+xlab('Publication year')
#Evidence points - year of study start
startyr<-ggplot(alldata,aes(x=as.numeric(year_start)))+geom_histogram()+ggtitle("Study start year")+xlab('Study start year')

pdf('Figures/Time.pdf')
grid.arrange(pub,startyr,ncol=2)
dev.off()

# Mapping study types -----------------------------------------------------

#Study design

studdes<-ggplot(alldata,aes(x=study_design))+geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Study design")+ theme(axis.title.x = element_blank()) 
exdes<-ggplot(alldata,aes(x=experimental_design))+geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Experimental design")+ theme(axis.title.x = element_blank()) 
studmeth<-ggplot(alldata,aes(x=study_method))+geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Study method")+ theme(axis.title.x = element_blank()) 
expquant<-ggplot(alldata,aes(x=exposure_quantification))+geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Exposure quantification")+ theme(axis.title.x = element_blank()) 

pdf('Figures/StudyDesign.pdf',width=6,height=10)
grid.arrange(studdes,studmeth,exdes,expquant,ncol=2)
dev.off()

#Herbivore type
pdf('Figures/HerbivoreType.pdf')
ggplot(alldata,aes(x=herbivore_type))+geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Herbivore type")+ theme(axis.title.x = element_blank()) 
dev.off()


# Mapping temporal and spatial resolutions & extents --------------------------------

tr1<-ggplot(alldata,aes(x=as.numeric(extent_of_temporal_scale)))+geom_histogram()+scale_x_log10()+
  ggtitle("Extent of temporal scale") +
  xlab("Temporal scale (years)")

tr2<-ggplot(alldata,aes(x=temporal_resolution))+geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Temporal resolution")+ theme(axis.title.x = element_blank()) 

pdf('Figures/TempRes.pdf',width=8,height=6)
grid.arrange(tr1,tr2,ncol=2)
dev.off()

#Contingency tables

ct1<-ggplot(alldata, aes(fill = study_design, x = herbivore_type)) + geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Herbivore type and study design")+ theme(axis.title.x = element_blank()) 


ct2<-ggplot(alldata, aes(fill = biological_organization_level_reported, x = herbivore_type)) + geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Herbivore type and plant level")+ theme(axis.title.x = element_blank()) 

pdf('Figures/ContingencyFigs.pdf',height=6,width=10)
grid.arrange(ct1,ct2,ncol=2)
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
dtmurl<-'https://uitno.box.com/shared/static/gw986nzxvif3cx6hhsqryjk1xzsdie5c.rrd'
download.file(dtmurl,'Data/GIS layeres/DTM.rrd')
dtm<-raster('Data/GIS layeres/DTM.rrd')#Canæ't open

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

#Herbivore diversity layers
vertherb_sr<-raster('Data/GIS_layers/ArcticHerbivore_Species.richness.tif')
vertherb_pd<-raster('Data/GIS_layers/ArcticHerbivore_Phylogenetic.diversity.tif')
vertherb_fd<-raster('Data/GIS_layers/ArcticHerbivore_Functional.diversity.tif')
vertherb_div<-stack(vertherb_sr,vertherb_pd,vertherb_fd)


#Context GIS layers
bioclimdat_laea<-projectRaster(bioclimdat,vertherb_sr)
bioclimdat_laea<-mask(crop(bioclimdat_laea,vertherb_sr),vertherb_sr)
arcelev_laea<-projectRaster(arcelev,vertherb_sr)
arcelev_laea
arcelev_laea<-mask(arcelev_laea,vertherb_sr)
plot(arcelev_laea)
context_stack<-stack(vertherb_div,bioclimdat_laea,arcelev_laea)
names(context_stack)[23]<-'Elevation'

#Extract variables
alldata_final<-read.csv('Data/AllCodedDataW_forEviAtlas.csv',header=T,sep=';')
alldata_final_sp<-SpatialPointsDataFrame(cbind(alldata_final$coordinates_E,alldata_final$coordinates_N),alldata_final)

alldata_final_sp1<-cbind(alldata_final_sp,extract(bioclimdat,alldata_final_sp))
alldata_final_sp1$elevation_DEM<-extract(arcelev,alldata_final_sp1)
alldata_final_sp2<-cbind(alldata_final_sp1,extract(projectRaster(vertherb_div,crs = crs(bioclimdat)),alldata_final_sp1))
head(alldata_final_sp2)

write.csv(alldata_final_sp2,'Data/AllCodedData_withGIScontext.csv')

#Herbivore diversity space figure
vertherbdat<-extract(vertherb_div,1:ncell(vertherb_div),df=T)
herbivorespace<-ggplot(data=vertherbdat,mapping=aes(x=ArcticHerbivore_Species.richness*70,y=ArcticHerbivore_Functional.diversity))+
  geom_point(alpha=1/10,size=0.01)+
  ggtitle("Herbivore space") +
  xlab("Vertebrate herbivore species richness")+ ylab('Vertebrate herbivore functional diversity')+
  #geom_point(data=arcclim_all,mapping=aes(x=bio12,y=bio1/10,colour=grey(0.5),size=0.1))+
  geom_point(data=alldata_final_sp2@data,aes(x=ArcticHerbivore_Species.richness*70, y=ArcticHerbivore_Functional.diversity,
                                             colour=herbivore_type))
png('Figures/HerbivoreSpace_Available.png')
herbivorespace
dev.off()
