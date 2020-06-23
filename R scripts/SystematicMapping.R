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
t3<-data.frame(t(codeddata3[,c(7:ncol(codeddata3))]),check.names = F,stringsAsFactors = F)
names(t3)<-codeddata3$`Coding variable`
t4<-data.frame(t(codeddata4[,c(7:ncol(codeddata4))]),check.names = F,stringsAsFactors = F)
names(t4)<-codeddata4$`Coding variable`

#Rbind these together into a single dataset
alldata<-do.call(rbind,list(t1,t2,t3,t4))

head(alldata)
View(alldata)

#Number of studies
length(levels(as.factor(alldata$title)))#344
#Number of evidence points
length(levels(as.factor(alldata$evidence_point_ID)))#803

#Write data
#Need to first replace \r\n in comment fields to prevent these being split between lines

#alldata$comment<-gsub("\r\n", " ", alldata$comment)
ad2<-data.frame(apply(alldata, 2,FUN=function(x) (gsub("\r\n", " ", x))))
ad3<-data.frame(apply(ad2, 2,FUN=function(x) (gsub("\r", "", x))))
write.table(ad3,'Data/AllCodedData.txt',row.names = F,sep=';')



# Mapping -----------------------------------------------------------------


# Mapping in space --------------------------------------------------------

#Evidence points
eps_country<-tapply(alldata$country,alldata$country,length)
pdf('Figures/Countries.pdf')
ggplot(data=alldata,aes(x=country))+geom_bar()+  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Countries")+ theme(axis.title.x = element_blank()) 
dev.off()
#World map
polarproj<-CRS('+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ')
boundaries <- map('worldHires', fill=TRUE,plot=FALSE,ylim=c(40,90))
IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
bPols <- map2SpatialPolygons(boundaries, IDs=IDs,
                             proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
bPolslaea<-spTransform(bPols,polarproj)
plot(bPolslaea,ylim=c(55,90))

#Spatial evidence points

#First remove evidence points with missing cooordinates
alldataCN<-alldata
alldataCN$coordinates_E<-as.numeric(alldataCN$coordinates_E)
alldataCN$coordinates_N<-as.numeric(alldataCN$coordinates_N)
alldatasp1<-alldataCN[!is.na(alldataCN$coordinates_E),]
dim(alldatasp1)#792 evidence points
#Change to spatial points df
alldata_sp<-SpatialPointsDataFrame(coords=cbind(alldatasp1$coordinates_E,alldatasp1$coordinates_N),data=alldatasp1,proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
alldata_splaea<-spTransform(alldata_sp,polarproj)

#CAFF limits
arczones<-readOGR('Data/ABA-Boundaries','Arctic_Zones')
arczones_laea<-spTransform(arczones,polarproj)
#Subarctic lower boundary
subarcbound<-arczones_laea[arczones_laea@data$Zone=='Sub arctic',]

pdf('Figures/SpatialDistribution.pdf')
plot(bPolslaea,ylim=c(55,90),main='Spatial distribution of evidence points')
points(alldata_splaea,pch=16,col='darkgreen',cex=0.5)
plot(subarcbound,border='red',lwd=2,lty=2,add=T)#Some coordinates outside the CAFF limit
dev.off()

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

ggplot(alldata, aes(fill = study_design, x = herbivore_type)) + geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Herbivore type and study design")+ theme(axis.title.x = element_blank()) 


ggplot(alldata, aes(fill = biological_organization_level_reported, x = herbivore_type)) + geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Temporal resolution")+ theme(axis.title.x = element_blank()) 


#NB: Identity of biology organization unit too many levels

# Temperature and precipitation axes --------------------------------------

#Downloading worldclim bioclimate data at 2.5deg resolution
bioclimdat<-getData('worldclim',var='bio',res=2.5)

#Using spatial data to extract
bioclim_ex<-extract(bioclimdat,alldata_sp)
#Bind to spatial data
alldata_sp<-cbind(alldata_sp,bioclim_ex)

# MAT and MAP are bio1 and bio12 respectively
climatespace<-ggplot(data=alldata_sp@data,aes(x=bio12, y=bio1/10))+geom_point()+
  ggtitle("Climatic space") + scale_y_reverse()+
  xlab("MAP (mm)")+ ylab(expression('MAT ' (degree~C)))

