#Systematic map of herbivory in Arctic tundra
# script for making figure5

# The script takes in a data file "AllCodedData_withGIScontext.csv" that has 
# 1) filtered away all redundant datapoints
# 2) filtered away points outside arctic study area
# 3) added all GIS-context variables (or NA for evidence points that cannot have them for various reasons)

# and a file "RangeofEcoContexts.csv"
# including ecological context variable data for entire arctic study area polygon

# load packages ---------------------------------------------------------

rm(list=ls())
objects()


#library(mapdata)#World map
#library(maptools)#Playing with maps
#library(sp)#Spatial data
#library(rgdal)#Spatial data
#library(raster)#Climate data
#library(rasterVis)#Visualisations
library(ggplot2)
library(gridExtra)
#library(rgeos)
#library(RColorBrewer)#Colours 
library(tidyverse) #data wrangling
#library(networkD3) # for sankey diagrams
library(harrypotter)# color palettes
library(cowplot) # form plotrring kewrnesl and histograms
library(magrittr) # for pipes and %<>%
#library(ncdf4) ## needed for NDVI rasters?
#library(ks)

# Take in filtered evidence point data and preprocess --------------------------------

alldata<-read.csv("Data/AllCodedData_withGIScontext.csv")
dim(alldata)

#make spatial version of evidence points
#alldatasp1<-alldata
#polarproj<-CRS('+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ')
#alldata_sp<-SpatialPointsDataFrame(coords=cbind(alldatasp1$coordinates_E,alldatasp1$coordinates_N),data=alldatasp1,proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
#alldata_splaea<-spTransform(alldata_sp,polarproj)
#obs<-alldata_splaea[alldata_splaea$evidence_point_ID%in%alldata_sp$evidence_point_ID==F,]
#obsRa<-alldata_splaea[alldata_sp$evidence_point_ID%in%alldata_splaea$evidence_point_ID==F,]

# Take in pre-processed ecological context data
data_eco_cont<-read.csv("Data/RangeofEcoContexts.csv")
names(data_eco_cont)

#source("Functions.R") 

# 
# names(alldata)
# alldata$elevation_DEM
# alldata$distance_to_treeline
# alldata$north_of_treeline
# alldata$distance_from_coast
# alldata$Subzone
# summary(alldata)
# 
# data_eco_cont$Elevation
# data_eco_cont$NorthofTreeline
# data_eco_cont$DistancetoCoast
# data_eco_cont$Current.NDVI
# data_eco_cont$NDVI.trend


# Figure 5    --------------------------------------

## geo, climate, climate change, food web, human

# create plot with kernels density distribution
cbPalette <- c("#999999", "#E69F00")

pal <- hp(n = 7, house = "Ravenclaw")
image(volcano, col = pal)
pal[7]

palette_points<-c("#006699FF", "#B35900FF")
palette_points_2<-c("#B35900FF", "#006699FF")

col_points_used<-c("#B35900FF")
col_points_available<-c("#006699FF")


####################### geographic space -------------

used<-cbind.data.frame("x_variable"=alldata$distance_from_coast, "y_variable" = alldata$elevation_DEM, "group" =rep("Evidence base", times=length(alldata$bio12)))
available<-cbind.data.frame("x_variable"=data_eco_cont$DistancetoCoast, "y_variable" = data_eco_cont$Elevation, "group" =rep("Study area", times=length(data_eco_cont$bio12)))
data<-rbind.data.frame(available, used)

#sample size for axes
length(na.omit(used$x_variable))
length(na.omit(used$y_variable))

legend_title <- "Data source"

 first_plot<-data %>%
   ggplot(aes(x=x_variable, y=y_variable, group=group))+
   geom_point(aes(color=group), alpha = 0.3, show.legend = TRUE)+
   scale_fill_manual(values=c(col_points_available,col_points_used))+
   scale_color_manual(legend_title, values=c("Evidence base"=col_points_used,"Study area"=col_points_available))+
   ggtitle("A) Geographic space")+
   xlab("Distance to coast (km, n=661)")+ ylab("Elevation (m, n=623)")+
   theme_light()+
    theme(legend.position = c(0.8, 0.8))
 first_plot  

#create y-axis histogram
y_density <- axis_canvas(first_plot, axis = "y", coord_flip = TRUE) +
  geom_density(data = data, aes(x = y_variable,fill = group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=palette_points_2)+
  coord_flip()

#create x-axis histogram
x_density <- axis_canvas(first_plot, axis = "x", coord_flip = TRUE) +
  geom_density(data = data, aes(y = x_variable,fill = group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=palette_points_2)+
  coord_flip()

# create the combined plot
combined_plot_geo_space <- insert_yaxis_grob(first_plot, y_density, position = "right")
combined_plot_geo_space %<>% insert_xaxis_grob(., x_density, position = "top")

# show the result
ggdraw(combined_plot_geo_space)

####################### climate space 1 -------------

used<-cbind.data.frame("x_variable"=alldata$bio12, "y_variable" = alldata$bio1/10, "group" =rep("used", times=length(alldata$bio12)))
available<-cbind.data.frame("x_variable"=data_eco_cont$bio12, "y_variable" = data_eco_cont$bio1/10, "group" =rep("available", times=length(data_eco_cont$bio12)))
data<-rbind.data.frame(used, available)
head(data)

#sample size for axes
length(na.omit(used$x_variable))
length(na.omit(used$y_variable))


 first_plot<-data %>%
   ggplot(aes(x_variable, y_variable)) +
   #stat_hpd_2d(aes(fill = group), prob = 0.8, alpha = 0.2, linetype = "22", size = 1,show.legend = FALSE) +
   #stat_hpd_2d(data=subset(data, group == "available"), prob = 0.8, alpha = 0.2, linetype = "22", size = 1,show.legend = FALSE) +
   #stat_density_2d(data=subset(data, group == "available"), geom = "polygon", aes(alpha = ..level.., fill = group), bins = 100, show.legend=FALSE) +
   #scale_fill_manual(values=col_points_available)+
   geom_point(data=subset(data, group == "available"), alpha = 0.3, color=col_points_available)+
   geom_point(data=subset(data, group == "used"), alpha = 0.3, color=col_points_used)+
   xlab("Mean Annual Precipitation")+ ylab(expression('Mean Annual Temperature  ' (degree~C)))+
   ggtitle("B) Climate space (n=643)")+
   theme_light()
 first_plot
 
 

#create y-axis histogram
y_density <- axis_canvas(first_plot, axis = "y", coord_flip = TRUE) +
  geom_density(data = data, aes(x = y_variable,fill = group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=palette_points)+
  coord_flip()

#create x-axis histogram
x_density <- axis_canvas(first_plot, axis = "x", coord_flip = TRUE) +
  geom_density(data = data, aes(y = x_variable,fill = group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=palette_points)+
  coord_flip()

# create the combined plot
combined_plot_climate <- insert_yaxis_grob(first_plot, y_density, position = "right")
combined_plot_climate %<>% insert_xaxis_grob(., x_density, position = "top")

# show the result
ggdraw(combined_plot_climate)

####################### climate space 2 -------------

names(alldata) 
names(data_eco_cont)

used<-cbind.data.frame("x_variable"=alldata$Current.NDVI, "y_variable" = alldata$CurrentGrowingSeasonLength, "group" =rep("used", times=length(alldata$bio12)))
head(used)
available<-cbind.data.frame("x_variable"=data_eco_cont$Current.NDVI, "y_variable" = data_eco_cont$CurrentGrowingSeasonLength, "group" =rep("available", times=length(data_eco_cont$bio12)))
head(available)

data<-rbind.data.frame(used, available)


#sample size for axes
length(na.omit(used$x_variable))
length(na.omit(used$y_variable))



first_plot<-data %>%
  ggplot(aes(x_variable, y_variable)) +
  #stat_hpd_2d(aes(fill = group), prob = 0.8, alpha = 0.2, linetype = "22", size = 1,show.legend = FALSE) +
  #scale_fill_manual(values=c(col_points_available,col_points_used), name = "Data", labels = c("The Arctic", "Evidence points"))+
  geom_point(data=subset(data, group == "available"), alpha = 0.3, color=col_points_available)+
  geom_point(data=subset(data, group == "used"), alpha = 0.3, color=col_points_used)+
  xlab("Growing season summed NDVI")+ ylab("Growing season length (days)")+
  ggtitle("C) Climate space (n=453)")+
  theme_light()
first_plot


#create y-axis histogram
y_density <- axis_canvas(first_plot, axis = "y", coord_flip = TRUE) +
  geom_density(data = data, aes(x = y_variable,fill = group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=palette_points)+
  coord_flip()

#create x-axis histogram
x_density <- axis_canvas(first_plot, axis = "x", coord_flip = TRUE) +
  geom_density(data = data, aes(y = x_variable,fill = group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=palette_points)+
  coord_flip()

# create the combined plot
combined_plot_climate_2 <- insert_yaxis_grob(first_plot, y_density, position = "right")
combined_plot_climate_2 %<>% insert_xaxis_grob(., x_density, position = "top")

# show the result
ggdraw(combined_plot_climate_2)

####################### climate change -------------

# NDVI.trend
# GrowingSeasonLength.trend
# Temperature.anomaly


#used<-cbind.data.frame("x_variable"=alldata$GrowingSeasonLength.trend, "y_variable" = alldata$NDVI.trend, "group" =rep("used", times=length(alldata$bio12)))
#head(used)
#available<-cbind.data.frame("x_variable"=data_eco_cont$GrowingSeasonLength.trend, "y_variable" = data_eco_cont$NDVI.trend, "group" =rep("available", times=length(data_eco_cont$bio12)))
#head(available)


used<-cbind.data.frame("x_variable"=alldata$Temperature.anomaly, "y_variable" = alldata$NDVI.trend, "group" =rep("used", times=length(alldata$bio12)))
head(used)
available<-cbind.data.frame("x_variable"=data_eco_cont$Temperature.anomaly, "y_variable" = data_eco_cont$NDVI.trend, "group" =rep("available", times=length(data_eco_cont$bio12)))
head(available)

data<-rbind.data.frame(used, available)

#sample size for axes
length(na.omit(used$x_variable))
length(na.omit(used$y_variable))




first_plot<-data %>%
  ggplot(aes(x_variable, y_variable)) +
  #stat_hpd_2d(aes(fill = group), prob = 0.8, alpha = 0.2, linetype = "22", size = 1, show.legend=FALSE) +
  #scale_fill_manual(values=c(col_points_available,col_points_used), name = "Data", labels = c("The Arctic", "Evidence points"))+
  geom_point(data=subset(data, group == "available"), alpha = 0.3, color=col_points_available)+
  geom_point(data=subset(data, group == "used"), alpha = 0.3, color=col_points_used)+
  xlab("Change in temperature (n=662)")+ ylab("Change in NDVI (% per decade, n=455)")+
  ggtitle("D) Climate change space")+
  theme_light()
first_plot



#create y-axis histogram
y_density <- axis_canvas(first_plot, axis = "y", coord_flip = TRUE) +
  geom_density(data = data, aes(x = y_variable,fill = group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=palette_points)+
  coord_flip()

#create x-axis histogram
x_density <- axis_canvas(first_plot, axis = "x", coord_flip = TRUE) +
  geom_density(data = data, aes(y = x_variable,fill = group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=palette_points)+
  coord_flip()

# create the combined plot
combined_plot_climate_change <- insert_yaxis_grob(first_plot, y_density, position = "right")
combined_plot_climate_change %<>% insert_xaxis_grob(., x_density, position = "top")

# show the result
ggdraw(combined_plot_climate_change)




####################### food web space -------------

used<-cbind.data.frame("x_variable"=alldata$ArcticHerbivore_Species.richness, "y_variable" = alldata$ArcticHerbivore_Functional.diversity, "group" =rep("used", times=length(alldata$bio12)))
available<-cbind.data.frame("x_variable"=data_eco_cont$ArcticHerbivore_Species.richness, "y_variable" = data_eco_cont$ArcticHerbivore_Functional.diversity, "group" =rep("available", times=length(data_eco_cont$bio12)))
data<-rbind.data.frame(used, available)

#sample size for axes
length(na.omit(used$x_variable))
length(na.omit(used$y_variable))


first_plot<-data %>%
  ggplot(aes(x_variable, y_variable)) +
  #stat_hpd_2d(aes(fill = group), prob = 0.8, alpha = 0.2, linetype = "22", size = 1, show.legend=FALSE) +
  #scale_fill_manual(values=c(col_points_available,col_points_used), name = "Data", labels = c("The Arctic", "Evidence points"))+
  geom_point(data=subset(data, group == "available"), alpha = 0.3, color=col_points_available)+
  geom_point(data=subset(data, group == "used"), alpha = 0.3, color=col_points_used)+
  xlab("Vertebrate herbivore species richness")+ ylab("Vertebrate herbivore functional diversity")+
  ggtitle("E) Food web space (n=580)")+
  theme_light()
first_plot


#create y-axis histogram
y_density <- axis_canvas(first_plot, axis = "y", coord_flip = TRUE) +
  geom_density(data = data, aes(x = y_variable,fill = group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=palette_points)+
  coord_flip()

#create x-axis histogram
x_density <- axis_canvas(first_plot, axis = "x", coord_flip = TRUE) +
  geom_density(data = data, aes(y = x_variable,fill = group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=palette_points)+
  coord_flip()

# create the combined plot
combined_plot_richness <- insert_yaxis_grob(first_plot, y_density, position = "right")
combined_plot_richness %<>% insert_xaxis_grob(., x_density, position = "top")

# show the result
ggdraw(combined_plot_richness)


####################### human space -------------



## seems to be necessary to log the variables before plotting, otherwise the histogram plotting messes up
used<-cbind.data.frame("x_variable"=log(alldata$GPW), "y_variable" = log(alldata$Footprint), "group" =rep("used", times=length(alldata$bio12)))
head(used)
available<-cbind.data.frame("x_variable"=log(data_eco_cont$HumanPopulationDensity), "y_variable" = log(data_eco_cont$Human.footprint), "group" =rep("available", times=length(data_eco_cont$bio12)))
head(available)

data<-rbind.data.frame(used, available)
summary(data)

#sample size for axes
length(na.omit(used$x_variable))
length(na.omit(used$y_variable))



labels_x<-c(2.06e-09, 0.0000003, 0.00005, 0.007,1, 140)
breaks_x<-c(-20,  -15, -10, -5, 0,  4.941642)

first_plot<-data %>%
  ggplot(aes(x_variable, y_variable)) +
  #stat_hpd_2d(aes(fill = group), prob = 0.8, alpha = 0.2, linetype = "22", size = 1, show.legend=FALSE) +
  #scale_fill_manual(values=c(col_points_available,col_points_used), name = "Data", labels = c("The Arctic", "Evidence points"))+
  geom_point(data=subset(data, group == "available"), alpha = 0.3, color=col_points_available)+
  geom_point(data=subset(data, group == "used"), alpha = 0.3, color=col_points_used)+
  xlab(expression("Human population density" ~(km^-2)))+ ylab("Human footprint index")+
  ggtitle("F) Human space (n=656)")+
  scale_y_continuous(breaks=c(-10, 5),labels=c('Low','High'))+
  scale_x_continuous(breaks=breaks_x,labels=labels_x)+
  theme_light()
first_plot



#create y-axis histogram
y_density <- axis_canvas(first_plot, axis = "y", coord_flip = TRUE) +
  geom_density(data = data, aes(x = y_variable,fill = group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=palette_points)+
  coord_flip()

#create x-axis histogram
x_density <- axis_canvas(first_plot, axis = "x", coord_flip = TRUE) +
  geom_density(data = data, aes(y = x_variable,fill = group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=palette_points)+
  coord_flip()

# create the combined plot
combined_plot_humans <- insert_yaxis_grob(first_plot, y_density, position = "right")
combined_plot_humans %<>% insert_xaxis_grob(., x_density, position = "top")

# show the result
ggdraw(combined_plot_humans)


###### combine to one figure -------------

## geo, climate, climate 2, climate change, food web, human


#legtit<- "Latitude (°)"
#png('Figures/5_contexts.png')
#tiff('Figures/5_contexts.tif')
#tiff('Figures/5_contexts_24032021.tif',height=10,width=9,units = 'in',res=150)
tiff('Figures/5_contexts_26032021.tif',height=10,width=9,units = 'in',res=150)


# grid.arrange(climatespace2+theme(legend.position = c(0.8,0.8))+labs(color=legtit),
#              climchangespace+theme(legend.position="none"),
#              geospace+theme(legend.position="none"),
#              herbivorespace+theme(legend.position=c(0.8,0.2))+theme(legend.title=element_blank()),
#              ncol=2)


print(grid.arrange(combined_plot_geo_space,
                   combined_plot_climate,
                   combined_plot_climate_2,
                   combined_plot_climate_change,
                   combined_plot_richness,
                   combined_plot_humans,
                   ncol=2))

dev.off()
dev.off()

####################### testing additional new variables   -------------------


names(alldata) 
names(data_eco_cont)
# Current.NDVI
# CurrentGrowingSeasonLength
# Temperature.anomaly
# NDVI.trend
# GrowingSeasonLength.trend

used<-cbind.data.frame("x_variable"=alldata$Temperature.anomaly, "y_variable" = alldata$bio1/10, "group" =rep("used", times=length(alldata$bio12)))
available<-cbind.data.frame("x_variable"=data_eco_cont$Temperature.anomaly, "y_variable" = data_eco_cont$bio1/10, "group" =rep("available", times=length(data_eco_cont$bio12)))
#
used<-cbind.data.frame("x_variable"=alldata$Current.NDVI, "y_variable" = alldata$bio1/10, "group" =rep("used", times=length(alldata$bio12)))
available<-cbind.data.frame("x_variable"=data_eco_cont$Current.NDVI, "y_variable" = data_eco_cont$bio1/10, "group" =rep("available", times=length(data_eco_cont$bio12)))
#
used<-cbind.data.frame("x_variable"=alldata$CurrentGrowingSeasonLength, "y_variable" = alldata$bio1/10, "group" =rep("used", times=length(alldata$bio12)))
available<-cbind.data.frame("x_variable"=data_eco_cont$CurrentGrowingSeasonLength, "y_variable" = data_eco_cont$bio1/10, "group" =rep("available", times=length(data_eco_cont$bio12)))
#
used<-cbind.data.frame("x_variable"=alldata$CurrentGrowingSeasonLength, "y_variable" = alldata$Current.NDVI, "group" =rep("used", times=length(alldata$bio12)))
available<-cbind.data.frame("x_variable"=data_eco_cont$CurrentGrowingSeasonLength, "y_variable" = data_eco_cont$Current.NDVI, "group" =rep("available", times=length(data_eco_cont$bio12)))
#
used<-cbind.data.frame("x_variable"=alldata$CurrentGrowingSeasonLength, "y_variable" = alldata$GrowingSeasonLength.trend, "group" =rep("used", times=length(alldata$bio12)))
available<-cbind.data.frame("x_variable"=data_eco_cont$CurrentGrowingSeasonLength, "y_variable" = data_eco_cont$GrowingSeasonLength.trend, "group" =rep("available", times=length(data_eco_cont$bio12)))
#
used<-cbind.data.frame("x_variable"=alldata$Current.NDVI, "y_variable" = alldata$NDVI.trend, "group" =rep("used", times=length(alldata$bio12)))
available<-cbind.data.frame("x_variable"=data_eco_cont$Current.NDVI, "y_variable" = data_eco_cont$NDVI.trend, "group" =rep("available", times=length(data_eco_cont$bio12)))
#
used<-cbind.data.frame("x_variable"=alldata$GrowingSeasonLength.trend, "y_variable" = alldata$Temperature.anomaly, "group" =rep("used", times=length(alldata$bio12)))
available<-cbind.data.frame("x_variable"=data_eco_cont$GrowingSeasonLength.trend, "y_variable" = data_eco_cont$Temperature.anomaly, "group" =rep("available", times=length(data_eco_cont$bio12)))
#
used<-cbind.data.frame("x_variable"=alldata$north_of_treeline, "y_variable" = alldata$elevation_DEM, "group" =rep("Evidence base", times=length(alldata$bio12)))
available<-cbind.data.frame("x_variable"=data_eco_cont$NorthofTreeline, "y_variable" = data_eco_cont$Elevation, "group" =rep("Study area", times=length(data_eco_cont$bio12)))
data<-rbind.data.frame(available, used)


data<-rbind.data.frame(used, available)
#

legend_title <- "Data source"
first_plot<-data %>%
  ggplot(aes(x=x_variable, y=y_variable, group=group))+
  geom_point(aes(color=group), alpha = 0.3, show.legend = TRUE)+
  scale_fill_manual(values=c(col_points_available,col_points_used))+
  scale_color_manual(legend_title, values=c("Evidence base"=col_points_used,"Study area"=col_points_available))+
  ggtitle("A) Geographic space")+
  xlab("Distance to coast (km, n=661)")+ ylab("Elevation (m, n=623)")+
  theme_light()+
  theme(legend.position = c(0.8, 0.8))
first_plot  

#create y-axis histogram
y_density <- axis_canvas(first_plot, axis = "y", coord_flip = TRUE) +
  geom_density(data = data, aes(x = y_variable,fill = group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=palette_points)+
  coord_flip()

#create x-axis histogram
x_density <- axis_canvas(first_plot, axis = "x", coord_flip = TRUE) +
  geom_density(data = data, aes(y = x_variable,fill = group), color = NA, alpha = 0.5) +
  scale_fill_manual(values=palette_points)+
  coord_flip()

# create the combined plot
combined_plot <- insert_yaxis_grob(first_plot, y_density, position = "right")
combined_plot %<>% insert_xaxis_grob(., x_density, position = "top")
ggdraw(combined_plot)



# show the result
tiff('Figures/current_growing_season_MAT.tif',height=5,width=5,units = 'in',res=150)
ggdraw(combined_plot)
dev.off()
dev.off()

####################### various versions before final figure  -------------------


names(alldata) 
names(data_eco_cont)
#x=bio12, y=bio1/10

used<-cbind.data.frame("MAP"=alldata$Current.NDVI, "MAT" = alldata$bio1/10, "group" =rep("used", times=length(alldata$bio12)))
head(used)
available<-cbind.data.frame("MAP"=data_eco_cont$Current.NDVI, "MAT" = data_eco_cont$bio1/10, "group" =rep("available", times=length(data_eco_cont$bio12)))
head(available)



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

# ## kernel on background
# first_plot<-data %>%
#   ggplot(aes(MAP, MAT)) +
#   stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = group), bins = 100, show.legend=FALSE) +
#  scale_alpha_continuous(range = c(0, 1))+
#  scale_fill_manual(values=c(col_kernel_available,col_kernel_used))+
#   geom_point(data=subset(data, group == "available"), alpha = 3/10, color=col_points_available)+
#   geom_point(data=subset(data, group == "used"), alpha = 3/10, color=col_points_used)+
#   xlab("MAP (mm)")+ ylab(expression('MAT ' (degree~C)))+
#   theme_light()
# first_plot


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


####################### statistics to results  -------------


## NDVI 
mean(na.omit(data_eco_cont$Current.NDVI)) 
mean(na.omit(alldata$Current.NDVI)) 
quantile(data_eco_cont$Current.NDVI, c(.25, .50, .90), na.rm=TRUE) 
quantile(alldata$Current.NDVI, c(.25, .50, .90), na.rm=TRUE) 

## soil types
names(alldata)
names(data_eco_cont)
table(alldata$soil_type.)
table(data_eco_cont$Soil.Type)

## permafrost
names(alldata)
names(data_eco_cont)
table(alldata$permafrost)
summary(alldata$permafrost)
summary(as.factor(data_eco_cont$Permafrost))
par(mfrow=c(1,2))
hist(data_eco_cont$Permafrost, main="arctic")
hist(alldata$permafrost, main="evidence base")



