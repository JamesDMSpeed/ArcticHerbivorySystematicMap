#Trialing systematic map visualisation
library(readxl)
#Data import
coding1<-read_excel('TestVisualisation in Yamal\\data_coding_HwangB.xlsx',
                    sheet='CODING_template')
coding2<-read_excel('TestVisualisation in Yamal\\data_coding_template Macek.xlsx',
                    sheet='CODING_template')
coding3<-read_excel('TestVisualisation in Yamal\\data_coding_template_Karen_M_Mathisen.xlsx',
                    sheet='CODING_template')
View(coding1)
View(coding2)
View(coding3)

#Merging together the data examples that we have
coding_complete<-cbind(coding1,coding2[,6:ncol(coding2)],coding3[,6:ncol(coding3)])
View(coding_complete)

transposed_dataset<-as.data.frame(t(coding_complete[,c(3,6:ncol(coding_complete))]))
View(transposed_dataset)
colnames(transposed_dataset)<-coding_complete$`Coding variable`
named_dataset<-droplevels(transposed_dataset[2:nrow(transposed_dataset),])
View(named_dataset)

#Check empty study vectors in import above

#Get coordinates
coords<-cbind(named_dataset$coordinates_E,named_dataset$coordinates_N)
#Here we need a big job standardising coordinates

#Plot start year against duration
par(mar=c(5,5,1,1))
plot(as.numeric(as.character(named_dataset$year_start)),
  as.numeric(as.character(named_dataset$year_end))-as.numeric(as.character(named_dataset$year_start)),
  xlab='Start year',ylab='Duration (years)',las=1,pch=16)
abline(lm((as.numeric(as.character(named_dataset$year_end))-as.numeric(as.character(named_dataset$year_start)))~
            as.numeric(as.character(named_dataset$year_start))),
       col=grey(0.5),lwd=2)
 # col=(named_dataset$country))
#legend('topr',pch=16,col=levels(named_dataset$country),legend=levels(named_dataset$country))

#Plot year start against spatial scale
#levels(named_dataset$extent_of_spatial_scale)<-c('1x1km','100x100km','10x10km','not relevant')
#named_dataset$extent_of_spatial_scale<-factor(named_dataset$extent_of_spatial_scale,
#                                              levels=c('1x1km ','10x10km ','100x100km', 'not relevant'))
par(mar=c(3,12,1,1))
boxplot(as.numeric(as.character(named_dataset$year_start))
        ~named_dataset$extent_of_spatial_scale,
        horizontal=T,las=1,cex.lab=0.5,
        names=rev(c('not relevant','1x1km-10x10km','100x100km-1000x1000km','1x1km or less')))

#Plot year start against study type
par(mar=c(3,10,1,1))
boxplot(as.numeric(as.character(named_dataset$year_start))
        ~named_dataset$study_design,
        horizontal=T,las=1,cex.lab=0.5)

#Plot herbivore type and spatial scale
tab1<-tapply(named_dataset$year_start,list(named_dataset$herbivore_type,named_dataset$extent_of_spatial_scale),length)
par(mar=c(7,5,1,1))
tab1[is.na(tab1)]<-0
barplot(t(tab1),legend=T,las=2,cex.names = 0.6,ylab='N Studies',
        args.legend = list(x='right',cex=0.55))


#Stydy type against herbivore type
par(mar=c(3,5,1,1))
mosaicplot(~study_design+herbivore_type,data=named_dataset)#This is count of studys in combinations

tab1<-tapply(named_dataset$year_start,list(named_dataset$herbivore_type,named_dataset$study_design),length)
tab1[is.na(tab1)]<-0
par(mar=c(6,3,1,1))
barplot((tab1),legend=T,las=2,cex.names = 0.5,col=1:max(dim(tab1)),args.legend=list('topr'))

#Country and herbiore type
tab1<-tapply(named_dataset$year_start,list(named_dataset$herbivore_type,named_dataset$country),length)
tab1[is.na(tab1)]<-0
par(mar=c(7,5,1,1))
barplot(t(tab1),legend=T,ylab='N Studies',las=2,cex.names = 0.6,col = 1:max(dim(tab1)),args.legend=list(ncol=2))
