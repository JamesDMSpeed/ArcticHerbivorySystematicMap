#Systematic map of herbivory in Arctic tundra
# script for producing figures 2 and 3


# The script takes in a data file that has 
# 1) filtered away all redundant datapoints
# 2) filtered away points outside arctic study area
# 3) added all GIS-context variables (or NA for evidence points that cannot have them for various reasons)

# the script also takes in a separatae data file on plant functional groups

### NOTES FROM EEVA TO JAMES --------------------------------------------------------

# I chose file "AllCodedData_withGIScontext.csv". It works here, but see comment in script 02.


# load packages ---------------------------------------------------------

library(ggplot2)  # plotting
library(gridExtra) # plotting: arranging multiple grops on a page
library(RColorBrewer)#Colours 
library(tidyverse) #data wrangling
library(networkD3) # for sankey diagrams
library(harrypotter)# color palettes

# Load data  -----------------------------------------------
# Take in data that has redundant evidence points removed, and a spatial filtering re-done

alldata<-read.table("Data/AllCodedDataW.txt", sep=";", header=TRUE)
head(alldata)
dim(alldata)[1]

# Figure 2 ---------------------------------------------------------

## sample sizes to figures
length(alldata$year[!is.na(alldata$year)])
length(alldata$year[!is.na(alldata$year_start)])
length(alldata$year[!is.na(alldata$extent_of_temporal_scale)])
length(alldata$year[!is.na(alldata$temporal_resolution)])


#Year of publication - panel A
pub<-ggplot(alldata,aes(x=as.numeric(year)))+geom_histogram()+ggtitle("A) Publication year (n=678)")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x  = element_text(size=16),
        axis.text.y =element_text(size=16)) + theme(plot.title = element_text(size=16))+
  theme(plot.margin=unit(c(0.2,0.4,0.4,0.1),"cm"))

pub

#Year of study start - panel B

alldata$year_start[alldata$year_start=="not available"]<-NA
alldata$year_start[alldata$year_start=="not relevant"]<-NA
alldata$year_start[alldata$year_start=="not reported" ]<-NA

startyr<-ggplot(alldata,aes(x=as.numeric(as.character(year_start))))+geom_histogram()+ggtitle("B) First year (n=610)")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x  = element_text(size=16),
        axis.text.y =element_text(size=16)) + theme(plot.title = element_text(size=16))+
  theme(plot.margin=unit(c(0.2,0.2,0.4,0.4),"cm"))
startyr

#Extent of temporal scale - panel C
# the study with more than 1000 years temporal scale is a modeling study that simulates current (and future) climate for 1500 years
# the study with length = zero is now corrected in the raw data. it was a study with start year = end year, and should therefore have had one as study length
levels(alldata$extent_of_temporal_scale)[levels(alldata$extent_of_temporal_scale)=="0"] <- "1"

tr1<-ggplot(alldata,aes(x=as.numeric(as.character(extent_of_temporal_scale))))+geom_histogram()+scale_x_continuous(trans='log10')+
  ggtitle("C) Temporal extent (n=672)") + xlab("Years")  +ylab("")+ theme(axis.title.x = element_text(size=16),
                                                                                            axis.text.x  = element_text(size=16),
                                                                                            axis.text.y =element_text(size=16)) + theme(plot.title = element_text(size=16))+
  theme(plot.margin=unit(c(0.4,0.4,0.2,0.1),"cm"))

tr1

#Temporal resolution  - panel D
# the study with temporal_resolution  = "twice" is now corrected in the raw data. it was a mistake, we have no level "twice"
# the edit below is therefore redundant
levels(alldata$temporal_resolution)[levels(alldata$temporal_resolution)=="twice"] <- "twice, interval longer than one year"

# rename factor levels shorter to read

levels(alldata$temporal_resolution)[levels(alldata$temporal_resolution)=="twice, interval one year or shorter"] <- "twice <= 1 yr"
levels(alldata$temporal_resolution)[levels(alldata$temporal_resolution)=="twice, interval longer than one year"] <- "twice > 1 yr"
levels(alldata$temporal_resolution)[levels(alldata$temporal_resolution)=="regular with intervals shorter than a year"] <- "regular < 1 yr"
levels(alldata$temporal_resolution)[levels(alldata$temporal_resolution)=="regular with intervals longer than a year"] <- "regular > 1 yr"
levels(alldata$temporal_resolution)[levels(alldata$temporal_resolution)=="irregular with intervals shorter than a year"] <- "irregular < 1 yr"
levels(alldata$temporal_resolution)[levels(alldata$temporal_resolution)=="irregular with intervals longer than a year"] <- "irregular > 1 yr"

# relevel factor
alldata$temporal_resolution<-fct_relevel(alldata$temporal_resolution, "once", 
                                                                    "twice <= 1 yr", "regular < 1 yr", "irregular < 1 yr",
                                                                    "twice > 1 yr", "regular > 1 yr", "irregular > 1 yr",
                                                                    "annual","not reported")

tr2<-ggplot(alldata,aes(x=temporal_resolution))+geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ ggtitle("D) Temporal resolution (n=678)") +
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
herb<-cbind.data.frame(alldata$evidence_point_ID,
                       alldata$herbivore_type,
                       alldata$herbivore_type_comments,
                       alldata$herbivore_identity)
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

data_fig_3<-as.data.frame(alldata) %>% select(evidence_point_ID, study_design, experimental_design, study_method, exposure_quantification, biological_organization_level_reported)
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
