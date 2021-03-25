#Systematic map of herbivory in Arctic tundra
# script for producing figures 2 and 3


# The script takes in a data file "AllCodedData_withGIScontext.csv"
# 1) filtered away all redundant datapoints
# 2) filtered away points outside arctic study area
# 3) added all GIS-context variables (or NA for evidence points that cannot have them for various reasons)

# the script also takes in a separatae data file on plant functional groups "PFTs_Systematic_Herbivory_Map_03022021.txt"


# load packages ---------------------------------------------------------

library(ggplot2)  # plotting
library(gridExtra) # plotting: arranging multiple grops on a page
#library(RColorBrewer)#Colours 
library(tidyverse) #data wrangling
library(networkD3) # for sankey diagrams
library(harrypotter)# color palettes
library(htmlwidgets) # to add title in sankeydiagram

# Load data  -----------------------------------------------
# Take in data that has redundant evidence points removed, and a spatial filtering re-done

alldata<-read.table("Data/AllCodedDataW.txt", sep=";", header=TRUE)
head(alldata)
#dim(alldata)[1]
#toto<-alldata[alldata$evidence_point_ID=="3540_a",]

plants<-read.delim("Data/PFTs_Systematic_Herbivory_Map_03022021.txt")
head(plants)  


# Figure 2 ---------------------------------------------------------

####Year of publication - panel A
# sample size
length(alldata$year[!is.na(alldata$year)]) # publication year

pub<-ggplot(alldata,aes(x=as.numeric(year)))+geom_histogram()+ggtitle("A) Publication year (n=662)")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x  = element_text(size=16),
        axis.text.y =element_text(size=16)) + theme(plot.title = element_text(size=16))+
  theme(plot.margin=unit(c(0.2,0.4,0.4,0.1),"cm"))

pub

####Year of study start - panel B
# sample size
alldata$year_start[alldata$year_start=="not available"]<-NA
alldata$year_start[alldata$year_start=="not relevant"]<-NA
alldata$year_start[alldata$year_start=="not reported" ]<-NA
length(alldata$year_start[!is.na(alldata$year_start)]) # study start year


startyr<-ggplot(alldata,aes(x=as.numeric(as.character(year_start))))+geom_histogram()+ggtitle("B) First year (n=594)")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x  = element_text(size=16),
        axis.text.y =element_text(size=16)) + theme(plot.title = element_text(size=16))+
  theme(plot.margin=unit(c(0.2,0.2,0.4,0.4),"cm"))
startyr

#####Extent of temporal scale - panel C
# the study with more than 1000 years temporal scale is a modeling study that simulates current (and future) climate for 1500 years
# sample size
alldata$extent_of_temporal_scale[alldata$extent_of_temporal_scale=="not relevant"]<-NA
alldata$extent_of_temporal_scale[alldata$extent_of_temporal_scale=="not reported" ]<-NA
length(alldata$extent_of_temporal_scale[!is.na(alldata$extent_of_temporal_scale)]) # study length

tr1<-ggplot(alldata,aes(x=as.numeric(as.character(extent_of_temporal_scale))))+geom_histogram()+scale_x_continuous(trans='log10')+
  ggtitle("C) Temporal extent (n=615)") + xlab("Years")  +ylab("")+ theme(axis.title.x = element_text(size=16),
                                                                                            axis.text.x  = element_text(size=16),
                                                                                            axis.text.y =element_text(size=16)) + theme(plot.title = element_text(size=16))+
  theme(plot.margin=unit(c(0.4,0.4,0.2,0.1),"cm"))

tr1

#####Temporal resolution  - panel D
# sample size
table(alldata$temporal_resolution)
alldata$temporal_resolution[alldata$temporal_resolution=="not reported" ]<-NA
length(alldata$temporal_resolution[!is.na(alldata$temporal_resolution)]) # study start year


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
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ ggtitle("D) Temporal resolution (n=635)") +
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
levels(as.factor(herb$herbivore_type))
herb$herbivore_type<-as.factor(herb$herbivore_type)
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
head(plants)

# change yes and no in plant data to zero and one
plants <-
  mutate(plants, 
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

data_plants<-plants %>% select(evidence_point_ID, 
                                    evergreen_dwarf_shrubs,decidious_dwarf_shrubs,decidious_tall_shrubs,
                                    evergreen_tall_shrubs,  decidious_trees,evergreen_trees,graminoids,
                                    forbs,ferns_and_allies,bryophytes, vascular_plant_community,lichens)
head(data_plants)


plantsL<-
  data_plants %>%
  pivot_longer(!c("evidence_point_ID"), names_to = "plants", values_to = "count_plants")
head(plantsL)

# merge plants and herbivores
data_fig_3A<-merge(herbLnew, plantsL) 

#remove all lines with zero plant data
data_fig_3A<-filter(data_fig_3A, count_plants== 1) 
head(data_fig_3A)


#rename herbivores and plants
levels(as.factor(data_fig_3A$herbivores))
data_fig_3A$herbivores<-as.factor(data_fig_3A$herbivores)
levels(data_fig_3A$herbivores)[levels(data_fig_3A$herbivores)=="small_rodents_and_pikas"] <- "small rodents, pikas"
levels(data_fig_3A$herbivores)[levels(data_fig_3A$herbivores)=="other_invertebrates"] <- "other invertebrates"
levels(data_fig_3A$herbivores)[levels(data_fig_3A$herbivores)=="defoliating_invertebrates"] <- "defoliating invertebrates"


levels(as.factor(data_fig_3A$plants))
data_fig_3A$plants<-as.factor(data_fig_3A$plants)
levels(data_fig_3A$plants)[levels(data_fig_3A$plants)=="decidious_dwarf_shrubs"] <- "deciduous dwarf shrubs"
levels(data_fig_3A$plants)[levels(data_fig_3A$plants)=="decidious_tall_shrubs"] <- "deciduous tall shrubs"
levels(data_fig_3A$plants)[levels(data_fig_3A$plants)=="decidious_trees"] <- "deciduous trees"
levels(data_fig_3A$plants)[levels(data_fig_3A$plants)=="evergreen_dwarf_shrubs"] <- "evergreen dwarf shrubs"
levels(data_fig_3A$plants)[levels(data_fig_3A$plants)=="evergreen_tall_shrubs"] <- "evergreen tall shrubs"
levels(data_fig_3A$plants)[levels(data_fig_3A$plants)=="evergreen_trees"] <- "evergreen trees"
levels(data_fig_3A$plants)[levels(data_fig_3A$plants)=="ferns_and_allies"] <- "ferns and allies"
levels(data_fig_3A$plants)[levels(data_fig_3A$plants)=="vascular_plant_community"] <- "vascular plant community"



# summerize so that count is sum across the whole dataset, get rid of evid
data_sankeyL<-data_fig_3A %>% select(c(!evidence_point_ID))
links<-aggregate(data_sankeyL$count_plants,list(herbivore = data_sankeyL$herbivores, plant = data_sankeyL$plants),sum)
names(links)<-c("source", "target", "value")
head(links)

# order first by herbivore to get nodes in the order you want
# Can the herbivores in the first column be ordered by «other vertebrates» (with the different species), then small rodents and pikas, waterfowl, defoliating inverts and unknown? Are the «other invertebrates» (shown in panels B and C) missing from this panel?
table(data_fig_3A$herbivores) # check that you take "other herbivores" in order of abundance

desired_order_h <- c("Rangifer", "Lagopus", "Lepus", "Ovibos", "Ovis", "Alces", "small rodents, pikas", "waterfowl", "defoliating invertebrates", "other invertebrates", "unknown" )
# Re-order the levels
links$source <- factor( as.character(links$source), levels=desired_order_h )
# Re-order the data.frame
links <- links[order(links$source),]

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% 
    unique()
)

nodes_plants<-nodes[12:23,]
nodes_herbivores<-desired_order_h
nodes<-data.frame(name=c(nodes_herbivores, nodes_plants))
str(nodes)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1


## color alternatives to play with
pal <- hp(n = 8, house = "Sprout")
image(volcano, col = pal)
pal

pal <- hp(n = 11, house = "Ravenclaw")
image(volcano, col = pal)
pal

pal <- hp(n = 10, house = "Mischief")
image(volcano, col = pal)
pal

  
color_scale <- 
  "d3.scaleOrdinal()
.range(['#e7e3af', '#daca92','#ceb176','#b18e5b','#8e6841','#774c2e','#6d3b22','#672c19','#6e2219','#761919','#808080',
'#76653b', '#bcb259', '#eaeb6d','#cee363','#b3d759','#9bc750','#89b14b','#7b9948']);
"

# Make the Network. 
p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name", 
                   fontSize = 15, fontFamily = "Arial",
                   nodeWidth=40, nodePadding = 10,
                   iterations = 0,
                  colourScale=color_scale,
                  width=700, height=690)

p <- htmlwidgets::prependContent(p, htmltools::tags$h1("A) Herbivore groups and plant groups", style="font-family: Arial; font-size:20px"))
p



# Figure 3B --------------------------------------------------------
# use same grouping for herbivores as done above 

# data for plants
names(alldata)
data_plants<-as.data.frame(alldata) %>% select(evidence_point_ID, biological_organization_level_reported, biological_organization_reported_comments)
head(data_plants)  
data_plants$biological_organization_level_reported<-as.factor(data_plants$biological_organization_level_reported)
levels(as.factor(data_plants$biological_organization_level_reported))

# modify level "groups of species" so its not interpreted as species
levels(data_plants$biological_organization_level_reported)[levels(data_plants$biological_organization_level_reported)=="groups of species"] <- "sp_groups"

# create new variables per plant group by combining data across columns
data_plants$individual <- ifelse(grepl("individual" , data_plants$biological_organization_level_reported) , 1 , 0)
data_plants$individual <- ifelse(grepl("individual" , data_plants$biological_organization_reported_comments) , 1 , data_plants$individual)
#
data_plants$pop_sp <- ifelse(grepl("population/species" , data_plants$biological_organization_level_reported) , 1 , 0)
data_plants$pop_sp <- ifelse(grepl("population/species" , data_plants$biological_organization_reported_comments) , 1 , data_plants$pop_sp)
data_plants$pop_sp <- ifelse(grepl("species" , data_plants$biological_organization_reported_comments) , 1 , data_plants$pop_sp)
#
data_plants$sp_groups <- ifelse(grepl("sp_groups" , data_plants$biological_organization_level_reported) , 1 , 0)
data_plants$sp_groups <- ifelse(grepl("sp_groups" , data_plants$biological_organization_reported_comments) , 1 , data_plants$sp_groups)
#
data_plants$community <- ifelse(grepl("community" , data_plants$biological_organization_level_reported) , 1 , 0)
data_plants$community <- ifelse(grepl("community" , data_plants$biological_organization_reported_comments) , 1 , data_plants$community)
#
data_plants$other <- ifelse(grepl("other" , data_plants$biological_organization_level_reported) , 1 , 0)


head(data_plants)
data_plants<-data_plants %>% select(evidence_point_ID, individual, pop_sp, sp_groups, community, other); head(data_plants)

plantsL<-
  data_plants %>%
  pivot_longer(!c("evidence_point_ID"), names_to = "plants", values_to = "count_plants")
head(plantsL)

#data for herbivores = herbLnew from figure 3A

# merge plants and herbivores
data_fig_3B<-merge(herbLnew, plantsL) 

#remove all lines with zero plant data
data_fig_3B<-filter(data_fig_3B, count_plants== 1) 


data_fig_3B$herbivores<-as.factor(data_fig_3B$herbivores)
data_fig_3B$plants<-as.factor(data_fig_3B$plants)

# re-name herbivore levels
levels(data_fig_3B$herbivores)[levels(data_fig_3B$herbivores)=="defoliating_invertebrates"] <- "defoliating inv."
levels(data_fig_3B$herbivores)[levels(data_fig_3B$herbivores)=="small_rodents_and_pikas"] <- "small rodents"
levels(data_fig_3B$herbivores)[levels(data_fig_3B$herbivores)=="other_invertebrates"] <- "other inv." 

# re-order herbivore variable
data_fig_3B$herbivores<-fct_relevel(data_fig_3B$herbivores, 
                                   "Rangifer", "small rodents", "waterfowl", "defoliating inv.","Lagopus", "Ovibos", 
                                   "Lepus", "Ovis", "Alces","other inv.","unknown")

# re-name plant levels
levels(data_fig_3B$plants)[levels(data_fig_3B$plants)=="pop_sp"] <- "population/species"
levels(data_fig_3B$plants)[levels(data_fig_3B$plants)=="sp_groups"] <- "groups of species"

# re-order plant variable
data_fig_3B$plants<-fct_relevel(data_fig_3B$plants, "individual", "population/species", "groups of species", "community", "other")

# color palette for panel B
hp_color_manual_3B <- c( "#006699FF",  "#72B6D9FF",  "#B35900FF",  "#D99E66FF", "#B3B8B3FF")
dim(data_fig_3B)[1]

ct2<-ggplot(data_fig_3B, aes(fill = plants, x = herbivores)) + geom_bar(aes(fill=plants))+
  scale_fill_manual("Plant level", values=hp_color_manual_3B)+
  ggtitle("B) Herbivore type and plant level (n=912)")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=16),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y =element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text( size = 14))
ct2

# Figure  3C ---------------------------------------------------------

data_studies<-as.data.frame(alldata) %>% select(evidence_point_ID, study_design,study_design_comments, study_method, study_method_comments)
head(data_studies) 
table(data_studies$study_design) # no several, other etc categories
table(data_studies$study_method) # 3 studies with "several", add "manually"
data_studies[data_studies$study_method=="several",]

# remove from data_studies rows with "several" in study_method
data_studies_2<-data_studies[data_studies$study_method!="several",]

# add to data_studies rows where data for studies with several approaches is  edited
edit<-data_studies[data_studies$study_method=="several",]
edit$study_method<-rep("field", times=3)
edit2<-edit
edit2$study_method<-c("remote sensing", "modelling", "remote sensing")
edit<-rbind.data.frame(edit, edit2)
data_studies<-rbind.data.frame(data_studies_2, edit)



data_studies$study_design<-as.factor(data_studies$study_design)
levels(data_studies$study_design)[levels(data_studies$study_design)=="quasi-experimental"] <- "observational"
data_studies$study_type<-paste(data_studies$study_design, data_studies$study_method, sep=", ")
data_studies$study_type<-as.factor(data_studies$study_type)
data_studies<-data_studies[,c(1,6)] #select only evidence point ID and study type
head(data_studies)
table(data_studies$study_type)

#observational, modeling??
data_studies[data_studies$study_type=="observational, modelling",]
alldata[alldata$evidence_point_ID=="290_a",]
data_studies[data_studies$evidence_point_ID=="290_a",]
levels(data_studies$study_type)[levels(data_studies$study_type)=="observational, modelling"] <- "modelling"
levels(data_studies$study_type)[levels(data_studies$study_type)=="modelling, modelling"] <- "modelling"
table(data_studies$study_type)


# merge study type and herbivores
data_fig_3C<-merge(herbLnew, data_studies) 
head(data_fig_3C)

# edit herbivore data
data_fig_3C$herbivores<-as.factor(data_fig_3C$herbivores)
levels(data_fig_3C$herbivores)[levels(data_fig_3C$herbivores)=="defoliating_invertebrates"] <- "defoliating inv."
levels(data_fig_3C$herbivores)[levels(data_fig_3C$herbivores)=="small_rodents_and_pikas"] <- "small rodents"
levels(data_fig_3C$herbivores)[levels(data_fig_3C$herbivores)=="other_invertebrates"] <- "other inv."

levels(data_fig_3C$herbivores)
table(data_fig_3C$herbivores)


# re-order variables  
data_fig_3C$herbivores<-fct_relevel(data_fig_3C$herbivores, 
                                   "Rangifer", "small rodents", "waterfowl", "defoliating inv.","Lagopus", "Ovibos", 
                                   "Lepus", "Ovis", "Alces","other inv.","unknown")
#data_fig_3C$herbivores<-fct_relevel(data_fig_3C$herbivores, 
#                                   "Rangifer", "small rodents", "waterfowl", "defoliating inv.","Lagopus", "Ovibos", 
#                                   "Lepus", "Ovis", "Alces","unknown")

data_fig_3C$study_type<-fct_relevel(data_fig_3C$study_type, 
                                   "experimental, field", "experimental, remote sensing", "experimental, greenhouse" ,
                                   "modelling",                  
                                   "observational, field","observational, remote sensing","observational, other")


# color palette for panel C
hp_color_manual_3C <- c( "#B35900FF", "#D3771CFF", "#D99E66FF" , "#B3B8B3FF" ,"#006699FF" ,"#369BCEFF", "#72B6D9FF")
levels(data_fig_3C$study_type)
dim(data_fig_3C)[1]

ct1<-ggplot(data_fig_3C, aes(fill = study_type, x = herbivores)) + geom_bar(aes(fill=study_type))+
  scale_fill_manual("Study type", values=hp_color_manual_3C)+
  ggtitle("C) Herbivore type and study type (n=849)")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=16),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y =element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text( size = 14)
  )
#  theme(plot.margin=unit(c(0.4,0.2,0.2,0.4),"cm"))
ct1

# Figure combine figure 3 A and B ---------------------------------------------------------


pdf('Figures/Figure3_BC.pdf',height=6,width=14)
tiff('Figures/Figure3_BC.tif',height=6,width=14,units = 'in',res=150)
grid.arrange(ct2,ct1,ncol=2)
dev.off()

