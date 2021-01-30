#####################################################################
#       Systematic map of herbivory in Arctic tundra
#                    ROSES diagram
#                   Isabel C Barrio
#                    4-October-2020
#####################################################################

#Script to import data from excel spreadsheet and basic data description
#to build a ROSES diagram and some summary of theses (end of script)

#libraries----
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)


#data import and wrangling ----
#data taken from Eeva's UiT Box account and read in using a direct download link
#the file is called "Overview_all_scored.xls"

#download overview data from box
#link2coded_data <- paste(tempfile(),".xlsx",sep = "")
#download.file("https://uitno.box.com/s/p9iuqrl59hoqcquinv2dtbbtxqp7mvv0", link2coded_data, mode = "wb")

#read in sheet with data
#overview <- read_excel(link2coded_data, sheet = 'all entries')

#I tried downloading the file directly from Box using the code above, but could not make it work... JAMES? :)
#instead, I just saved the file in my computer and import from there

#set the working directory
setwd("C:/Users/isabel/OneDrive - Landbúnaðarháskóli Íslands/ISABEL/HERBIVORY NETWORK/HN systematic map")
overview <- read_excel("Overview_all_scored.xlsx", sheet="all entries", range=cell_cols("A:AD"))

#check
View(overview) #I got some warning messages when importing the dataset but it seems fine?


#Total number of documents
length(levels(as.factor(overview$ID_Eeva))) #3586

#Total number of documents by source
overview %>%
  group_by(search_source) %>% 
  count()  #Google Scholar = 75, Local search = 311, WoS_Scopus = 3200

#number of duplicate documents that had to be removed
score <- overview %>%
            group_by(current_score) %>% 
            count()

#number of duplicate documents            
score[score$current_score == "remove_duplicate", ]$n  #684
  #number of documents after removing duplicates
  length(levels(as.factor(overview$ID_Eeva))) - score[score$current_score == "remove_duplicate", ]$n  #2902

#number of documents excluded at the title stage            
score[score$current_score == "exclude_title", ]$n  #1332
  #number of documents after title screening
  length(levels(as.factor(overview$ID_Eeva))) - score[score$current_score == "remove_duplicate", ]$n -
                                                score[score$current_score == "exclude_title", ]$n  #1570

#number of documents excluded at the abstract stage            
score[score$current_score == "exclude_abstract", ]$n  #670
  #number of documents after title screening
  length(levels(as.factor(overview$ID_Eeva))) - score[score$current_score == "remove_duplicate", ]$n -
                                                score[score$current_score == "exclude_title", ]$n -
                                                score[score$current_score == "exclude_abstract", ]$n  #900

#number of unretrievable full texts            
score[score$current_score == "exclude_no_full_text", ]$n  #44
  #number of documents after title screening
  length(levels(as.factor(overview$ID_Eeva))) - score[score$current_score == "remove_duplicate", ]$n -
                                                score[score$current_score == "exclude_title", ]$n -
                                                score[score$current_score == "exclude_abstract", ]$n -
                                                score[score$current_score == "exclude_no_full_text", ]$n  #856

#there are several categories that refer to unsuitable document types:
#correction to published articles, raw datasets, maps and supplementary materials
sum(score[score$current_score %in% c("remove_correction","remove_dataset",
                                 "remove_map","remove_suppplement"), ]$n)  #15

#studies excluded based on locality (both exclude_site_coordinates and exclude_site_text)
sum(score[score$current_score %in% c("exclude_site_coordinates","exclude_site_text"), ]$n)  #88


#total number of studies excluded at full stage phase
sum(score[score$current_score %in% c("remove_correction","remove_dataset",
                                 "remove_map","remove_suppplement", "exclude_redundant",
                                 "exclude_site_coordinates","exclude_site_text",
                                 "exclude_full_text"), ]$n)  #536

#number of documents after full text screening
  length(levels(as.factor(overview$ID_Eeva))) - score[score$current_score == "remove_duplicate", ]$n -
                                                score[score$current_score == "exclude_title", ]$n -
                                                score[score$current_score == "exclude_abstract", ]$n -
                                                score[score$current_score == "exclude_no_full_text", ]$n -
                                                sum(score[score$current_score %in% c("remove_correction","remove_dataset",
                                                       "remove_map","remove_suppplement", "exclude_redundant", 
                                                       "exclude_site_coordinates","exclude_site_text",
                                                       "exclude_full_text"), ]$n)  #320
  #this should be the same as 
    score[score$current_score == "include", ]$n
    
#reasons for excluding at full text stage
score[score$current_score == "exclude_redundant", ]$n #8
full_text_excl <- overview %>% filter(current_score == "exclude_full_text") %>% 
                      group_by(full_text_exclusion) %>% 
                      count()
full_text_excl[full_text_excl$full_text_exclusion == "no_population", ]$n  #55
full_text_excl[full_text_excl$full_text_exclusion == "no_exposure", ]$n  #24
full_text_excl[full_text_excl$full_text_exclusion == "no_comparator", ]$n  #11
sum(full_text_excl[full_text_excl$full_text_exclusion %in% c("no_outcome","no_outcome_data"), ]$n)  #315
  #this category includes studies that did not measure an effect of herbivores on plants
  #and studies that did not include primary data (e.g. synthesis papers, reviews, conceptual papers)
full_text_excl[full_text_excl$full_text_exclusion == "no_study_design", ]$n  #20



#theses----
theses <- read_excel("Overview_all_scored.xlsx", sheet="theses", range=cell_cols("A:AF"))
length(levels(as.factor(theses$ID_Eeva))) #39 theses

#Total number of theses by type
theses %>%
  group_by(thesis) %>% 
  count()  #BSc 4, MSc 15, PhD 20

#Total number of theses by country
theses %>%
  group_by(country) %>% 
  count()  #Canada: 13, Sweden:13, Iceland:4, Norway:3, US:3, UK:2, Denmark:1, Germany:1




##compare dataset with separate evidence points
#from James' script putting all data together
AllCodedDataW <- read.csv("C:/ISABEL/R/ArcticHerbivorySystematicMap/Data/AllCodedDataW.txt", sep=";")

#Number of studies
length(levels(as.factor(AllCodedDataW$title)))  #315
#Number of evidence points
length(levels(as.factor(AllCodedDataW$evidence_point_ID)))  #678


