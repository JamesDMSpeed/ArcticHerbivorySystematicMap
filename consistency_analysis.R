############################
# Consistency analysis: how consistently did two reviewers code the same paper?
# Elina Kaarlejärvi
# 13 Nov 2020
#############


#Script to import coded data from excel spreadsheet.
#Data taken from Eeva's UiT Box account and read in using a direct download link
#Data is in file 'Overview_all_scored.xlsx' on sheet 'all entries'

library(tidyverse)
library(readxl)#Read in xls from box
library(irr) #Cohen's kappa and other stats

#Data import
--------------------------------------------------------
  
#Download coded data from box
link2coded_data <- paste(tempfile(),".xlsx",sep = "")
download.file("https://uitno.box.com/s/p9iuqrl59hoqcquinv2dtbbtxqp7mvv0", link2coded_data, mode = "wb")

#Read in the sheet with all scored publications
overview_all_scored <- read_excel(link2coded_data, sheet = "all entries") #if this does not work -> download "Overview_all_scored.xlsx" to local disk & read in manually:
scored <- read_excel(file.choose(), sheet = "all entries")

str(scored)
head(scored)


#Data cleaning and wrangling
#-----------------------------
#remove duplicates
scored_unique <- scored %>% filter(current_score != "remove_duplicate") #2902 records
  
scored_unique <- scored_unique %>%
  mutate_at(c('score_1', 'score_2', 'score_3', 
              'person_1', 'person_2', 'person_3', 'current_score'), as.factor)
str(scored_unique)
levels(scored_unique$score_1)
levels(scored_unique$score_2)
levels(scored_unique$score_3)
levels(scored_unique$person_1)
levels(scored_unique$person_2)
levels(scored_unique$person_3)
levels(scored_unique$current_score)

#check how many scored according to instructions
scored_unique %>% group_by(score_1) %>% summarize(n()) %>% print(n=30) #14 cases with 'exclude' can be skipped, as only 1 reviewer
scored_unique %>% group_by(score_2) %>% summarize(n()) %>% print(n=30) # inconsistencies in writing style -> fix (below)
scored_unique %>% group_by(score_3) %>% summarize(n()) %>% print(n=30) #FINE!

#fix inconsistencies in writing style in score_2
# some weird values cannot be classified to any preferred values, e.g. 'cant find'
scored2 <- scored_unique %>% 
  mutate(score_2 = fct_recode(score_2,
                              'exclude_abstract' = "exclude_abstact",
                              'exclude_abstract' = "Exclude_abstract",
                              'include' = "Include",
                              'include' = "Include?",   #1 unsure inclusion, here converted to include
                              'include' = "include?"))  #71 unsure inclusions, here converted to sure

# select papers, which were scored as advised
good_scores <- c("exclude_title", "exclude_abstract", "include")

# papers scored correctly by only by reviewers 1 AND 2
ok_scored12 <- scored2 %>%
  filter(score_1 %in% good_scores &
           score_2 %in% good_scores &
           !(score_3 %in% good_scores))%>%
  select(score_1, score_2, current_score, ID_Eeva)#n = 2597 

# papers scored correctly by only by reviewers 1 AND 3
ok_scored13 <- scored2 %>%
  filter(score_1 %in% good_scores &
           score_3 %in% good_scores &
           !(score_2 %in% good_scores))%>%
  rename(score_2_original = score_2,
         score_2 = score_3) %>%
  select(score_1, score_2, current_score, ID_Eeva) #n=7

# papers scored correctly by only by reviewers 2 AND 3
ok_scored23 <- scored2 %>%
  filter(score_2 %in% good_scores &
           score_3 %in% good_scores &
           !(score_1 %in% good_scores))%>%
  rename(score_1_original = score_1,
         score_2_original = score_2,
         score_1 = score_2,
         score_2 = score_3)%>%
  select(score_1, score_2, current_score, ID_Eeva)  #n=6

# papers scored correctly by all three reviewers
ok_scored123 <- scored2 %>%
  filter(score_1 %in% good_scores &
           score_2 %in% good_scores &
           score_3 %in% good_scores)            #n=94

#when 3 reviewers, select only two:
#when all three agreed, select reviewers 1 & 2
all_agree <- ok_scored123%>%
  filter(score_1 == score_2 & 
           score_1 == score_3)%>%
  select(score_1, score_2, current_score, ID_Eeva) # n = 57

#when reviewers 1 & 2 agree, but reviewer 3 disagree, select reviewers 1 & 3
disagree1 <- ok_scored123 %>%
  filter(score_1 ==  score_2 &
           (score_3 != score_1))%>%
  rename(score_2_original = score_2,
         score_2 = score_3)%>%
  select(score_1, score_2, current_score, ID_Eeva) #n = 15

#when reviewers 1 & 2 disagree, select reviewers 1 & 2, except when the reviewer3 recommends 'include', then select reviewers 1 & 3
disagree2 <- ok_scored123%>%
  filter(score_1 !=  score_2 &
           (score_3 != "include"))%>%
  select(score_1, score_2, current_score, ID_Eeva)       #n = 16

disagree3 <- ok_scored123%>%
  filter(score_1 !=  score_2 &
           (score_3 == "include"))%>%
  rename(score_2_original = score_2,
         score_2 = score_3)%>%
  select(score_1, score_2, current_score, ID_Eeva)    #n = 6


#combine all cases that were correctly reviewed by two reviewers into one dataframe
scored3 <- bind_rows(ok_scored12, ok_scored13, ok_scored23,
                     all_agree, disagree1, disagree2, disagree3) #n = 2704



#Stats
#-------------------------------

################
#title screening
################
#how often did two reviewers agree on excluding a paper based on title?

title_screening <- scored3 %>%
  mutate(score_1 = case_when(
  .$score_1 == "exclude_abstract" ~ 'further',
  .$score_1 == "exclude_title" ~ 'exclude_title',
  .$score_1 == "include" ~ 'further'),
  score_2 = case_when(
  .$score_2 == "exclude_abstract" ~ 'further',
  .$score_2 == "exclude_title" ~ 'exclude_title',
  .$score_2 == "include" ~ 'further'))

agree(title_screening[1:2], tolerance=0) # 78% of the cases reviewers agreed on exclude_title vs. further
kap <- kappa2(title_screening[1:2], weight = c("equal"), sort.levels = TRUE)
kap # Cohen's kappa = 0.541 -> moderate agreement
#z = 28.2, p = 0, N = 2704

both_exclude_title <- title_screening %>% filter(score_1 == "exclude_title" &
                                 score_2 == "exclude_title") #1332 out of 2704 = 49.3%

both_further <- title_screening %>% filter(score_1 == "further" &
                                   score_2 == "further") # 778 out of 2704 = 28.8%
# not agreeing on 582 cases out of 2704 = 22 %



################
#combine title & abstract screening
################
#how often did two reviewers agree on include vs. exclude a paper after BOTH title and abstract screening?

abstract_screening <- scored3 %>%
  mutate(score_1 = case_when(
    .$score_1 == "exclude_abstract" ~ 'exclude',
    .$score_1 == "exclude_title" ~ 'exclude',
    .$score_1 == "include" ~ 'include'),
    score_2 = case_when(
      .$score_2 == "exclude_abstract" ~ 'exclude',
      .$score_2 == "exclude_title" ~ 'exclude',
      .$score_2 == "include" ~ 'include'))

agree(abstract_screening[1:2], tolerance=0) # 87.5% of the cases reviewers agreed on exclude vs. include
kap <- kappa2(abstract_screening[1:2], weight = c("equal"), sort.levels = TRUE)
kap # Cohen's kappa = 0.605 
#z = 31.6, p = 0, N = 2704

both_include <- abstract_screening %>% filter(score_1 == "include" &
                                                score_2 == "include") #363 out of 2704 = 13.4%

both_exclude <- abstract_screening %>% filter(score_1 == "exclude" &
                                                score_2 == "exclude") #2003 out of 2704 = 74.1%
# disagreeing in 333 cases out of 2704 = 12.5 %


################
#abstract screening
################
#how often did two reviewers agree on excluding a paper based on abstract screening?
#filter first out papers that were excluded based on title
abstract_screening <- scored3 %>%
  filter(!(score_1 == "exclude_title" &
             score_2 == "exclude_title"))%>%
  mutate(score_1 = case_when(
    .$score_1 == "exclude_abstract" ~ 'exclude',
    .$score_1 == "exclude_title" ~ 'exclude',
    .$score_1 == "include" ~ 'include'),
    score_2 = case_when(
      .$score_2 == "exclude_abstract" ~ 'exclude',
      .$score_2 == "exclude_title" ~ 'exclude',
      .$score_2 == "include" ~ 'include')) # n = 1372

agree(abstract_screening[1:2], tolerance=0) # 75.7% of the cases reviewers agreed on exclude vs. include
kap <- kappa2(abstract_screening[1:2], weight = c("equal"), sort.levels = TRUE)
kap # Cohen's kappa = 0.49 
#z = 18.3, p = 0, N = 1372

both_include <- abstract_screening %>% filter(score_1 == "include" &
                                                score_2 == "include") #368 out of 1372 = 26.8%

both_exclude <- abstract_screening %>% filter(score_1 == "exclude" &
                                                score_2 == "exclude") #670 out of 1372 = 48.8%
# disagreeing in 334 cases out of 1324 = 24.3 %
