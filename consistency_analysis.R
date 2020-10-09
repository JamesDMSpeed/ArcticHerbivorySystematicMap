#Consistency analysis: how consistently two reviewers coded the same paper?

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
scored <- scored %>%
  mutate_at(c('score_1', 'score_2', 'score_3', 
              'person_1', 'person_2', 'person_3', 'current_score'), as.factor)
str(scored2)
levels(scored$score_1)
levels(scored$score_2)
levels(scored$score_3)
levels(scored$person_1)
levels(scored$person_2)
levels(scored$person_3)
levels(scored$current_score)

#check how many scored according to instructions
scored %>% group_by(score_1) %>% summarize(n()) %>% print(n=30) #14 exclude, based on nothing
scored %>% group_by(score_2) %>% summarize(n()) %>% print(n=30) # inconsistencies in writing style -> fix
scored %>% group_by(score_3) %>% summarize(n()) %>% print(n=30) #FINE!

#fix inconsistencies in writing style in score_2
scored2 <- scored %>% 
  mutate(score_2 = fct_recode(score_2,
                              "exclude_abstract" = "exclude_abstact",
                              "exclude_abstract" = "Exclude_abstract",
                              "exclude_abstract" = "exclude abstract",
                              "include" = "Include",
                              "include" = "Include?",   #1 unsure inclusion, here converted to sure
                              "include" = "include?"))  #71 unsure inclusions, here converted to sure

#If these unsure inclusions by reviewer 1 are converted to 'exclude_abstract' agreement between reviewers 1 and 2 becomes bit weaker, but no significant changes.


# select papers, which were scored as advised
good_scores <- c("exclude_title", "exclude_abstract", "include")

# papers scored correctly by reviewers 1 AND 2
ok_scored12 <- scored2 %>%
  filter(score_1 %in% good_scores)%>%          #3523 records scored correctly by reviewer 1
  filter(score_2 %in% good_scores)             #3174 records scored correctly by both 1 AND 2

ok_scored13 <- scored2 %>%
  filter(score_1 %in% good_scores)%>%          #3523 records scored correctly by reviewer 1
  filter(score_3 %in% good_scores)             #104 records scored correctly by both 1 AND 3

ok_scored23 <- scored2 %>%
  filter(score_2 %in% good_scores)%>%          #3199 records scored correctly by reviewer 1
  filter(score_3 %in% good_scores)             #102 records scored correctly by both 2 AND 3

# 
a <- ok_scored13 %>% select(ID_Eeva) 
b <- ok_scored23 %>% select(ID_Eeva)
y <-
  semi_join(a, b, by = "ID_Eeva") # 96 records scored by 3 reviewers, 8 records scored only by 2 and 3.

#checking that reviewer 2 is different than reviewer 1
a <- ok_scored12 %>% select(person_1) %>% rename('person' ="person_1")
b <- ok_scored12 %>% select(person_2) %>% rename('person' ="person_2")
y <-
  semi_join(a, b, by = "person") #YES! always different persons


#Stats
#-------------------------------

# In how many percent of papers the two raters agreed on the score?
# Note here, that agreeing on including is kind of heavier than agreeing on whether to exluclude based on title or abstract
comp1 <- ok_scored12 %>% select(score_1,score_2)
agree(comp1, tolerance=0) # 71.8% of the ratings are the same (out of three options)

#"Limitation of percent agreement is that it does not take account of the possibility that raters guessed on scores. It thus may overestimate the true agreement among raters."
#Cohen's kappa takes into account the possibility that two raters were not sure whether to include or exclude a paper and decided the score randomly:
kap <- kappa2(comp1, weight = c("equal"), sort.levels = TRUE)
kap # 0.441 -> weak agreement (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3900052/)
#z = 34.3, p = 0


#----
#how often two reviewers agreed on including a paper?
comp2 <- comp1 %>% mutate(score_1 = case_when(
  .$score_1 == "exclude_abstract" ~ 'exclude',
  .$score_1 == "exclude_title" ~ 'exclude',
  .$score_1 == "include" ~ 'include'))
comp2 <- comp2 %>% mutate(score_2 = case_when(
  .$score_2 == "exclude_abstract" ~ 'exclude',
  .$score_2 == "exclude_title" ~ 'exclude',
  .$score_2 == "include" ~ 'include'))
                                              
agree(comp2, tolerance=0) # 87.9% of the cases reviewers agreed on include vs.exclude
kap <- kappa2(comp2, weight = c("equal"), sort.levels = TRUE)
kap # Cohen's kappa = 0.624 -> moderate agreement
#z = 35.4, p = 0

both_incl <- comp1 %>% filter(score_1 == "include" &
                                score_2 == "include")
temp <- anti_join(comp1, both_incl, by = "score_1")#remove papers which both reviewers agreed to include
comp3 <- temp %>% filter(score_2 != "include")#remove papers which reviewer 2 included
#the reviewers agreed to include a paper in 394 cases out of 3174 checked, i.e. on 12.4 %
#they agreed on excluding a paper in 2370 cases out of 3174 checked, i.e. on 74.7 %
#they disagreed on whether to include/exclude in 3174-2370-394 = 410 cases, i.e. on 12.9%


#----
#if both reviewers excluded a paper, how often they did it based on the same argument (title vs. abstract)
agree(comp3, tolerance=0) # in 78.2% of cases they agreed on why to exclude a paper (title vs. abstract)
kap <- kappa2(comp3, weight = c("equal"), sort.levels = TRUE)
kap # Cohen's kappa = 0.344 -> minimal agreement
# z = 16.7, p = 0

both_title <- comp3 %>% filter(score_1 == "exclude_title" &
                                score_2 == "exclude_title")

#they agreed on excluding a paper based on title in 1596 cases out of 2348 checked, i.e. on 70.0%
