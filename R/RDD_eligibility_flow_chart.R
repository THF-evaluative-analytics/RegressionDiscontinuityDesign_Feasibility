#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project: Evaluation of the 'COVID oximetry at home' intervention using a Regression Discontinuity Design
# Purpose: CCG level onboarding analysis
# Author: Dr Will Parry
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Load packages and data ----

pkgs <- c('here', 'tidyverse', 'data.table', 'lubridate', 'DiagrammeR', 'DiagrammeRsvg', 'rsvg', 'glue')
sapply(pkgs, require, character.only = TRUE) #load all packages

setDTthreads(0)#set processors available for data.table
rdd <- readRDS(here('../Data', 'RDD_analysis.RDS')) #load dataset


#_______________________________

#Applying eligibility rules ----

e1 <- rdd[, .N, by = .(ONBOARDED)][, stage := '01_clean_GDPPR'] #count patients by onboarded status
e2 <- rdd[!is.na(FINDEXDATE), .N, by = .(ONBOARDED)][, stage := '02_and_with_test'] #remove those without a test date and repeat
e3 <- rdd[!is.na(FINDEXDATE) & CEV == 0, .N, by = .(ONBOARDED)][, stage := '03_and_non_CEV'] #remove those who are CEV and repeat
e4 <- rdd[!is.na(FINDEXDATE) & CEV == 0 & CARE_HOME_FLAG == 0, .N, by = .(ONBOARDED)][, stage := '04_and_non_care'] #remove those in a care home and repeat

#For the two different age cut-offs (65 and 50)...

#Remove patients with indeterminate ages at the cut-off and not between 5 years older and younger than the cut-off...
e5_65 <- rdd[abs(wks65) <= (5*52) & wks65 != 0 & !is.na(FINDEXDATE) & CEV == 0 & CARE_HOME_FLAG == 0, .N, by = .(ONBOARDED)][, stage := '05_and_determined_65']
e6_50 <- rdd[abs(wks50) <= (5*52) & wks50 != 0 & !is.na(FINDEXDATE) & CEV == 0 & CARE_HOME_FLAG == 0, .N, by = .(ONBOARDED)][, stage := '06_and_determined_50']

#Remove patients that had their first positive COVID test prior to 01-01-2021...
e7_65 <- rdd[abs(wks65) <= (5*52) & wks65 != 0 & FINDEXDATE >= ymd('2021-01-01') & !is.na(FINDEXDATE) & CEV == 0 & CARE_HOME_FLAG == 0, .N, by = .(ONBOARDED)][, stage := '07_and_Jan_onwards_65']
e8_50 <- rdd[abs(wks50) <= (5*52) & wks50 != 0 & FINDEXDATE >= ymd('2021-01-01') & !is.na(FINDEXDATE) & CEV == 0 & CARE_HOME_FLAG == 0, .N, by = .(ONBOARDED)][, stage := '08_and_Jan_onwards_50']

#Keep patients that are listed at CCGs which provided 'complete' data...
e9_65 <- rdd[COMPLETE_CCG == T & abs(wks65) <= (5*52) & wks65 != 0 & FINDEXDATE >= ymd('2021-01-01') & !is.na(FINDEXDATE) & CEV == 0 & CARE_HOME_FLAG == 0, .N, by = .(ONBOARDED)][, stage := '09_and_"complete"_CCGs_65']
e10_50 <- rdd[COMPLETE_CCG == T & abs(wks50) <= (5*52) & wks50 != 0 & FINDEXDATE >= ymd('2021-01-01') & !is.na(FINDEXDATE) & CEV == 0 & CARE_HOME_FLAG == 0, .N, by = .(ONBOARDED)][, stage := '10_and_"complete"_CCGs_50']

elig_numbers <- rbind(e1, e2, e3, e4, e5_65, e6_50, e7_65, e8_50, e9_65, e10_50) %>% #bind everything together into one table
  dcast(... ~ ONBOARDED, value.var = 'N') %>% #cast wide by onboarded status
  .[order(stage)] %>% setnames(c('Y', 'N'), c('ONBOARDED_Y', 'ONBOARDED_N')) %>% #order by stage description and change the names of some columns
  .[, `:=`(Base = trimws(format(ONBOARDED_N + ONBOARDED_Y, big.mark = ',')), #add a formatted Base column (count of all patients)
           Y = trimws(format(ONBOARDED_Y, big.mark = ',')))] #add a formatted onboarded count column

fwrite(elig_numbers, here('../Outputs', 'RDD_feasibility', 'RDD_eligibility_numbers.csv')) #write to file


#Saving analytical datasets after preliminary eligibility rules applied ----

#Create initial eligible patient datasets
rdd_f65 <- rdd[abs(wks65) <= (5*52) & wks65 != 0 & !is.na(FINDEXDATE) & CEV == 0 & CARE_HOME_FLAG == 0] #at the age 65 cut-off
rdd_f50 <- rdd[abs(wks50) <= (5*52) & wks50 != 0 & !is.na(FINDEXDATE) & CEV == 0 & CARE_HOME_FLAG == 0] #at the age 50 cut-off

saveRDS(rdd_f65, here('../Data', 'RDD65_analysis.RDS')) #write to file
saveRDS(rdd_f50, here('../Data', 'RDD50_analysis.RDS')) #write to file


#__________________________________

#Draw eligibility flow diagram ----


#Read in eligibility numbers

e <- fread(here('../Outputs', 'RDD_feasibility', 'RDD_eligibility_numbers.csv')) #read previously created file


#Checking CCG numbers

#Check CCGs in eligible patient datasets
ccg1_65 <- rdd_f65[COMPLETE_CCG %in% c(T, F), .N, keyby = .(CCG)] %>% nrow() 
ccg1_50 <- rdd_f50[COMPLETE_CCG %in% c(T, F), .N, keyby = .(CCG)] %>% nrow()

#Check CCGs in eligible patient datasets, tested from 01-01-2021 onwards
ccg2_65 <- rdd_f65[COMPLETE_CCG %in% c(T, F) & FINDEXDATE >= ymd('2021-01-01'), .N, keyby = .(CCG)] %>% nrow()
ccg2_50 <- rdd_f50[COMPLETE_CCG %in% c(T, F) & FINDEXDATE >= ymd('2021-01-01'), .N, keyby = .(CCG)] %>% nrow()

#Check CCGs in eligible patient datasets, tested from 01-01-2021 onwards, in 'complete' CCGs
ccg3_65 <- rdd_f65[COMPLETE_CCG %in% c(T) & FINDEXDATE >= ymd('2021-01-01'), .N, keyby = .(CCG)] %>% nrow()
ccg3_50 <- rdd_f50[COMPLETE_CCG %in% c(T) & FINDEXDATE >= ymd('2021-01-01'), .N, keyby = .(CCG)] %>% nrow()


#Create labels for eligibility flow diagram

n1 <- paste0('Cleaned GDPPR data set:\\lOnboarded = ', e[1,'Y'], ' (', e[1,'Base'], ')\\l')
n2 <- paste0('Patients with a first positive test:\\lOnboarded = ', e[2,'Y'], ' (', e[2,'Base'], ')\\l')
n3 <- paste0('Patients who are not CEV:\\lOnboarded = ', e[3,'Y'], ' (', e[3,'Base'], ')\\l')
n4 <- paste0('Patients who are not living in a care home:\\lOnboarded = ', e[4,'Y'], ' (', e[4,'Base'], ')\\l')
n5 <- paste0('ELIGIBLE PATIENTS\\lPatients who are 60\u201369 years old at test,\\lexcluding indeterminate ages at the cut-off,\\lOnboarded = ', e[5,'Y'], ' (', e[5,'Base'], ')\\lNumber of CCGs = ', ccg1_65, '\\l')
n6 <- paste0('ELIGIBLE PATIENTS\\lPatients who are 45\u201354 years old at test,\\lexcluding indeterminate ages at the cut-off,\\lOnboarded = ', e[6,'Y'], ' (', e[6,'Base'], ')\\lNumber of CCGs = ', ccg1_50, '\\l')
n7 <- paste0('ELIGIBLE PATIENTS\\lPatients who are 60\u201369 years old at test,\\lexcluding indeterminate ages at the cut-off,\\ltested in Jan 2021 onwards:\\lOnboarded = ', e[7,'Y'], ' (', e[7,'Base'], ')\\lNumber of CCGs = ', ccg2_65, '\\l')
n8 <- paste0('ELIGIBLE PATIENTS\\lPatients who are 45\u201354 years old at test,\\lexcluding indeterminate ages at the cut-off,\\ltested in Jan 2021 onwards:\\lOnboarded = ', e[8,'Y'], ' (', e[8,'Base'], ')\\lNumber of CCGs = ', ccg2_50, '\\l')
n9 <- paste0('ELIGIBLE PATIENTS\\lPatients who are 60\u201369 years old at test,\\lexcluding indeterminate ages at the cut-off,\\ltested in Jan 2021 onwards, including only\\lCCGs with complete* data:\\lOnboarded = ', e[9,'Y'], ' (', e[11,'Base'], ')\\lNumber of CCGs = ', ccg3_65, '\\l')
n10 <- paste0('ELIGIBLE PATIENTS\\lPatients who are 45\u201354 years old at test,\\lexcluding indeterminate ages at the cut-off,\\ltested in Jan 2021 onwards, including only\\lCCGs with complete* data:\\lOnboarded = ', e[10,'Y'], ' (', e[12,'Base'], ')\\lNumber of CCGs = ', ccg3_50, '\\l')


#Create eligibility flow diagram using DiagrammeR

mygraph <- glue("digraph mygraph {{
  
  graph [layout = dot, rankdir = TB]
  
  node [shape = box, fontname = 'Arial', width = 4, height = 1, fontsize = 20, style = filled, fillcolor = lavender]
  edge [arrowhead = open]
  
  a [label = '{n1}'];
  b [label = '{n2}'];
  c [label = '{n3}'];
  d [label = '{n4}'];
  e [label = '{n5}'];
  f [label = '{n6}'];
  g [label = '{n7}'];
  h [label = '{n8}'];
  i [label = '{n9}'];
  j [label = '{n10}'];
  
  a -> b -> c -> d
  d -> e -> g -> i
  d -> f -> h -> j
  
  }}      
  
")


#Draw flow diagram and save to png

grViz(mygraph) %>% #draw grViz
  export_svg %>% #export to svg
  charToRaw %>% #convert to raw format
  rsvg_png(here('../Outputs', 'RDD_feasibility', 'RDD_eligibility_flowchart_updated.png')) #write image to file





