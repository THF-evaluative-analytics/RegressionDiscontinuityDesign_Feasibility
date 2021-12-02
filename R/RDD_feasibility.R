#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project: Evaluation of the 'COVID oximetry at home' intervention
# Purpose: Feasibility analysis for Regression Discontinuity Design (RDD) study
# Author: Dr Will Parry
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Load packages and data ----

pkgs <- c('here', 'tidyverse', 'data.table', 'lubridate', 'pwr', 'scales')
sapply(pkgs, require, character.only = TRUE) #load packages

setDTthreads(0) #set processors available for data.table
rdd_f65 <- readRDS(here('../Data', 'RDD65_analysis.RDS')) #load datasets
rdd_f50 <- readRDS(here('../Data', 'RDD50_analysis.RDS'))


#_____________________________________________

#Plot onboarding/testing by calendar week ----

plot_by_week <- function(.data, .onboarded, .SDCfilename){

  .onboard_vals <- if(.onboarded) {'Y'} else {c('Y', 'N')} #plot onboarded or all tested?
  
  x <- .data[, week := floor_date(FINDEXDATE, 'week')][ #round FINDEXDATE down to weeks
    ONBOARDED %in% .onboard_vals, #filter to get all tested or just onboarded
    .N, keyby = .(week)][ #count patients by week
      N >= 10] #ensure that there are at least 10 patients in each data point (for SDC compliance)
  
  fwrite(x, here('../Outputs', 'RDD_feasibility', .SDCfilename)) #write data to file
  
  ggplot(x, aes(x = week, y = N)) + #plot the data
    geom_bar(stat = 'identity', fill = 'steelblue4') + #use a bar chart
    scale_y_continuous(labels = scales::number_format(big.mark = ',')) + #format the y-axis 
    ylab('Number of patients') + xlab('Calendar time (weekly bins)') + #add axis labels
    theme_minimal() + theme(panel.grid.minor = element_blank(), #edit the theme...
                            axis.title.y = element_text(margin = margin(0,10,0,0)),
                            axis.title.x = element_text(margin = margin(10,0,0,0)))
}

#Run function for onboarded and all tested for both cut-offs...
p1 <- plot_by_week(rdd_f65, .onboarded = T, 'onboarded_by_week_65.csv')
p2 <- plot_by_week(rdd_f65, .onboarded = F, 'tested_by_week_65.csv')
p3 <- plot_by_week(rdd_f50, .onboarded = T, 'onboarded_by_week_50.csv')
p4 <- plot_by_week(rdd_f50, .onboarded = F, 'tested_by_week_50.csv')

#Save plots to file...
ggsave(here('../Outputs', 'RDD_feasibility', 'Onboarding_by_time_60to70.png'), plot = p1, width = 12, height = 8, units = 'cm')
ggsave(here('../Outputs', 'RDD_feasibility', 'Testing_by_time_60to70.png'), plot = p2, width = 12, height = 8, units = 'cm')
ggsave(here('../Outputs', 'RDD_feasibility', 'Onboarding_by_time_45to55.png'), plot = p3, width = 12, height = 8, units = 'cm')
ggsave(here('../Outputs', 'RDD_feasibility', 'Testing_by_time_45to55.png'), plot = p4, width = 12, height = 8, units = 'cm')


#________________________________________

#Functions to identify discontinuity ----

plot_data <- function(.data, .timevar, .bandsize = 52, .complete = c(T, F), .fromdate = '2020-10-01'){
  
  x <- .data[COMPLETE_CCG %in% .complete & FINDEXDATE >= ymd(.fromdate)][ #filter by date/'complete' CCGs if provided
    abs(get(.timevar)) <= .bandsize, #filter data using provided timevar and bandwidth
    .N, keyby = c(.timevar, 'ONBOARDED')][ #count patients by timevar and onboarded status
      , dcast(.SD, ... ~ ONBOARDED, value.var = 'N', fill = 0)] #cast wide by onboarded status
  
  if(grepl('wks', .timevar)){ #if using a weeks-based timevar
    x <- x[, paste(.timevar) := 2 * ceiling(get(.timevar)/2)][ #round the weekly timevar to every two weeks
      , .(N = sum(N), Y = sum(Y)), keyby = c(.timevar)] #re-sum everything by the new timevar
  } #NB: this was done due to low patient numbers when plotting weekly percentages
  
  x <- x[, perc := Y / (Y + N)][ #calculate percentages
    Y >= 10 & N >= 10] #ensure at least 10 patients in each data point (for SDC compliance)
}


plot_disc <- function(.data, .timevar, .bandsize = 52, .complete = c(T, F), 
                      .fromdate = '2020-10-01', .subtitle = NULL, .SDCfilename){
  
  pd <- plot_data(.data, .timevar, .bandsize, .complete, .fromdate) #get data for the plot
  fwrite(pd, here('../Outputs', 'RDD_feasibility', .SDCfilename)) #write to file
  byvar <- if(grepl('wks', .timevar)){'weeks'} else {'years'} #get correct x-axis label text
  
  ggplot(pd, aes(x = get(.timevar), y = perc)) + #plot the data
    geom_bar(stat = 'identity', color = 'steelblue4', fill = 'steelblue4') + #use a bar chart
    xlab(paste('Distance from the cut-off in', byvar)) + ylab('Percentage onboarded') + #add axis labels...
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) + #format y-axis
    geom_vline(xintercept = 0, colour = 'red') + #add cut-off marker line
    theme_minimal() + theme(panel.grid.minor = element_blank(), #edit the theme...
                            axis.title.y = element_text(margin = margin(0,10,0,0)),
                            axis.title.x = element_text(margin = margin(10,0,0,0)))
}

#Run function for various discontinuity plots...
p1 <- plot_disc(rdd_f65, 'wks65', .bandsize = 5*52, .SDCfilename = 'discontinuity_by_week_65.csv')
p2 <- plot_disc(rdd_f65, 'yrs65', .bandsize = 5, .SDCfilename = 'discontinuity_by_year_65.csv')
p3 <- plot_disc(rdd_f65, 'yrs65', .bandsize = 5, .fromdate = '2021-01-01', .SDCfilename = 'discontinuity_by_year_65_Jan.csv')
p4 <- plot_disc(rdd_f50, 'yrs50', .bandsize = 5, .SDCfilename = 'discontinuity_by_year_50.csv')
p5 <- plot_disc(rdd_f50, 'yrs50', .bandsize = 5, .fromdate = '2021-01-01', .SDCfilename = 'discontinuity_by_year_50_Jan.csv')
p6 <- plot_disc(rdd_f65, 'yrs65', .complete = T, .bandsize = 5, .fromdate = '2021-01-01', .SDCfilename = 'discontinuity_by_year_65_Jan_complete.csv')
p7 <- plot_disc(rdd_f50, 'yrs50', .complete = T, .bandsize = 5, .fromdate = '2021-01-01', .SDCfilename = 'discontinuity_by_year_50_Jan_complete.csv')

#Save plots to file...
ggsave(here('../Outputs', 'RDD_feasibility', 'RDD65_discontinuity_plot_all_weeks.png'), plot = p1, width = 12, height = 8, units = 'cm')
ggsave(here('../Outputs', 'RDD_feasibility', 'RDD65_discontinuity_plot_all_years.png'), plot = p2, width = 12, height = 8, units = 'cm')
ggsave(here('../Outputs', 'RDD_feasibility', 'RDD65_discontinuity_plot_from_Jan_years.png'), plot = p3, width = 12, height = 8, units = 'cm')
ggsave(here('../Outputs', 'RDD_feasibility', 'RDD50_discontinuity_plot_all_years.png'), plot = p4, width = 12, height = 8, units = 'cm')
ggsave(here('../Outputs', 'RDD_feasibility', 'RDD50_discontinuity_plot_from_Jan_years.png'), plot = p5, width = 12, height = 8, units = 'cm')
ggsave(here('../Outputs', 'RDD_feasibility', 'RDD65_discontinuity_plot_from_Jan_years_complete.png'), plot = p6, width = 12, height = 8, units = 'cm')
ggsave(here('../Outputs', 'RDD_feasibility', 'RDD50_discontinuity_plot_from_Jan_years_complete.png'), plot = p7, width = 12, height = 8, units = 'cm')


#______________________________________

#Plot onboarding by CCG and cutoff ----

pdata <- function(.data, .cutoff, .complete = c(T, F), .fromdate = '2020-10-01'){
  
  .data <- .data[COMPLETE_CCG %in% .complete & FINDEXDATE >= ymd(.fromdate)] #filter by provided date/'complete' CCGs
  
  pdata <- .data[, .N, keyby = c('CCG', .cutoff, 'ONBOARDED')][ #count by CCG, over/under cut-off & onboarded status
    , dcast(.SD, paste('... ~ ONBOARDED +', .cutoff), value.var = 'N', fill = 0)][ #cast wide by onboarded status and over/under cut-off
      , `:=`(over_cutoff_perc_onboarded = Y_over/(Y_over + N_over), #add percentages for over...
             under_cutoff_perc_onboarded = Y_under/(Y_under + N_under), #...and under cut-off
             N = Y_under + N_under + Y_over + N_over)][ #add count of all patients
               , discontinuity := over_cutoff_perc_onboarded - under_cutoff_perc_onboarded][ #calculate the discontinuity
                 order(-discontinuity)][ #sort the data in descending order by the discontinuity
                   Y_over >= 10 & Y_under >= 10 & N_over >= 10 & N_under >= 10] #ensure the data is SDC compliant
  
  print(paste('Max discontinuity = ', format(max(pdata$discontinuity, na.rm = T), digits = 3))) #print the max discontinuity to console
  return(pdata) #return the data
}


plot_CCG <- function(.data, .cutoff, .complete = c(T, F), .fromdate = '2020-10-01',
                     .subtitle = NULL, .SDCfilename){
  
  pd <- pdata(.data, .cutoff, .complete, .fromdate) #get the data for the plot
  fwrite(pd, here('../Outputs', 'RDD_feasibility', .SDCfilename)) #write to file
  
  ggplot(pd, aes(x = under_cutoff_perc_onboarded, y = over_cutoff_perc_onboarded, size = N)) + #plot the data
    geom_point(shape = 1, colour = 'steelblue4') +  #use an x-y point plot with size = N
    geom_abline(slope = 1, intercept = 0, size = .5, colour = 'red3') + #add an x = y line
    scale_size_continuous(labels = comma) + #format the size scale
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) + #format the x-axis
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + #format the y-axis
    xlab('Percentage onboarded below cut-off') + ylab('Percentage onboarded above cut-off') + #add axis labels
    theme_minimal() + theme(panel.grid.minor = element_blank(), #edit the theme...
                            axis.title.y = element_text(margin = margin(0,10,0,0)),
                            axis.title.x = element_text(margin = margin(10,0,0,0)))

}

#Run function to create the CCG plots...
p65 <- plot_CCG(rdd_f65, 'over65', .fromdate = '2021-01-01', .SDCfilename = 'CCG_plot_65_Jan_on_updated.csv')
p50 <- plot_CCG(rdd_f50, 'over50', .fromdate = '2021-01-01', .SDCfilename = 'CCG_plot_50_Jan_on_updated.csv')

#Save plots to file...
ggsave(here('../Outputs', 'RDD_feasibility', 'RDD65_perc_onboarded_by_CCG_Jan_updated.png'), plot = p65, width = 12, height = 8, units = 'cm')
ggsave(here('../Outputs', 'RDD_feasibility', 'RDD50_perc_onboarded_by_CCG_Jan_updated.png'), plot = p50, width = 12, height = 8, units = 'cm')


#___________________

#Power analysis ----

rddpwr <- function(.data, .cutoff, .complete = c(T, F), .fromdate = '2020-10-01',  
                   .min_discontinuity = 0.1, .base_risk, .treated_risk){
  
  x <- pdata(.data, .cutoff, .complete, .fromdate)[ #get data using the pdata function
    discontinuity >= .min_discontinuity,  #filter by minimum discontinuity
    lapply(.SD, sum), #use lapply to sum a set of columns
    .SDcols = c('N_under', 'Y_under', 'N_over', 'Y_over')] #sum these columns
  
  N1 <- x$N_under + x$Y_under #get the total number of patients under the cut-off
  N2 <- x$N_over + x$Y_over  #get the total number of patients over the cut-off
  
  p1 <- (x$N_under * .base_risk + x$Y_under * .treated_risk)/N1 #estimate resulting outcome % under cut-off
  p2 <- (x$N_over  * .base_risk + x$Y_over * .treated_risk)/N2 #estimate resulting outcome % over cut-off
  print(paste('p1 =', format(p1, digits = 3))) #print result to console
  print(paste('p2 =', format(p2, digits = 3))) #print result to console
  
  pwr.2p2n.test(h = ES.h(p1, p2), N1, N2) #conduct power analysis
  
}


#Run function for a range of assumptions...

p1a <- rddpwr(rdd_f65, 'over65', .complete = c(T, F), .fromdate = '2021-01-01', 
              .min_discontinuity = 0.05, .base_risk = 0.1, .treated_risk = 0.05)
p2a <- rddpwr(rdd_f65, 'over65', .complete = c(T), .fromdate = '2021-01-01', 
              .min_discontinuity = 0.05, .base_risk = 0.1, .treated_risk = 0.05)
p3a <- rddpwr(rdd_f65, 'over65', .complete = c(T, F), .fromdate = '2021-01-01', 
              .min_discontinuity = 0.1, .base_risk = 0.1, .treated_risk = 0.05)
p4a <- rddpwr(rdd_f65, 'over65', .complete = c(T), .fromdate = '2021-01-01', 
              .min_discontinuity = 0.1, .base_risk = 0.1, .treated_risk = 0.05)

p1b <- rddpwr(rdd_f50, 'over50', .complete = c(T, F), .fromdate = '2021-01-01', 
              .min_discontinuity = 0.05, .base_risk = 0.1, .treated_risk = 0.05)
p2b <- rddpwr(rdd_f50, 'over50', .complete = c(T), .fromdate = '2021-01-01', 
              .min_discontinuity = 0.05, .base_risk = 0.1, .treated_risk = 0.05)
p3b <- rddpwr(rdd_f50, 'over50', .complete = c(T, F), .fromdate = '2021-01-01', 
             .min_discontinuity = 0.1, .base_risk = 0.1, .treated_risk = 0.05)
p4b <- rddpwr(rdd_f50, 'over50', .complete = c(T), .fromdate = '2021-01-01', 
             .min_discontinuity = 0.1, .base_risk = 0.1, .treated_risk = 0.05)


#Bind all the results together and name them...
results <- rbindlist(list(Jan_65_all_0.05 = p1a, Jan_65_complete_0.05 = p2a, 
                          Jan_65_all_0.10 = p3a, Jan_65_complete_0.10 = p4a, 
                          Jan_50_all_0.05 = p1b, Jan_50_complete_0.05 = p2b, 
                          Jan_50_all_0.10 = p3b, Jan_50_complete_0.10 = p4b), 
                     idcol = 'source') %>% #add the names as an ID col
  .[, `:=`(`N under cut-off` = trimws(format(n1, big.mark = ',')), #format the outputs...
           `N over cut-off` = trimws(format(n2, big.mark = ',')),
           Power = trimws(format(power, digits = 3)),
           `Effect size (h)` = trimws(format(h, digits = 3)))] %>%
  #split the ID info in the source column into separate columns...
  separate(source, c('From', 'Cut-off', 'Included CCGs', 'Minimum discont-inuity'), '_') %>%
  .[, .(`Cut-off`, `Minimum discont-inuity`, `Included CCGs`, `N under cut-off`,
        `N over cut-off`, `Effect size (h)`, Power)] #select and rearrange columns

saveRDS(results, here('../Outputs', 'RDD_feasibility', 'power_results.RDS')) #save to file
fwrite(results, here('../Outputs', 'RDD_feasibility', 'power_results.csv')) #write to csv






