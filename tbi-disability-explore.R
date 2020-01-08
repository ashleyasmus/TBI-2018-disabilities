#install.packages('data.table')
library(data.table)

#install.packages('bit64')
library(bit64)

# install.packages('ggplot2')
library(ggplot2)

###########################################################.
# Read in 2018 data
# shortcut file path:
file_path_2018<-'C:/Users/AsmusAL/Metropolitan Council/Ehrlich, Jonathan - TBI_UnweightedQ1Deliverable/Interim_Dataset'
# each csv: 
  # day18<-fread(paste0(file_path_2018, '/day_table.csv'))
  # hh18<-fread(paste0(file_path_2018, '/hh_table.csv'))
  # loc18<-fread(paste0(file_path_2018, '/location_table.csv'))
  # trip18<-fread(paste0(file_path_2018, '/trip_table.csv'))
  # veh18<-fread(paste0(file_path_2018, '/vehicle_table.csv'))
  per18<-fread(paste0(file_path_2018, '/person_table.csv'), 
               na.strings = c('-9999', # Technical error
                              '-9998', # Non-response
                              '995', # Not applicable
                              '998', # Don't know
                              '999')) # prefer not to answer


# Read in 2010 data
# shortcut filepath:
file_path_2010<-'C:/Users/AsmusAL/OneDrive - Metropolitan Council/TBI-HouseHoldSurvey-2018/TBI-2010/'
# each csv:
# hh10<-fread(paste0(file_path_2010, '/2010-TBI-household.csv'))
per10<-fread(paste0(file_path_2010, '/2010-TBI-person.csv'),
             na.strings = c('98', '99', '999'))
# trip10<-fread(paste0(file_path_2010, '/2010-TBI-trip.csv'))
###########################################################.


###########################################################.
# NUMBER OF RESPONDENTS WITH A DISABILITY
# 2018 N with disabilities
nrow(per18[per18$disability == 1,])
# 155 ppl with disabilities in 2018
nrow(per18)
# of 5047 people in total

nrow(per18[per18$disability == 1,])/nrow(per18)
# 0.031 = 3.1% of respondents have some kind of disability.

# 2010 N with disabilities
nrow(per10[per10$DISABLE == 1,])
# 820 ppl with disabilities in 2010
nrow(per10)
# of 21299 people in total

nrow(per10[per10$DISABLE == 1,])/nrow(per10)
# 0.038 = 3.8% of respondents have some kind of disability.
###########################################################.


###########################################################.
# NUMBER OF TRIPS TAKEN BY RESPONDENTS W/ DISABILITY
# 2018
sum(per18$num_trips[per18$disability == 1], na.rm = T)
# 1353 total trips by ppl w/ disabilities
sum(per18$num_trips, na.rm = T)
# 101433 total trips
sum(per18$num_trips[per18$disability == 1], na.rm = T)/sum(per18$num_trips, na.rm = T)
# 1.3% of all trips by ppl with disabilities
mean(per18$num_trips[per18$disability == 1], na.rm = T)
# 8.7 trips per person w/ disabilities
mean(per18$num_trips[per18$disability == 0], na.rm = T)
# compared to 22 trips per person w/o disabilities

# 2010
sum(per10$PerTrips[per10$DISABLE == 1], na.rm = T)
# 1646 total trips by ppl w/ disabilities
sum(per10$PerTrips, na.rm = T)
# 79236 total trips
sum(per10$PerTrips[per10$DISABLE == 1], na.rm = T)/sum(per10$PerTrips, na.rm = T)
# 2.07 % of all trips taken by ppl w/ disabilities
mean(per10$PerTrips[per10$DISABLE == 1], na.rm = T)
# average 2 trips per person w/ disabilities
mean(per10$PerTrips[per10$DISABLE == 2], na.rm = T)
# compared to 3.8 per person w/o disabilities
###########################################################.

###########################################################.
# Average age
# 2010
mean(per10$age4, na.rm = T)
# 46
mean(per10$age4[per10$DISABLE == 1], na.rm = T)
# 60

ggplot(per10[per10$DISABLE %in% c('1', '2'),], aes(x = age4, fill = as.factor(DISABLE)))+
  geom_density(alpha = 0.5)+
  scale_fill_manual(values = c('pink', 'blue'))+
  theme_cowplot()

###########################################################.



###########################################################.
# WHAT KINDS OF DISABILITY?
# Not available in 2018 data?


# 2010 Type Codes
# 0=INAP
# 1=Eye or vision
# 2=Hearing
# 3=Walking (requires wheelchair or cane)
# 4=General health
# 5=Other (Do Not Specify)
# 98=Don't know (Ashley coded as NA)
# 99=Refused (Ashley coded as NA)

summary(as.factor(per10$TYPDISABLE[per10$DISABLE ==1]))
# 1    2    3    4    5 NA's 
#   60    7  341  147  250   20 
# Eye/vision: 60 ppl
# Hearing: 7 ppl
# Walking: 341 ppl
# General health: 147 ppl
# Other: 250 ppl
# (plus 20 NA's)
###########################################################.


