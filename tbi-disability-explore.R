#install.packages('data.table')
suppressMessages(library(data.table, quietly = T))

#install.packages('bit64')
suppressMessages(library(bit64, quietly = T))

# install.packages('ggplot2')
suppressMessages(library(ggplot2, quietly = T))

suppressMessages(library(cowplot, quietly = T))

#install.packages('plyr')
suppressMessages(library(plyr, quietly = T))

###########################################################.
# Read in 2018 data
# shortcut file path:
file_path_2018<-'C:/Users/AsmusAL/Metropolitan Council/Ehrlich, Jonathan - TBI_UnweightedQ1Deliverable/Interim_Dataset'
# each csv: 
  # day18<-fread(paste0(file_path_2018, '/day_table.csv'))
  # hh18<-fread(paste0(file_path_2018, '/hh_table.csv'))
  # loc18<-fread(paste0(file_path_2018, '/location_table.csv'))
  trip18<-fread(paste0(file_path_2018, '/trip_table.csv'))
  # veh18<-fread(paste0(file_path_2018, '/vehicle_table.csv'))
  per18<-fread(paste0(file_path_2018, '/person_table.csv'), 
               na.strings = c('-9999', # Technical error
                              '-9998', # Non-response
                              '995', # Not applicable
                              '998', # Don't know
                              '999')) # prefer not to answer


# Read in 2010 data
# shortcut filepath:
file_path_2010<-'TBI-2010-data/'
# each csv:
hh10<-fread(paste0(file_path_2010, '/2010-TBI-household.csv'))
per10<-fread(paste0(file_path_2010, '/2010-TBI-person.csv'),
             na.strings = c('98', '99', '999'))
trip10<-fread(paste0(file_path_2010, '/2010-TBI-trip.csv'))
###########################################################.


###########################################################.
# NUMBER OF RESPONDENTS WITH A DISABILITY ####
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
# NUMBER OF TRIPS TAKEN BY RESPONDENTS W/ DISABILITY ####
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
# AVERAGE AGE ####
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
# WHAT KINDS OF DISABILITY?####
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


### need to reshape trip file to add up by trip type by person ####

# HBW	Home-based work trip
# HBSch	Home-based school trip
# HBColl	Home-based college trip
# HBMain	Home-based maintenance trip
# HBShop	Home-based shopping trip
# HBEat	Home-based eating trip
# HBDisc	Home-based discretionary trip
# HBChaf	Home-based chauffer trip
# HBOther	Home-based other trip
# WBOther	Work-based other trip
# HBTurn	Home-based turnaround trip
# NHB	Non-home-based  trip
# HBH	Home-based home trip

dput(names(trip10))
trip10_sub<-trip10[,c("HHID", "PERSONID", "HHPERSONID", "HBW", "HBSch", "HBColl", "HBMain", "HBShop", 
                   "HBEat", "HBDisc", "HBChaf", "HBOther", "WBOther", "HBTurn", 
                   "NHB", "HBH", "primarymode", 
                   "owncar", "HHWeight", "triptime"),
               with  = F]

# wide to long
trip10_sub<-melt(trip10_sub,
                 value.name = "n_trips", 
                 variable.name = "trip_type",
                 id.vars = c( "HHID", "PERSONID", "HHPERSONID", "primarymode", 
                             "owncar", "HHWeight", "triptime"))

trip10_sub<-trip10_sub[n_trips>0]

# triptime a bit weird
trip10_sub$triptime[grep("[(]", trip10_sub$triptime)]<-NA
trip10_sub$triptime[grep("[-]", trip10_sub$triptime)]<-NA
trip10_sub$triptime<-as.numeric(as.character(trip10_sub$triptime))
sort(unique(trip10_sub$triptime))
hist(trip10_sub$triptime) # good, short.

trip10_sub<-trip10_sub[,lapply(.SD, sum),
                       .SDcols = c('n_trips', 'triptime'),
                       by = c("HHID", "PERSONID", "HHPERSONID", "primarymode", 
                             "owncar", "HHWeight")]
trip10_sub$av_triptime<-trip10_sub$triptime/trip10_sub$n_trips

# by mode:
#primarymode
    # 1=Auto
    # 2=Bus
    # 3=Light Rail
    # 4=Commuter Rail
    # 5=Bicycle (skateboard, scooter)
    # 6=Walk
    # 7=School Bus
    # 8=Taxi (Ambulance, dial-a-ride, private bus)
    # 9=Other
# owncar #Using HH-owned vehicle
# hhweight # household expansion factor
# triptime # length of trip


###########################################################.
# Similar demographics of ppl with disabilities in 2010 vs. 2018?
sort(names(per10))

demvars_2010<-c(
  'HHID', 'PERSONID', 'HHPERSONID',
  'INCOME', # household table - income
       # 1=Less than $5,000
       # 2=$5,000 but less than $10,000 
       # 3=$10,000 but less than $15,000 
       # 4=$15,000 but less than $20,000 
       # 5=$20,000 but less than $25,000 
       # 6=$25,000 but less than $30,000  
       # 7=$30,000 but less than $35,000  
       # 8=$35,000 but less than $40,000  
       # 9=$40,000 but less than $45,000
       # 10=$45,000 but less than $50,000 
       # 11=$50,000 but less than $60,000  
       # 12=$60,000 but less than $75,000 
       # 13=$75,000 but less than $100,000 
       # 14=$100,000 but less than $125,000 
       # 15=$125,000 but less than $150,00
       # 16=$150,000 but less than $200,000
       # 17=$200,000 but less than $250,000
       # 18=$250,000 or more
       # 96=Below $50,000
       # 97=Above $50,000
       # 98=Don’t know  
       # 99=Refused
  'WRKR', #Employment Status
      # 0=INAP
      # 1=A PAID full-time worker  
      # 2=A PAID part-time worker                                                                                               
      # 3=AN UNPAID Worker or Volunteer                                                                                                      
      # 4=A Homemaker                                                                                                           
      # 5=Retired
      # 6=Unemployed, looking for a job
      # 7=Unemployed, not looking for a job
      # 8=Disabled non-worker
      # 9=Student
      # 96=Other
      # 98=Don't know
      # 99=Refused
  'STUDENT', # Student Status
      # 1=Yes, full-time                                                                                            
      # 2=Yes, part-time                                                                                    
      # 3=No, Not in school                                                                       
      # 8=Don't know                                                                                        
      # 9=Refused
  'TOTVEH', # household table - total number of vehicles
  'age4',  # 
  'GENDER',
  'TYPDISABLE',
  'DISABLE',
  'SAMPL_REG', # Region classficiations
      # MN=Minneapolis
      # SP=St. Paul
      # SMN=CORE WEST (C/H)--Carver Hennepin
      # SSP=CORE EAST (A/R/W)--Anoka Ramsey Washington
      # RCR=CORE SOUTH (D/S)--Dakota Scott
      # RMN=Ring Counties, MN
      # RWI=Ring Counties, WI
  'n_trips',#  number of trips
  'primarymode',
  'owncar',
  'HHWeight',# household weight
  'av_triptime' 
  # trip weight
  )

per10_demo<-per10[,names(per10) %in% demvars_2010, with = F]
hh10_demo<-hh10[,names(hh10) %in% demvars_2010, with = F]
hh10_demo$DISABLE<-NULL
trip10_demo<-trip10_sub[,names(trip10_sub) %in% demvars_2010, with = F]

# merge these together
dat10_1<-merge(hh10_demo, per10_demo, all = T)
dat10<-merge(dat10_1, trip10_demo,
             by = c("HHID", "PERSONID", "HHPERSONID", "HHWeight"), all = T)

sort(unique(dat10$HHWeight))
str(dat10)
dat10$HHWeight<-as.numeric(dat10$HHWeight)


#### What is the distribution of weights for households with disabilities in 2010 compared to those w/o disabilities?
dat10$DISABLE <- mapvalues(dat10$DISABLE, from = c(0, 1, 2, NA), to = c(NA, 'Has Disability', 'No Disability', NA))

ggplot(dat10, aes(x = HHWeight))+
  geom_histogram(bins = 10)+
  facet_wrap(~DISABLE, scale = "free_y", nrow = 1)+
  theme_bw()

ggplot(dat10, aes(x = HHWeight))+
  stat_density(aes(fill = as.factor(DISABLE)), alpha = 0.5)+
  theme_bw()

ggplot(dat10, aes(x = DISABLE, y = HHWeight))+
  geom_boxplot(aes(fill = as.factor(DISABLE)), alpha = 0.5)+
  theme_bw()



