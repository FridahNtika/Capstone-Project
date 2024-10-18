## import packages
library("tidyverse")
library(lubridate)
install.packages("hms")
library(hms)

## load data
resp <- read.csv(file.choose())
act <- read.csv(file.choose())
who <- read.csv(file.choose())
ros <- read.csv(file.choose())

## explore data
# size
dim(resp) # 8548 175
dim(act) # 153120 30
dim(who) # 182600 5
dim(ros) # 20794 8

# missing data
colSums(is.na(resp)) # none
colSums(is.na(act)) # none
colSums(is.na(who)) # none
colSums(is.na(ros)) # none

## clean data
# -- roster --
# we want households with at least 1 child (<18 yrs),
rosdf <- ros %>% group_by(TUCASEID) %>%
  mutate(Child_present = min(TEAGE) < 18) %>% # new column, for each household, is there a child?
  mutate(Num_adults = sum(TEAGE >= 27 & TEAGE <= 60)) %>% # utmost 2 adults between 30 and 60
  select(TUCASEID, TULINENO, TEAGE, TESEX, Child_present, Num_adults)

# merge the two conditions
rosdf <- rosdf[!is.na(rosdf$Child_present) & rosdf$Child_present & (rosdf$Num_adults == 1 | rosdf$Num_adults == 2),]
head(rosdf)

# -- activity --
# convert from character
act$start <- as_datetime(act$TUSTARTTIM, format = "%H:%M:%S")
act$start <- as_hms(act$TUSTARTTIM)
act$stop <- as_datetime(act$TUSTOPTIME, format = "%H:%M:%S")
act$stop <- as_hms(act$TUSTOPTIME)
# calculate duration in minutes
act$duration <- (act$stop - act$start)
