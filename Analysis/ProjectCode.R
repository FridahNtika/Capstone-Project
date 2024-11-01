## import packages
library(tidyverse)
library(lubridate)
install.packages("hms")
library(hms)

## load data
resp <- read.csv(file.choose())
ros <- read.csv(file.choose())
act <- read.csv(file.choose())
who <- read.csv(file.choose())

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
# -------------- roster --------------
# we want households with at least 1 child (<18 yrs),
ros$Under18 <- ros$TEAGE < 18
rosdf <- ros %>% group_by(TUCASEID) %>%
  mutate(Child_present = min(TEAGE) < 18) %>% # for each household, is there a child?
  mutate(Num_adults = sum(TEAGE >= 27 & TEAGE <= 60)) %>% # utmost 2 adults between 30 and 60
  filter(TEAGE >= 27 & TEAGE <= 60) %>% # ensure adults are in desired age range
  select(TUCASEID, TULINENO, TEAGE, TESEX, Under18, Child_present, Num_adults)

# filter households that meet both
rosdf <- rosdf %>% filter(Child_present & (Num_adults == 1 | Num_adults == 2))
head(rosdf)

# merge with respondent file
resp <- resp %>% select(TUCASEID, TULINENO, TELFS) 
rosdf <- rosdf %>% filter(TULINENO == 1) # keep primary respondents
df <- merge(resp, rosdf, by="TUCASEID", all = FALSE)

# --------------activity --------------
# convert from character
#act$start <- as_datetime(act$TUSTARTTIM, format = "%H:%M:%S")
act$start <- as_hms(act$TUSTARTTIM)
#act$stop <- as_datetime(act$TUSTOPTIME, format = "%H:%M:%S")
act$stop <- as_hms(act$TUSTOPTIME)
# calculate duration in minutes and ensure there are no negative values (activity goes to next day)
act$duration_mins <- ifelse(act$stop < act$start,
                       as.numeric(act$stop + 24*3600 - act$start) / 60,
                       as.numeric(act$stop - act$start) / 60) 

# filter to find childcare and household chores
childcareCodes <- c(301, 302, 303, 401, 402, 403)
actdf <- act %>% 
  filter(TUTIER1CODE == 2 | TRTIER2 %in% childcareCodes) %>%
  select(TUCASEID, TUACTIVITY_N, TUTIER1CODE, TRTIER2, duration_mins)

# ---------------------------analyzing hh chores ---------------------------
houseChores <- actdf %>% filter(TUTIER1CODE == 2)
# merge with household data
choredf <- merge(df, houseChores, by = "TUCASEID", all = FALSE)
choredf <- choredf %>% select(TUCASEID, TULINENO.x, TELFS, TEAGE, TESEX, TUTIER1CODE, TRTIER2, duration_mins)

employed <- resp %>% filter(TELFS == 1 | TELFS == 2) # for now only working with respondents
employedGuardian <- rosdf %>% filter(TUCASEID %in% employed$TUCASEID)
codes <- c(3,4,5)
unemployed <- resp %>% filter(TELFS %in% codes)
unemployedGuardian <- rosdf %>% filter(TUCASEID %in% unemployed$TUCASEID)

# filter chores by employment status
chores_employed <- choredf %>% 
  filter(TUCASEID %in% employedGuardian$TUCASEID) %>%
  mutate(status = "Employed")
chores_unemployed <- choredf %>% 
  filter(TUCASEID %in% unemployedGuardian$TUCASEID) %>%
  mutate(status = "Unemployed")
chores_both <- bind_rows(chores_unemployed, chores_employed)
chores_summary <- chores_both %>% group_by(status) %>%
  summarise(average_time = mean(duration_mins, na.rm = TRUE),
            count = n())
chores_summary

# filter chores by gender
chores_summary2 <- choredf %>% group_by(TESEX) %>%
  summarise(average_time = mean(duration_mins, na.rm = TRUE),
            count = n())
chores_summary2

# filter chores by employment status and gender
chores_summary3 <- chores_both %>% group_by(status, TESEX) %>%
  summarise(average_time = mean(duration_mins, na.rm = TRUE),
            count = n())
chores_summary3

# ---------------------------analyzing childcare ---------------------------
# get each of the activities separately
children <- actdf %>% filter(TRTIER2 %in% childcareCodes)

# merge with household data
childdf <- merge(df, children, by = "TUCASEID", all = FALSE)

# find if the guardian is with a child
whoChild <- who %>% filter(TUCASEID %in% childdf$TUCASEID)
withChild <- merge(whoChild, rosdf, by=c("TUCASEID","TULINENO")) %>%
  filter(Under18 == TRUE)

# dataframe with childcare activities where a child is present
new_df <- merge(childdf, withChild, by=c("TUCASEID","TUACTIVITY_N"), all.x=TRUE, all.y=FALSE)

# visual: generally how much time is spent with hh vs non-hh children?
children$Type <- rep("HH", nrow(children))
children$Type[children$TUTIER1CODE == 4] <- "Non-HH"
# logged
ggplot(children, aes(x=Type, y=log(duration_mins), fill = Type)) + geom_violin() +
  labs(title= "Time dedicated to Household v Non-Household Children", x="Child type", y="Log Time Spent (mins)")
# not logged
ggplot(children, aes(x=Type, y=duration_mins, fill = Type)) + geom_violin() +
  labs(title= "Time dedicated to Household v Non-Household Children", x="Child type", y="Time Spent (mins)")
