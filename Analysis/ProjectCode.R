## import packages
library(tidyverse)
library(lubridate)
#install.packages("hms")
library(hms)
library(ggplot2)
#install.packages("rsample")
library(rsample)

## load data
resp <- read.csv("/Users/fridahntika/Documents/DS340H/Capstone-Project/Data/atusresp-2023/atusresp_2023.dat")
ros <- read.csv("/Users/fridahntika/Documents/DS340H/Capstone-Project/Data/atusrost-2023/atusrost_2023.dat")
act <- read.csv("/Users/fridahntika/Documents/DS340H/Capstone-Project/Data/atusact-2023/atusact_2023.dat")
who <- read.csv("/Users/fridahntika/Documents/DS340H/Capstone-Project/Data/atuswho-2023/atuswho_2023.dat")

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
respdf <- resp %>% select(TUCASEID, TULINENO, TELFS, TRCHILDNUM) 
rosdf <- rosdf %>% filter(TULINENO == 1) # keep primary respondents
df <- merge(respdf, rosdf, by="TUCASEID", all = FALSE)

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
choredf <- choredf %>% select(TUCASEID, TULINENO.x, TELFS, TEAGE, TESEX, TRCHILDNUM,
                              TUTIER1CODE, TRTIER2, duration_mins)

employed <- respdf %>% filter(TELFS == 1 | TELFS == 2) # for now only working with respondents
employedGuardian <- rosdf %>% filter(TUCASEID %in% employed$TUCASEID)
codes <- c(3,4,5)
unemployed <- respdf %>% filter(TELFS %in% codes)
unemployedGuardian <- rosdf %>% filter(TUCASEID %in% unemployed$TUCASEID)

## filter chores by employment status
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

# violin plot
ggplot(chores_both, aes(x = status, y = duration_mins, fill = status)) + geom_violin() + 
         geom_boxplot() + labs(title = "Time allocated to household chores", 
                               x = "Employment status", y = "Minutes spent") + theme()
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

## Linear Regression Models
chores_both$TESEX <- factor(chores_both$TESEX, levels = c(1, 2), labels = c("Male", "Female"))

# Cross-validation split
set.seed(123)
split <- initial_split(chores_both, prop = 0.7)
train_chore <- training(split)
test_chore <- testing(split)

chores.lm <- lm(duration_mins ~ status, data = train_chore)
chores.lm2 <- lm(duration_mins ~ status + TEAGE + TESEX, data = train_chore)
chores.lm3 <- lm(duration_mins ~ status + TEAGE + TESEX + TRCHILDNUM, data = train_chore)
interaction.lm <- lm(duration_mins ~ status * TESEX, data = train_chore)
interaction.lm2 <- lm(duration_mins ~ status * TRCHILDNUM, data = train_chore)

# Evaluate models
summary(chores.lm)
summary(chores.lm2)
summary(chores.lm3)
summary(interaction.lm)
summary(interaction.lm2)

# BIC for Model Comparison
BIC(chores.lm) 
BIC(chores.lm2) 
BIC(chores.lm3) 
BIC(interaction.lm)
BIC(interaction.lm2)

# RMSE
train_rmse <- function(model) sqrt(mean(model$residuals^2))

train_rmse(chores.lm) 
train_rmse(chores.lm2)
train_rmse(chores.lm3)
train_rmse(interaction.lm)
train_rmse(interaction.lm2)

# Evaluating on test set
evaluate_model <- function(model, test_data) {
  predictions <- predict(model, test_data)
  rmse <- sqrt(mean((test_data$duration_mins - predictions)^2))
  r_squared <- 1 - sum((test_data$duration_mins - predictions)^2) / 
    sum((test_data$duration_mins - mean(test_data$duration_mins))^2)
  list(RMSE = rmse, R2 = r_squared)
}

evaluate_model(chores.lm, test_chore)
evaluate_model(chores.lm2, test_chore)
evaluate_model(chores.lm3, test_chore)
evaluate_model(interaction.lm, test_chore)
evaluate_model(interaction.lm2, test_chore)

# ---------------------------analyzing childcare ---------------------------
# Extract childcare activities and assign Type
children <- actdf %>%
  filter(TRTIER2 %in% childcareCodes) %>%
  mutate(Type = ifelse(TUTIER1CODE == 4, "Non-HH", "HH"))

# Merge with household data
childdf <- merge(df, children, by = "TUCASEID", all = FALSE)

# Find if the guardian is with a child
whoChild <- who %>% filter(TUCASEID %in% childdf$TUCASEID)
withChild <- merge(whoChild, rosdf, by = c("TUCASEID", "TULINENO")) %>%
  filter(Under18 == TRUE)

# Create a dataframe with childcare activities where a child is present
new_df <- merge(childdf, withChild, by = c("TUCASEID", "TUACTIVITY_N"), all.x = TRUE, all.y = FALSE)

# Merge employment status and gender with the childcare data
employed_childcare <- childdf %>% filter(TUCASEID %in% employedGuardian$TUCASEID) %>% 
  mutate(status = "Employed")
unemployed_childcare <- childdf %>% filter(TUCASEID %in% unemployedGuardian$TUCASEID) %>% 
  mutate(status = "Unemployed")
children_both <- bind_rows(employed_childcare, unemployed_childcare)

# Summarize average time spent on childcare by type, employment status, and gender
childcare_summary <- children_both %>%
  group_by(Type, status, TESEX) %>%
  summarise(average_time = mean(duration_mins, na.rm = TRUE),
            count = n()) %>%
  mutate(TESEX = factor(TESEX, levels = c(1, 2), labels = c("Male", "Female")))
childcare_summary

# visual: generally how much time is spent with hh vs non-hh children?
# logged
ggplot(children, aes(x=Type, y=log(duration_mins), fill = Type)) + geom_violin() +
  labs(title= "Time dedicated to Household v Non-Household Children", x="Child type", y="Log Time Spent (mins)")
# not logged
ggplot(children, aes(x=Type, y=duration_mins, fill = Type)) + geom_violin() +
  labs(title= "Time dedicated to Household v Non-Household Children", x="Child type", y="Time Spent (mins)")

## Linear Regression Models
# Cross-validation split
split2 <- initial_split(children, prop = 0.7)
train_child <- training(split2)
test_child <- testing(split2)