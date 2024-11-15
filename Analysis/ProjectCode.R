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
# we want households with at least 1 child (<15 yrs),
ros$Under15 <- ros$TEAGE < 15
rosdf <- ros %>% group_by(TUCASEID) %>%
  mutate(Child_present = min(TEAGE) < 15) %>% # for each household, is there a child?
  mutate(Num_adults = sum(TEAGE >= 27 & TEAGE <= 60)) %>% # utmost 2 adults between 30 and 60
  filter(TEAGE >= 27 & TEAGE <= 60) %>% # ensure adults are in desired age range
  select(TUCASEID, TULINENO, TEAGE, TESEX, Under15, Child_present, Num_adults)

# filter households that meet both
rosdf <- rosdf %>% filter(Child_present & (Num_adults == 1 | Num_adults == 2))
head(rosdf)

# merge with respondent file
respdf <- resp %>% select(TUCASEID, TULINENO, TELFS, TRCHILDNUM) 
rosdf <- rosdf %>% filter(TULINENO == 1) # keep primary respondents
df <- merge(respdf, rosdf, by="TUCASEID", all = FALSE)

# --------------activity --------------
# convert from character
act$start <- as_hms(act$TUSTARTTIM)
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

employed <- respdf %>% filter(TELFS == 1 | TELFS == 2) # for now only working with respondents
employedGuardian <- rosdf %>% filter(TUCASEID %in% employed$TUCASEID)
codes <- c(3,4,5)
unemployed <- respdf %>% filter(TELFS %in% codes)
unemployedGuardian <- rosdf %>% filter(TUCASEID %in% unemployed$TUCASEID)

# ---------------------------analyzing hh chores ---------------------------
houseChores <- actdf %>% filter(TUTIER1CODE == 2)
# merge with household data
choredf <- merge(df, houseChores, by = "TUCASEID", all = FALSE)
choredf <- choredf %>% select(TUCASEID, TULINENO.x, TELFS, TEAGE, TESEX, TRCHILDNUM,
                              TUTIER1CODE, TRTIER2, duration_mins)

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

# filter chores by gender
chores_summary2 <- choredf %>% group_by(TESEX) %>%
  summarise(average_time = mean(duration_mins, na.rm = TRUE),
            count = n())
chores_summary2

# filter chores by employment status and gender
chores_summary3 <- chores_both %>% group_by(status, TESEX) %>%
  summarise(average_time = mean(duration_mins, na.rm = TRUE)) %>%
  mutate(TESEX = factor(TESEX, levels = c(1, 2), labels = c("Male", "Female")))
chores_summary3

## Visuals
# box plot
ggplot(chores_both, aes(x = status, y = duration_mins, fill = status)) +
  geom_boxplot(alpha = 0.7, width = 0.5) +
  labs(title = "Time Spent on Household Chores by Employment Status",
       x = "Employment Status", y = "Daily Time Spent (minutes)") + theme_minimal()

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
summary(chores.lm) # 5.026e-05
summary(chores.lm2) # 0.003124
summary(chores.lm3) # 0.003667
summary(interaction.lm) # 0.003835
summary(interaction.lm2) # 0.0007684

# BIC for Model Comparison
BIC(chores.lm) # 45622.07
BIC(chores.lm2) # 45625.6
BIC(chores.lm3) # 45631.63
BIC(interaction.lm) # 45622.54
BIC(interaction.lm2) # 45635.71

# RMSE
train_rmse <- function(model) sqrt(mean(model$residuals^2))

train_rmse(chores.lm) # 49.54283
train_rmse(chores.lm2) # 49.46663
train_rmse(chores.lm3) # 49.45315
train_rmse(interaction.lm) # 49.44898
train_rmse(interaction.lm2) # 49.52504

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

# Evaluate on whole dataset
evaluate_model(chores.lm, chores_both)
evaluate_model(chores.lm2, chores_both)
evaluate_model(chores.lm3, chores_both)
evaluate_model(interaction.lm, chores_both)
evaluate_model(interaction.lm2, chores_both)

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
  filter(Under15 == TRUE)

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
  summarise(average_time = mean(duration_mins, na.rm = TRUE)) %>%
  mutate(TESEX = factor(TESEX, levels = c(1, 2), labels = c("Male", "Female")))
childcare_summary

## Visuals
# generally how much time is spent with hh vs non-hh children?
# logged
ggplot(children, aes(x=Type, y=log(duration_mins), fill = Type)) + geom_violin() +
  labs(title= "Time dedicated to Household v Non-Household Children", x="Child type", y="Log Time Spent (mins)")
# not logged
ggplot(children, aes(x=Type, y=duration_mins, fill = Type)) + geom_violin() +
  labs(title= "Time dedicated to Household v Non-Household Children", x="Child type", y="Time Spent (mins)")

# time on childcare activities by type and employment status
# ggplot(childcare_summary, aes(x = status, y = average_time, fill = Type)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.5) +
#   labs(title = "Average Daily Time on Childcare Activities by Type and Employment Status",
#        x = "Employment Status", y = "Average Time (minutes)", fill = "Childcare Type") +
#   theme_minimal()

## Linear Regression Models
# Cross-validation split
split2 <- initial_split(children_both, prop = 0.7)
train_child <- training(split2)
test_child <- testing(split2)

child.lm <- lm(duration_mins ~ status, data = train_child)
child.lm2 <- lm(duration_mins ~ status + TEAGE + TESEX, data = train_child)
child.lm3 <- lm(duration_mins ~ status + TEAGE + TESEX + TRCHILDNUM, data = train_child)
childInteraction.lm <- lm(duration_mins ~ status * TESEX, data = train_child)
childInteraction.lm2 <- lm(duration_mins ~ status * TRCHILDNUM, data = train_child)

# Evaluate models
summary(child.lm) # 0.0002161
summary(child.lm2) # 0.006413
summary(child.lm3) # 0.00722
summary(childInteraction.lm) # 0.007156
summary(childInteraction.lm2) # 0.001041

# BIC for Model Comparison
BIC(child.lm) # 36806.11
BIC(child.lm2) # 36801.04
BIC(child.lm3) # 36806.39
BIC(childInteraction.lm) # 36798.47
BIC(childInteraction.lm2) # 36819.56

# RMSE
train_rmse(child.lm) # 51.2429
train_rmse(child.lm2) # 51.08386
train_rmse(child.lm3) # 51.0631
train_rmse(childInteraction.lm) # 51.06475
train_rmse(childInteraction.lm) # 51.06475

# Evaluating on test set
evaluate_model(child.lm, test_child)
evaluate_model(child.lm2, test_child)
evaluate_model(child.lm3, test_child)
evaluate_model(childInteraction.lm, test_child)
evaluate_model(childInteraction.lm, test_child)

# Evaluate on whole data set
evaluate_model(child.lm, children_both)
evaluate_model(child.lm2, children_both)
evaluate_model(child.lm3, children_both)
evaluate_model(childInteraction.lm, children_both)
evaluate_model(childInteraction.lm, children_both)

## compare the times spent on both
# Histogram for household chores
ggplot(chores_both, aes(x = duration_mins, fill = status)) +
  geom_histogram(binwidth = 10, alpha = 0.7, position = "identity") +
  facet_wrap(~ status) + labs(title = "Variability in Time Spent on Household Chores",
       x = "Time Spent (minutes)", y = "Frequency") + theme_minimal()

# Histogram for childcare time
ggplot(children_both, aes(x = duration_mins, fill = status)) +
  geom_histogram(binwidth = 10, alpha = 0.7, position = "identity") +
  facet_wrap(~ status) + labs(title = "Variability in Time Spent on Childcare",
       x = "Time Spent (minutes)", y = "Frequency") + theme_minimal()

chores_combined <- chores_combined %>%
  mutate(ActivityType = "Household Chores")

# childcare_combined <- childcare_combined %>%
#   select(-Type) %>%  # Remove Type column
#   mutate(ActivityType = "Childcare")

# Combine the two summaries
combined_summary <- bind_rows(chores_combined, childcare_combined) %>%
  mutate(TESEX = factor(TESEX, levels = c(1, 2), labels = c("Male", "Female")))

# Plot
ggplot(combined_summary, aes(x = status, y = average_time, fill = ActivityType)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.5) +
  facet_wrap(~ TESEX, labeller = labeller(TESEX = c("Male" = "Male", "Female" = "Female"))) +
  labs(title = "Average Time Spent on Household Chores and Childcare \nby Employment Status and Gender",
       x = "Employment Status", y = "Average Time (minutes)", fill = "Activity Type") +
  theme_minimal() +
  scale_fill_manual(values = c("Household Chores" = "#8c564b", "Childcare" = "#9467bd"))
