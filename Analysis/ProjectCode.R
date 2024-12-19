## import packages
library(tidyverse)
library(lubridate)
#install.packages("hms")
library(hms)
library(ggplot2)
#install.packages("rsample")
library(rsample)
#install.packages("caret")
library(caret)

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

houseChores <- actdf %>% filter(TUTIER1CODE == 2)
# merge with household data
choredf <- merge(df, houseChores, by = "TUCASEID", all = FALSE)
choredf <- choredf %>% select(TUCASEID, TULINENO.x, TELFS, TEAGE, TESEX, TRCHILDNUM,
                              TUTIER1CODE, TRTIER2, duration_mins)

# one row per person, columns are time spent on chores and childcare
chores_per_person <- choredf %>% group_by(TUCASEID) %>%
  summarise(chores_time = sum(duration_mins, na.rm = TRUE), TESEX = first(TESEX),
    TELFS = first(TELFS), TEAGE = first(TEAGE), TRCHILDNUM = first(TRCHILDNUM), 
    .groups = "drop")

children <- actdf %>% filter(TRTIER2 %in% childcareCodes)
# merge with household data
childdf <- merge(df, children, by = "TUCASEID", all = FALSE)
childcare_per_person <- childdf %>% group_by(TUCASEID) %>%
  summarise(childcare_time = sum(duration_mins, na.rm = TRUE), TESEX = first(TESEX),
            TELFS = first(TELFS), TEAGE = first(TEAGE), TRCHILDNUM = first(TRCHILDNUM),
            .groups = "drop")

# Combine chores and childcare
combined <- full_join(chores_per_person, childcare_per_person, by = "TUCASEID") %>%
  mutate(chores_time = replace_na(chores_time, 0), # put 0 if activity not done
         childcare_time = replace_na(childcare_time, 0),
         TESEX = coalesce(TESEX.x, TESEX.y), # merge the two columns
         TELFS = coalesce(TELFS.x, TELFS.y),
         TEAGE = coalesce(TEAGE.x, TEAGE.y),
         TRCHILDNUM = coalesce(TRCHILDNUM.x, TRCHILDNUM.y)) %>%
  select(TUCASEID, chores_time, childcare_time, TESEX, TELFS, TEAGE, TRCHILDNUM)

# employment status
combined <- combined %>% 
  mutate(status = case_when(TELFS %in% c(1, 2) ~ "Employed",
    TELFS %in% c(3, 4, 5) ~ "Unemployed", TRUE ~ "Unknown"))

# sex labels
combined <- combined %>%
  mutate(sex = factor(TESEX, levels = c(1, 2), labels = c("Male", "Female")))

atus <- combined %>% select(TUCASEID, chores_time, childcare_time, status, sex, 
                            TRCHILDNUM, TEAGE)

# getting proportions, can be 0
atus$timeProp <- (atus$chores_time) / (atus$chores_time + atus$childcare_time) # spend more time on chores
summary(atus$timeProp)
hist(atus$timeProp) # logging can't really work because of 0s and 1s
boxplot(atus$timeProp~atus$status)
#summary(atus$chores_time)
#summary(atus$childcare_time)
#head(atus)

## Visuals
# Violin + boxplots for Chore Time
ggplot(atus, aes(x = status, y = chores_time, fill = sex)) +
  geom_violin(alpha = 0.5) + # highlights the boxplots compared to violins
  geom_boxplot(width = 0.2, position = position_dodge(0.9)) + # boxplots go inside
  labs(title = "Distribution of Chore Time by Employment Status and Sex",
       x = "Employment Status", y = "Chore Time (minutes)") +
  scale_fill_manual(values = c("Male" = "orange", "Female" = "cyan")) +
  theme_minimal()

# Violin + boxplots for Childcare Time
ggplot(atus, aes(x = status, y = childcare_time, fill = sex)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Distribution of Childcare Time by Employment Status and Sex",
       x = "Employment Status", y = "Childcare Time (minutes)") +
  scale_fill_manual(values = c("Male" = "orange", "Female" = "cyan")) +
  theme_minimal()

# Violin + boxplots for time proportions
ggplot(atus, aes(x = status, y = timeProp, fill = sex)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.1, position = position_dodge(0.9), outlier.shape = NA) +
  labs(title = "Proportion of Time Spent on Chores to Childcare \nby Employment Status and Sex",
       x = "Employment Status", y = "Proportion of Chores Time") +
  scale_fill_manual(values = c("Male" = "orange", "Female" = "cyan")) +
  theme_minimal()

set.seed(123)
## Regression Models
# Cross-validation split
split <- initial_split(atus, prop = 0.7)
train <- training(split)
test <- testing(split)

# slr
slr.lm <- lm(timeProp ~ status, data = train)

# adjust for age, sex, and number of children
mlr.lm <- lm(timeProp ~ status + TEAGE + sex, data = train)
mlr.lm2 <- lm(timeProp ~ status + TEAGE + sex + TRCHILDNUM, data = train)
interact.lm <- lm(timeProp ~ status * sex + TEAGE + TRCHILDNUM, data = train)
interact.lm2 <- lm(timeProp ~ status * TRCHILDNUM + sex + TEAGE, data = train)
noStatus <- lm(timeProp ~ sex + TEAGE + TRCHILDNUM, data = train)

# Evaluate models
summary(slr.lm)
summary(mlr.lm) 
summary(mlr.lm2) 
summary(interact.lm)
summary(interact.lm2) 

# p-value - anova
anova(slr.lm)
anova(mlr.lm)
anova(mlr.lm2)
anova(interact.lm) # interactions aren't significant
anova(interact.lm2)
anova(noStatus, interact.lm)

# calculate rmse on train and test
calculate_rmse <- function(model, data) {
  predictions <- predict(model, data)
  rmse <- sqrt(mean((data$timeProp - predictions)^2, na.rm = TRUE))
  return(rmse)
}

calculate_rmse(slr.lm, train) # 0.3364047
calculate_rmse(mlr.lm, train) # 0.3263021
calculate_rmse(mlr.lm2, train) # 0.3254458
calculate_rmse(interact.lm, train) # 0.3252003
calculate_rmse(interact.lm2, train) # 0.3253046

calculate_rmse(slr.lm, test) # 0.3366657
calculate_rmse(mlr.lm, test) # 0.3304004
calculate_rmse(mlr.lm2, test) # 0.3299375
calculate_rmse(interact.lm, test) # 0.3301379
calculate_rmse(interact.lm2, test) # 0.3301058

# adj, r, bic for whole dataset
evaluate_model <- function(model, data, p) {
  predictions <- predict(model, data)
  actuals <- data$timeProp
  n <- nrow(data)
  
  r_squared <- 1 - sum((actuals - predictions)^2, na.rm = TRUE) / 
    sum((actuals - mean(actuals, na.rm = TRUE))^2, na.rm = TRUE)
  adj_r_squared <- 1 - (sum((actuals - predictions)^2) / (n - (p+1))) / 
    (sum((actuals - mean(actuals))^2) / (n - 1))
  bic <- BIC(model)
  
  list(R2 = r_squared, adj_R2 = adj_r_squared, BIC = bic)
}

evaluate_model(slr.lm, atus, 1) 
evaluate_model(mlr.lm, atus, 3) 
evaluate_model(mlr.lm2, atus, 4) 
evaluate_model(interact.lm, atus, 4) 
evaluate_model(interact.lm2, atus, 4) 

# readme: explain what the other files are i.e poster, data set, atleast one code file

# checking model assumptions
plot(interact.lm)

# Predict with confidence intervals
confint(interact.lm)
employed <- train %>% filter(status == "Employed") %>% sample_n(20)
unemployed <- train %>% filter(status == "Unemployed") %>% sample_n(20)
avg_children <- mean(train$TRCHILDNUM, na.rm = TRUE)
employed$TRCHILDNUM <- avg_children
unemployed$TRCHILDNUM <- avg_children
predictData <- bind_rows(employed, unemployed)
predictions <- predict(interact.lm, newdata = predictData, interval = "confidence")
predictData <- predictData %>% mutate(fit = predictions[, "fit"], lwr = predictions[, "lwr"], upr = predictions[, "upr"])

# Plotting the Confidence Intervals
ggplot(predictData, aes(x = TEAGE, y = fit, color = sex, fill = sex)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  labs(title = "Confidence Intervals for Time Proportion Allocation by Status and Sex",
    x = "Age", y = "Predicted Proportion of Time Spent on Chores") +
  facet_wrap(~status) + # Separate by employment status
  scale_color_manual(values = c("Male" = "orange", "Female" = "cyan")) +
  scale_fill_manual(values = c("Male" = "orange", "Female" = "cyan")) +
  theme_minimal()
