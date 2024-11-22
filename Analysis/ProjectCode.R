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

# Missing those who don't do chores: minutes on chores, childcare and whether they are employed
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

# gender labels
combined <- combined %>%
  mutate(gender = factor(TESEX, levels = c(1, 2), labels = c("Male", "Female")))

atus <- combined %>% select(TUCASEID, chores_time, childcare_time, status, gender, 
                            TRCHILDNUM, TEAGE)
atus <- atus %>% mutate(time_ratio = ifelse(childcare_time == 0, NA, chores_time / childcare_time))
head(atus)
set.seed(123)
## Multivariate Regression Models
# Cross-validation split
split <- initial_split(atus, prop = 0.7)
train <- training(split)
test <- testing(split)

# slr
slr.lm <- lm(cbind(chores_time, childcare_time) ~ status, data = train)

# adjust for age, sex, and number of children
mlr.lm <- lm(cbind(chores_time, childcare_time) ~ status + TEAGE + gender, data = train)
mlr.lm2 <- lm(cbind(chores_time, childcare_time) ~ status + TEAGE + gender + TRCHILDNUM, data = train)
interact.lm <- lm(cbind(chores_time, childcare_time) ~ status * gender, data = train)
interact.lm2 <- lm(cbind(chores_time, childcare_time) ~ status * TRCHILDNUM, data = train)

# Evaluate models
summary(slr.lm) # 0.03939, 0.02534
summary(mlr.lm) # 0.06434, 0.07571
summary(mlr.lm2) # 0.06389, 0.07498
summary(interact.lm) # 0.06193, 0.0287
summary(interact.lm2) # 0.03778, 0.02654

# RMSE
train_rmse <- function(model) sqrt(mean(model$residuals^2))
train_rmse(slr.lm) # 135.3572
train_rmse(mlr.lm) # 132.6421
train_rmse(mlr.lm2) # 132.6225
train_rmse(interact.lm) # 134.2748
train_rmse(interact.lm2) # 135.2553

# checking mlr.lm model assumptions
chore_coef <- c(Intercept = 66.818, statusUnemployed = 62.1713, TEAGE = 1.1145, genderFemale = 46.6792)
child_coef <- c(Intercept = 259.8826, statusUnemployed = 55.778, TEAGE = -4.1997, genderFemale = 9.6143)

# Add predictions to the dataset
atus <- atus %>% mutate(statusN = ifelse(status == "Unemployed", 1, 0),
                        genderN = ifelse(gender == "Female", 1, 0))

atus <- atus %>% mutate(chore_predicted = chore_coef["Intercept"] + 
                          chore_coef["statusUnemployed"] * statusN +
                          chore_coef["TEAGE"] * TEAGE + 
                          chore_coef["genderFemale"] * genderN,
                        child_predicted = child_coef["Intercept"] + 
                          child_coef["statusUnemployed"] * statusN +
                          child_coef["TEAGE"] * TEAGE + 
                          child_coef["genderFemale"] * genderN)

# get residuals
atus <- atus %>% mutate(chore_residual = chores_time - chore_predicted,
                        child_residual = childcare_time - child_predicted)

# Residual Plots # use jitter and transparent points
ggplot(atus, aes(x = chore_predicted, y = chore_residual)) +
  geom_point(alpha = 0.5) + geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Predicted: Chore Time", x = "Predicted Chore Time", 
       y = "Residuals")

ggplot(atus, aes(x = child_predicted, y = child_residual)) +
  geom_point(alpha = 0.5) + geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Predicted: Childcare Time", x = "Predicted Childcare Time", 
       y = "Residuals")

# Histogram of Residuals
ggplot(atus, aes(x = chore_residual)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Histogram of Residuals: Chore Time", x = "Residuals", y = "Count")

ggplot(atus, aes(x = child_residual)) +
  geom_histogram(bins = 30, fill = "green", alpha = 0.7) +
  labs(title = "Histogram of Residuals: Childcare Time", x = "Residuals", y = "Count")

# QQ Plots
qqnorm(atus$chore_residual, main = "QQ Plot: Chore Time Residuals")
qqline(atus$chore_residual)

qqnorm(atus$child_residual, main = "QQ Plot: Childcare Time Residuals")
qqline(atus$child_residual)

## Models comparing ratios
# Fit models to predict the ratio
# slr
rslr.lm <- lm(time_ratio ~ status, data = train)

# adjust for age, sex, and number of children
rmlr.lm <- lm(time_ratio ~ status + TEAGE + gender, data = train)
rmlr.lm2 <- lm(time_ratio ~ status + TEAGE + gender + TRCHILDNUM, data = train)
rinteract.lm <- lm(time_ratio ~ status * gender, data = train)
rinteract.lm2 <- lm(time_ratio ~ status * TRCHILDNUM, data = train)

# evaluate models on test data
evaluate_model <- function(model, test_data) {
  predictions <- predict(model, newdata = test_data)
  actuals <- test_data$time_ratio
  rmse <- sqrt(mean((actuals - predictions)^2, na.rm = TRUE))
  #r_squared <- 1 - sum((actuals - predictions)^2) / sum((actuals - mean(actuals, na.rm = TRUE))^2)
  bic <- BIC(model)
  list(RMSE = rmse, BIC = bic) 
  }

evaluate_model(rslr.lm, test) # 8.16, 7843
evaluate_model(rmlr.lm, test) # 8.05, 7854
evaluate_model(rmlr.lm2, test) # 8.01, 7860
evaluate_model(rinteract.lm, test) # 8.13, 7855
evaluate_model(rinteract.lm2, test) #8.14, 7855

# evaluate on whole dataset
evaluate_model(rslr.lm, atus) # 21.46, 7843
evaluate_model(rmlr.lm, atus) # 21.42, 7854
evaluate_model(rmlr.lm2, atus) # 21.40, 7860
evaluate_model(rinteract.lm, atus) # 21.44, 7855
evaluate_model(rinteract.lm2, atus) # 21.44, 7855

## Visuals
# Violin + boxplots for Chore Time
ggplot(atus, aes(x = status, y = chores_time, fill = gender)) +
  geom_violin(alpha = 0.5) + # highlights the boxplots compared to violins
  geom_boxplot(width = 0.2, position = position_dodge(0.9)) + # boxplots go inside
  labs(title = "Distribution of Chore Time by Employment Status and Gender",
    x = "Employment Status", y = "Chore Time (minutes)") +
  scale_fill_manual(values = c("Male" = "purple", "Female" = "pink")) +
  theme_minimal()

# Violin + boxplts for Childcare Time
ggplot(atus, aes(x = status, y = childcare_time, fill = gender)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Distribution of Childcare Time by Employment Status and Gender",
    x = "Employment Status", y = "Childcare Time (minutes)") +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "green")) +
  theme_minimal()

# proportions on tasks by employment status
proportions <- atus %>% group_by(status) %>%
  summarise(mean_chores = mean(chores_time, na.rm = TRUE),
    mean_childcare = mean(childcare_time, na.rm = TRUE)) %>%
  pivot_longer(cols = c(mean_chores, mean_childcare), names_to = "activity", 
               values_to = "time")

ggplot(proportions, aes(x = status, y = time, fill = activity)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  labs(title = "Average Time Spent on Chores vs. Childcare by Employment Status",
    x = "Employment Status", y = "Average Time (minutes)") +
  scale_fill_manual(values = c("mean_chores" = "skyblue", "mean_childcare" = "orange"), 
                    labels = c("Chores", "Childcare")) + theme_minimal()
