# Time Allocation to Chores and Childcare Among Employed and Unemployed Guardians
_Fridah Karimi Ntika '25, Data Science Capstone_

## Project Overview
Balancing work responsibilities with household activities and childcare is a critical challenge, particularly for families with children under 15 years old. This study investigates how employment status, gender, and other demographic factors influence time spent on chores and childcare.

## Research Question
How does allocating time to household activities and childcare in the United States differ between employed and unemployed guardians (ages 27 to 60) of children under 15 years in 2023?

## Data
The analysis uses 2023 data from the American Time Use Survey (ATUS):
* Dataset Size: 1566 observations and 7 variables
* Variables Used:
  * Employment status (employed/unemployed)
  * Sex (male/female)
  * Age
  * Number of children
  * Proportion of time allocation to chores by childcare (outcome variable)

### Cleaning
Households without children under 15 were excluded.
Households also had to have at least one guardian between 27 and 60 years old, inclusive
Data for only household chores and childcare activities was selected.

## Modeling
The study utilized multiple linear regression (MLR) models, including interaction terms, to examine the combined effects of employment status and sex.
The model with the interaction term Employment status * Sex was selected based on the adjusted R² and BIC values.
Equation:
$$
\text{Time Allocation Proportion} = 0.1863 + 0.0874 \cdot \text{Unemployed} + 0.0798 \cdot \text{Female} \\
+ 0.0104 \cdot \text{Age} - 0.0250 \cdot \text{Children} - 0.0979 \cdot (\text{Unemployed} \cdot \text{Female})
$$

## Conclusion
Employment status significantly impacts time allocation, with unemployed guardians spending more time on chores.
Sex remains a strong predictor: females consistently allocate more time to chores, regardless of employment status.
Age and the number of children also contribute to time allocation patterns.
The interaction between employment status and sex highlights nuanced differences, particularly among unemployed males and females.

## Technologies Used
R
