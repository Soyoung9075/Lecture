
library(tidyverse)
library(readr)
library(rstatix)
library(skimr)

lead_dat <- read_csv("R data/lead.dat.csv", col_names = TRUE)

lead_dat <- lead_dat %>% rename(Week_0 = Y_1, Week_1 = Y_2, Week_4 = Y_3,
                                Week_6 = Y_4)

summary <- lead_dat %>%
  get_summary_stats(Week_0, Week_1, Week_4, Week_6, show=c("mean", "sd"))

var <- round(c(var(lead_dat$Week_0), 
               var(lead_dat$Week_1),
               var(lead_dat$Week_4),
               var(lead_dat$Week_6)), digit=2)
summary$var <- var
summary <-
  summary %>% rename(sample_mean = mean, standard_deviations = sd, variance = var)
summary

#   variable     n sample_mean standard_deviations variance
#   <chr>    <dbl>       <dbl>               <dbl>    <dbl>
# 1 Week_0     100        26.4                5.00     25.0
# 2 Week_1     100        19.1                8.67     75.2
# 3 Week_4     100        19.8                8.09     65.4
# 4 Week_6     100        22.2                7.76     60.2


#---------------------------------------------------------------
install.packages("readr")
library(readr)
lead_dat <- read_csv("lead.dat.csv", col_names = TRUE)
lead_dat

lead_dat <- lead_dat %>% rename(Week_0 = Y_1, Week_1 = Y_2, Week_4 = Y_3,
                    Week_6 = Y_4)

lead_dat_longer <-lead_dat %>%
  pivot_longer(c('Week_0', 'Week_1', 'Week_4',
                 'Week_6'), names_to = "time_point",
               values_to = "blood_lead_level")

library(ggplot2)
ggplot(lead_dat_longer, aes(time_point, blood_lead_level)) +
  geom_line(aes(group=ID)) +
  geom_point(aes(colour = ID), colour = "black") +
  theme(legend.position = "none") +
  labs(x = "Time(weeks)", y = "Blood lead level")
#--------------------------------------------------------------------
# Construct a time plot of the mean blood lead level versus time.

lead_dat <- lead_dat %>% rename(Week_0 = Y_1, Week_1 = Y_2, Week_4 = Y_3,
                                Week_6 = Y_4)

lead_dat_longer <-lead_dat %>%
  pivot_longer(c('Week_0', 'Week_1', 'Week_4',
                 'Week_6'), names_to = "time_point",
               values_to = "blood_lead_level")

lead_dat_mean <- lead_dat_longer %>%
  group_by(time_point) %>%
  summarise(mean_blood_level = mean(blood_lead_level, na.rm = TRUE))

ggplot(lead_dat_mean, aes(time_point, mean_blood_level)) +
  geom_line(aes(group=1)) +
  geom_point() +
  labs(x = "Time(weeks)", y = "Mean Blood lead level",
       title = "The change of mean blood lead level over time")

#--------------------------------------------------------------------
# 2.1.3 Calculate the 4X4 covariance and correlation matrices 
# for the four repeated measures of blood lead levels. 

lead_dat_cov <- select(lead_dat, -(ID))
round(cov(lead_dat_cov),1)

# Covariance matrix
#        Week_0 Week_1 Week_4 Week_6
# Week_0   25.0   18.2   18.9   21.8
# Week_1   18.2   75.2   59.2   37.5
# Week_4   18.9   59.2   65.4   36.5
# Week_6   21.8   37.5   36.5   60.2

round(cor(lead_dat_cov),2)

# Correlation matrix
#        Week_0 Week_1 Week_4 Week_6
# Week_0   1.00   0.42   0.47   0.56
# Week_1   0.42   1.00   0.84   0.56
# Week_4   0.47   0.84   1.00   0.58
# Week_6   0.56   0.56   0.58   1.00


lead_dat_full <- read_csv("lead.dat_full.csv", col_names = TRUE)
lead_full_cov <- lead_dat_full %>%
  filter(Treatment_Group == 'P') %>%
  select(-(ID:Treatment_Group))
round(cov(lead_full_cov),1)
round(cor(lead_full_cov),2)

lead_full_tr <- lead_dat_full %>%
  filter(Treatment_Group == 'A') %>%
  select(-(ID:Treatment_Group))
round(cov(lead_full_tr),1)
round(cor(lead_full_tr),2)

#--------------------------------------------------------------------
round(18.2/(5.00 * 8.67),2)
# [1] 0.42
      