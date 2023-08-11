library(foreign)
library(haven)
library(tidyverse)
library(rstatix)
library(reshape)

dental <- read.dta("R data/dental.dta")
dental

# 7.1.1 On a single graph, construct a time plot that displays the mean distance
# (mm) versus age (in years) for boys and girls. Describe the time trends for boys
# and girls.

dental_mean <- dental %>% group_by(gender) %>%
  summarise(
    age_8 = mean(y1, na.rm = TRUE),
    age_10 = mean(y2, na.rm = TRUE),
    age_12 = mean(y3, na.rm = TRUE),
    age_14 = mean(y4, na.rm = TRUE))
dental_mean

dental_mean_lf <- dental_mean %>% 
  pivot_longer(c("age_8","age_10","age_12","age_14"), 
               names_to = "time_point", values_to = "y")
dental_mean_lf$gender <- as.factor(dental_mean_lf$gender)

ggplot(dental_mean_lf, aes(time_point, y)) +
  geom_line(aes(group = gender, color = gender)) +
  geom_point(aes(colour = gender)) +
  labs(x = "Age", y = "Distance(mm)") +
  scale_x_discrete(limits 
                   = c("age_8","age_10","age_12","age_14"))

# 7.1.2 Read the data from the external file and put the data in a "univariate"
# or "long" format, with four "records" per subject.

dentallong <- reshape(dental, idvar="id", 
                   varying=c("y1","y2","y3","y4"), 
                   v.names="y", timevar="age", time=1:4, direction="long")
head(dentallong)

# 7.1.3 For the "maximal" model, assume a saturated model for the mean response.
# Fit the following models for the covariance:

# (a) unstructured covariance.

attach(dentallong)

time <- dentallong$age
time[dentallong$age==1] <- 8
time[dentallong$age==2] <- 10
time[dentallong$age==3] <- 12
time[dentallong$age==4] <- 14
time.f <- factor(time, c(8,10,12,14))

library(nlme)

model1 <- gls(y ~ gender*time.f, dentallong, corr=corSymm(, form= ~ age | id), 
             weights = varIdent(form = ~ 1 | time.f))
summary(model1)

# 최종적으로 (c) heterogeneous compound symmetry 모델 선택됨
# unstructured와의 lR test 결과 p-value >0.05. 
# AIC 도 두번째로 작음

#. 7.1.4 Given the choice of model for the covariance from Problem 7.1.3, treat
# age (or time) as a categorical variable and fit a model that includes the
# effects of age, gender, and their interactions. Determine whether the pattern
# of change over time is different for boys and girls.

