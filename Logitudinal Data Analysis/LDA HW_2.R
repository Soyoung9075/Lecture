
library(tidyverse)
library(rstatix)

# 5.1.1 Read the data from the external file and keep it in a "multivaraite or "wide" format.
ch <- read.dta("R data/cholesterol.dta")
ch
ch <- ch %>% rename(baseline = y1, month_6 = y2, month_12 = y3, month_20 = y4,
                    month_24 = y5)



# 5.1.2 Calculate the sample means, standard deviations, and variances of the serum cholesterol levels 
# at each occasion for each treatment group.

# group 1
ch %>%
  filter(group == 1) %>%
  get_summary_stats(baseline, month_6, month_12, month_20, month_24, 
                    show = c("mean", "sd")) %>%
  mutate(var = sd^2)

# group 2
ch %>%
  filter(group == 2) %>%
  get_summary_stats(baseline, month_6, month_12, month_20, month_24, 
                    show = c("mean", "sd")) %>%
  mutate(var = sd^2)

# 5.1.3 On a single graph, construct a time plot that displays the mean serum
# cholesterol versus time (in months) for the two treatment group. Describe
# the general characteristics of the time trends for the two groups.

ch_mean <- ch %>% group_by(group) %>%
  summarise(
    baseline = mean(baseline, na.rm = TRUE),
    month_6 = mean(month_6, na.rm = TRUE),
    month_12 = mean(month_12, na.rm = TRUE),
    month_20 = mean(month_20, na.rm = TRUE),
    month_24 = mean(month_24, na.rm = TRUE)
    
  ) %>% 
  mutate(group = replace(group, group ==1, "High-Dose")) %>%
  mutate(group = replace(group, group == 2, "Placebo"))
ch_mean 

ch_mean_lf <- ch_mean %>% pivot_longer(c("baseline","month_6","month_12","month_20", "month_24"),
                                       names_to = "time_point", values_to = "y")
ch_mean_lf$group <- as.factor(ch_mean_lf$group)

ggplot(ch_mean_lf, aes(time_point, y)) +
  geom_line(aes(group = group, color = group)) +
  geom_point(aes(colour = group)) +
  labs(x = "Time(month)", y = "Mean serum cholesterol") +
  scale_x_discrete(limits 
                   = c("baseline","month_6","month_12","month_20", "month_24"))

# 5.1.4 Next read the data from the external file and put the data in a
# "univariate" or "long" format, with five "records" per subject.

chlong <- reshape(ch, idvar="id", 
                  varying=c("baseline","month_6","month_12","month_20", "month_24"), 
                  v.names="y", timevar="time", time=1:5, direction="long")
chlong

# 5.1.5 Assuming an unstructured covariance matrix, conduct and analysis of
# response profiles. Determine whether the patterns of change over time differ
# in the two treatment groups

na <- chlong %>%
  filter(is.na(y))
dim(na)
chlong_na <- na.omit(chlong)
dim(chlong_na)
attach(chlong)

month <- time
month[time==1] <- 0
month[time==2] <- 6
month[time==3] <- 12
month[time==4] <- 20
month[time==5] <- 24

month.f <- factor(month, c(0,6,12,20,24))

library(nlme)

model <- gls(y ~ group*month.f, chlong, corr=corSymm(, form= ~ time | id), 
             weights = varIdent(form = ~ 1 | month.f), na.action = na.omit)
summary(model)

library(aod)
Sigma <- vcov(model)
b <-summary(model)$coefficients
x <- wald.test(Sigma, b, Terms = 6:9)
print(x, digits = 5)

#------------------------------------------------------------------

chlong_na <- na.omit(chlong)
month <- chlong_na$time
month[chlong_na$time == 1] <- 0
month[chlong_na$time == 2] <- 6
month[chlong_na$time == 3] <- 12
month[chlong_na$time == 4] <- 20
month[chlong_na$time == 5] <- 24
month
month.f <- factor(month, c(0,6,12,20,24))
month.f
library(nlme)

model <- gls(y ~ group*month.f, chlong_na, corr=corSymm(, form= ~ time | id), 
             weights = varIdent(form = ~ 1 | month.f))
summary(model)

library(aod)
Sigma <- vcov(model)
b <-summary(model)$coefficients
x <- wald.test(Sigma, b, Terms = 6:9)
print(x, digits = 5)