library(foreign)
library(haven)
library(tidyverse)
library(rstatix)
library(reshape)


rat <- read.dta("rat.dta")
rat

# 6.1.1 On a single graph, construct a time plot that displays the mean weights
# versus time (in weeks) for the three groups. Describe the general characteristics
# of the time trends for the three groups

rat_mean <- rat %>% group_by(group) %>%
  summarise(
    baseline = mean(y1, na.rm = TRUE),
    week_1 = mean(y2, na.rm = TRUE),
    week_2 = mean(y3, na.rm = TRUE),
    week_3 = mean(y4, na.rm = TRUE),
    week_4 = mean(y5, na.rm = TRUE)
    
  ) 
rat_mean

rat_mean_lf <- rat_mean %>% pivot_longer(c("baseline","week_1","week_2","week_3", "week_4"),
                                       names_to = "time_point", values_to = "y")
rat_mean_lf$group <- as.factor(rat_mean_lf$group)

ggplot(rat_mean_lf, aes(time_point, y)) +
  geom_line(aes(group = group, color = group)) +
  geom_point(aes(colour = group)) +
  labs(x = "Time(week)", y = "Weight(grams)") +
  scale_x_discrete(limits 
                   = c("baseline","week_1","week_2","week_3", "week_4"))

# 6.1.2 Read the data from the external file and put the data in a "univariate"
# or "long" format, with five "records" per subject.

ratlong <- reshape(rat, idvar="id", 
                  varying=c("y1","y2","y3","y4", "y5"), 
                  v.names="y", timevar="time", time=0:4, direction="long")
head(ratlong)

# 6.1.3 Assume that that the rate of increase in each group is approximately
# constant throughout the duration of the study. Assuming an unstructured
# covariance matrix, construct a test of whether the rate of increase differs
# in the groups. 

attach(ratlong)

group.f <- factor(group, c(1,2,3))
week <- time
week[time==0] <- 1
week[time==1] <- 2
week[time==2] <- 3
week[time==3] <- 4
week[time==4] <- 5

library(nlme)

model <- gls(y ~ group.f*time, ratlong, corr=corSymm(, form= ~ week | id), 
             weights = varIdent(form = ~ 1 | week))
summary(model)

library(aod)
Sigma <- vcov(model)
b <-summary(model)$coefficients
x <- wald.test(Sigma, b, Terms = 5:6)
print(x, digits = 5)
L <- matrix(c(0,0,0,0,0,0,0,0,1,0,0,1), nrow = 2)
print(wald.test(Sigma, b, L=L), digits=5)

# 6.1.4 On a single graph, construct a time plot that displays the estimated
# mean weight versus time (in weeks) for the three treatment groups from
# the results generated from Problem 6.1.3

f1 <- function(t){b[1]+b[4]*t}
f2 <- function(t){b[1]+b[2]+(b[4]+b[5])*t}
f3 <- function(t){b[1]+b[3]+(b[4]+b[6])*t}
t<- 1:5
plot(t, f1(t), type='l', ylim=c(50,190), col = 'black', xlab = "Week"
     , ylab= "Weight(grams)")
lines(t, f2(t), type='l',ylim=c(50,190), col = 'red')
lines(t, f3(t), type='l',ylim=c(50,190), col = 'blue')
legend("topleft", c("control", "group2", "group3"), 
       col=c("black", "red", "blue"), lty = 1)

# 6.1.6 The study investigators conjectured that there would be an increase
# in weight, but that the rate of increase would level-off toward the end of
# the study. They also conjectured that this pattern of change may differ in
# the three treatment groups. Assuming an unstructured covariance matrix,
# construct a test of this hypothesis.

attach(ratlong)
timesqr <- time^2
ratlong$timesqr <- timesqr
model2 <- gls(y ~ group.f*time + group.f*timesqr,
              corr=corSymm(, form= ~ week | id), 
              weights = varIdent(form = ~ 1 | week),method="ML")

summary(model2)

Sigma2 <- vcov(model2)
b2 <-summary(model2)$coefficients
x2 <- wald.test(Sigma2, b2, Terms = 6:9)
print(x2, digits = 5)

# 6.1.7 Compare and contrast the results from Problems 6.1.3 and 6.1.6.
# Does a model with only a linear trend in time adequately account for the
# pattern of change in the three treatments groups? Provide results that
# support your conclusion.

# H0 : reduced model (y ~ group.f * time)
# H1 : full model (y ~ group.f * time + group.f * timesqr)
model1 <- gls(y ~ group.f*time, ratlong, corr=corSymm(, form= ~ week | id), 
             weights = varIdent(form = ~ 1 | week), method = "ML")
install.packages("lmtest")
library(lmtest)
lrtest(model1, model2)

# -> model with quadratic terms are more appropriate than linear model.


