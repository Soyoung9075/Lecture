library(foreign)
library(haven)
library(tidyverse)
library(rstatix)
library(reshape)
library(nlme)

exer <- read.dta("R data/exercise.dta")
exer

# read_dta 로 불러오면 tibble 형식
# read.dta 로 불러오면 data frame 형식으로 불러옴
# reshape 함수 쓰려면 반드시 read.dta로 불러와야 함

# 8.1.1 On a single graph, construct a time plot that displays the mean strength
# versus time(in days) for the two treatment groups. Describe the general 
# characteristics of the time trend for the two exercise groups

exer_mean <- exer %>% group_by(group) %>%
  summarise(
    day_0 = mean(y0, na.rm = TRUE),
    day_2 = mean(y2, na.rm = TRUE),
    day_4 = mean(y4, na.rm = TRUE),
    day_6 = mean(y6, na.rm = TRUE),
    day_8 = mean(y8, na.rm = TRUE),
    day_10 = mean(y10, na.rm = TRUE),
    day_12 = mean(y12, na.rm = TRUE)
    
  ) 
exer_mean

exer_mean_lf <- exer_mean %>% pivot_longer(c("day_0","day_2","day_4","day_6", 
                                             "day_8", "day_10", "day_12"),
                                         names_to = "time_point", values_to = "y")
exer_mean_lf$group <- as.factor(exer_mean_lf$group)

ggplot(exer_mean_lf, aes(time_point, y)) +
  geom_line(aes(group = group, color = group)) +
  geom_point(aes(colour = group)) +
  labs(x = "Time(day)", y = "Measures of Strength") +
  scale_x_discrete(limits 
                   = c("day_0","day_2","day_4","day_6", 
                       "day_8", "day_10", "day_12"))

# 8.1.2 Read the data from the external file and put the data in a "u////////////nivariate"
# or "long" format, with 7 "records" per patient.



exerlong <- reshape(exer, varying = c("y0", "y2", "y4", "y6", "y8", "y10", "y12"),
                    v.names="y", timevar = "time",
                    idvar = "id", times = c(0,2,4,6,8,10,12), direction="long")


# 8.1.3 Fit a model with randomly varying intercepts and slopes, and allow
# the mean values of the intercept and slope to depend on treatment group. 

# group을 다시코딩
# trt = 1 -> 1, trt =2 -> 0 으로 코딩
exerlong$group <- ifelse(exerlong$group == 1, 1, 0)

model <- lme(y ~ group + time + group*time, random= ~ time | id, data = exerlong,
             na.action = na.omit)
summary(model)

# (a) What is the estimated variance of the random intercepts?
# 3.1548522^2 = 9.953092

# (b) what is the estimated variance of the random slopes?
# 0.1852790^2 = 0.03432831

# (c) what is the estimated correlation between the random intercepts and slopes?
# -0.029

# (d) Give an interpretation to the magnitude of the estimated variance of the
# random intercepts. For example, "approximately 95% of subjects have baseline
# measures of strength between a and b)

# (e) Give an interpretation to the magnitude of the estimated variance of the
# random slopes.

# 8.1.4 Is a model with only randomly varying intercepts defensible? Explain?

# 8.1.5 What are the mean intercept and slope in the two exercise programs?


# 8.1.6 Based on the previous analysis, interpret the effect of treatment on
# changes in strength. Does your analysis suggest a difference between the
# two groups? 

# 8.1.8 Obtain the predicted (empirical BLUP) intercept and slope for each subject.
random.effects(model)


-------------------------------------------------------

cd4_factor <- cd4
cd4_factor$group <- as.factor(cd4_factor$group)

ggplot(data = cd4_factor, mapping = aes(x = week, y = logcd4))+
  geom_smooth(aes(group=group, color = group), se = FALSE) +
  scale_x_continuous(breaks=c(0, 8, 16, 24, 32, 40))

cd4 <- read.dta("cd4.dta")
cd4
attach(cd4)
trt2 <- ifelse(group == 2, 1, 0)
trt3 <- ifelse(group == 3, 1, 0)
trt4 <- ifelse(group == 4, 1, 0)

week16 <- (week-16)*I(week>16)
trt2.week <- I(trt2==1)*week
trt2.week16 <- I(trt2==1)*week16
trt3.week <- I(trt3==1)*week
trt3.week16 <- I(trt3==1)*week16
trt4.week <- I(trt4==1)*week
trt4.week16 <- I(trt4==1)*week16

model1 <- lme(logcd4 ~ week + week16 
              + trt2.week + trt2.week16
              + trt3.week + trt3.week16
              + trt4.week + trt4.week16,
              random= ~ week + week16 | id)

summary(model1)

Sigma <- vcov(model1)
b <- fixed.effects(model1)
L1 <- matrix(c(0,0,0,1,1,0,0,0,0), nrow = 1)
L2 <- matrix(c(0,0,0,0,0,1,1,0,0), nrow = 1)
L3 <- matrix(c(0,0,0,0,0,0,0,1,1), nrow = 1)
x1 <- wald.test(Sigma, b, L=L1)
x2 <- wald.test(Sigma, b, L=L2)
x3 <- wald.test(Sigma, b, L=L3)
print(x1, digits = 5)
print(x2, digits = 5)
print(x3, digits = 5)

model1_reduced <- lme(logcd4 ~ week + week16,random= ~ week + week16 | id)
library(lmtest)
lrtest(model1, model1_reduced)

# 8.2.7

b <- fixed.effects(model1)
# trt1
# 0~16
y1 <- function(t) {
  if (t<=16) {
    b[1]+b[2]*t
  } else {
      b[1]+b[2]*t+b[3]*(t-16)
    }
}


x = c(0,8,16,24,32,40)
y = vector("double")
for (i in seq_along(x)) {
  y[i] <- y1(x[i])
}
y
plot(x, y, type = "l", xaxt='n')
axis(side=1, at=c(0,8,16,24,32,40))
