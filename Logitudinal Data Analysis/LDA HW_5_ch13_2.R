ep <- read.dta("R data/epilepsy.dta")
ep

eplong <- reshape(ep, idvar="id", varying = c("y0", "y1", "y2", "y3", "y4"),
                  v.names = "y", timevar = "time", time = 1:5, direction = "long")
eplong <- eplong[order(eplong$id, eplong$time),]
eplong
attach(eplong)
eplong$time <-as.factor(eplong$time)
eplong$time <- relevel(eplong$time, ref="1")

timen <- as.numeric(eplong$time)
timex1 <- eplong$trt*I(eplong$time==2)
timex2 <- eplong$trt*I(eplong$time==3)
timex3 <- eplong$trt*I(eplong$time==4)
timex4 <- eplong$trt*I(eplong$time==5) 

install.packages("geepack")
library(geepack)

model1 <- geeglm(y ~ time + timex1 + timex2 + timex3 + timex4, data = eplong,
                 id = id,family = poisson("log"), corstr = "exch",
                 std.err = "san.se")
summary(model1)

# interaction term에 대한 검정
library(aod)
sigma1 <- vcov(model1)
b1 <- coef(model1)
x1 <- wald.test(sigma1, b1, Terms = 6:9)
print(x1, digit=5)

model2 <- geeglm(y~ time + time*trt, data = eplong, id=id, family = poisson("log"),
                 waves = time, corstr = "exch", std.err = "san.se")
summary(model2)

eplong2 <- eplong
eplong2$time <- as.numeric(eplong2$time)
eplong2 <- subset(eplong2, time == 1|time == 4)
eplong2$time <- ifelse(eplong2$time==1, 0, 1) 
eplong2

model3 <- geeglm(y ~ time + time*trt, data = eplong2, id=id, family = poisson("log"),
                 waves = time, corstr = "exch", std.err = "san.se")
summary(model3)

timextrt <- eplong2$trt*eplong2$time
timextrt
model4 <- geeglm(y ~ time + timextrt, data = eplong2, id=id, family = poisson("log"),
                 waves = time, corstr = "exch", std.err = "san.se")
summary(model4)


# remove outlier id = 49

eplong3 <- eplong
eplong3 <- eplong3[!(eplong3$id == 49),] 

timen_a <- as.numeric(eplong3$time)
timex2_a <- eplong3$trt*I(eplong3$time==2)
timex3_a <- eplong3$trt*I(eplong3$time==3)
timex4_a <- eplong3$trt*I(eplong3$time==4)
timex5_a <- eplong3$trt*I(eplong3$time==5) 

model5 <- geeglm(y ~ time + timex2_a + timex3_a + timex4_a + timex5_a, 
                 data = eplong3,
                 id = id,family = poisson("log"), corstr = "exch",
                 std.err = "san.se")
summary(model5)
sigma5 <- vcov(model5)
b5 <- coef(model5)
x5 <- wald.test(sigma5, b5,Terms = 6:9)
print(x5, digits = 5)

eplong5 <- eplong2
eplong5 <- eplong5[!(eplong5$id == 49),]
timextrt_a <- eplong5$trt*eplong5$time
model6 <- geeglm(y ~ time + timextrt_a, data = eplong5, id=id, family = poisson("log"),
                 waves = time, corstr = "exch", std.err = "san.se")
summary(model6)


# 그래프 그려보기 
ep_mean <- aggregate(cbind(y0,y1,y2,y3,y4)~trt, data = ep, mean)
ep_mean <- as_tibble(ep_mean)
ep_mean_long <- ep_mean %>% pivot_longer(c("y0", "y1", "y2", "y3", "y4"),
                                         names_to = "time", values_to = "y")
ep_log <- ep_mean_long %>% mutate("log(y)" = log(y))
ep_log$trt <- as.factor(ep_log$trt)

g1 <- ggplot(ep_log, aes(time, log(y))) +
  geom_line(aes(group = trt, color = trt)) +
  geom_point(aes(colour = trt)) +
  labs(x = "Time", y = "log(y)") +
  scale_x_discrete(limits = c("y0", "y1", "y2", "y3", "y4"))


# outlier 제거?
ep2 <- ep
ep2 <- ep2[!(ep2$id ==49),]
ep2_mean <- aggregate(cbind(y0,y1,y2,y3,y4)~trt, data = ep2, mean)
ep2_mean <- as_tibble(ep2_mean)
ep2_mean_long <- ep2_mean %>% pivot_longer(c("y0", "y1", "y2", "y3", "y4"),
                                           names_to = "time", values_to = "y")
ep2_log <- ep2_mean_long %>% mutate("log(y)" = log(y))
ep2_log$trt <- as.factor(ep2_log$trt)

g2 <- ggplot(ep2_log, aes(time, log(y))) +
  geom_line(aes(group = trt, color = trt)) +
  geom_point(aes(colour = trt)) +
  labs(x = "Time", y = "log(y)") +
  scale_x_discrete(limits = c("y0", "y1", "y2", "y3", "y4"))
g2

