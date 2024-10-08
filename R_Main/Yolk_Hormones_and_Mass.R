
### Analysis of Yolk Hormones and Yolk Mass
# Yolk Corticosterone, Yolk Testosterone, and Yolk Thyroxine
# Yolk Wet Mass

# Load Packages ----
library(ggplot2)
library(lme4)
library(lmerTest)
library(car)
library(lubridate)
library(car)
library(emmeans)

# Load "Yolk_Hormones.csv" 
# Hereon, data file is named "dat"
dat <- read.csv("Data/Yolk_Hormones.csv")

# Clean data
dat$nest <- as.factor(dat$nest) # unique nest ID
dat$site <- as.factor(dat$site) # codes for the 3 sites: "N" natural, "R" rural, "U" urban
dat$trt <- as.factor(dat$trt) # code for treatment groups: "C" control, "T" food supplemented
dat$layorder <- as.numeric(dat$layorder) # order number an egg was laid in, 1-6
dat$year <- as.factor(dat$year) # year egg was laid and collected
dat$date <- as.Date(dat$date, "%m/%d/%y") # modify the format of the date of the year
dat$julian <- yday(dat$date) # create a new column for julian date
dat$plate <- as.factor(dat$plate) # the EIA plate the sample was on for corticosterone and Testosterone
dat$plate_T4 <- as.factor(dat$plate_T4) # the EIA plate the sample was on for Thyroxine
dat$yolk_mass <- as.numeric(dat$yolk_mass) # wet yolk mass measured
dat$clutch_size <- as.integer(dat$clutch_size) # number of eggs in the entire clutch
dat$cort <- as.numeric(dat$cort) # corticosterone level measured in yolk
dat$test <- as.numeric(dat$test) # testosterone level measured in yolk
dat$t4 <- as.numeric(dat$t4) # thyroxine level measured in yolk

# Rename groups
levels(dat$site)
levels(dat$site) <- c("Natural", "Rural", "Urban")
levels(dat$trt)
levels(dat$trt) <- c("Control", "Food")


################################################################################
## Yolk Corticosterone

# Visualize
# Basic plot of yolk Corticosterone across sites and treatment groups
ggplot(data = dat, aes(x = site, y = cort, colour = trt, group = trt)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.9)) +
  stat_summary(fun = mean, geom = "point", size = 5, position=position_dodge(width=0.9), 
               color = "black", alpha = 0.8) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1, linewidth = 1, 
               color = "black", alpha = 0.5, position=position_dodge(width=0.9))

# Basic plot of yolk Corticosterone across laying sequence
ggplot(dat, aes(x = layorder, y = cort, color = trt, group = trt)) +
  geom_point(aes(color=trt), position = position_jitterdodge(dodge.width = 0.5)) +
  geom_smooth(method = "lm", aes(colour = trt)) +
  facet_wrap(~site)+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9)) +
  theme_classic()
 

#################################
# Generalized Linear Mixed Effect Model of Yolk Corticosterone
cort_best_model <- glmer(cort ~ site * trt + layorder + clutch_size + julian + year + 
                      (1|nest), family = Gamma(link = "log"), data = dat)
summary(cort_best_model)
AIC(cort_best_model)

# Post Hoc Tests
post_hoc_yolk_cort <- emmeans(cort_best_model, list(pairwise ~ site * trt))
pairs(post_hoc_yolk_cort, simple = "each")
plot(post_hoc_yolk_cort)



################################################################################
## Yolk Testosterone

# Visualize
# Basic plot of yolk Testosterone across sites and treatment groups
ggplot(data = dat, aes(x = site, y = test, colour = trt, group = trt)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.9)) +
  stat_summary(fun = mean, geom = "point", size = 5, position=position_dodge(width=0.9), 
               color = "black", alpha = 0.8) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1, linewidth = 1, 
               color = "black", alpha = 0.5, position=position_dodge(width=0.9))

# Basic plot of yolk Testosterone across laying sequence
ggplot(dat, aes(x = layorder, y = test, color = trt, group = trt)) +
  geom_point(aes(color=trt), position = position_jitterdodge(dodge.width = 0.5)) +
  geom_smooth(method = "lm", aes(colour = trt)) +
  facet_wrap(~site)+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9)) +
  theme_classic()


#################################
# Generalized Linear Mixed Effect Model of Yolk Testosterone
test_best_model <- glmer(test ~ site * trt + layorder + clutch_size + julian + year +
                      (1|nest), family = Gamma(link = "log"), data = dat)
summary(test_best_model)
AIC(test_best_model)

# Post Hoc Tests
post_hoc_yolk_test <- emmeans(test_best_model, list(pairwise ~ site * trt))
pairs(post_hoc_yolk_test, simple = "each")
plot(post_hoc_yolk_test)



################################################################################
## Yolk Thyroxine

# Visualize
# Basic plot of yolk Thyroxine across sites and treatment groups
ggplot(data = dat, aes(x = site, y = t4, colour = trt, group = trt)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.9)) +
  stat_summary(fun = mean, geom = "point", size = 5, position=position_dodge(width=0.9), 
               color = "black", alpha = 0.8) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1, linewidth = 1, 
               color = "black", alpha = 0.5, position=position_dodge(width=0.9)) +
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7)) +
  theme_classic()

# Basic plot of yolk Thyroxine across laying sequence
ggplot(dat, aes(x = layorder, y = t4, color = trt, group = trt)) +
  geom_point(aes(color=trt), position = position_jitterdodge(dodge.width = 0.5)) +
  geom_smooth(method = "lm", aes(colour = trt)) +
  facet_wrap(~site)+
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7)) +
  theme_classic()


#################################
# Generalized Linear Mixed Effect Model of Yolk Thyroxine
t4_best_model <- glmer(t4 ~ site * trt + layorder + clutch_size + julian + year + 
                      (1|nest), family = Gamma(link = "log"), data = dat)
summary(t4_best_model)
AIC(t4_best_model)

# Post Hoc Tests
post_hoc_yolk_t4 <- emmeans(t4_best_model, list(pairwise ~ site + trt))
pairs(post_hoc_yolk_t4, simple = "each")
plot(post_hoc_yolk_t4)



################################################################################
## Yolk Mass

# Visualize
# Basic plot of yolk mass across sites and treatment groups
ggplot(data = dat, aes(x = site, y = yolk_mass, colour = trt, group = trt)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.9)) +
  stat_summary(fun = mean, geom = "point", size = 5, position=position_dodge(width=0.9), 
               color = "black", alpha = 0.8) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1, linewidth = 1, 
               color = "black", alpha = 0.5, position=position_dodge(width=0.9)) +
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7)) +
  theme_classic()

# Basic plot of yolk mass across laying sequence
ggplot(dat, aes(x = layorder, y = yolk_mass, color = trt, group = trt)) +
  geom_point(aes(color=trt), position = position_jitterdodge(dodge.width = 0.1)) +
  geom_smooth(method = "lm", aes(colour = trt)) +
  facet_wrap(~site)+
  scale_x_continuous(breaks = c(1,2,3,4,5,6)) +
  scale_y_continuous(breaks = c(0.15,0.2,0.25,0.3,0.35,0.4)) +
  theme_classic()


#################################
# Linear Mixed Effect Model of Yolk Mass
mass_best_model <- lmer(yolk_mass ~ site * trt + layorder + clutch_size + julian + year + 
                     (1|nest), data = dat)
summary(mass_best_model)
AIC(mass_best_model)

# Post Hoc Tests
post_hoc_yolk_mass <- emmeans(mass_best_model, list(pairwise ~ site * trt))
pairs(post_hoc_yolk_mass, simple = "each")
plot(post_hoc_yolk_mass)

#### Normality Test ####
shapiro.test(residuals(mass_best_model));length(residuals(mass_best_model))
qqnorm(resid(mass_best_model))
qqline(resid(mass_best_model))
plot(density(resid(mass_best_model)))
  # residuals are normal if p is > 0.05.
  # residuals are normally distributed. 

#### Homogeneity of variance ####
library(DHARMa)
simulation_output <- simulateResiduals(mass_best_model)
plot(simulation_output)
testDispersion(simulation_output)  # This will test for over/underdispersion
  # p-value > 0.05 means there is no dispersion problems

model_data <- na.omit(dat[, c("yolk_mass", "site", "trt", "layorder", "clutch_size", "julian", "year", "nest")])
leveneTest(resid(mass_best_model) ~ model_data$trt)
leveneTest(resid(mass_best_model) ~ model_data$site)
  # p-value is > 0.05, variance is equal among site and trt groups

