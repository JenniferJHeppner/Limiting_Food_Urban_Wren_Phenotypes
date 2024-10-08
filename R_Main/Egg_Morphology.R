
### Analysis of Egg Morphology
## Egg Mass and Volume

# Load Packages ----
library(ggplot2)
library(lme4)
library(lmerTest)
library(lubridate)    # to transform dates
library(hms)          # to transform time
library(car)
library(emmeans)

# Load "Egg_Morphology.csv" 
# Hereon, data file is named "dat"
dat <- read.csv("Data/Egg_Morphology.csv")

# Clean data
dat$nest <- as.factor(dat$nest)  # unique nest ID
dat$site <- as.factor(dat$site) # codes for the 3 sites: "N" natural, "R" rural, "U" urban
dat$trt <- as.factor(dat$trt) # code for treatment groups: "C" control, "T" food supplemented
dat$layorder <- as.integer(dat$layorder) # order number an egg was laid in, 1-6
dat$year <- as.factor(dat$year) # year data was collected
dat$date <- as.Date(dat$date, "%m/%d/%y") # modify the format of the date of the year
dat$julian <- yday(dat$date) # create a new column for julian date
dat$clutch_size <- as.integer(dat$clutch_size) # number of eggs in the entire clutch
dat$length <- as.numeric(dat$length) # length measured of egg (longest part of egg)
dat$width <- as.numeric(dat$width) # width measured of egg (widest part of egg)
dat$mass <- as.numeric(dat$mass) # mass measured of egg

# Rename groups
levels(dat$site)
levels(dat$site) <- c("Natural", "Rural", "Urban")
levels(dat$trt)
levels(dat$trt) <- c("Control", "Food")


################################################################################
## Egg Mass

# Visualize
# Basic plot of egg mass across sites and treatment groups
ggplot(data = dat, aes(x = site, y = mass, colour = trt, group = trt)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.9)) +
  stat_summary(fun = mean, geom = "point", size = 5, position=position_dodge(width=0.9), 
               color = "black", alpha = 0.8) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1, linewidth = 1, 
               color = "black", alpha = 0.5, position=position_dodge(width=0.9)) +
  theme_classic()

# Basic plot of yolk Corticosterone across laying sequence
ggplot(dat, aes(x = layorder, y = mass, color = trt, group = trt)) +
  geom_point(aes(color=trt), position = position_jitterdodge(dodge.width = 0.5)) +
  geom_smooth(method = "lm", aes(colour = trt)) +
  facet_wrap(~site)+
  scale_x_continuous(breaks = c(1:6)) +
  theme_classic()


#################################
# Linear Mixed Effect Model of Egg Mass
mass_best_model <- lmer(mass ~ site + trt + layorder + clutch_size + julian + year +
                     (1|nest), data = dat)
summary(mass_best_model)
AIC(mass_best_model)

# Post Hoc Tests
post_hoc_egg_mass <- emmeans(mass_best_model, list(pairwise ~ site))
pairs(post_hoc_egg_mass, simple = "each")
plot(post_hoc_egg_mass)

#### Normality ####
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

model_data <- na.omit(dat[, c("mass", "site", "trt", "layorder", "clutch_size", "julian", "year", "nest")])
leveneTest(resid(mass_best_model) ~ model_data$trt)
leveneTest(resid(mass_best_model) ~ model_data$site)
  # p-value is > 0.05, variance is equal among site and trt groups



################################################################################
## Egg Volume

## Egg Volume Calculations
get_egg_volume <- function(mass, length, width){
    # get Kw - egg mass shape coefficient
  kw = mass / (length * width^2)
    # get Kv - egg volume shape coefficient
  kv = kw / 1.024
    # get volume
  vol = kv * length * width^2
    return(vol)
  }

dat$volume <- get_egg_volume(mass = dat$mass, length = dat$length, width = dat$width)

# Visualize
# Basic plot of egg volume across sites and treatment groups
ggplot(data = dat, aes(x = site, y = volume, colour = trt, group = trt)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.9)) +
  stat_summary(fun = mean, geom = "point", size = 5, position=position_dodge(width=0.9), 
               color = "black", alpha = 0.8) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1, linewidth = 1, 
               color = "black", alpha = 0.5, position=position_dodge(width=0.9)) +
  theme_classic()

# Basic plot of egg volume across laying sequence
ggplot(dat, aes(x = layorder, y = volume, color = trt, group = trt)) +
  geom_point(aes(color=trt), position = position_jitterdodge(dodge.width = 0.5)) +
  geom_smooth(method = "lm", aes(colour = trt)) +
  facet_wrap(~site)+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9)) +
  theme_classic()


#################################
# Linear Mixed Effect Model of Egg Volume
volume_best_model <- lmer(volume ~ site + trt + layorder + clutch_size + julian + year + 
                     (1|nest), data = dat)
summary(volume_best_model)
AIC(volume_best_model)

# Post Hoc Tests
post_hoc_egg_volume <- emmeans(volume_best_model, list(pairwise ~ site))
pairs(post_hoc_egg_volume, simple = "each")
plot(post_hoc_egg_volume)

#### Normality ####
shapiro.test(residuals(volume_best_model));length(residuals(volume_best_model))
qqnorm(resid(volume_best_model))
qqline(resid(volume_best_model))
plot(density(resid(volume_best_model)))
  # residuals are normal if p is > 0.05

#### Homogeneity of variance ####
library(DHARMa)
simulation_output <- simulateResiduals(volume_best_model)
plot(simulation_output)
testDispersion(simulation_output)  # This will test for over/under dispersion
  # p-value > 0.05 means there is no dispersion problems

model_data <- na.omit(dat[, c("volume", "site", "trt", "layorder", "clutch_size", "julian", "year", "nest")])
leveneTest(resid(volume_best_model) ~ model_data$trt)
leveneTest(resid(volume_best_model) ~ model_data$site)
  # p-value is > 0.05, variance is equal among site and trt groups

