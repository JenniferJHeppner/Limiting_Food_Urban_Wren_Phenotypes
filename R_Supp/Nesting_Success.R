
### Analysis of Nesting Success
# Clutch size, hatching brood size, and fledgling brood size

# Load Packages ----
library(ggplot2)
library(lme4)
library(lmerTest)
library(lubridate)
library(car)
library(emmeans)

# Load "nest_info.csv" 
# Hereon, data file is named "dat"
dat <- read.csv("Data/nest_info.csv")

# Clean data
dat$nest <- as.factor(dat$nest) # unique nest ID
dat$site <- as.factor(dat$site) # codes for the 3 sites: "N" natural, "R" rural, "U" urban
dat$trt <- as.factor(dat$trt) # code for treatment groups: "C" control, "T" food supplemented
dat$year <- as.factor(dat$year) # year data was collected
dat$clutch_size <- as.integer(dat$clutch_size) # number of eggs in the entire clutch
dat$hatch_brood_size <- as.integer(dat$hatch_brood_size) # number of chicks that hatched from eggs
dat$fledge_brood_size <- as.integer(dat$fledge_brood_size) # Number of chicks that fledge the nest
dat$date_first_egg <- as.Date(dat$date_first_egg, "%m/%d/%y") # modify the date the first egg was laid
dat$julian_egg <- yday(dat$date_first_egg) # julian date of first egg laid
dat$date_hatch <- as.Date(dat$date_hatch, "%m/%d/%y") # modify the date eggs hatched
dat$julian_hatch <- yday(dat$date_hatch) # julian date when eggs hatched
dat$date_fledge <- as.Date(dat$date_fledge, "%m/%d/%y") # modify the date chicks fledge the nest
dat$julian_fledge <- yday(dat$date_fledge) # julian date when chicks fledge the nest

# Rename groups
levels(dat$site)
levels(dat$site) <- c("Natural", "Rural", "Urban")
levels(dat$trt)
levels(dat$trt) <- c("Control", "Food")



################################################################################
## Clutch Size

# Subset the data to include only nests wtih clutch sizes of 3 eggs or more to exclude nests 
# that were predated
clutch <- subset(dat, clutch_size > 3)

# Visualize
ggplot(clutch, aes(x = site, y = clutch_size, colour = trt, group = trt)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.9)) +
  stat_summary(fun = mean, geom = "point", size = 5, position=position_dodge(width=0.9), 
               color = "black", alpha = 0.8) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1, linewidth = 1, 
               color = "black", alpha = 0.5, position=position_dodge(width=0.9)) +
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9)) +
  theme_classic()

#################################
# Generalized Linear Model of Clutch Size
clutch_size_model <- glm(clutch_size ~ site * trt + year + julian_egg,
          data = clutch, family = poisson)
summary(clutch_size_model)

# Post Hoc Tests
post_hoc_clutch_full_model <- emmeans(clutch_size_model, list(pairwise ~ site * trt))
pairs(post_hoc_clutch_full_model, simple = "each")
plot(post_hoc_clutch_full_model)



################################################################################
## Hatch Brood Size

# Visualize
ggplot(dat, aes(x = site, y = hatch_brood_size, colour = trt, group = trt)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.9)) +
  stat_summary(fun = mean, geom = "point", size = 5, position=position_dodge(width=0.9), 
               color = "black", alpha = 0.8) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1, linewidth = 1, 
               color = "black", alpha = 0.5, position=position_dodge(width=0.9)) +
  scale_y_continuous() +
  theme_classic()

#################################
# Generalized Linear Model of Clutch Size
hatch_size_model <- glm(hatch_brood_size ~ site * trt + year + julian_hatch,
          data = dat, family = poisson)
summary(hatch_size_model)

# Post Hoc Tests
post_hoc_hatch_full_model <- emmeans(hatch_size_model, list(pairwise ~ site * trt))
pairs(post_hoc_hatch_full_model, simple = "each")
plot(post_hoc_hatch_full_model)



################################################################################
## Fledge Brood Size

# Visualize
ggplot(dat, aes(x = site, y = fledge_brood_size, colour = trt, group = trt)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.9)) +
  stat_summary(fun = mean, geom = "point", size = 5, position=position_dodge(width=0.9), 
               color = "black", alpha = 0.8) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1, linewidth = 1, 
               color = "black", alpha = 0.5, position=position_dodge(width=0.9)) +
  scale_y_continuous() +
  theme_classic()

#################################
# Generalized Linear Model of Clutch Size
fledge_size_model <- glm(fledge_brood_size ~ site * trt + year + julian_fledge,
          data = dat, family = poisson)
summary(fledge_size_model)

# Post Hoc Tests
post_hoc_fledge_full_model <- emmeans(fledge_size_model, list(pairwise ~ site * trt))
pairs(post_hoc_fledge_full_model, simple = "each")
plot(post_hoc_fledge_full_model)
