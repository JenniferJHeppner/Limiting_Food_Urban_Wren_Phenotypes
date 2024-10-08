
### Analysis of Adult Morphology
# Adult Mass and Body Condition

# Load Packages ----
library(ggplot2)
library(lme4)
library(lmerTest)
library(lubridate)    # to transform dates
library(hms)          # to transform time
library(emmeans)
library(tidyverse)

# Load "Adult_Morphology.csv" 
# Hereon, data file is named "dat"
dat <- read.csv("Data/Adult_Morphology.csv")

# Clean data
dat$nest <- as.factor(dat$nest) # unique nest ID
dat$site <- as.factor(dat$site) # codes for the 3 sites: "N" natural, "R" rural, "U" urban
dat$trt <- as.factor(dat$trt) # code for treatment groups: "C" control, "T" food supplemented
dat$sex <- as.factor(dat$sex) # sex of adult: "F" female, "M" male
dat$ID <- as.integer(dat$ID) # individual USGS band ID code
dat$year <- as.factor(dat$year) # year data was collected
dat$date <- as.Date(dat$date, "%m/%d/%y") # modify the format of the date of the year
dat$julian <- yday(dat$date) # create a new column for julian date
dat$time <- hm(dat$time) # time of the day blood was taken
dat$brood_size <- as.integer(dat$brood_size) # the total number of nestlings within the brood
dat$mass <- as.numeric(dat$mass) # mass of individual the day blood was taken
dat$tarsus <- as.numeric(dat$tarsus) # tarsus length of individual the day blood was taken
dat$wing <- as.numeric(dat$wing) # wing length of individual the day blood was taken

# Rename groups
levels(dat$site)
levels(dat$site) <- c("Natural", "Rural", "Urban")
levels(dat$trt)
levels(dat$trt) <- c("Control", "Food")
levels(dat$sex)
levels(dat$sex) <- c("Female", "Male")


################################################################################
## Adult Mass

# Visualize
ggplot(data = dat, aes(x = site, y = mass, colour = trt, group = trt)) +
  geom_jitter(size = 4, alpha = 0.7, position = position_jitterdodge(dodge.width = 0.9)) +
  stat_summary(fun = mean, geom = "point", size = 10, position = position_dodge(width = 0.9),
               color = "black", alpha = 0.8, shape = 18) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1, linewidth = 1,
               color = "black", alpha = 0.5, position = position_dodge(width = 0.9)) +
  #facet_wrap(~sex) +
  scale_y_continuous(name = 'Adult Mass (g)', breaks = c(8, 9,10,11,12)) +
  scale_x_discrete(name = 'Site') +
  labs(color = "Treatment") +
  scale_color_manual(values=c("#999999",  "#008176")) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 20, colour = "black", face = "bold"),
    legend.title = element_text(size = 18,
                                colour="black",face = "bold",vjust = 1),
    legend.text = element_text(size=13))


#################################
# Linear Model of Adult Mass
mass_best_model <- lm(mass ~ site * trt + brood_size + sex + julian + year 
                 , data = dat)
summary(mass_best_model)
AIC(mass_best_model)

# Post Hoc Tests
post_hoc_full_model <- emmeans(mass_best_model, list(pairwise ~ site * trt))
pairs(post_hoc_full_model, simple = "each")
plot(post_hoc_full_model)

#### Normality ####
shapiro.test(residuals(mass_best_model));length(residuals(mass_best_model))
qqnorm(resid(mass_best_model))
qqline(resid(mass_best_model))
plot(density(resid(mass_best_model)))
  # residuals are normal if p is > 0.05.

#### Homogeneity of variance ####
plot(mass_best_model, which = 1)
leveneTest(residuals(mass_best_model) ~ interaction(site, trt), data = dat)
  # Variances are equal if p-value is > 0.05
bptest(mass_best_model)
  # Assumption of homoscedasticity holds if p-value is > 0.05



################################################################################
## Adult Body Condition

#### Calculate Body Condition with chick mass and tarsus ####
smi  = function (M, L, plot = TRUE, ...)
  
{
  if (plot) plot(log(M)~log(L), ...)
  {
    if (require(smatr)) {
      SMA = sma(log(M)~log(L))
      bSMA = coef(SMA)[2]}
    
    else {
      OLS = lm(log(M)~log(L))
      bOLS = coef(OLS)[2]
      r = cor.test(~log(M)+log(L), method = "pearson")$estimate
      
      #outliers = which(abs(rstandard(ols))>3)
      
      bSMA = bOLS/r }
  }
  
  L0 = median(L, na.rm = T)
  SMi = M*((L0/L)^bSMA)
  return(SMi)
}

# Body Condition 
BCmetric <- smi(dat$mass, dat$tarsus)
dat$bc <- BCmetric

# Visualize
ggplot(data = dat, aes(x = site, y = bc, colour = trt, group = trt)) +
  geom_jitter(size = 4, alpha = 0.7, position = position_jitterdodge(dodge.width = 0.9)) +
  stat_summary(fun = mean, geom = "point", size = 10, position = position_dodge(width = 0.9),
               color = "black", alpha = 0.8, shape = 18) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1, linewidth = 1,
               color = "black", alpha = 0.5, position = position_dodge(width = 0.9)) +
  #facet_wrap(~sex) +
  scale_y_continuous(name = 'Adult Body Condition', breaks = c(8, 9,10,11,12)) +
  scale_x_discrete(name = 'Site') +
  labs(color = "Treatment") +
  scale_color_manual(values=c("#999999",  "#008176")) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 20, colour = "black", face = "bold"),
    legend.title = element_text(size = 18,
                                colour="black",face = "bold",vjust = 1),
    legend.text = element_text(size=13))



#################################
# Linear Model of Adult Body Condition
bc_best_model <- lm(bc ~ site * trt + brood_size + sex + julian  + year, data = dat)
summary(bc_best_model)
AIC(bc_best_model)

#Post Hoc Tests
post_hoc_full_model <- emmeans(bc_best_model, list(pairwise ~ site * trt))
pairs(post_hoc_full_model, simple = "each")
plot(post_hoc_full_model)

#### Normality ####
shapiro.test(residuals(bc_best_model));length(residuals(bc_best_model))
qqnorm(resid(bc_best_model))
qqline(resid(bc_best_model))
plot(density(resid(bc_best_model)))
  # residuals are normal if p is > 0.05.

#### Homogeneity of variance ####
plot(bc_best_model, which = 1)
leveneTest(residuals(bc_best_model) ~ interaction(site, trt), data = dat)
  # Variances are equal if p-value is > 0.05
bptest(bc_best_model)
  # Assumption of homoscedasticity holds if p-value is > 0.05