
### Analysis of Adult Baseline Corticosterone

# Load Packages ----
library(ggplot2)
library(lme4)
library(lmerTest)
library(lubridate)    # to transform dates
library(hms)          # to transform time
library(car)
library(emmeans)

# Load "Adult_Cort.csv" 
# Hereon, data file is named "dat"
dat <- read.csv("Data/Adult_Cort.csv")

# Clean data
dat$nest <- as.factor(dat$nest) # unique nest ID
dat$site <- as.factor(dat$site) # codes for the 3 sites: "N" natural, "R" rural, "U" urban
dat$trt <- as.factor(dat$trt) # code for treatment groups: "C" control, "T" food supplemented
dat$sex <- as.factor(dat$sex) # sex of adult: "F" female, "M" male
dat$ID <- as.integer(dat$ID) # individual USGS band ID code
dat$cort <- as.numeric(dat$cort_ng) # corticosterone level measured in plasma
dat$bleed_time<- as.integer(dat$bleed_time) # the time in seconds it took to bleed a birds start to finish
dat$mass <- as.numeric(dat$mass) # mass of individual the day blood was taken
dat$date <- as.Date(dat$date, "%m/%d/%y") # modify the format of the date of the year
dat$julian <- yday(dat$date) # create a new column for julian date
dat$year <- as.factor(dat$year) # year data was collected
dat$time <- hm(dat$time) # time of the day blood was taken
dat$brood_size <- as.integer(dat$brood_size) # the total number of nestlings within the brood
dat$mod <- hour(dat$time)*60 + minute(dat$time) # modifies time of day value

# Rename groups
levels(dat$site)
levels(dat$site) <- c("Natural", "Rural", "Urban")
levels(dat$trt)
levels(dat$trt) <- c("Control", "Food")
levels(dat$sex)
levels(dat$sex) <- c("Female", "Male")

# Visualize
ggplot(data = dat, aes(x = site, y = cort, colour = trt, group = trt)) +
  geom_jitter(size = 4, alpha = 0.7, position = position_jitterdodge(dodge.width = 0.9)) +
  stat_summary(fun = mean, geom = "point", size = 10, position = position_dodge(width = 0.9),
               color = "black", alpha = 0.8, shape = 18) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1, linewidth = 1,
               color = "black", alpha = 0.5, position = position_dodge(width = 0.9)) +
  #facet_wrap(~sex) +
  scale_y_continuous(name = 'Adult Corticosterone (ng/mL)', limits = c(0,4), breaks = c(0,1,2,3,4)) +
  scale_x_discrete(name = 'Site') +
  labs(color = "Treatment") +
  scale_color_manual(values=c("#999999",  "#008176")) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 20, colour = "black", face = "bold"),
    legend.title = element_text(size = 18,
                                colour="black",face = "bold",vjust = 1),
    legend.text = element_text(size=13),
    strip.text = element_text(size = 18))


#################################
# Generalized Linear Model of Adult Corticosterone
cort_best_model <- glm(cort ~ site * trt + mass + brood_size + sex + julian + year , 
                  family = Gamma(link = "log"), data = dat)
summary(cort_best_model)
AIC(cort_best_model) 

# Post Hoc Test
post_hoc_adult_cort <- emmeans(cort_best_model, list(pairwise ~ site + trt))
pairs(post_hoc_adult_cort, simple = "each")
plot(post_hoc_adult_cort)
