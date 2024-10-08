
### Analysis of Caterpillar Frass Biomass

# Load Packages ----
library(lubridate)
library(ggplot2)
library(emmeans)
library(lme4)

# Load "Frass_20-22.csv" 
# Hereon, data file is named "dat"
dat <- read.csv("Data/Frass_20-22.csv")

# Clean data
dat$frassID <- as.factor(dat$frassID) # individual ID of the frass net (named after the nest box it was closest to)
dat$site <- as.factor(dat$site)  # codes for the 3 sites: "N" natural, "R" rural, "U" urban
dat$date <- as.Date(dat$date, "%m/%d/%y")  # modify the format of the date of the year
dat$julian <- yday(dat$date)  # create a new column for julian date
dat$year <- as.factor(dat$year)  # year data was collected
dat$mass_g <- as.numeric(dat$mass_g) # dried mass of frass
dat$biomass <- as.numeric(dat$biomass) # estimated caterpillar biomass with correctino of temperature

# Rename groups
levels(dat$site)
levels(dat$site) <- c("Natural", "Rural", "Urban")


# Visualize
ggplot(data = dat, aes(x = Julian, y = biomass, colour = site, fill = site)) +
  stat_summary(size = 1, linewidth = 1) +
  stat_summary(fun = mean, geom = "line", size = 1.5) +
  #geom_jitter() +
  scale_color_manual(values=c("#008176", "darkorange", "#999999")) +
  scale_y_continuous(name = 'Caterpillar Biomass (g/m\u00b2)') +
  scale_x_continuous(name = 'Julian Date') +
  theme_classic() +
  theme(legend.position = 'right',
        plot.title = element_text(size = 25, hjust = 0.5, colour="black",face = "bold"),
        axis.text = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 20, colour = "black", face = "bold"),
        legend.title = element_text(size = 16,
                                    colour="black",face = "bold",vjust = 1),
        legend.text = element_text(size=13)) +
  labs(color = "Site") 


#################################
# Generalized Linear Model of Caterpillar Biomass
glm_model <- glm(biomass ~ site + year + julian, 
                 family = Gamma(link = "log"), data = dat)
summary(glm_model)

# Post Hoc Tests
post_hoc_model <- emmeans(glm_model, list(pairwise ~ site))
pairs(post_hoc_model, simple = "each")
plot(post_hoc_model)
