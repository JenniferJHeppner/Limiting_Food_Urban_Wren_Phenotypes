
### Replication of Main Manuscript Figures
# Figure 1: yolk cort, yolk T, yolk T4, nestling Cort, adult cort, and frass biomass
# Figure 2: egg volume, yolk mass, fledgling mass, adult mass 

# Load Packages ----
library(ggplot2)
library(wesanderson)
library(RColorBrewer)
library(ggpubr)


# ----- Figure 1 -----

###########################################################
# Yolk Hormones
# Load "yolk_hormones.csv" 
# Hereon, data file is named "yolk_hormones"
yolk_hormones <- read.csv("Data/yolk_hormones.csv")

# Clean data
yolk_hormones$nest <- as.factor(yolk_hormones$nest)
yolk_hormones$site <- as.factor(yolk_hormones$site)
yolk_hormones$trt <- as.factor(yolk_hormones$trt)
yolk_hormones$layorder <- as.numeric(yolk_hormones$layorder)
yolk_hormones$cort <- as.numeric(yolk_hormones$cort)
yolk_hormones$test <- as.numeric(yolk_hormones$test)
yolk_hormones$t4 <- as.numeric(yolk_hormones$t4)

# Rename groups
levels(yolk_hormones$site)
levels(yolk_hormones$site) <- c("Natural", "Rural", "Urban")
levels(yolk_hormones$trt)
levels(yolk_hormones$trt) <- c("Control", "Food Treatment")


# Yolk Corticosterone
pal1 <- c("indianred4", "salmon")
yolk_cort <- ggplot(data = yolk_hormones, aes(x = site, y = cort, group = trt, shape = trt)) +
  geom_jitter(aes(colour = trt, shape = trt), size = 4, alpha = 0.6, position = position_jitterdodge(dodge.width = 0.9)) +
  stat_summary(fun = mean, geom = "point", size = 6, position = position_dodge(width = 0.9),
               color = "black", alpha = 0.9) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1, linewidth = 1,
               color = "black", alpha = 0.9, position = position_dodge(width = 0.9)) +
  scale_y_continuous(name = 'Yolk Corticosterone (ng/mL)', breaks = c(2,3,4,5,6,7)) +
  scale_x_discrete(name = 'Site') +
  labs(color = "Treatment") +
  scale_color_manual(values = pal1) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 17, colour = "black", face = "bold"),
    legend.title = element_text(size = 18,
                                colour="black",face = "bold",vjust = 1),
    legend.text = element_text(size=13),
    legend.position = "none")
yolk_cort


# Yolk Testosterone
pal2 <- wes_palettes$Cavalcanti1
yolk_test <- ggplot(data = yolk_hormones, aes(x = site, y = test, group = trt, shape = trt)) +
  geom_jitter(aes(colour = trt, shape = trt), size = 4, alpha = 0.6, position = position_jitterdodge(dodge.width = 0.9)) +
  stat_summary(fun = mean, geom = "point", size = 6, position = position_dodge(width = 0.9),
               color = "black", alpha = 0.9) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1, linewidth = 1,
               color = "black", alpha = 0.9, position = position_dodge(width = 0.9)) +
  scale_y_continuous(name = 'Yolk Testosterone (ng/mL)', breaks = c(2,4,6,8,10,12,14,16)) +
  scale_x_discrete(name = 'Site') +
  labs(color = "Treatment") +
  scale_color_manual(values = pal2[2:3]) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 17, colour = "black", face = "bold"),
    legend.title = element_text(size = 18,
                                colour="black",face = "bold",vjust = 1),
    legend.text = element_text(size=13),
    legend.position = "none")
yolk_test


# Yolk Thyroxine
pal3 <- c("steelblue", "lightsteelblue")
yolk_t4 <- ggplot(data = yolk_hormones, aes(x = site, y = t4, group = trt, shape = trt)) +
  geom_jitter(aes(colour = trt, shape = trt), size = 4, alpha = 0.7, position = position_jitterdodge(dodge.width = 0.9)) +
  stat_summary(fun = mean, geom = "point", size = 6, position = position_dodge(width = 0.9),
               color = "black", alpha = 0.9) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1, linewidth = 1,
               color = "black", alpha = 0.9, position = position_dodge(width = 0.9)) +
  scale_y_continuous(name = 'Yolk Thyroxine (ng/mL)', breaks = c(0,1,2,3,4,5,6,7)) +
  scale_x_discrete(name = 'Site') +
  labs(color = "Treatment") +
  scale_color_manual(values = pal3) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 17, colour = "black", face = "bold"),
    legend.title = element_text(size = 18,
                                colour="black",face = "bold",vjust = 1),
    legend.text = element_text(size=13),
    legend.position = "none")
yolk_t4


###########################################################
# Chick Cort

# Load "Nestling_Cort.csv" 
# Hereon, data file is named "chick_hormones"
chick_hormone <- read.csv("Data/Nestling_Cort.csv")

# Clean Data
chick_hormone$nest <- as.factor(chick_hormone$nest)
chick_hormone$site <- as.factor(chick_hormone$site)
chick_hormone$trt <- as.factor(chick_hormone$trt)
chick_hormone$cort <- as.numeric(chick_hormone$cort)

# Rename groups
levels(chick_hormone$site)
levels(chick_hormone$site) <- c("Natural", "Rural", "Urban")
levels(chick_hormone$trt)
levels(chick_hormone$trt) <- c("Control", "Food")

chick_cort <- ggplot(data = chick_hormone, aes(x = site, y = cort, group = trt, shape = trt)) +
  geom_jitter(aes(colour = trt, shape = trt), size = 4, alpha = 0.6, position = position_jitterdodge(dodge.width = 0.9)) +
  stat_summary(fun = mean, geom = "point", size = 6, position = position_dodge(width = 0.9),
               color = "black", alpha = 0.9) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1, linewidth = 1,
               color = "black", alpha = 0.9, position = position_dodge(width = 0.9)) +
  scale_y_continuous(name = 'Nestling Corticosterone (ng/mL)', limits = c(0,9.5), breaks = c(1,3,5,7,9)) +
  scale_x_discrete(name = 'Site') +
  labs(color = "Treatment") +
  scale_color_manual(values = pal1) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 17, colour = "black", face = "bold"),
    legend.title = element_text(size = 18,
                                colour="black",face = "bold",vjust = 1),
    legend.text = element_text(size=13),
    legend.position = "none")
chick_cort


###########################################################
# Adult Cort

# Load "Adult_Cort.csv" 
# Hereon, data file is named "adult_hormones"
adult_hormone <- read.csv("Data/Adult_Cort.csv")

#Clean Data
adult_hormone$nest <- as.factor(adult_hormone$nest)
adult_hormone$site <- as.factor(adult_hormone$site)
adult_hormone$trt <- as.factor(adult_hormone$trt)
adult_hormone$cort <- as.numeric(adult_hormone$cort_ng)

# Rename groups
levels(adult_hormone$site)
levels(adult_hormone$site) <- c("Natural", "Rural", "Urban")
levels(adult_hormone$trt)
levels(adult_hormone$trt) <- c("Control", "Food")

adult_cort <- ggplot(data = adult_hormone, aes(x = site, y = cort, group = trt, shape = trt)) +
  geom_jitter(aes(colour = trt, shape = trt), size = 4, alpha = 0.6, position = position_jitterdodge(dodge.width = 0.9)) +
  stat_summary(fun = mean, geom = "point", size = 6, position = position_dodge(width = 0.9),
               color = "black", alpha = 0.9) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1, linewidth = 1,
               color = "black", alpha = 0.9, position = position_dodge(width = 0.9)) +
  scale_y_continuous(name = 'Adult Corticosterone (ng/mL)', limits = c(0,4), breaks = c(0,1,2,3,4)) +
  scale_x_discrete(name = 'Site') +
  labs(color = "Treatment") +
  scale_color_manual(values = pal1) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 17, colour = "black", face = "bold"),
    legend.title = element_text(size = 18,
                                colour="black",face = "bold",vjust = 1),
    legend.text = element_text(size=13),
    legend.position = "none")
adult_cort



###########################################################
# Frass Biomass

# Load "Frass_20-22.csv" 
# Hereon, data file is named "frass"
frass <- read.csv("Data/Frass_20-22.csv")

# Clean Data
frass$ID <- as.factor(frass$frassID)
frass$site <- as.factor(frass$site)
frass$date <- as.Date(frass$date, "%m/%d/%y")
frass$julian <- yday(frass$date)
frass$year <- as.factor(frass$year)
frass$mass_g <- as.numeric(frass$mass_g)
frass$biomass <- as.numeric(frass$biomass)

# Subset Data
frass <- subset(frass, Julian < 201)

# Rename groups
levels(frass$site)
levels(frass$site) <- c("Natural", "Rural", "Urban")

frass_biomass <- ggplot(data = frass, aes(x = Julian, y = biomass, colour = site, fill = site)) +
  stat_summary(size = 1, linewidth = 1) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.5) +
  scale_color_manual(values=c("#008176", "darkorange", "#999999")) +
  scale_y_continuous(name = 'Caterpillar Biomass (g/m\u00b2)') +
  scale_x_continuous(name = 'Julian Date') +
  theme_classic() +
  theme(plot.title = element_text(size = 25, hjust = 0.5, colour="black",face = "bold"),
        axis.text = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 20, colour = "black", face = "bold"),
        legend.position = "none") 

frass_biomass



# Combining figures for Figure 1 
figure1 <- ggarrange(yolk_cort, yolk_test, yolk_t4, chick_cort, adult_cort, frass_biomass,
                    labels = c("A", "B", "C", "D", "E", "F"),
                    ncol = 3, nrow = 2)
figure1




# ----- Figure 2 -----

pal4 <- c("#999999",  "#008176")

###########################################################
# Egg Volume

# Load "Egg_Morphology.csv" 
# Hereon, data file is named "egg_volume"
egg_volume <- read.csv("Data/Egg_Morphology.csv")

# Subset to only include eggs 1 through 6
egg_volume <- subset(egg_volume, layorder < 7)

# Clean data
egg_volume$nest <- as.factor(egg_volume$nest)
egg_volume$site <- as.factor(egg_volume$site)
egg_volume$trt <- as.factor(egg_volume$trt)
egg_volume$layorder <- as.integer(egg_volume$layorder)
egg_volume$length <- as.numeric(egg_volume$length)
egg_volume$width <- as.numeric(egg_volume$width)
egg_volume$mass <- as.numeric(egg_volume$mass)


##########################################################################
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

egg_volume$volume <- get_egg_volume(mass = egg_volume$mass, length = egg_volume$length, width = egg_volume$width)

# Rename groups
levels(egg_volume$site)
levels(egg_volume$site) <- c("Natural", "Rural", "Urban")
levels(egg_volume$trt)
levels(egg_volume$trt) <- c("Control", "Food")


egg_vol <- ggplot(data = egg_volume, aes(x = site, y = volume, group = trt, shape = trt)) +
  geom_jitter(aes(colour = trt, shape = trt), size = 4, alpha = 0.6, position = position_jitterdodge(dodge.width = 0.9)) +
  stat_summary(fun = mean, geom = "point", size = 6, position = position_dodge(width = 0.9),
               color = "black", alpha = 0.9) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1, linewidth = 1,
               color = "black", alpha = 0.9, position = position_dodge(width = 0.9)) +
  scale_y_continuous(name = 'Egg Volume', breaks =c(1.2,1.3,1.4,1.5,1.6,1.7)) +
  scale_x_discrete(name = 'Site') +
  labs(color = "Treatment") +
  scale_color_manual(values = pal4) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 17, colour = "black", face = "bold"),
    legend.title = element_text(size = 18,
                                colour="black",face = "bold",vjust = 1),
    legend.text = element_text(size=13),
    legend.position = "none")
egg_vol


###########################################################
# Yolk Mass

# Load "yolk_hormones.csv" 
# Hereon, data file is named "yolk_mass"
yolk_mass <- read.csv("Data/yolk_hormones.csv")

# Clean data
yolk_mass$nest <- as.factor(yolk_mass$nest)
yolk_mass$site <- as.factor(yolk_mass$site)
yolk_mass$trt <- as.factor(yolk_mass$trt)
yolk_mass$layorder <- as.integer(yolk_mass$layorder)
yolk_mass$yolk <- as.numeric(yolk_mass$yolk)

# Rename groups
levels(yolk_mass$site)
levels(yolk_mass$site) <- c("Natural", "Rural", "Urban")
levels(yolk_mass$trt)
levels(yolk_mass$trt) <- c("Control", "Food")

yolk <- ggplot(data = yolk_mass, aes(x = site, y = yolk, group = trt, shape = trt)) +
  geom_jitter(aes(colour = trt, shape = trt), size = 4, alpha = 0.6, position = position_jitterdodge(dodge.width = 0.9)) +
  stat_summary(fun = mean, geom = "point", size = 6, position = position_dodge(width = 0.9),
               color = "black", alpha = 0.9) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1, linewidth = 1,
               color = "black", alpha = 0.9, position = position_dodge(width = 0.9)) +
  scale_y_continuous(name = 'Yolk Mass (g)', breaks = c(0.25, 0.3, 0.35)) +
  scale_x_discrete(name = 'Site') +
  labs(color = "Treatment") +
  scale_color_manual(values = pal4) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 17, colour = "black", face = "bold"),
    legend.title = element_text(size = 18,
                                colour="black",face = "bold",vjust = 1),
    legend.text = element_text(size=13),
    legend.position = "none")
yolk


###########################################################
# Chick Mass

# Load "Nestling_Morphology.csv" 
# Hereon, data file is named "nestling"
nestling <- dat <- read.csv("Data/Nestling_Morphology.csv")

# Clean Data
nestling$nest <- as.factor(nestling$nest)
nestling$site <- as.factor(nestling$site)
nestling$trt <- as.factor(nestling$trt)
nestling$mass <- as.numeric(nestling$mass)
nestling$bc <- as.numeric(nestling$bc)

# Rename groups
levels(nestling$site)
levels(nestling$site) <- c("Natural", "Rural", "Urban")
levels(nestling$trt)
levels(nestling$trt) <- c("Control", "Food")

chick_mass <- ggplot(data = nestling, aes(x = site, y = mass, group = trt, shape = trt)) +
  geom_jitter(aes(colour = trt, shape = trt), size = 4, alpha = 0.6, position = position_jitterdodge(dodge.width = 0.9)) +
  stat_summary(fun = mean, geom = "point", size = 6, position = position_dodge(width = 0.9),
               color = "black", alpha = 0.9) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1, linewidth = 1,
               color = "black", alpha = 0.9, position = position_dodge(width = 0.9)) +
  scale_y_continuous(name = 'Fledging Mass (g)') +
  scale_x_discrete(name = 'Site') +
  labs(color = "Treatment") +
  scale_color_manual(values = pal4) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 17, colour = "black", face = "bold"),
    legend.title = element_text(size = 18,
                                colour="black",face = "bold",vjust = 1),
    legend.text = element_text(size=13),
    legend.position = "none")
chick_mass



###########################################################
# Adult Mass

# Load "Adult_Morphology.csv" 
# Hereon, data file is named "adult"
adult <-read.csv("Data/Adult_Morphology.csv")

# Clean Data
adult$nest <- as.factor(adult$nest)
adult$site <- as.factor(adult$site)
adult$trt <- as.factor(adult$trt)
adult$sex <- as.factor(adult$sex)
adult$mass <- as.numeric(adult$mass)
adult$tarsus <- as.numeric(adult$tarsus)

# Rename groups
levels(adult$site)
levels(adult$site) <- c("Natural", "Rural", "Urban")
levels(adult$trt)
levels(adult$trt) <- c("Control", "Food")

adult_mass <- ggplot(data = adult, aes(x = site, y = mass, group = trt, shape = trt)) +
  geom_jitter(aes(colour = trt, shape = trt), size = 4, alpha = 0.6, position = position_jitterdodge(dodge.width = 0.9)) +
  stat_summary(fun = mean, geom = "point", size = 6, position = position_dodge(width = 0.9),
               color = "black", alpha = 0.9) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1, linewidth = 1,
               color = "black", alpha = 0.9, position = position_dodge(width = 0.9)) +
  scale_y_continuous(name = 'Adult Mass (g)') +
  scale_x_discrete(name = 'Site') +
  labs(color = "Treatment") +
  scale_color_manual(values = pal4) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 17, colour = "black", face = "bold"),
    legend.title = element_text(size = 18,
                                colour="black",face = "bold",vjust = 1),
    legend.text = element_text(size=13),
    legend.position = "none")
adult_mass


# Combining figures for Figure 2
figure2 <- ggarrange(egg_vol, yolk, chick_mass, adult_mass, 
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2)
figure2
