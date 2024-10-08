
### Replication of Supplementary Manuscript Figures
# Figure S1: Yolk Hormone EIA Validation
# Figure S2: Nestling Plasma Corticosterone EIA Validation
# Figure S3: Egg volume, wet yolk mass, yolk corticosterone, yolk testosterone, 
  # and yolk thyroxine concentrations across the laying sequence

# Load Packages ----
library(ggplot2)
library(wesanderson)
library(RColorBrewer)
library(ggpubr)


# ----- Figure S1 -----

###########################################################
# Yolk Hormone EIA Validation
# Load "Yolk_Validations.csv" 
# Hereon, data file is named "yolkval"
yolkval <- read.csv("Data/Yolk_Validations.csv")

pal1 <- c("darkslategrey", "darkgoldenrod1") 

# Clean data
yolkval$Curves <- as.factor(yolkval$curves)
yolkval$standard <- as.factor(yolkval$standard)
yolkval$cort <- as.numeric(yolkval$cort)
yolkval$cort_bound <- as.numeric(yolkval$cort_bound)
yolkval$T <- as.numeric(yolkval$T)
yolkval$T_bound <- as.numeric(yolkval$T_bound)
yolkval$T4 <- as.numeric(yolkval$T4)
yolkval$T4_bound <- as.numeric(yolkval$T4_bound)

# Yolk Corticosterone
cort_val <- ggplot(yolkval, aes(x = cort, y = cort_bound, colour = Curves, group = Curves)) +
  geom_line(aes(linetype = Curves), linewidth = 2) +
  geom_point(size = 4) +
  scale_color_manual(values = pal1) +
  scale_x_continuous(trans='log10') +
  theme_classic() +
  labs(x="log(Cort)", y = "% bound") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 20)) +
  theme(axis.title.x = element_text(size = 17)) +
  theme(axis.title.y = element_text(size = 17)) +
  theme(legend.position = c(0.84,0.85),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        axis.text = element_text(size = 15)) 
cort_val

# Yolk Testosterone
T_val <- ggplot(yolkval, aes(x = T, y = T_bound, colour = Curves, group = Curves)) +
  geom_line(aes(linetype = Curves), size = 2) +
  geom_point(size = 4) +
  scale_color_manual(values = pal1) +
  scale_x_continuous(trans='log10') +
  theme_classic() +
  labs(x="log(T)", y = "% bound") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 20)) +
  theme(axis.title.x = element_text(size = 17)) +
  theme(axis.title.y = element_text(size = 17)) +
  theme(legend.position = "none",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        axis.text = element_text(size = 15)) 
T_val

# Yolk Thyroxine
T4_val <- ggplot(yolkval, aes(x = T4, y = T4_bound, colour = Curves, group = Curves)) +
  geom_line(aes(linetype = Curves), size = 2) +
  geom_point(size = 4) +
  scale_color_manual(values = pal1) +
  scale_x_continuous(trans='log10') +
  theme_classic() +
  labs(x="log(T4)", y = "% bound") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 20)) +
  theme(axis.title.x = element_text(size = 17)) +
  theme(axis.title.y = element_text(size = 17)) +
  theme(legend.position = "none",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        axis.text = element_text(size = 15)) 
T4_val

# Combining figures for Figure S1
figureS1 <- ggarrange(cort_val, T_val, T4_val,  
                             labels = c("A", "B", "C"),
                             ncol = 1, nrow = 3)
figureS1



# ----- Figure S2 -----

###########################################################
# Nestling Plasma Validation Figure
# Load "Nestling_Plasma_Validation.csv" 
# Hereon, data file is named "plasmaval"
plamsaval <- read.csv("Data/Nestling_Plasma_Validation.csv")

# Clean data
plamsaval$Curves <- as.factor(plamsaval$curves)
plamsaval$cort <- as.numeric(plamsaval$cort)
plamsaval$bound <- as.numeric(plamsaval$bound)


plasma_cort_val <- ggplot(plamsaval, aes(x = cort, y = bound, colour = Curves, group = Curves)) +
  geom_line(aes(linetype = Curves), linewidth = 2) +
  geom_point(size = 4) +
  scale_color_manual(values = pal1) +
  scale_x_continuous(trans='log10') +
  theme_classic() +
  labs(x="log(Cort)", y = "% bound") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 20)) +
  theme(axis.title.x = element_text(size = 17)) +
  theme(axis.title.y = element_text(size = 17)) +
  theme(legend.position = c(0.84,0.85),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        axis.text = element_text(size = 15)) 
plasma_cort_val



# ----- Figure S3 -----

###########################################################
# Egg volume, wet yolk mass, yolk corticosterone, yolk testosterone, 
    # and yolk thyroxine concentrations across the laying sequence

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
pal2 <- c("indianred4", "salmon")

yolk_cort <- ggplot(yolk_hormones, aes(x = layorder, y = cort, color = trt, group = trt, shape = trt)) +
  geom_jitter(aes(color=trt, shape = trt), size = 3, alpha = 0.7, position = position_jitterdodge(dodge.width = 0.5)) +
  geom_smooth(method = "lm", aes(colour = trt)) +
  facet_wrap(~site)+
  scale_y_continuous(name = 'Yolk Corticosterone (ng/mL)', breaks = c(2,3,4,5,6,7)) +
  scale_x_continuous(name = 'Layorder', breaks = c(1,2,3,4,5,6)) +
  labs(color = "Treatment") +
  scale_color_manual(values = pal2) +
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
pal3 <- wes_palettes$Cavalcanti1

yolk_test <- ggplot(yolk_hormones, aes(x = layorder, y = test, color = trt, group = trt, shape = trt)) +
  geom_jitter(aes(color=trt, shape = trt), size = 3, alpha = 0.7, position = position_jitterdodge(dodge.width = 0.5)) +
  geom_smooth(method = "lm", aes(colour = trt)) +
  facet_wrap(~site)+
  scale_y_continuous(name = 'Yolk Testosterone (ng/mL)', breaks = c(2,4,6,8,10,12,14,16)) +
  scale_x_continuous(name = 'Layorder', breaks = c(1,2,3,4,5,6)) +
  labs(color = "Treatment") +
  scale_color_manual(values = pal3[2:3]) +
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
pal4 <- c("steelblue", "lightsteelblue")

yolk_t4 <- ggplot(yolk_hormones, aes(x = layorder, y = t4, color = trt, group = trt, shape = trt)) +
  geom_jitter(aes(color=trt, shape = trt), size = 3, alpha = 0.8, position = position_jitterdodge(dodge.width = 0.5)) +
  geom_smooth(method = "lm", aes(colour = trt)) +
  facet_wrap(~site)+
  scale_y_continuous(name = 'Yolk Thyroxine (ng/mL)', breaks = c(0,1,2,3,4,5,6,7)) +
  scale_x_continuous(name = 'Layorder', breaks = c(1,2,3,4,5,6)) +
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
yolk_t4


############ Egg Volume Across Lay order
# Load "Egg_Morphology" 
# Hereon, data file is named "egg_volume"
egg_volume <- read.csv("Data/Egg_Morphology.csv")
egg_volume <- subset(egg_volume, layorder < 7)

# Clean data
egg_volume$nest <- as.factor(egg_volume$nest)
egg_volume$site <- as.factor(egg_volume$site)
egg_volume$trt <- as.factor(egg_volume$trt)
egg_volume$layorder <- as.integer(egg_volume$layorder)
egg_volume$length <- as.numeric(egg_volume$length)
egg_volume$width <- as.numeric(egg_volume$width)
egg_volume$mass <- as.numeric(egg_volume$mass)

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

pal5 <- c("#999999",  "#008176")

egg_vol <- ggplot(egg_volume, aes(x = layorder, y = volume, color = trt, group = trt, shape = trt)) +
  geom_jitter(aes(color=trt, shape = trt), size = 3, alpha = 0.7, position = position_jitterdodge(dodge.width = 0.5)) +
  geom_smooth(method = "lm", aes(colour = trt)) +
  facet_wrap(~site)+
  scale_y_continuous(name = 'Egg Volume', breaks =c(1.2,1.3,1.4,1.5,1.6,1.7)) +
  scale_x_continuous(name = 'Layorder', breaks = c(1,2,3,4,5,6)) +
  labs(color = "Treatment") +
  scale_color_manual(values = pal5) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 17, colour = "black", face = "bold"),
    legend.title = element_text(size = 18,
                                colour="black",face = "bold",vjust = 1),
    legend.text = element_text(size=13),
    legend.position = "none")
egg_vol



############ Yolk Mass Across Lay order
# Load "yolk_hormones.csv" 
# Hereon, data file is named "yolk_hormones"
yolk_hormones <- read.csv("Data/yolk_hormones.csv")

# Clean data
yolk_hormones$layorder <- as.integer(yolk_hormones$layorder)
yolk_hormones$yolk <- as.numeric(yolk_hormones$yolk)

yolk <- ggplot(yolk_hormones, aes(x = layorder, y = yolk, color = trt, group = trt, shape = trt)) +
  geom_jitter(aes(color=trt, shape = trt), size = 3, alpha = 0.7, position = position_jitterdodge(dodge.width = 0.5)) +
  geom_smooth(method = "lm", aes(colour = trt)) +
  facet_wrap(~site)+
  scale_y_continuous(name = 'Yolk Mass (g)', breaks = c(0.25, 0.3, 0.35)) +
  scale_x_continuous(name = 'Layorder', breaks = c(1,2,3,4,5,6)) +
  labs(color = "Treatment") +
  scale_color_manual(values = pal5) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 17, colour = "black", face = "bold"),
    legend.title = element_text(size = 18,
                                colour="black",face = "bold",vjust = 1),
    legend.text = element_text(size=13),
    legend.position = "none")
yolk


# Combining figures for Figure S3
figureS3 <- ggarrange(yolk_cort, yolk_test, yolk_t4, egg_vol, yolk,   
                    labels = c("A", "B", "C", "D", "E"),
                    ncol = 2, nrow = 3)
figureS3


yolk_cort