## <<<<<<<<<<<<<<<HEAD

## load packages
library(tidyverse)
library(ggpubr)
library(ggsci)
library(devtools)
library(ggh4x)

# source R functions
source_url("https://raw.githubusercontent.com/MBender1992/base_scripts/Marc/R_functions.R") 

####
# Data processing

# load data 
dat <- read_csv("Data/cSCC_TGFB_Scratch_Assay.csv") %>%
  mutate(time = parse_number(str_extract(.$Label, "t\\d+"))) %>%
  mutate(treatment = str_match(.$Label, "SCC12_(.*?)_")[,2]) %>%
  mutate(Messung = str_extract(.$Label, "\\d{6}")) %>%
  mutate(Rep = str_match(.$Label, "_(\\d)_")[,2]) %>%
  mutate(Wound_closure = Wound_closure*100) 

## calculate pixel to µm and pixel^2 to µm^2 based on a reference picture processed with imageJ
pixel_to_µm <- 1772/421.94
pixel2_to_µm2 <- 3605314/204391
dat <- dat %>% mutate(Area_µm = Area_pixels/pixel2_to_µm2,
               Width_µm = Width_pixels/pixel_to_µm,
               Standard_deviation_µm = Standard_deviation_pixels/pixel_to_µm)

####
# Data analysis
dat_summary <- Rmisc::summarySE(dat, measurevar="Wound_closure", groupvars=c("time","treatment")) %>%
  mutate(treatment = factor(ifelse(treatment == "ctrl", "Kontrolle", "TGF-beta (10 ng/mL)"))) %>%
  mutate(treatment = relevel(treatment, ref = "Kontrolle"))
  
## plot wound closure
svg("Results/scratch_assay_TGFbeta.svg",  width=3.6, height=2.5)
dat_summary %>%
  ggplot(aes(time, Wound_closure, color = treatment)) + 
  geom_smooth(aes(fill = treatment), method = "lm", se = TRUE,formula=y~x-1) +
  stat_regline_equation(label.x=c(14,14), label.y=c(25,50), size = 3.5) +
  stat_regline_equation(aes(label=..adj.rr.label..),label.x=c(14,14), label.y=c(15,40), size = 3.5) +
  geom_errorbar(aes(ymin = Wound_closure - sd, ymax = Wound_closure + sd), width = 0.4, alpha = 0.7) +
  geom_point(size = 2, alpha = 0.7) +
  theme_PhD(axis.text.size = 12) +
  theme(
    strip.background = element_blank(),
    legend.key.size = (unit(0.7,"line")),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  scale_color_jco() +
  scale_fill_jco() +
  xlab("Time (h)") +
  ylab("Wound closure (%)") + 
  scale_x_continuous(expand = c(0, 0), breaks = seq(0,24,4), guide = "axis_minor") +
  scale_y_continuous(expand = c(0, 0), guide = "axis_minor", breaks = seq(0, 120, 20))
dev.off()  

## calculate area
dat_summary_area <- Rmisc::summarySE(dat, measurevar="Area_µm", groupvars=c("time","treatment"))

## plot area 
svg("Results/scratch_assay_TGFbeta_area.svg",  width=4, height=2.5)
dat_summary_area %>%
  ggplot(aes(time, Area_µm, color = treatment)) + 
  geom_smooth(aes(fill = treatment), method = "lm", se = T, lty = 1) +
  stat_regline_equation(label.x=c(10,10), label.y=c(180000,140000), size = 3) +
  stat_regline_equation(aes(label=..adj.rr.label..),label.x=c(10,10), label.y=c(165000,125000), size = 3) +
  geom_errorbar(aes(ymin = Area_µm - sd, ymax = Area_µm + sd), width = 0.4, alpha = 0.7) +
  geom_point(size = 2, alpha = 0.7) +
  theme_PhD(axis.text.size = 12) +
  theme(
    strip.background = element_blank(),
    legend.key.size = (unit(0.7,"line")),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  scale_color_jco() +
  scale_fill_jco() +
  xlab("Time (h)") +
  ylab(expression(bold(Fläche~("\u03bc"~m^2)))) +
  scale_x_continuous(expand = c(0, 0),breaks = seq(0,24,4), guide = "axis_minor") +
  scale_y_continuous(expand = c(0, 0), guide = "axis_minor", limits = c(-40000, 220000), breaks = seq(0, 250000, 50000))
dev.off()  

## summarize scratch width
dat_summary_width <- Rmisc::summarySE(dat, measurevar="Width_µm", groupvars=c("time","treatment"))

svg("Results/scratch_assay_TGFbeta_width.svg",  width=3.6, height=2.5)
dat_summary_width %>%
  ggplot(aes(time, Width_µm, color = treatment)) + 
  geom_smooth(aes(fill = treatment), method = "lm", se = TRUE, lty = 1) +
  stat_regline_equation(label.x=c(14,14), label.y=c(350,270), size = 3) +
  stat_regline_equation(aes(label=..adj.rr.label..),label.x=c(14,14), label.y=c(320, 240), size = 3) +
  geom_errorbar(aes(ymin = Width_µm - sd, ymax = Width_µm + sd), width = 0.4, alpha = 0.7) +
  geom_point(size = 2, alpha = 0.7) +
  theme_PhD(axis.text.size = 12) +
  theme(
    axis.title.x = element_text(face = "bold", size = 9),
    strip.background = element_blank(),
    legend.key.size = (unit(0.7,"line")),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  scale_color_jco() +
  scale_fill_jco() +
  xlab("Time (h)") +
  ylab("Scratch wound width (\u03bcm)") + 
  scale_x_continuous(breaks = seq(0,24,4), guide = "axis_minor") +
  scale_y_continuous(expand = c(0, 0), guide = "axis_minor",limits = c(-50, 450), breaks = seq(0, 400, 100))
dev.off()  

## calcualte summary of sd width
dat_summary_sd_width <- Rmisc::summarySE(dat, measurevar="Standard_deviation_µm", groupvars=c("time","treatment"))

svg("Results/scratch_assay_TGFbeta_sd_width.svg",  width=4, height=2.5)
dat_summary_sd_width %>%
  ggplot(aes(time, Standard_deviation_µm, color = treatment)) + 
  geom_errorbar(aes(ymin = Standard_deviation_µm - sd, ymax = Standard_deviation_µm + sd), width = 0.4) +
  geom_point(size = 2) +
  theme_PhD(axis.text.size = 9) +
  theme(
    axis.title.x = element_text(face = "bold", size = 9),
    strip.background = element_blank(),
    legend.key.size = (unit(0.7,"line")),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  scale_color_jco() +
  xlab("Time (h)") +
  ylab("Standard deviation of \n scratch wound (\u03bcm)") + 
  scale_x_continuous(breaks = seq(0,24,4), guide = "axis_minor") +
  scale_y_continuous(expand = c(0, 0),limits = c(-10,80), guide = "axis_minor", breaks = seq(0, 80, 10))
dev.off() 

### Statistics

## calculate significance of differences in slope for wound closure
m.interaction <- lm(Wound_closure ~ treatment*time, data = dat)
anova(m.interaction)

## calculate significance of differences in slope for closed area
m.interaction <- lm(Area_µm ~ treatment*time, data = dat)
anova(m.interaction)

## calculate significance of differences in slope for scratch width
m.interaction <- lm(Width_µm ~ treatment*time, data = dat)
anova(m.interaction)

## compare results from previous statistical tests to white adjusted ANOVA (calculated with heteroscedasticity 
## corrected covariance matrices)
dat %>% anova_test(Wound_closure ~ treatment*time, white.adjust = TRUE)
dat %>% anova_test(Area_µm ~ treatment*time, white.adjust = TRUE)
dat %>% anova_test(Width_µm ~ treatment*time, white.adjust = TRUE)



