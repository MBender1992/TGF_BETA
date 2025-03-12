## <<<<<<<<<<<<<<<HEAD

## load packages
library(tidyverse)
library(ggpubr)
library(ggsci)
library(devtools)
library(ggh4x)
library(rstatix)

## source R functions
source_url("https://raw.githubusercontent.com/MBender1992/base_scripts/Marc/R_functions.R") 

## load data 
dat <- read_csv("Data/cSCC_TGFB_EMT_Marker_v0.2_20220404.csv") %>%
  filter(!Messung %in% c("20220120")) %>%
  mutate(Name = factor(Name, levels = c("negative_control", "transfection_control", "anti_miR_205", "TGF_beta_10ng", "TGF_beta_10ng_48h", "TGF_beta_130ng", "anti_miR_205+TGF_beta", "fibroblasts"))) %>%
  select(-Messung)
colnames(dat) <- c("ID", "Name", "N-Cadherin", "E-Cadherin")

svg("Results/EMT_markers.svg",  width=3, height=5)
dat %>% gather(-Name, key = "marker", value ="expression") %>%
ggplot(aes(marker, expression, fill = marker)) +
  geom_bar(stat = "summary", fun = "mean",
           color = "black", position = position_dodge(0.6),
           mapping = aes(x = marker,y = expression),
           width=0.6
  )  +
  geom_errorbar(stat = "summary", fun.data = "mean_sdl", fun.args = list(mult = 1),
                position = position_dodge(0.6), width = 0.25, size = 0.6) +
  facet_wrap(~Name, scales = "free", nrow = 4) +
  theme_PhD(axis.text.size = 12) +
  theme(
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  ) + 
  scale_y_continuous(expand = c(0,0), guide = "axis_minor") +
  scale_fill_jco(alpha = 0.7) +
  ylab("Proteinexpression (a.u.)")
dev.off()

# display ratio of E to N-Cadherin
dat_ratio <- dat %>% mutate(ratio = log2(N_Cadherin/E_Cadherin)) 

# Dunnett Test to compare means vs control
anova_test(dat_ratio, ratio~Name, white.adjust = TRUE)
DescTools::DunnettTest(dat_ratio$ratio, dat_ratio$Name, control = "negative_control", conf.level = 0.95)

svg("Results/EMT_markers_ratio.svg",  width=3, height=3)
dat_ratio %>%
  ggplot(aes(Name, ratio, fill =Name)) + 
  geom_boxplot() +
  theme_PhD(axis.text.size = 12) +
  theme(
    axis.title.x = element_blank(),
    strip.background = element_blank(), 
    legend.position = "none",
    legend.title = element_blank()
  )  + 
  ylab("N-Cadherin/E-Cadherin (log2)")+
  scale_y_continuous(guide = "axis_minor", limits = c(-1.7, 2.7), breaks = c(seq(-1.5,2.5, 0.5))) +
  scale_fill_brewer(palette = "Blues")
dev.off()
 


