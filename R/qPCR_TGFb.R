## <<<<<<<<<<<<<<< HEAD

# load packages
library(tidyverse)
library(ggpubr)
library(devtools)
library(EnvStats)
library(readxl)
library(rstatix)
library(ggh4x)
library(ggsci)

# source R functions
source_url("https://raw.githubusercontent.com/MBender1992/base_scripts/Marc/R_functions.R")  

## define custom function only used within this script
## calculate nested anova
nested_aov <- function(dat, x, y){
  m.full <- aov(dat[[y]]~dat[[x]])
  m.red  <- aov(dat[[y]]~0)
  anova(m.red, m.full)
}

# load and transform data
dat_qPCR <- read_csv("Data/cSCC_TGFB_qPCR_20221011.csv")%>% 
  rename(Name = sample_name) %>%
  mutate(time = str_extract(.$Name, "t\\d+")) %>%
  mutate(treatment = str_replace_all(.$Name, "^.+_","")) %>%
  mutate(cell_line = str_replace_all(.$Name, "_.+",""))

## replace numbers after ctrl or irr 
dat_qPCR$treatment <- gsub("[[:digit:]]+", "", dat_qPCR$treatment)

## calculate geometric mean of housekeeping genes
dat_HK <- dat_qPCR %>% 
  group_by(Name) %>%
  filter(gene_type == "HK") %>%
  mutate(geomean_HK = geoMean(ct_val, na.rm=T))  %>%
  distinct(geomean_HK) 

## calculate dCT values
dat_dCT <- inner_join(dat_qPCR, dat_HK, by = c("Name")) %>%
  ungroup() %>%
  filter(gene_type != "HK") %>%
  select(-gene_type) %>%
  mutate(dCT = geomean_HK - ct_val)

## calculate ddCT values
ddCT <- dat_dCT %>% 
  group_by(gene_name, cell_line, time) %>%
  filter(treatment == "ctrl") %>%
  summarize(ctrl_dCT = mean(dCT, na.rm = T)) %>%
  inner_join(dat_dCT %>% filter(treatment != "ctrl")) %>%
  mutate(ddCT = dCT - ctrl_dCT) %>%
  ungroup()

## plot results
svg("Results/TGFb_Pathway_UV.svg")
ddCT %>% 
  filter(!is.na(ddCT)) %>% 
  ggplot(aes(gene_name, ddCT, fill = cell_line)) +
  geom_boxplot(width = 0.4, outlier.shape = NA) +
  facet_wrap(cell_line~time, scales = "free") +
  geom_hline(yintercept= 0, lty = 2) +
  theme_PhD(axis.text.size = 10) +
  theme(
    strip.background = element_blank(),
    legend.position = "none"
  ) + 
  xlab("")+
  scale_y_continuous(limits = c(-3.5, 1.5), breaks = seq(-3,1, 1), guide = "axis_minor") +
  scale_fill_jco(alpha = 0.7)
dev.off()

## nested ANOVA 
dat_Fibros_t4 <- ddCT %>% filter(cell_line == "FVH2819" & gene_name != "TGFBRI" & time == "t4")
dat_Fibros_t72 <- ddCT %>% filter(cell_line == "FVH2819" & gene_name != "TGFBRI" & time == "t72")
dat_SCC12_t4 <- ddCT %>% filter(cell_line == "SCC-12"  & time == "t4")
dat_SCC12_t72 <- ddCT %>% filter(cell_line == "SCC-12"  & time == "t72")

## extract pvals
p_Fibros_t4 <- nested_aov(dat_Fibros_t4, x = "gene_name", y = "ddCT")$`Pr(>F)`[2]
p_Fibros_t72 <- nested_aov(dat_Fibros_t72, x = "gene_name", y = "ddCT")$`Pr(>F)`[2]
p_SCC12_t4 <- nested_aov(dat_SCC12_t4, x = "gene_name", y = "ddCT")$`Pr(>F)`[2]
p_SCC12_t72 <- nested_aov(dat_SCC12_t72, x = "gene_name", y = "ddCT")$`Pr(>F)`[2]

## combine and adjust pvals
p <- c(p_Fibros_t4, p_Fibros_t72, p_SCC12_t4,p_SCC12_t72)
p.adjust(p, method = "fdr", n = length(p))

## calculate gene wise one sample t-tests
ddCT %>%  filter(cell_line == "FVH2819" & gene_name != "TGFBRI") %>% group_by(gene_name, time) %>% t_test(ddCT~ 1, mu = 0) %>%
  adjust_pvalue(method = "fdr")
ddCT %>%  filter(cell_line == "SCC-12") %>% group_by(gene_name, time) %>% t_test(ddCT~ 1, mu = 0) %>%
  adjust_pvalue(method = "fdr")
