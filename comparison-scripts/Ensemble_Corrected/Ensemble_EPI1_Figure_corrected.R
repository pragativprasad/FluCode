# Corrected copy of Ensemble/Ensemble_EPI1_Figure.R. Changes from the original:
#  - reads the corrected outputs for the chosen ensemble method (options(flucode.ensemble_method);
#    default "mean_of_quantiles", the paper's method) instead of MeanBased-Ensemble
#  - age-specific ENS (fig 2) comes from the age_interval files for the chosen method
#  - averted hospitalisations and deaths are plotted in thousands (corrected counts are ~32.6x
#    smaller than the originals, which were plotted in millions)
#  - no Windows setwd(); output goes to Ensemble_Corrected/figures/<method>/
#  - reshape/khroma are not loaded (not used)

repo_root <- local({
  d <- normalizePath(getwd())
  while (!dir.exists(file.path(d, "model-forecasts"))) {
    if (dirname(d) == d) stop("Run from inside the FluCode repository.")
    d <- dirname(d)
  }
  d
})
setwd(file.path(repo_root, "comparison-scripts"))
ensemble_method <- match.arg(getOption("flucode.ensemble_method", "mean_of_quantiles"),
                             c("mean_of_quantiles", "linear_pool"))
ens_dir <- c(mean_of_quantiles = "MeanOfQuantiles-Corrected", linear_pool = "LinearPool-Ensemble")[[ensemble_method]]
fig_dir <- file.path(repo_root, "comparison-scripts", "Ensemble_Corrected", "figures", ensemble_method)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
path <- file.path(repo_root, "model-forecasts/")

library(ggplot2)
library(dplyr)
library(viridis)
library(ggsci)
library(ggpubr)
library(gridExtra)
library(egg)
library(plyr)
library(tidyr)
library(scales)
library(lubridate)
library(data.table)
library(magrittr)
library(grid)
library(doBy)

#Environment -------------------------------------------------------------------------------------

path <- file.path(repo_root, "model-forecasts/")

ens.levels <- c("ENS", "UVA", "UTA", "NEU3", "IMP", "COL2", "COL")
scenarios = c("HP01_", "HP02_", "HP03_", "HP04_", "HP05_", "HP06_", "HP07_", "HP08_", "HP09_", "HP10_", "HP11_", "HP12_")

# Figures -------------------------------------------------------------

dat <- read.csv(paste0(path, ens_dir, "/PAN1_symillness_ensemble.csv"))
dat$Date <- as.Date(dat$Date)
dat$team <- factor(dat$team, ens.levels)

ens <- dat %>% 
  filter(team == "ENS") %>%
  select(Date, paste0(scenarios,"mean")) %>%
  pivot_longer(cols = paste0(scenarios,"mean"), names_to = "scenario", values_to = "sm.mean") %>%
  mutate(scenario = gsub("_mean", "", scenario))

png(file.path(fig_dir, "supp_fig4a.png"),width = 1100, height=750, res= 220)
ggplot(data = ens, aes(x = Date, group=scenario)) +
  geom_line(aes(y = sm.mean, color=scenario),lwd = 1.5) +
  ylab("% Incident Symptomatic Illness") +
  scale_fill_viridis(discrete=TRUE)+
  scale_color_viridis(discrete=TRUE)+
  ggtitle("a.")+
  theme_light()+
  scale_x_date(labels = date_format("%b"))
dev.off()



