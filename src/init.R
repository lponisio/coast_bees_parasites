## models
library(brms)
library(bayestestR)
library(Matrix)
library(lme4)

## general
library(dplyr)
library(tidyverse)

## plotting
library(ggplot2)
library(ggpubr)
library(tidybayes)
library(ggthemes)
library(ggtext)
library(stringr)
library(viridis)
library(gridExtra)

## trees
library(phytools)
library(ggtree)
library(ape)
library(tidytree)

save.dir0 <- "saved/"
if(!dir.exists(save.dir0)) {
  dir.create(save.dir0, showWarnings = FALSE)
}

save.dir <- "saved/tables"
if(!dir.exists(save.dir)) {
  dir.create(save.dir, showWarnings = FALSE)
}

fig.dir <- "figures"
if(!dir.exists(fig.dir)) {
  dir.create(fig.dir, showWarnings = FALSE)
}


fig.dir2 <- "figures/diagnostics"
if(!dir.exists(fig.dir2)) {
  dir.create(fig.dir2, showWarnings = FALSE)
}

#binning stand categories

load("data/spec_net_coast.Rdata")

#bins: young stands 0-20, thins 20-40, thins 40-60, non thinned old
spec.net$categories <- with(spec.net,
      ifelse(spec.net$ThinStatus == "N" &
            spec.net$DomTreeDiam_cm <= 20, "Recent harvest",
      ifelse(spec.net$ThinStatus == "Y" &
            spec.net$DomTreeDiam_cm > 20 &
            spec.net$DomTreeDiam_cm <= 40, "Younger thin",
      ifelse(spec.net$ThinStatus == "Y" &
            spec.net$DomTreeDiam_cm > 40 &
            spec.net$DomTreeDiam_cm <= 60, "Older thin",
      ifelse(spec.net$ThinStatus == "N" &
            spec.net$DomTreeDiam_cm > 60, "Mature",
      "Other")))))

#Drop NA and Other - stands that are unthinned and mid-aged or don't
#have thin/dbh data

spec.net <- spec.net %>%
  filter(!is.na(categories) & categories != "Other")

spec.net$categories <-
  factor(spec.net$categories, levels = c("Recent harvest",
                                         "Younger thin",
                                         "Older thin",
                                         "Mature"))


