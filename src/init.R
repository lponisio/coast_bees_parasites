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

