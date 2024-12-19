
## This script plots the phylogeny and associated traits.

rm(list=ls())

load("data/spec_net_coast.Rdata")
source("src/init.R")

## ## install ggtree
## if (!require("BiocManager", quietly = TRUE))
##     install.packages("BiocManager")
## BiocManager::install("ggtree")

## Load tree from : Henriquez Piskulich, Patricia Andrea; Hugall,
## Andrew F.; Stuart-Fox, Devi (2023).  A supermatrix phylogeny of the
## world’s bees (Hymenoptera: Anthophila) [Dataset].
## Dryad. https://doi.org/10.5061/dryad.80gb5mkw1
load("data/phylo.Rdata")

## Subset specimens to only Bombus since that is the only group where
## the phylogeny is relevant (in the parasite prevalence model)
spec.net <- spec.net[spec.net$Genus == "Bombus",]
spec.net.uni <- unique(spec.net[, c("StandRoundYear", "GenusSpecies",
                                    "rare.degree")])

## Calculate the mean strait values across stands, sample rounds for
## rarefied degree and crithidia prevalence
sp.means <- spec.net.uni   %>%
  group_by(GenusSpecies) %>%
  summarize(Degree = mean(rare.degree,
                               na.rm=TRUE)
)

sp.means <- sp.means[!apply(sp.means, 1, function(x) all(is.na(x))),]

## add on species-level foraging distance
forage <- unique(spec.net[, c("GenusSpecies", "ForageDist_km")])
sp.means <- merge(sp.means, forage)

sp.means <- sp.means[, c("GenusSpecies", "Degree",
                         "ForageDist_km")]

## Drop tips not in the data (the phylogeny is for Oregon bombus)
phylo <- drop.tip(phylo, phylo$tip.label[!phylo$tip.label %in% unique(spec.net$GenusSpecies)])
sp.means[is.na(sp.means)] <- 0

colnames(sp.means) <- c("label", "Degree",
                         "Foraging_distance_km")

## bind traits to phylogeny and make a plot
tree.traits <- full_join(phylo, sp.means, by='label')

trs <- list(Degree = tree.traits, Foraging_distance_km = tree.traits)
class(trs) <- 'treedataList'

supp.labs <- c("Diet breadth", "Foraging distance (km)")
names(supp.labs) <- c("Degree", "Foraging_distance_km")

p1 <- ggtree(trs) +  geom_tiplab(size =5, fontface=3) + hexpand(0.8) +
  labs(tag = "A")

p2 <-  ggtree(trs) + facet_wrap(~.id,
                                labeller = labeller(.id = supp.labs)) +
  geom_tippoint(aes(subset=.id == 'Degree',
                    colour=Degree, cex=2,
                    )) +
  scale_colour_gradient(low='yellow', high='red',
                        name = "Diet breadth") +
  labs(tag = "B") +
  ggnewscale::new_scale_colour()  +
  geom_tippoint(aes(subset=.id == 'Foraging_distance_km',
                    colour=Foraging_distance_km, cex=2,)) +
  scale_colour_gradient(low='grey', high='blue',
                         name = "Foraging distance (km)") +
  ggnewscale::new_scale_colour()  +
  guides(cex = "none")

p3 <- grid.arrange(p1, p2, nrow = 1)

ggsave(p3,
       file="figures/comm_phylo.pdf",
       width=11, height=6)
