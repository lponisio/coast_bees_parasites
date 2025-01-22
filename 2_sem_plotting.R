
## Prepares the data for model fitting (standardizes continuous
## variables, creates dummy variables to be used as weights to all
## different subsets of data to be used in different model levels),
## builds the models, and fits the models in brms. The model outputs
## are saved as tables, and chain diagnostic plots created.

## this tutorial was used to inform plotting:
## https://www.andrewheiss.com/blog/2022/05/09/hurdle-lognormal-gaussian-brms/

setwd("C:/Users/mycol/Dropbox (University of Oregon)/coast_bees_parasites/coast_bees_parasites")

rm(list=ls())
source("src/ggplotThemes.R")
source("src/init.R")
source("src/misc.R")

## ***********************************************************************
## stand type boxplots
## ***********************************************************************

load("data/spec_net_coast.Rdata")

#bins: young stands 0-20, thins 20-40, thins 40-60, non thinned old
spec.net$categories <- with(spec.net, 
                      ifelse(spec.net$ThinStatus == "N" & 
                               spec.net$DomTreeDiam_cm <= 20, "Recent harvest",
                      ifelse(spec.net$ThinStatus == "Y" & 
                               spec.net$DomTreeDiam_cm > 20 & 
                               spec.net$DomTreeDiam_cm <= 40, "Thin 1",
                      ifelse(spec.net$ThinStatus == "Y" & 
                               spec.net$DomTreeDiam_cm > 40 & 
                               spec.net$DomTreeDiam_cm <= 60, "Thin 2",
                      ifelse(spec.net$ThinStatus == "N" & 
                               spec.net$DomTreeDiam_cm > 60, "Mature",
                      "Other")))))

##Drop NA and Other - stands that are unthinned and mid-aged or don't 
##have thin/dbh data 

spec.net <- spec.net %>%
  filter(!is.na(categories) & categories != "Other")

spec.net$categories <- 
  factor(spec.net$categories, levels = c("Recent harvest", "Thin 1",
                                       "Thin 2", "Mature"))

box1 <- ggplot(spec.net, aes(x = categories, y = log(VegAbundance))) +
  geom_boxplot() +
  labs(x = "Stand type", 
       y = "Flowering plant abundance (log)") +
  theme_classic()

box2 <- ggplot(spec.net, aes(x = categories, y = VegDiversity)) +
  geom_boxplot() +
  labs(x = "Stand type", 
       y = "Flowering plant diversity") +
  theme_classic()

box3 <- ggplot(spec.net, aes(x = categories, y = BeeAbundance)) +
  geom_boxplot() +
  labs(x = "Stand type", 
       y = "Bee abundance") +
  theme_classic()

box4 <- ggplot(spec.net, aes(x = categories, y = BeeDiversity)) +
  geom_boxplot() +
  labs(x = "Stand type", 
       y = "Bee diversity") +
  theme_classic()

all.box <- ggarrange(box1, box2, box3, box4,
                     nrow=2, ncol=2, labels = c("A", "B", "C", "D"),
                     common.legend = TRUE, legend="bottom")

ggsave(all.box, file="figures/boxplots.pdf", height=7, width=10)

## ***********************************************************************
## descriptive bar charts
## ***********************************************************************

## binning canopy - remember that canopy is unintuitive and 0-25 is lowest
spec.net$CanopyBin <- cut(spec.net$MeanCanopy,
                     breaks = c(0, 25, 75, 100),
                     include.lowest = T, right = F)

## veg abundance summary by canopy type
spec.net <- spec.net[!is.na(spec.net$CanopyBin),]

stats <- spec.net %>%
  group_by(ThinStatus) %>%
  summarise(meanveg =mean(VegAbundance),
            sdveg = sd(VegAbundance))

spec.net.2 <- spec.net %>%
  filter(ThinStatus == 'N')

stats2 <- spec.net.2 %>%
  group_by(CanopyBin) %>%
  summarise(meanveg =mean(VegAbundance),
            sdveg = sd(VegAbundance))

## bee diversity summary by canopy type
stats <- spec.net %>%
  group_by(ThinStatus) %>%
  summarise(meanbee =mean(BeeDiversity),
            sdbee = sd(BeeDiversity))

stats2 <- spec.net.2 %>%
  group_by(CanopyBin) %>%
  summarise(meanbee =mean(BeeDiversity),
            sdbee = sd(BeeDiversity))

spec.net <- spec.net[!is.na(spec.net$CanopyBin),]

## per canopy type mean and sd of veg abund/diversity
closed <- spec.net[spec.net$CanopyBin %in% c("[0,25)"),]
intermed <- spec.net[spec.net$CanopyBin %in% c("[25,75)"),]
open <- spec.net[spec.net$CanopyBin %in% c("[75,100]"),]

## ***********************************************************************
## all bee genus bar graph
## ***********************************************************************
## summarize individuals per n type of stand in each category
standstype <- spec.net %>%
  group_by(CanopyBin) %>%
  summarise(count = n_distinct(Stand))

beesper <- spec.net %>%
  group_by(CanopyBin, Genus) %>%
  summarise(count = n()) %>%
  filter(!is.na(Genus))

summary <- beesper %>%
  left_join(standstype, by="CanopyBin") %>%
  mutate(avg = count.x/count.y)

## modifying to show prop of total
bee.spp.bar <- ggplot(summary,
                         aes(y = fct_reorder(Genus, avg, .desc = TRUE),
                             x = avg)) +
  geom_bar(stat = 'identity',
           aes(fill = factor(CanopyBin)), position = "dodge") +
  scale_fill_manual(values=c('#004D40', '#FFC107', 'lightblue'),
                    name = "Canopy type",
                    labels=c('Closed', 'Intermediate', 'Open')) +
  theme_classic() +
  theme(legend.position = "top",
                axis.text.y = element_text(angle = 0, hjust = 1,
                                   face ='italic', color = 'black'),
        axis.text.x = element_text(color="black"),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        axis.ticks = element_line(color = "black"),
        text = element_text(size=14)) +
  labs(y=expression(paste('Genus')), x='Average individuals
       collected per canopy type')

bee.spp.bar

## ***********************************************************************
## plant spp bar graph
## ***********************************************************************
## summarize individuals per n type of stand in each category
standstype <- spec.net %>%
  group_by(CanopyBin) %>%
  summarise(count = n_distinct(Stand))

plantper <- spec.net %>%
  group_by(CanopyBin, PlantGenusSpecies) %>%
  summarise(count = n()) %>%
  filter(!is.na(PlantGenusSpecies))

summary <- plantper %>%
  left_join(standstype, by="CanopyBin") %>%
  mutate(avg = count.x/count.y) 

summary <- subset(summary, summary$count.x>2) 


## modifying to show prop of total
plant.bar <- ggplot(summary,
                      aes(y = fct_reorder(PlantGenusSpecies, avg, .desc = TRUE),
                          x = avg)) +
  geom_bar(stat = 'identity',
           aes(fill = factor(CanopyBin)), position = "dodge") +
  scale_fill_manual(values=c('#004D40', '#FFC107', 'lightblue'),
                    name = "Canopy type",
                    labels=c('Closed', 'Intermediate', 'Open')) +
  theme_classic() +
  theme(legend.position = "top",
        axis.text.y = element_text(angle = 0, hjust = 1,
                                   face ='italic', color = 'black'),
        axis.text.x = element_text(color="black"),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        axis.ticks = element_line(color = "black"),
        text = element_text(size=14)) +
  labs(y=expression(paste('Plant species')), x='Average bees
       collected on plant species per canopy type')

plant.bar 

ggsave(plant.bar, file="figures/plant_bar.pdf",
       height=9, width=11)

## ***********************************************************************
## bombus species bar graph
## ***********************************************************************

bomb.orig <- spec.net %>%
  filter(Genus == 'Bombus')

beesper <- bomb.orig %>%
  group_by(CanopyBin, GenusSpecies) %>%
  summarise(count = n())

bombsummary <- beesper %>%
  left_join(standstype, by="CanopyBin") %>%
  mutate(avg = count.x/count.y)

#modifying to show prop of total
bombus.spp.bar <- ggplot(bombsummary,
                         aes(y = fct_reorder(GenusSpecies, avg, .desc = TRUE),
                             x = avg)) +
  geom_bar(stat = 'identity',
           aes(fill = factor(CanopyBin)), position = "dodge",
           show.legend = F) +
  scale_fill_manual(values=c('#004D40', '#FFC107', 'lightblue')) +
  theme_classic() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1,
                                   face ='italic', color = 'black'),
        axis.text.x = element_text(color="black"),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        axis.ticks = element_line(color = "black"),
        text = element_text(size=14)) +
  labs(y=expression(paste('Species')), x='Average individuals
       collected per canopy type')

bombus.spp.bar

## parasite counts
parasite.count.table <- spec.net %>%
  filter(Apidae == 1, Genus == "Bombus") %>%
    select(SpecimenID, ApicystisSpp, AscosphaeraSpp, CrithidiaBombi,
           CrithidiaExpoeki,
           CrithidiaSpp, NosemaBombi, NosemaCeranae, ParasitePresence,
           ParasiteRichness,
         CanopyBin) %>%
  pivot_longer(cols=c(ApicystisSpp, AscosphaeraSpp, CrithidiaBombi,
                      CrithidiaSpp, CrithidiaExpoeki, NosemaCeranae,
                      NosemaBombi),
               names_to = 'ParasiteName', values_to = 'HasParasite') %>%
  filter(HasParasite == 1)

#using #4C4C5A
parasite.hist <- parasite.count.table %>%
  ggplot(aes(y= fct_infreq(ParasiteName))) +
  geom_bar(stat = 'count') +
  theme_classic() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, color = "black"),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        axis.text.x = element_text(color = "black"),
        text = element_text(size=14)) +
  labs(y='Parasite', x='Number of \n infected individuals') +
  xlim(0,75) +
  scale_y_discrete(labels=c("NosemaCeranae"=expression(italic('Nosema ceranae')),
                            "NosemaBombi"=expression(italic('Nosema bombi')),
                            'AscosphaeraSpp'=expression(paste(italic('Ascosphaera'), ' spp.')),
                            'ApicystisSpp'=expression(paste(italic('Apicystis'), ' spp.')),
                            "CrithidiaBombi"=expression(italic('Crithidia bombi')),
                            "CrithidiaExpoeki"=expression(italic('Crithidia expoeki')),
                            'CrithidiaSpp'=expression(paste(italic('Crithidia'), ' spp.')),
                            parse=TRUE))  + labs(fill = "Canopy openness")

parasite.hist

left.col <- ggarrange(bee.spp.bar, ncol=1, nrow=1,
                      labels = c("A"))

right.col <- ggarrange(bombus.spp.bar,
                       parasite.hist, nrow=2, ncol=1,
                       labels = c("B", "C"))

## combining summary plots
summaries <- ggarrange(left.col,
                       right.col,
                       common.legend = TRUE, legend="bottom"
                       )

ggsave(summaries, file="figures/summaries.pdf",
       height=9, width=11)

## ***********************************************************************
## prepping for newdata draws
## ***********************************************************************
## load model results and data
load(file="saved/CrithidiaFitAllBee_coast.Rdata")

## log + 1  Veg abundance, Bee diversity and bee abundance
## log "ForageDist_km", "rare.degree"

## Bee diversity and bee abundance are not scaled


new.net <- spec.net[spec.net$Weights == 1, ]
new.orig <- spec.orig[spec.orig$Weights == 1, ]

## x and y labs
## canopy
labs.canopy <- pretty(c(0, new.orig$MeanCanopy), n=8)
axis.canopy <- standardize.axis(labs.canopy,
                                      new.orig$MeanCanopy)

##  log + 1
## floral diversity
labs.fdiv <- (pretty(new.orig$VegDiversity, n=5))
axis.fdiv <- standardize.axis(labs.fdiv,
                                 new.orig$VegDiversity)

## floral abundance
labs.fabund <- pretty(exp(new.orig$VegAbundance), n=10)
axis.fabund <- standardize.axis(labs.fabund,
                                 exp(new.orig$VegAbundance))

## bee diversity
labs.bdiv <- pretty((exp(new.orig$BeeDiversity)), n=10)
axis.bdiv <- standardize.axis(labs.bdiv,
                                 new.orig$BeeDiversity)

## bee abund
labs.babund <- pretty(new.orig$BeeAbundance, n=10)
axis.babund <- standardize.axis(labs.babund,
                                 new.orig$BeeAbundance)

## foraging distance km
labs.forage.dist <- pretty(new.orig$ForageDist_km[
                                        new.orig$Genus == "Bombus"], n=10)
axis.forage.dist <- standardize.axis(labs.forage.dist,
                                 new.orig$ForageDist_km)


## ***********************************************************************
## mean canopy ~ floral diversity
## ***********************************************************************

## for hurdle models, include both count and hurdle parts of the models
all.cond.effects <- conditional_effects(fit.bombus)

## different slopes and intercepts for thinned/unthinned
## thinned slope is not strongly different from zero

vdiv <-
    all.cond.effects[["VegDiversity.VegDiversity_MeanCanopy:ThinStatus"]]

vdiv.stand <- ggplot(vdiv, aes(x = MeanCanopy,
                               y = estimate__)) +
    geom_line(aes(x = MeanCanopy, y=estimate__ ,
                  color = ThinStatus, linetype = ThinStatus), size=1.5) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, fill=ThinStatus,
                    alpha=0.3)) +
    scale_fill_manual(values = c("darkolivegreen4", "forestgreen"),
                      labels = c("Unthinned", "thinned")) +
    scale_color_manual(values = c("black", "#999999")) +
    geom_point(data=new.net,
               aes(x=MeanCanopy, y=VegDiversity,
                   color = ThinStatus), cex=2) +
    geom_point(data=new.net,
               aes(x=MeanCanopy, y=VegDiversity), cex=2, pch=1) +
    labs(x = "", y = "Floral diversity",
         fill = "95% credible interval") +
    theme_ms() +
                                        #theme(legend.position = "none") +
    scale_x_continuous(
        breaks = axis.canopy,
        labels =  labs.canopy) +
    scale_y_continuous(
        labels = labs.fdiv,
        breaks = axis.fdiv) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size=16),
          text = element_text(size=16)) +
    guides(linetype = "none", alpha="none", color="none")

## ***********************************************************************
## mean canopy and floral abundance
## ***********************************************************************
## different intercepts and slopes for thinned/unthinned
## thinned

vabund <-
    all.cond.effects[["VegAbundance.VegAbundance_MeanCanopy:ThinStatus"]]

vabund.stand <- ggplot(vabund, aes(x = MeanCanopy,
                                   y = estimate__)) +
    geom_line(aes(x = MeanCanopy, y=estimate__ ,
                  color = ThinStatus),  size=1.5) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, fill=ThinStatus,
                    alpha=0.3)) +
    scale_fill_manual(values = c("darkolivegreen4", "forestgreen"),
                      labels = c("Unthinned", "Thinned")) +
    scale_color_manual(values = c("black", "#999999")) +
    geom_point(data=new.net,
               aes(x=MeanCanopy, y=VegAbundance,
                   color = ThinStatus), cex=2) +
    geom_point(data=new.net,
               aes(x=MeanCanopy, y=VegAbundance), cex=2, pch=1) +
    labs(x = "", y = "Floral abundance (log)",
         fill = "95% credible interval") +
    theme_ms() +
    scale_x_continuous(
        breaks = axis.canopy,
        labels =  labs.canopy
        ) +
    scale_y_continuous(
        labels = labs.fabund,
        breaks = axis.fabund) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size=16),
          text = element_text(size=16))  +
    guides(linetype = "none", alpha="none", color="none")

## ***********************************************************************
## mean canopy and bee diversity
## ***********************************************************************
## different intercepts and slopes for thinned/unthinned
## thinned

bdiv <-
    all.cond.effects[["BeeDiversity.BeeDiversity_MeanCanopy:ThinStatus"]]

bdiv.stand <- ggplot(bdiv, aes(x = MeanCanopy,
                               y = estimate__)) +
    geom_line(aes(x = MeanCanopy, y=estimate__ ,
                  color = ThinStatus, linetype = ThinStatus), size=1.5) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, fill=ThinStatus,
                    alpha=0.3)) +
    scale_fill_manual(values = c("darkolivegreen4", "forestgreen"),
                      labels = c("Unthinned", "Thinned")) +
    scale_color_manual(values = c("black", "#999999")) +
    geom_point(data=new.net,
               aes(x=MeanCanopy, y=BeeDiversity,
                   color = ThinStatus), cex=2) +
    geom_point(data=new.net,
               aes(x=MeanCanopy, y=BeeDiversity), cex=2, pch=1) +
    labs(x = "Canopy openness", y = "Bee diversity (log)",
         fill = "95% credible interval") +
    theme_ms() +
    scale_x_continuous(
        breaks = axis.canopy,
        labels =  labs.canopy) +
    scale_y_continuous(
        labels = labs.bdiv,
        breaks = axis.bdiv) +
    theme(axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          text = element_text(size=16))  +
    guides(linetype = "none", alpha="none", color="none")

## ***********************************************************************
## mean canopy and bee abundance
## ***********************************************************************

babund <-
    all.cond.effects[["BeeAbundance.BeeAbundance_MeanCanopy:ThinStatus"]]

babund.stand <- ggplot(babund, aes(x = MeanCanopy,
                                   y = estimate__)) +
    geom_line(aes(x = MeanCanopy, y=estimate__ ,
                  color = ThinStatus,
                  linetype = ThinStatus), size=1.5) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, fill=ThinStatus,
                    alpha=0.3)) +
    scale_fill_manual(values = c("darkolivegreen4", "forestgreen"),
                      labels = c("Unthinned", "Thinned")) +
    scale_color_manual(values = c("black", "#999999")) +
    geom_point(data=new.net,
               aes(x=MeanCanopy, y=BeeAbundance,
                   color = ThinStatus), cex=2) +
    geom_point(data=new.net,
               aes(x=MeanCanopy, y=BeeAbundance), cex=2, pch=1) +
    labs(x = "Canopy openness", y = "Bee abundance (log)",
         fill = "95% credible interval") +
    theme_ms() +
    scale_x_continuous(
        breaks = axis.canopy,
        labels =  labs.canopy) +
    lims(y=c(0, max(axis.babund)))+
    scale_y_continuous(
        labels = labs.babund,
        breaks = axis.babund) +
    theme(axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          text = element_text(size=16))  +
    guides(linetype = "none", alpha="none", color="none")

all.canopy <- ggarrange(vabund.stand, vdiv.stand, babund.stand, bdiv.stand,
                        nrow=2, ncol=2, labels = c("A", "B", "C", "D"),
                        common.legend = TRUE, legend="bottom")

ggsave(all.canopy, file="figures/all_canopy.pdf", height=7, width=8)

## ***********************************************************************
## veg diversity and bee diversity
## ***********************************************************************

babund.fabund <-
    all.cond.effects[["BeeAbundance.BeeAbundance_VegAbundance"]]

babund.fabund.p <- ggplot(babund.fabund, aes(x = VegAbundance,
                                   y = estimate__)) +
    geom_line(aes(x = VegAbundance, y=estimate__ ), size=1.5) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__,
                    alpha=0.3)) +
    scale_fill_manual(values = c("dodgerblue")) +
    scale_color_manual(values = c("black", "#999999")) +
    geom_point(data=new.net,
               aes(x=VegAbundance, y=BeeAbundance,
                   color = ThinStatus), cex=2) +
    geom_point(data=new.net,
               aes(x=VegAbundance, y=BeeAbundance), cex=2, pch=1) +
    labs(x = "Floral abundance (log)", y = "Bee abundance (log)",
         fill = "Credible interval") +
    theme_ms() +
    theme(legend.position = "none") +
    scale_x_continuous(
        breaks = axis.fabund,
        labels =  labs.fabund) +
    scale_y_continuous(
        labels = labs.babund,
        breaks = axis.babund) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size=16),
          text = element_text(size=16))

babund.fabund.p

## ***********************************************************************
## bee community diversity and abundance and parasitism
## ***********************************************************************

## ***********************************************************************
## parasitism ~ foraging dist
## ***********************************************************************

crithidia.foraging.dist <-
    all.cond.effects[["HasCrithidia.HasCrithidia_ForageDist_km"]]

crithidia.foraging.dist.p <-
    ggplot(crithidia.foraging.dist, aes(x = ForageDist_km,
                                        y = estimate__)) +
    geom_line(aes(x = ForageDist_km, y=estimate__ )) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__,
                    alpha=0.3)) +
    scale_fill_manual(values = c("dodgerblue")) +
    labs(x = "Foraging distance (log km)", y = "Crithidia prevalence",
         fill = "Credible interval") +
    theme_ms() +
    theme(legend.position = "none") +
    scale_x_continuous(
        breaks = axis.forage.dist,
        labels =  labs.forage.dist) +
    xlim(range(axis.forage.dist)) +
    theme(axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          text = element_text(size=16))

crithidia.foraging.dist.p

ggsave(crithidia.foraging.dist.p, file="figures/crithidia_foragingdis.pdf",
       height=5, width=5)


## ***********************************************************************
## scatterplot of canopy cover and dbh, highlighting thins
## ***********************************************************************

stands <- spec.orig[!duplicated(spec.orig$StandRoundYear), ]

standsplot <- stands %>%
  ggplot(aes(x=DomTreeDiam_cm, y=MeanCanopy, color=ThinStatus)) +
  geom_point() +
  scale_color_manual(values = c("Y" = "#999999", "N" = "black"),
                     labels=c('Not thinned', 'Thinned'),
                     name="Management history") +
  labs(x="Dominant tree class DBH (cm)", y="Canopy openness") +
  xlim(0,75) +
  theme_classic()+
  theme(
    axis.text.x = element_text(color="black"),
    axis.text.y = element_text(color="black"),
    axis.ticks = element_line(color = "black"))

standsplot

ggsave(standsplot, file="figures/canopyDBH.pdf",
       height=2, width=4)
