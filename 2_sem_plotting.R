
## Prepares the data for model fitting (standardizes continuous
## variables, creates dummy variables to be used as weights to all
## different subsets of data to be used in different model levels),
## builds the models, and fits the models in brms. The model outputs
## are saved as tables, and chain diagnostic plots created.

setwd("C:/Users/mycol/Dropbox (University of Oregon)/coast_bees_parasites/coast_bees_parasites")

## plotting based on tutorial:
## https://www.andrewheiss.com/blog/2021/11/10/ame-bayes-re-guide/#different-kinds-of-averCanopyBin-predictions-with-multilevel-models

rm(list=ls())
source("src/ggplotThemes.R")
source("src/init.R")
source("src/misc.R")

library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

## load model results and data
load(file="saved/CrithidiaFitAllBee_coast.Rdata")

## log + 1  Veg abundance, Bee diversity and bee abundance 
## log "ForageDist_km", "rare.degree"

## Bee diversity and bee abundance are not scaled

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

## ***********************************************************************
## descriptive bar charts
## ***********************************************************************

#binning canopy - remember that canopy is unintuitive and 0-25 is lowest
spec.orig$CanopyBin <- cut(spec.orig$MeanCanopy,
                     breaks = c(0, 25, 75, 100), 
                     include.lowest = T, right = F)

#veg abundance summary by canopy type 
spec.orig <- spec.orig[!is.na(spec.orig$CanopyBin),]

stats <- spec.orig %>% 
  group_by(ThinStatus) %>% 
  summarise(meanveg =mean(VegAbundance),
            sdveg = sd(VegAbundance)) 

spec.orig.2 <- spec.orig %>%
  filter(ThinStatus == 'N')
  
stats2 <- spec.orig.2 %>% 
  group_by(CanopyBin) %>% 
  summarise(meanveg =mean(VegAbundance),
            sdveg = sd(VegAbundance)) 

#bee diversity summary by canopy type 
stats <- spec.orig %>% 
  group_by(ThinStatus) %>% 
  summarise(meanbee =mean(BeeDiversity),
            sdbee = sd(BeeDiversity)) 

stats2 <- spec.orig.2 %>% 
  group_by(CanopyBin) %>% 
  summarise(meanbee =mean(BeeDiversity),
            sdbee = sd(BeeDiversity)) 

spec.orig <- spec.orig[!is.na(spec.orig$CanopyBin),]

#per canopy type mean and sd of veg abund/diversity
closed <- spec.orig[spec.orig$CanopyBin %in% c("[0,25)"),]
intermed <- spec.orig[spec.orig$CanopyBin %in% c("[25,75)"),]
open <- spec.orig[spec.orig$CanopyBin %in% c("[75,100]"),]


####bee genus bars

#summarize individuals per n type of stand in each category
standstype <- spec.orig %>% 
  group_by(CanopyBin) %>% 
  summarise(count = n_distinct(Stand)) 
  
beesper <- spec.orig %>% 
  group_by(CanopyBin, Genus) %>% 
  summarise(count = n()) %>% 
  filter(!is.na(Genus))

summary <- beesper %>% 
  left_join(standstype, by="CanopyBin") %>% 
  mutate(avg = count.x/count.y)

## ***********************************************************************
## all bee genus bar graph
## ***********************************************************************

#modifying to show prop of total
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

ggsave(bee.spp.bar, file="figures/beeSppbar.pdf",
       height=3, width=5)

## ***********************************************************************
## bombus species bar graph
## ***********************************************************************

bomb.orig <- spec.orig %>%
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

ggsave(bombus.spp.bar, file="figures/bombusSppbar.pdf",
       height=3, width=5)


## parasite counts
parasite.count.table <- spec.orig %>%
  filter(Apidae == 1) %>%
  select(SpecimenID, ApicystisSpp, AscosphaeraSpp, CrithidiaBombi, CrithidiaExpoeki, 
         CrithidiaSpp, NosemaBombi, NosemaCeranae, ParasitePresence, ParasiteRichness, 
         CanopyBin) %>%
  pivot_longer(cols=c(ApicystisSpp, AscosphaeraSpp, CrithidiaBombi,
                      CrithidiaSpp, CrithidiaExpoeki, NosemaCeranae, NosemaBombi),
               names_to = 'ParasiteName', values_to = 'HasParasite') %>%
  filter(HasParasite == 1)

#using #4C4C5A
parasite.hist <- parasite.count.table %>%
  ggplot(aes(y= fct_infreq(ParasiteName))) + 
  geom_bar(stat = 'count',
           aes(fill = factor(CanopyBin)), position = "dodge",
           show.legend = F) +
  scale_fill_manual(values=c('#004D40', '#FFC107', 'lightblue')) +
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

ggsave(parasite.hist, file="figures/coastparasiteSppHist.pdf",
       height=3, width=5)

## combining summary plots
summaries <- grid.arrange(bee.spp.bar, 
                          bombus.spp.bar, 
                          parasite.hist, 
                          nrow = 2,
                          layout_matrix = rbind(c(1, 2),
                                                c(1, 3))
)

ggsave(summaries, file="figures/summaries.pdf",
       height=9, width=11)

## ***********************************************************************
## prepping for newdata draws 
## ***********************************************************************

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
labs.fabund <- pretty(new.orig$VegAbundance, n=10)
axis.fabund <- standardize.axis(labs.fabund,
                                 new.orig$VegAbundance)

## bee diversity
labs.bdiv <- pretty(new.orig$BeeDiversity, n=10)
axis.bdiv <- standardize.axis(labs.bdiv,
                                 new.orig$BeeDiversity)

## bee abund
labs.babund <- pretty(new.orig$BeeAbundance, n=10)
axis.babund <- standardize.axis(labs.babund,
                                 new.orig$BeeAbundance)

## foraging distance km
labs.forage.dist <- pretty(new.orig$ForageDist_km, n=10)
axis.forage.dist <- standardize.axis(labs.forage.dist,
                                 new.orig$ForageDist_km)

## degree (diet breadth)
labs.degree <- pretty(new.orig$rare.degree, n=10)
axis.degree <- standardize.axis(labs.degree,
                                 new.orig$rare.degree)


## ***********************************************************************
## mean canopy ~ floral diversity
## ***********************************************************************

all.cond.effects <- conditional_effects(fit.bombus)
hurdle.cond.effects <- conditional_effects(fit.bombus, dpar = "mu")

## different slopes and intercepts for thinned/unthinned
## thinned

vdiv <-
    all.cond.effects[["VegDiversity.VegDiversity_MeanCanopy:ThinStatus"]]

vdiv.stand <- ggplot(vdiv, aes(x = MeanCanopy,
                                  y = estimate__)) +
    geom_line(aes(x = MeanCanopy, y=estimate__ ,
                  color = ThinStatus)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, fill=ThinStatus,
                    alpha=0.3)) +
    scale_fill_manual(values = c("darkolivegreen4", "forestgreen"),
                      labels = c("Unthinned", "thinned")) +
    scale_color_manual(values = c("black", gray(.4))) +
    geom_point(data=new.net,
               aes(x=MeanCanopy, y=VegDiversity,
                   color = ThinStatus), cex=2) +
    geom_point(data=new.net,
               aes(x=MeanCanopy, y=VegDiversity), cex=2, pch=1) +
    labs(x = "Canopy openness", y = "Floral diversity",
         fill = "Credible interval") +
    theme_ms() +
    theme(legend.position = "none") +
    scale_x_continuous(
        breaks = axis.canopy,
        labels =  labs.canopy) +
    scale_y_continuous(
        labels = labs.fdiv,
        breaks = axis.fdiv) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size=16),
          text = element_text(size=16)) 

vdiv.stand

## ***********************************************************************
## mean canopy and floral abundance
## ***********************************************************************
## different intercepts for thinned/unthinned
## thinned

vabund <-
    all.cond.effects[["VegAbundance.VegAbundance_MeanCanopy:ThinStatus"]]

vabund.stand <- ggplot(vabund, aes(x = MeanCanopy,
                                      y = estimate__)) +
    geom_line(aes(x = MeanCanopy, y=estimate__ ,
                  color = ThinStatus)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, fill=ThinStatus,
                    alpha=0.3)) +
    scale_fill_manual(values = c("darkolivegreen4", "forestgreen"),
                      labels = c("Unthinned", "Thinned")) +
    scale_color_manual(values = c("black", gray(.4))) +
    geom_point(data=new.net,
               aes(x=MeanCanopy, y=VegAbundance,
                   color = ThinStatus), cex=2) +
    geom_point(data=new.net,
               aes(x=MeanCanopy, y=VegAbundance), cex=2, pch=1) +
    labs(x = "Canopy openness", y = "Floral abundance (log)",
         fill = "Credible interval") +
    theme_ms() +
    theme(legend.position = "none") +
    scale_x_continuous(
        breaks = axis.canopy,
        labels =  labs.canopy) +
    scale_y_continuous(
        labels = labs.fabund,
        breaks = axis.fabund) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size=16),
          text = element_text(size=16)) 

vabund.stand

## ***********************************************************************
## mean canopy and bee diversity 
## ***********************************************************************
## different intercepts for thinned/unthinned
## thinned

bdiv <-
    hurdle.cond.effects[["BeeDiversity.BeeDiversity_MeanCanopy:ThinStatus"]]

bdiv.stand <- ggplot(bdiv, aes(x = MeanCanopy,
                                      y = estimate__)) +
    geom_line(aes(x = MeanCanopy, y=estimate__ ,
                  color = ThinStatus,
                  linetype = ThinStatus)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, fill=ThinStatus,
                    alpha=0.3)) +
    scale_fill_manual(values = c("darkolivegreen4", "forestgreen"),
                      labels = c("Thinned", "Unthinned")) +
    scale_color_manual(values = c("black", gray(.4))) +
    geom_point(data=new.net,
               aes(x=MeanCanopy, y=BeeDiversity,
                   color = ThinStatus), cex=2) +
    geom_point(data=new.net,
               aes(x=MeanCanopy, y=BeeDiversity), cex=2, pch=1) +
    labs(x = "Canopy openness", y = "Bee diversity (log)",
         fill = "Credible interval") +
    theme_ms() +
    theme(legend.position = "none") +
    scale_x_continuous(
        breaks = axis.canopy,
        labels =  labs.canopy) +
    scale_y_continuous(
        labels = labs.bdiv,
        breaks = axis.bdiv) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size=16),
          text = element_text(size=16)) 

bdiv.stand

## ***********************************************************************
## mean canopy and bee abundance
## ***********************************************************************

babund <-
    hutdle.cond.effects[["BeeAbundance.BeeAbundance_MeanCanopy:ThinStatus"]]

babund.stand <- ggplot(babund, aes(x = MeanCanopy,
                                      y = estimate__)) +
    geom_line(aes(x = MeanCanopy, y=estimate__ ,
                  color = ThinStatus,
                  linetype = ThinStatus)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, fill=ThinStatus,
                    alpha=0.3)) +
    scale_fill_manual(values = c("darkolivegreen4", "forestgreen"),
                      labels = c("Thinned", "Unthinned")) +
    scale_color_manual(values = c("black", gray(.4))) +
    geom_point(data=new.net,
               aes(x=MeanCanopy, y=BeeAbundance,
                   color = ThinStatus), cex=2) +
    geom_point(data=new.net,
               aes(x=MeanCanopy, y=BeeAbundance), cex=2, pch=1) +
    labs(x = "Canopy openness", y = "Bee abundance (log)",
         fill = "Credible interval") +
    theme_ms() +
    theme(legend.position = "none") +
    scale_x_continuous(
        breaks = axis.canopy,
        labels =  labs.canopy) +
    lims(y=c(0, max(axis.babund)))+
    scale_y_continuous(
        labels = labs.babund,
        breaks = axis.babund) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size=16),
          text = element_text(size=16)) 

babund.stand

all.canopy <- grid.arrange(vabund.stand, vdiv.stand, babund.stand, bdiv.stand,
             nrow=2)
ggsave(all.canopy, file="figures/all_canopy.pdf", height=7, width=8)

## ***********************************************************************
## veg diversity and bee diversity
## ***********************************************************************

babund.fabund <-
    hurdle.cond.effects[["BeeAbundance.BeeAbundance_VegAbundance"]]

babund.fabund.p <- ggplot(babund.fabund, aes(x = VegAbundance,
                                   y = estimate__)) +
    geom_line(aes(x = VegAbundance, y=estimate__ )) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__,
                    alpha=0.3)) +
    scale_fill_manual(values = c("dodgerblue")) +
    scale_color_manual(values = c("black", gray(.4))) +
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
    hurdle.cond.effects[["HasCrithidia.HasCrithidia_ForageDist_km"]]

crithidia.foraging.dist.p <-
    ggplot(crithidia.foraging.dist, aes(x = ForageDist_km,
                                        y = estimate__)) +
    geom_line(aes(x = ForageDist_km, y=estimate__ )) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__,
                    alpha=0.3)) +
    scale_fill_manual(values = c("dodgerblue")) +
    scale_color_manual(values = c("black", gray(.4))) +
    geom_point(data=new.net,
               aes(x=ForageDist_km, y=HasCrithidia,
                   color = ThinStatus), cex=2) +
    geom_point(data=new.net,
               aes(x=ForageDist_km, y=HasCrithidia), cex=2, pch=1) +
    labs(x = "Foraging distance (km)", y = "Crithidia prevalence",
         fill = "Credible interval") +
    theme_ms() +
    theme(legend.position = "none") +
    scale_x_continuous(
        breaks = axis.forage.dist,
        labels =  labs.forage.dist) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size=16),
          text = element_text(size=16)) 

crithidia.foraging.dist.p

## ***********************************************************************
## parasitism ~ diet breadth
## ***********************************************************************

newdata.diet <- tidyr::crossing(rare.degree =
                                seq(min(data.par$rare.degree, na.rm=TRUE),
                                    max(data.par$rare.degree, na.rm=TRUE),
                                    length.out=10),
                              Stand="100:Camp",
                              Year = "2020",
                              #TempCStart = mean(data.par$TempCStart),
                              VegDiversity = mean(data.par$VegDiversity),
                              BeeAbundance = mean(data.par$BeeAbundance),
                              ForageDist_km = mean(data.par$ForageDist_km),
                              BeeDiversity = mean(data.par$BeeDiversity),
                              Weights = 1,
                              WeightsPar = 1
                              
)

pred_diet <- fit.bombus %>%
  epred_draws(newdata = newdata.diet,
              resp = "HasCrithidia",
              allow_new_levels = TRUE)


parasite.diet <- ggplot(pred_diet, aes(x = rare.degree, y =
                                     .epred)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "Estimated diet breadth", y = "Stand *Crithidia* rate",
       fill = "Credible interval") +
  theme(legend.position = "bottom")  +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        text = element_text(size=16)) +
  theme_ms() +
  theme(axis.title.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown()) +
  geom_point(data=data.par,
             aes(y=SpStandCrithidiaRate, x=rare.degree), cex=2)

parasite.diet

