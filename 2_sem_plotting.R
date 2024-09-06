
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
  scale_color_manual(values = c("Y" = "gold", "N" = "maroon"),
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
  summarise(count = n())

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
                    name = "Canopy closure", 
                    labels=c('Closed', 'Intermediate', 'Open')) +
  theme_classic() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, 
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

summary <- beesper %>% 
  left_join(standstype, by="CanopyBin") %>% 
  mutate(avg = count.x/count.y) 

#modifying to show prop of total
bombus.spp.bar <- ggplot(summary, 
                         aes(y = fct_reorder(GenusSpecies, avg, .desc = TRUE),
                             x = avg)) + 
  geom_bar(stat = 'identity',
           aes(fill = factor(CanopyBin)), position = "dodge") +
  scale_fill_manual(values=c('#004D40', '#FFC107', 'lightblue'),
                    name = "Canopy closure", 
                    labels=c('Closed', 'Intermediate', 'Open')) +
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
           aes(fill = factor(CanopyBin)), position = "dodge") +
  scale_fill_manual(values=c('#004D40', '#FFC107', 'lightblue'),
                    name = "Canopy closure", 
                    labels=c('Closed', 'Intermediate', 'Open')) +
  theme_classic() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, color = "black"),
        axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16),
        axis.text.x = element_text(color = "black"),
        text = element_text(size=16)) +
  labs(y='Parasite', x='Number of \n Infected Individuals') +
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
y.labs.fdiv <- (pretty(new.orig$VegDiversity, n=5))
y.axis.fdiv <- standardize.axis(y.labs.fdiv,
                                 new.orig$VegDiversity)

## floral abundance
y.labs.fabund <- pretty(new.orig$VegAbundance, n=10)
y.axis.fabund <- standardize.axis(y.labs.fabund,
                                 new.orig$VegAbundance)

## bee diversity
y.labs.bdiv <- pretty(new.orig$BeeDiversity, n=10)
y.axis.bdiv <- standardize.axis(y.labs.bdiv,
                                 new.orig$BeeDiversity)

## bee abund
y.labs.babund <- pretty(new.orig$BeeAbundance, n=10)
y.axis.babund <- standardize.axis(y.labs.babund,
                                 new.orig$BeeAbundance)


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
        labels = y.labs.fdiv,
        breaks = y.axis.fdiv) +
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
        labels = y.labs.fabund,
        breaks = y.axis.fabund) +
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
        labels = y.labs.bdiv,
        breaks = y.axis.bdiv) +
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
    lims(y=c(0, max(y.axis.babund)))+
    scale_y_continuous(
        labels = y.labs.babund,
        breaks = y.axis.babund) +
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
        breaks = y.axis.fabund,
        labels =  y.labs.fabund) +
    scale_y_continuous(
        labels = y.labs.babund,
        breaks = y.axis.babund) +
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

newdata.fd <- tidyr::crossing(ForageDist_km =
                                      seq(min(data.par$ForageDist_km),
                                          max(data.par$ForageDist_km),
                                          length.out=10),
                                    Stand="100:Camp",
                                    Year = "2020",
                                    TempCStart = mean(data.par$TempCStart),
                                    VegDiversity = mean(data.par$VegDiversity),
                                    BeeAbundance = mean(data.par$BeeAbundance),
                                    rare.degree = mean(data.par$rare.degree, na.rm = TRUE),
                                    BeeDiversity = mean(data.par$BeeDiversity),
                                    Weights = 1,
                                    WeightsPar = 1
                                    
)

pred_fd <- fit.bombus %>%
  epred_draws(newdata = newdata.fd,
              resp = "HasCrithidia",
              allow_new_levels = TRUE)


parasite.fd <- ggplot(pred_fd, aes(x = ForageDist_km, y =
                                                  .epred)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "Foraging distance (km)", y = "Stand *Crithidia* rate",
       fill = "Credible interval") +
  theme(legend.position = "bottom")  +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        text = element_text(size=16)) +
  theme_ms() +
  theme(axis.title.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown()) +
  geom_point(data=data.par,
             aes(y=SpStandCrithidiaRate, x=ForageDist_km), cex=2)

parasite.fd

ggsave(parasite.fd, file="figures/parasite_fd.pdf",
       height=4, width=5)

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

ggsave(parasite.diet, file="figures/parasite_diet.pdf",
       height=4, width=5)

## ***********************************************************************
#putting plots together

#canopy and veg 
scatter.1 <- grid.arrange(veg.div.stand,
                          flower.ab.stand,
                          ncol=2)

ggsave(scatter.1, file="figures/vegcanopy.pdf",
       height=3, width=9)


#canopy and bees
scatter.2 <- grid.arrange(bee.div.stand,
                          bee.ab.stand,
                          ncol=2)

ggsave(scatter.2, file="figures/beecanopy.pdf",
       height=3, width=9)

#veg div/bee div and veg ab/bee ab                    
scatter.3 <- grid.arrange(bee.div.plant,
                           bee.ab.plant,
                          ncol=2)

ggsave(scatter.3, file="figures/beevegplots.pdf",
       height=3, width=9)

#bees and crithidia rate
scatter.4 <- grid.arrange(bee.div.crithidia,
                          bee.abund.parasite,
                          ncol=2)

ggsave(scatter.4, file="figures/CRdiseaseplots.pdf",
       height=3, width=9)

#crithidia and distance/diet
scatter.5 <- grid.arrange(parasite.fd,
                          parasite.diet,
                          ncol=2)

ggsave(scatter.5, file="figures/CRdietforage.pdf",
       height=3, width=9)

# #############
# Deprecated parasite plots:  
# 
# labs.veg.div <- (pretty(spec.net$FlowerRareRichness, n=8))
# axis.veg.div <-  standardize.axis(labs.veg.div,
#                                   spec.net$FlowerRareRichness)
# 
# ## parasitism ~ floral div
# newdata.beeabund <- tidyr::crossing(FlowerRareRichness =
#                                       seq(min(data.par$FlowerRareRichness),
#                                           max(data.par$FlowerRareRichness),
#                                           length.out=10),
#                                     BombusDiversity=mean(data.par$BombusDiversity),
#                                     Stand="100:Camp",
#                                     Year = "2021",
#                                     DoyStart = mean(data.par$DoyStart),
#                                     VegAbundance = mean(data.par$VegAbundance),
#                                     VegDiversity = mean(data.par$VegDiversity),
#                                     BombusAbundance = mean(data.par$BombusAbundance),
#                                     BombusRareRichness = mean(data.par$BombusRareRichness),
#                                     ForageDist_km = mean(data.par$ForageDist_km)
# )
# 
# pred_beeabund <- fit.bombus %>%
#   epred_draws(newdata = newdata.beeabund ,
#               resp = "HasCrithidia",
#               allow_new_levels = TRUE)
# 
# 
# veg.div.parasite <- ggplot(pred_beeabund, aes(x = FlowerRareRichness, y =
#                                                 .epred)) +
#   stat_lineribbon() +
#   scale_fill_brewer(palette = "Reds") +
#   labs(x = "Veg diversity", y = "Bombus Stand Parasitism Rate",
#        fill = "Credible interval") +
#   theme(legend.position = "bottom") +
#   scale_x_continuous(
#     breaks = axis.veg.div,
#     labels =  labs.veg.div) +
#   theme(axis.title.x = element_text(size=16),
#         axis.title.y = element_text(size=16),
#         text = element_text(size=16)) +
#   theme_ms() +
#   geom_point(data=data.par,
#              aes(y=BombusStandParasitismRate, x=FlowerRareRichness), cex=2)
# 
# veg.div.parasite
# 
# ggsave(veg.div.parasite, file="figures/parasite_vegDiv.pdf",
#        height=4, width=5)

############################

