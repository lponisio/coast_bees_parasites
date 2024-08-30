
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

##summarize flowers per n type of stand in each category
##find stands per stand type 
# standstype <- spec.orig %>% 
#   group_by(age, PlantGenusSpecies) %>% 
#   summarise(count = n())
# 
##bees per stand type
# beesper <- spec.orig %>% 
#   group_by(age, GenusSpecies) %>% 
#   summarise(count = n())

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


#plants per stand type
# plantsper <- spec.orig %>% 
#   group_by(CanopyBin, PlantGenusSpecies) %>% 
#   summarise(count = n())
# 
# #add total stands in each stand type, + divide 
# summary <- plantsper %>% 
#   left_join(standstype, by="CanopyBin") %>% 
#   mutate(avg = count.x/count.y) 
# 
# plant.spp.bar <- ggplot(summary, 
#                          aes(y = fct_reorder(PlantGenusSpecies, avg, .desc = TRUE),
#                              x = avg)) + 
#   geom_bar(stat = 'identity',
#            aes(fill = factor(CanopyBin)), position = "dodge") +
#   theme_classic() +
#   theme(axis.text.y = element_text(angle = 0, hjust = 1, 
#                                    face ='italic', color = 'black'),
#         axis.title.y = element_text(size=14),
#         axis.title.x = element_text(size=14),
#         text = element_text(size=14)) +
#   labs(y=expression(paste('Species')), x='Average individuals 
#        collected per stand type')
# 
# plant.spp.bar
# 
# 
# #total bees collected (not per stand)
# bombus.tot.bar <- ggplot(spec.orig, aes(y = fct_infreq(GenusSpecies))) + 
#   geom_bar(stat = 'count',
#            aes(fill = factor(age)), position = "dodge") +
#   scale_fill_manual(values=c('darkorange4', 'darkorange', 'gold')) +
#   theme_classic() +
#   theme(axis.text.y = element_text(angle = 0, hjust = 1, 
#                                    face ='italic', color = 'black'),
#         axis.title.y = element_text(size=16),
#         axis.title.x = element_text(size=16),
#         text = element_text(size=16)) +
#   labs(y=expression(paste('Species')), x='Number of \n Collected Individuals') +
#   geom_text(stat='count', aes(label=..count..), hjust=-0.5) + 
#   xlim(0, 100)
# 
# bombus.tot.bar
# 
# ggsave(bombus.tot.bar, file="figures/bombusTotbar.pdf",
#        height=5, width=7.5)

##parasite counts
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

## ***********************************************************************
## mean canopy ~ floral diversity
## ***********************************************************************

newdata.fl.div <- tidyr::crossing(MeanCanopy =
                                    seq(min(new.net$MeanCanopy, na.rm = TRUE),
                                        max(new.net$MeanCanopy, na.rm = TRUE),
                                        length.out=10),
                                  Stand="100:Camp",
                                  Year = "2021",
                                  ThinStatus = "Y",
                                  DoyStart = mean(new.net$DoyStart, na.rm = TRUE),
                                  Weights = 1,
                                  WeightsPar = 1
)

pred_fldiv <- fit.bombus %>%
  epred_draws(newdata = newdata.fl.div,
              resp = "VegDiversity",
              allow_new_levels = TRUE)

labs.canopy <- (pretty(c(0, new.orig$MeanCanopy), n=8))
axis.canopy <- (pretty(c(0, new.orig$MeanCanopy), n=8))

y.lab.fl.div <- (pretty(exp(new.orig$VegDiversity), n=5))
y.axis.fl.div <- standardize.axis(y.lab.fl.div,
                                 exp(new.orig$VegDiversity))


veg.div.stand <- ggplot(pred_fldiv, aes(x = MeanCanopy,
                                        y = .epred)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Greys") +
  labs(y = "Flowering plant diversity", x = "Canopy openness",
       fill = "Credible interval") +
  theme(legend.position = "bottom")  +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        text = element_text(size=16)) +
  theme_ms() +
  geom_point(data=new.net,
             aes(x=MeanCanopy, y=VegDiversity, color = ThinStatus), cex=2) +
  scale_color_manual(values=c("#000000","#999999")) +
  scale_x_continuous(
    labels = labs.canopy,
    breaks = axis.canopy) +
  scale_y_continuous(
    labels = y.lab.fl.div,
    breaks = y.axis.fl.div) 

veg.div.stand

ggsave(veg.div.stand, file="figures/vegdiv_stand.pdf",
       height=4, width=5)

## ***********************************************************************
## mean canopy and floral abundance
## ***********************************************************************

labs.canopy <- (pretty(c(0, new.orig$MeanCanopy), n=8))
axis.canopy <-  (pretty(c(0, new.orig$MeanCanopy), n=8))

y.labs.fl.ab <- (pretty(exp(new.orig$VegAbundance), n=10))
y.axis.fl.ab <- standardize.axis(y.labs.fl.ab,
                                 exp(new.orig$VegAbundance))

newdata.fl.ab <- tidyr::crossing(MeanCanopy =
                                   seq(min(new.net$MeanCanopy, na.rm=TRUE),
                                       max(new.net$MeanCanopy, na.rm=TRUE),
                                       length.out=10),
                                 Stand="100:Camp",
                                 Year = "2021",
                                 DoyStart = mean(new.net$DoyStart, na.rm=TRUE),
                                 Weights = 1, 
                                 WeightsPar = 1
)

pred_flab <- fit.bombus %>%
  epred_draws(newdata = newdata.fl.ab,
              resp = "VegAbundance",
              allow_new_levels = TRUE)

flower.ab.stand <- ggplot(pred_flab, aes(x = MeanCanopy,
                                         y = .epred)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Blues") +
  labs(y = str_wrap("Flowering plant abundance", width =20),
       x = "Canopy openness",
       fill = "Credible interval") +
  theme(legend.position = "bottom")  +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        text = element_text(size=16)) +
  theme_ms() +
  geom_point(data=new.net,
             aes(x=MeanCanopy, y=VegAbundance, color = ThinStatus), cex=2) +
  scale_color_manual(values=c("#000000","#999999")) +
  scale_x_continuous(
    breaks = axis.canopy,
    labels = labs.canopy) +
  scale_y_continuous(
    breaks = y.axis.fl.ab,
    labels = y.labs.fl.ab)

flower.ab.stand

ggsave(flower.ab.stand, file="figures/vegabund_stand.pdf",
       height=4, width=5)

## ***********************************************************************
## mean canopy and bee diversity
## ***********************************************************************

labs.canopy <- (pretty(c(0, new.orig$MeanCanopy), n=8))
axis.canopy <-  (pretty(c(0, new.orig$MeanCanopy), n=8))

y.labs.bdiv <- (pretty(c(0, new.orig$BeeDiversity), n=10))
y.axis.bdiv <- standardize.axis(y.labs.bdiv,
                                 new.orig$BeeDiversity)

newdata.bee.div <- tidyr::crossing(MeanCanopy =
                                     seq(min(new.net$MeanCanopy, na.rm=TRUE),
                                         max(new.net$MeanCanopy, na.rm=TRUE),
                                         length.out=10),
                                   Stand="100:Camp",
                                   Year = "2021",
                                   DoyStart = mean(new.net$DoyStart, na.rm=TRUE),
                                   Weights = 1, 
                                   WeightsPar = 1,
                                   VegAbundance = mean(new.net$VegAbundance, na.rm = TRUE),
                                   TempCStart = mean(new.net$TempCStart, na.rm = TRUE),
                                   VegDiversity = mean(new.net$VegDiversity, na.rm = TRUE)
)

pred_beediv <- fit.bombus %>%
  epred_draws(newdata = newdata.bee.div,
              resp = "BeeDiversity",
              allow_new_levels = TRUE)

bee.div.stand <- ggplot(pred_beediv, aes(x = MeanCanopy,
                                         y = .epred)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Blues") +
  scale_x_continuous(
    breaks = axis.canopy,
    labels = labs.canopy) +
  scale_y_continuous(
    breaks = y.axis.bdiv,
    labels = y.labs.bdiv) +
  labs(y = str_wrap("Bee Diversity", width =20),
       x = "Canopy openness",
       fill = "Credible interval") +
  theme(legend.position = "bottom")  +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        text = element_text(size=16)) +
  theme_ms() +
  geom_point(data=new.net,
             aes(x=MeanCanopy, y=BeeDiversity, color = ThinStatus), cex=2) +
  xlim(0,96) +
  scale_color_manual(values=c("#000000","#999999"))

bee.div.stand

ggsave(bee.div.stand, file="figures/beediv_canopy.pdf",
       height=4, width=5)

## ***********************************************************************
## mean canopy and bee abundance
## ***********************************************************************

labs.canopy <- (pretty(c(0, new.orig$MeanCanopy), n=8))
axis.canopy <-  (pretty(c(0, new.orig$MeanCanopy), n=8))

y.labs.fl.ab <- (pretty(c(0, new.orig$BeeAbundance), n=5))
y.axis.fl.ab <- standardize.axis(y.labs.fl.ab,
                                 new.orig$BeeAbundance)

newdata.bee.ab <- tidyr::crossing(MeanCanopy =
                                   seq(min(new.net$MeanCanopy, na.rm=TRUE),
                                       max(new.net$MeanCanopy, na.rm=TRUE),
                                       length.out=10),
                                 Stand="100:Camp",
                                 Year = "2021",
                                 DoyStart = mean(new.net$DoyStart, na.rm=TRUE),
                                 Weights = 1, 
                                 WeightsPar = 1,
                                 VegAbundance = mean(new.net$VegAbundance, na.rm = TRUE),
                                 TempCStart = mean(new.net$TempCStart, na.rm = TRUE)
)

pred_beeab <- fit.bombus %>%
  epred_draws(newdata = newdata.bee.ab,
              resp = "BeeAbundance",
              allow_new_levels = TRUE)

bee.ab.stand <- ggplot(pred_beeab, aes(x = MeanCanopy,
                                         y = .epred)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Blues") +
  scale_x_continuous(
    breaks = axis.canopy,
    labels = labs.canopy) +
  # scale_y_continuous(
  #   breaks = y.axis.fl.ab,
  #   labels = y.labs.fl.ab) +
  labs(y = str_wrap("Bee abundance", width =20),
       x = "Canopy openness",
       fill = "Credible interval") +
  theme(legend.position = "bottom")  +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        text = element_text(size=16)) +
  theme_ms() +
  geom_point(data=new.net,
             aes(x=MeanCanopy, y=BeeAbundance, color = ThinStatus), cex=2) +
  scale_color_manual(values=c("#000000","#999999")) 

bee.ab.stand

ggsave(bee.ab.stand, file="figures/beeabund_canopy.pdf",
       height=4, width=5)

## ***********************************************************************
## veg diversity and bee diversity
## ***********************************************************************

labs.bee.div <- (pretty(new.orig$VegDiversity, n=8))
axis.bee.div <-  standardize.axis(labs.bee.div,
                                  new.orig$VegDiversity)

newdata.beediv <- tidyr::crossing(VegDiversity =
                                    seq(min(new.net$VegDiversity, na.rm=TRUE),
                                        max(new.net$VegDiversity, na.rm=TRUE),
                                        length.out=10),
                                  FlowerRareRichness=mean(new.net$FlowerRareRichness, na.rm=TRUE),
                                  Stand="100:Camp",
                                  Year = "2020",
                                  ThinStatus = "Y",
                                  DoyStart = mean(new.net$DoyStart, na.rm=TRUE),
                                  VegAbundance = mean(new.net$VegAbundance, na.rm=TRUE),
                                  TempCStart = mean(new.net$TempCStart, na.rm=TRUE),
                                  ForageDist_km = mean(new.net$ForageDist_km, na.rm=TRUE),
                                  MeanCanopy = mean(new.net$MeanCanopy, na.rm=TRUE),
                                  Weights = 1,
                                  WeightsPar = 1
)

pred_beediv <- fit.bombus %>%
  epred_draws(newdata = newdata.beediv,
              resp = "BeeDiversity",
              allow_new_levels = TRUE)

bee.div.plant <- ggplot(pred_beediv, aes(x = VegDiversity,
                                             y = .epred)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = 'Greys') +
  labs(x = "Flowering plant diversity", y = "Bee diversity",
       fill = "Credible interval") +
  theme(legend.position = "bottom")  +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        text = element_text(size=16)) +
  theme_ms() +
  theme(axis.title.y = ggtext::element_markdown()) +
  geom_point(data=spec.net,
             aes(x=VegDiversity, y=BeeDiversity), cex=2) +
  scale_x_continuous(
    breaks = axis.bee.div,
    labels =  labs.bee.div)

bee.div.plant

ggsave(bee.div.plant, file="figures/beediv_veg.pdf",
       height=4, width=5)

## ***********************************************************************
## floral abundance on bee abundance
## ***********************************************************************

labs.bee.ab <- (pretty(c(0, new.orig$VegAbundance), n=8))
axis.bee.ab <- standardize.axis(labs.bee.ab,
                                    new.orig$VegAbundance)
                            
# y.labs.fl.ab <- (pretty(exp(new.orig$VegAbundance), n=10))
# y.axis.fl.ab <- standardize.axis(y.labs.fl.ab,
#                                  exp(new.orig$VegAbundance))


newdata.beeab <- tidyr::crossing(VegAbundance =
                                   seq(min(new.net$VegAbundance, na.rm=TRUE),
                                       max(new.net$VegAbundance, na.rm=TRUE),
                                       length.out=10),
                                 FlowerRareRichness=mean(new.net$FlowerRareRichness, na.rm=TRUE),
                                 Stand ="100:Camp",
                                 Year = "2020",
                                 ThinStatus = "Y",
                                 DoyStart = mean(new.net$DoyStart, na.rm=TRUE),
                                 ForageDist_km = mean(new.net$ForageDist_km, na.rm=TRUE),
                                 TempCStart = mean(new.net$TempCStart, na.rm=TRUE),
                                 VegDiversity = mean(new.net$VegDiversity, na.rm=TRUE),
                                 MeanCanopy = mean(new.net$MeanCanopy, na.rm=TRUE),
                                 Weights = 1,
                                 WeightsPar = 1
)

pred_beeab <- fit.bombus %>%
  epred_draws(newdata = newdata.beeab,
              resp = "BeeAbundance",
              allow_new_levels = TRUE)

bee.ab.plant <- ggplot(pred_beeab, aes(x = VegAbundance,
                                       y = .epred)) +
  stat_lineribbon() +
  scale_fill_brewer(palette="Blues") +
  labs(x = "Flowering plant abundance (log)", y = "Bee abundance",
       fill = "Credible interval") +
  theme(legend.position = "bottom")  +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        text = element_text(size=16)) +
  theme_ms() +
  theme(axis.title.y = ggtext::element_markdown()) +
  geom_point(data=new.net,
             aes(x=VegAbundance, y=BeeAbundance), cex=2) +
  scale_x_continuous(
    breaks = axis.bee.ab,
    labels = labs.bee.ab)

bee.ab.plant

ggsave(bee.ab.plant, file="figures/bombusabund_plant.pdf",
       height=4, width=5)
# 
# # ***********************************************************************
# # canopy and bee diversity
# # ***********************************************************************
# 
# labs.canopy <- (pretty(c(0, new.orig$MeanCanopy), n=8))
# axis.canopy <-  (pretty(c(0, new.orig$MeanCanopy), n=8))
# 
# newdata.beediv <- tidyr::crossing(MeanCanopy =
#                                     seq(min(new.net$MeanCanopy, na.rm=TRUE),
#                                         max(new.net$MeanCanopy, na.rm=TRUE),
#                                         length.out=10),
#                                   FlowerRareRichness=mean(new.net$FlowerRareRichness, na.rm=TRUE),
#                                   Stand="100:Camp",
#                                   Year = "2020",
#                                   ThinStatus = "Y",
#                                   DoyStart = mean(new.net$DoyStart, na.rm=TRUE),
#                                   VegAbundance = mean(new.net$VegAbundance, na.rm=TRUE),
#                                   VegDiversity = mean(new.net$VegDiversity, na.rm=TRUE),
#                                   TempCStart = mean(new.net$TempCStart, na.rm=TRUE),
#                                   ForageDist_km = mean(new.net$ForageDist_km, na.rm=TRUE),
#                                   Weights = 1,
#                                   WeightsPar = 1
# )
# 
# pred_beediv <- fit.bombus %>%
#   epred_draws(newdata = newdata.beediv,
#               resp = "BeeDiversity",
#               allow_new_levels = TRUE)
# 
# bee.div.canopy <- ggplot(pred_beediv, aes(x = MeanCanopy,
#                                              y = .epred)) +
#   stat_lineribbon() +
#   scale_fill_brewer(palette = 'Blues') +
#   labs(x = "Canopy openness", y = "Bee diversity",
#        fill = "Credible interval") +
#   theme(legend.position = "bottom")  +
#   theme(axis.title.x = element_text(size=16),
#         axis.title.y = element_text(size=16),
#         text = element_text(size=16)) +
#   theme_ms() +
#   theme(axis.title.y = ggtext::element_markdown()) +
#   geom_point(data=spec.net,
#              aes(x=MeanCanopy, y=BeeDiversity, color = ThinStatus), cex=2) +
#   scale_color_manual(values=c("#000000","#999999")) +
#   scale_x_continuous(
#     breaks = axis.canopy,
#     labels =  labs.canopy)
# 
# bee.div.canopy
# 
# ggsave(bee.div.canopy, file="figures/beerich_stand.pdf",
#        height=4, width=5)
# 
# # ***********************************************************************
# # canopy and bee abundance
# # ***********************************************************************
# 
# # labs.bee.ab <- (pretty(c(0, new.orig$MeanCanopy), n=8))
# # axis.bee.ab <-  standardize.axis(labs.bee.ab,
# #                                  new.orig$MeanCanopy)
# 
# labs.canopy <- (pretty(c(0, new.orig$MeanCanopy), n=8))
# axis.canopy <- (pretty(c(0, new.orig$MeanCanopy), n=8))
# 
# newdata.beeab <- tidyr::crossing(MeanCanopy =
#                                    seq(min(new.net$MeanCanopy, na.rm=TRUE),
#                                        max(new.net$MeanCanopy, na.rm=TRUE),
#                                        length.out=10),
#                                  FlowerRareRichness=mean(new.net$FlowerRareRichness, na.rm=TRUE),
#                                  Stand ="100:Camp",
#                                  Year = "2020",
#                                  ThinStatus = "Y",
#                                  DoyStart = mean(new.net$DoyStart, na.rm=TRUE),
#                                  VegAbundance = mean(new.net$VegAbundance, na.rm=TRUE),
#                                  ForageDist_km = mean(new.net$ForageDist_km, na.rm=TRUE),
#                                  TempCStart = mean(new.net$TempCStart, na.rm=TRUE),
#                                  VegDiversity = mean(new.net$VegDiversity, na.rm=TRUE),
#                                  Weights = 1,
#                                  WeightsPar = 1
# )
# 
# pred_beeab <- fit.bombus %>%
#   epred_draws(newdata = newdata.beeab,
#               resp = "BeeAbundance",
#               allow_new_levels = TRUE)
# 
# bee.ab.canopy <- ggplot(pred_beeab, aes(x = MeanCanopy,
#                                            y = .epred)) +
#   stat_lineribbon() +
#   scale_fill_brewer(palette="Blues") +
#   labs(x = "Canopy openness", y = "Bee abundance",
#        fill = "Credible interval") +
#   theme(legend.position = "bottom")  +
#   theme(axis.title.x = element_text(size=16),
#         axis.title.y = element_text(size=16),
#         text = element_text(size=16)) +
#   theme_ms() +
#   theme(axis.title.y = ggtext::element_markdown()) +
#   geom_point(data=new.net,
#              aes(x=MeanCanopy, y=BeeAbundance, color = ThinStatus), cex=2) +
#   scale_color_manual(values=c("#000000","#999999")) +
#   scale_x_continuous(
#     breaks = axis.canopy,
#     labels = labs.canopy)
# 
# bee.ab.canopy
# 
# ggsave(bee.ab.canopy, file="figures/beeabund_stand.pdf",
#        height=4, width=5)

## ***********************************************************************
## veg diversity and doy
## ***********************************************************************
# 
# labs.doy <- (pretty(new.orig$DoyStart, n=8))
# axis.doy <-  standardize.axis(labs.doy,
#                               new.orig$DoyStart)
# 
# newdata.vegdiv <- tidyr::crossing(DoyStart =
#                                       seq(min(new.net$DoyStart, na.rm=TRUE),
#                                           max(new.net$DoyStart, na.rm=TRUE),
#                                           length.out=10),
#                                     Year = new.net$Year,
#                                     ThinStatus = (new.net$ThinStatus),
#                                     MeanCanopy = (new.net$MeanCanopy),
#                                     Weights = 1,
#                                     WeightsPar = 1
# )
# 
# pred_vegdiv <- fit.bombus %>%
#   epred_draws(newdata = newdata.vegdiv,
#               resp = "VegDiversity",
#               allow_new_levels = TRUE)
# 
# doy.veg.div <- ggplot(pred_vegdiv, aes(x = DoyStart, y =
#                                            .epred)) +
#   stat_lineribbon() +
#   scale_fill_brewer(palette = "Blues") +
#   labs(y = str_wrap("Flowering plant diversity (log)", width =20),
#        x = "Day of year",
#        fill = "Credible interval") +
#   theme(legend.position = "bottom") +
#   scale_x_continuous(
#     breaks = axis.doy,
#     labels =  labs.doy) +
#   theme(axis.title.x = element_text(size=16),
#         axis.title.y = element_text(size=16),
#         text = element_text(size=16)) +
#   theme_ms() +
#   geom_point(data=new.net,
#              aes(y=VegDiversity, x=DoyStart), cex=2)
# 
# doy.veg.div

# ggsave(doy.veg.div, file="figures/vegDiv_doy.pdf",
#        height=4, width=5)

## ***********************************************************************
## temp and bee richness 
## ***********************************************************************
# 
# labs.bee.div <- (pretty(new.orig$TempCStart, n=8))
# axis.bee.div <-  standardize.axis(labs.bee.div,
#                                   new.orig$TempCStart)
# 
# newdata.beediv <- tidyr::crossing(TempCStart =
#                                     seq(min(new.net$TempCStart, na.rm=TRUE),
#                                         max(new.net$TempCStart, na.rm=TRUE),
#                                         length.out=10),
#                                   FlowerRareRichness=mean(new.net$FlowerRareRichness, na.rm=TRUE),
#                                   Stand="100:Camp",
#                                   Year = "2020",
#                                   ThinStatus = "Y",
#                                   DoyStart = mean(new.net$DoyStart, na.rm=TRUE),
#                                   VegAbundance = mean(new.net$VegAbundance, na.rm=TRUE),
#                                   VegDiversity = mean(new.net$VegDiversity, na.rm=TRUE),
#                                   ForageDist_km = mean(new.net$ForageDist_km, na.rm=TRUE)
# )
# 
# pred_beediv <- fit.bombus %>%
#   epred_draws(newdata = newdata.beediv,
#               resp = "BeeDiversity",
#               allow_new_levels = TRUE)
# 
# bee.div.temp <- ggplot(pred_beediv, aes(x = TempCStart,
#                                              y = .epred)) +
#   stat_lineribbon() +
#   scale_fill_brewer(palette = 'Blues') +
#   labs(fill = "Credible interval") +
#   xlab("Survey starting temperature (\u00b0C)") +
#   ylab("Rarefied *Bombus* <br> species richness") +
#   theme(legend.position = "bottom")  +
#   theme(axis.title.x = element_text(size=16),
#         axis.title.y = element_text(size=16),
#         text = element_text(size=16)) +
#   theme_ms() +
#   theme(axis.title.y = ggtext::element_markdown()) +
#   geom_point(data=spec.net,
#              aes(x=TempCStart, y=BombusRareRichness), cex=2) +
#   scale_x_continuous(
#     breaks = axis.bee.div,
#     labels =  labs.bee.div)
# 
# bombus.div.temp
# 
# ggsave(bombus.div.temp, file="figures/bombusrich_temp.pdf",
#        height=4, width=5)

## ***********************************************************************
## bombus abundance ~ temp
## ***********************************************************************
# 
# labs.bee.ab <- (pretty(c(0, new.orig$TempCStart), n=8))
# axis.bee.ab <-  standardize.axis(labs.bee.ab,
#                                  new.orig$TempCStart)
# 
# newdata.beeab <- tidyr::crossing(TempCStart =
#                                    seq(min(new.net$TempCStart, na.rm=TRUE),
#                                        max(new.net$TempCStart, na.rm=TRUE),
#                                        length.out=10),
#                                  FlowerRareRichness=mean(new.net$FlowerRareRichness, na.rm=TRUE),
#                                  Stand ="100:Camp",
#                                  Year = "2020",
#                                  ThinStatus = "Y",
#                                  DoyStart = mean(new.net$DoyStart, na.rm=TRUE),
#                                  VegAbundance = mean(new.net$VegAbundance, na.rm=TRUE),
#                                  ForageDist_km = mean(new.net$ForageDist_km, na.rm=TRUE),
#                                  VegDiversity = mean(new.net$VegDiversity, na.rm=TRUE)
# )
# 
# pred_beeab <- fit.bombus %>%
#   epred_draws(newdata = newdata.beeab,
#               resp = "BombusAbundance",
#               allow_new_levels = TRUE)
# 
# bombus.ab.temp <- ggplot(pred_beeab, aes(x = TempCStart, 
#                                            y = .epred)) +
#   stat_lineribbon() +
#   scale_fill_brewer(palette="Blues") +
#   labs(x = "Survey starting temperature (\u00b0C)", 
#        y = "*Bombus* abundance",
#        fill = "Credible interval") +
#   theme(legend.position = "bottom")  +
#   theme(axis.title.x = element_text(size=16),
#         axis.title.y = element_text(size=16),
#         text = element_text(size=16)) +
#   theme_ms() +
#   theme(axis.title.y = ggtext::element_markdown()) +
#   geom_point(data=new.net,
#              aes(x=TempCStart, y=BombusAbundance), cex=2) +
#   scale_x_continuous(
#     breaks = axis.bee.ab,
#     labels = labs.bee.ab) 
# 
# bombus.ab.temp
# 
# ggsave(bombus.ab.temp, file="figures/bombusabund_temp.pdf",
#        height=4, width=5)

## ***********************************************************************
## bee community diversity and abundance and parasitism
## ***********************************************************************

## ***********************************************************************
## parasitism ~ bombus diversity
## ***********************************************************************

labs.bee.div <- (pretty(c(0, spec.orig$BeeDiversity), n=8))
axis.bee.div <-  standardize.axis(labs.bee.div,
                                  spec.net$BeeDiversity)

data.par <- spec.net[spec.net$WeightsPar == 1, ]

newdata.beediv <- tidyr::crossing(BeeDiversity =
                                    seq(min(data.par$BeeDiversity),
                                        max(data.par$BeeDiversity),
                                        length.out=10),
                                  Stand="100:Camp",
                                  Year = "2021",
                                  VegDiversity = mean(data.par$VegDiversity),
                                  BeeAbundance = mean(data.par$BeeAbundance),
                                  ForageDist_km = mean(data.par$ForageDist_km),
                                  rare.degree = mean(data.par$rare.degree, na.rm = TRUE), 
                                  Weights = 1,
                                  WeightsPar = 1
)

pred_beediv <- fit.bombus %>%
  epred_draws(newdata = newdata.beediv,
              resp = "HasCrithidia",
              allow_new_levels = TRUE)

bee.div.crithidia <- ggplot(pred_beediv, aes(x = BeeDiversity, y =
                                               .epred)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Greys") +
  labs(x = "Bee diversity", 
       y = "Stand *Crithidia* rate",
       fill = "Credible interval") +
  theme(legend.position = "bottom") +
  scale_x_continuous() +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        text = element_text(size=16)) +
  theme_ms() +
  theme(axis.title.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown()) +
  geom_point(data=data.par,
             aes(y=SpStandCrithidiaRate, x=BeeDiversity), cex=2)

bee.div.crithidia

ggsave(bee.div.crithidia, file="figures/parasite_beeDiv.pdf",
       height=4, width=5)

## ***********************************************************************
## parasitism ~ bee abundance
## ***********************************************************************

newdata.beeabund <- tidyr::crossing(BeeAbundance =
                                      seq(min(data.par$BeeAbundance),
                                          max(data.par$BeeAbundance),
                                          length.out=10),
                                    Stand="100:Camp",
                                    Year = "2020",
                                    TempCStart = mean(data.par$TempCStart),
                                    VegDiversity = mean(data.par$VegDiversity),
                                    ForageDist_km = mean(data.par$ForageDist_km),
                                    rare.degree = mean(data.par$rare.degree, na.rm = TRUE),
                                    BeeDiversity = mean(data.par$BeeDiversity),
                                    Weights = 1,
                                    WeightsPar = 1
                                    
)

pred_beeabund <- fit.bombus %>%
  epred_draws(newdata = newdata.beeabund ,
              resp = "HasCrithidia",
              allow_new_levels = TRUE)


bee.abund.parasite <- ggplot(pred_beeabund, aes(x = BeeAbundance, y =
                                                     .epred)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Greys") +
  labs(x = "Bee abundance", y = "Stand *Crithidia* rate",
       fill = "Credible interval") +
  theme(legend.position = "bottom")  +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        text = element_text(size=16)) +
  theme_ms() +
  theme(axis.title.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown()) +
  geom_point(data=data.par,
             aes(y=SpStandCrithidiaRate, x=BeeAbundance), cex=2)

bee.abund.parasite

ggsave(bombus.abund.parasite, file="figures/parasite_beeAbund.pdf",
       height=4, width=5)

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

#jfb check for redundancy in bee/canopy plots 

#bees and crithidia rate
scatter.4 <- grid.arrange(bee.div.crithidia,
                          bee.abund.parasite,
                          ncol=2)

ggsave(scatter.4, file="figures/CRdiseaseplots.pdf",
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

