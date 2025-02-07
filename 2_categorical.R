#setwd("/coast_bees_parasites")

rm(list=ls())


source("src/ggplotThemes.R")
source("src/init.R")
source("src/misc.R")

spec.net <- spec.net %>%
  filter(Order == "Hymenoptera")
# 
# unique_bee <- beetable %>%
#   group_by(PlantGenusSpecies) %>%
#   summarise(count = n()) %>%
#   arrange(desc(count))

## ***********************************************************************
## stand type boxplots
## ***********************************************************************
spec.net <- spec.net[!is.na(spec.net$categories),]
u_stand <- unique(spec.net[, c("Stand", "Round", "Year", 
                               "VegAbundance", "VegDiversity",
                               "BeeAbundance", "BeeDiversity")])
u_stand <- u_stand %>%
  left_join(spec.net %>%
              select("Stand", "categories") %>%
              distinct(), by = "Stand")

VAbox <- ggplot(u_stand, aes(x = categories, y = log(VegAbundance))) +
  geom_boxplot() +
  labs(x = "Stand type", 
       y = "Flowering plant abundance (log)") +
  theme_classic()

VDbox <- ggplot(u_stand, aes(x = categories, y = VegDiversity)) +
  geom_boxplot() +
  labs(x = "Stand type", 
       y = "Flowering plant diversity") +
  theme_classic()

BAbox <- ggplot(u_stand, aes(x = categories, y = BeeAbundance)) +
  geom_boxplot() +
  labs(x = "Stand type", 
       y = "Bee abundance") +
  theme_classic()

BDbox <- ggplot(u_stand, aes(x = categories, y = BeeDiversity)) +
  geom_boxplot() +
  labs(x = "Stand type", 
       y = "Bee diversity") +
  theme_classic()

all.box <- ggarrange(VAbox, VDbox, BAbox, BDbox,
                     nrow=2, ncol=2, labels = c("A", "B", "C", "D"),
                     common.legend = TRUE, legend="bottom")

all.box

ggsave(all.box, file="figures/boxplots.pdf", height=7, width=10)

##statistics to report
vegstats <- u_stand %>%
  group_by(categories) %>%
  summarise(meanveg =mean(VegDiversity),
            sdveg = sd(VegDiversity))
vegstats2 <- u_stand %>%
  group_by(categories) %>%
  summarise(meanveg =mean(VegAbundance),
            sdveg = sd(VegAbundance))

## bee diversity summary by canopy type
beestats <- u_stand %>%
  group_by(categories) %>%
  summarise(meanbee =mean(BeeAbundance),
            sdbee = sd(BeeAbundance))
beestats2 <- u_stand %>%
  group_by(categories) %>%
  summarise(meanbee =mean(BeeDiversity),
            sdbee = sd(BeeDiversity))

## ***********************************************************************
## all bee genus bar graph
## ***********************************************************************
## summarize individuals per n type of stand in each category
standstype <- spec.net %>%
  group_by(categories) %>%
  summarise(count = n_distinct(Stand))

beesper <- spec.net %>%
  group_by(categories, Genus) %>%
  summarise(count = n()) %>%
  filter(!is.na(Genus))

summary <- beesper %>%
  left_join(standstype, by="categories") %>%
  mutate(avg = count.x/count.y)

## modifying to show prop of total
bee.spp.bar <- ggplot(summary,
                      aes(y = fct_reorder(Genus, avg, .desc = TRUE),
                          x = avg)) +
  geom_bar(stat = 'identity',
           aes(fill = factor(categories)), 
           position = position_dodge(preserve = "single")) +
  scale_fill_manual(values=c('lightblue', '#FFC107', 'darkorange', '#004D40'),
                    name = "Canopy type",
                    labels=c('Recent harvest', 'Younger thin', 
                             'Older thin', 'Mature')) +
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
  group_by(categories) %>%
  summarise(count = n_distinct(Stand))

plantper <- spec.net %>%
  group_by(categories, PlantGenusSpecies) %>%
  summarise(count = n()) %>%
  filter(!is.na(PlantGenusSpecies))

summary <- plantper %>%
  left_join(standstype, by="categories") %>%
  mutate(avg = count.x/count.y) 

summary <- subset(summary, summary$count.x>2) 

## modifying to show prop of total
plant.bar <- ggplot(summary,
                    aes(y = fct_reorder(PlantGenusSpecies, avg, .desc = TRUE),
                        x = avg)) +
  geom_bar(stat = 'identity',
           aes(fill = factor(categories)), 
           position = position_dodge(preserve = "single")) +
  scale_fill_manual(values=c('lightblue', '#FFC107', 'darkorange', '#004D40'),
                    name = "Canopy type",
                    labels=c('Recent harvest', 'Younger thin', 
                             'Older thin', 'Mature')) +
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

#still as canopybin and not standcat
bomb.orig <- spec.net %>%
  filter(Genus == 'Bombus')

beesper <- bomb.orig %>%
  group_by(categories, GenusSpecies) %>%
  summarise(count = n())

bombsummary <- beesper %>%
  left_join(standstype, by="categories") %>%
  mutate(avg = count.x/count.y)

#modifying to show prop of total
bombus.spp.bar <- ggplot(bombsummary,
                         aes(y = fct_reorder(GenusSpecies, avg, .desc = TRUE),
                             x = avg)) +
  geom_bar(stat = 'identity',
           aes(fill = factor(categories)), 
           position = position_dodge(preserve = "single"),
           show.legend = F) + 
  scale_fill_manual(values=c('lightblue', '#FFC107', 'darkorange', '#004D40')) +
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
         CrithidiaSpp, NosemaBombi, NosemaCeranae, #ParasitePresence,
         #ParasiteRichness,
         categories) %>%
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
       height=9, width=14)