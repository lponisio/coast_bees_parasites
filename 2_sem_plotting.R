
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
## prepping for newdata draws
## ***********************************************************************
## load model results and data
load(file="saved/CrithidiaFitAllBee_coast.Rdata")

## log + 1  Veg abundance, Bee diversity and bee abundance
## log "ForageDist_km", "rare.degree"

##Looking at metrics in thinned stands only now
spec.net <- spec.net %>%
  filter(ThinStatus != "N")
spec.orig <- spec.orig %>%
  filter(ThinStatus != "N")

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
    all.cond.effects[["VegDiversity.VegDiversity_MeanCanopy"]]

vdiv.stand <- ggplot(vdiv, aes(x = MeanCanopy,
                               y = estimate__)) +
    geom_line(aes(x = MeanCanopy, y=estimate__ 
                  #,
                  #color = ThinStatus, linetype = ThinStatus
                  ), size=1.5) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, 
                    #fill=ThinStatus,
                    alpha=0.3)) +
    #scale_fill_manual(values = c("darkolivegreen4", "forestgreen"),
     #                 labels = c("Unthinned", "thinned")) +
    scale_color_manual(values = c("black", "#999999")) +
    geom_point(data=new.net,
               aes(x=MeanCanopy, y=VegDiversity,
    #               color = ThinStatus
                    ), cex=2) +
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
    all.cond.effects[["VegAbundance.VegAbundance_MeanCanopy"]]

vabund.stand <- ggplot(vabund, aes(x = MeanCanopy,
                                   y = estimate__)) +
    geom_line(aes(x = MeanCanopy, y=estimate__),  size=1.5) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, 
                    alpha=0.3)) +
    scale_color_manual(values = c("black", "#999999")) +
    geom_point(data=new.net,
               aes(x=MeanCanopy, y=VegAbundance), cex=2) +
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
    all.cond.effects[["BeeDiversity.BeeDiversity_MeanCanopy"]]

bdiv.stand <- ggplot(bdiv, aes(x = MeanCanopy,
                               y = estimate__)) +
    geom_line(aes(x = MeanCanopy, y=estimate__), size=1.5) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, 
                    alpha=0.3)) +
    geom_point(data=new.net,
               aes(x=MeanCanopy, y=BeeDiversity), cex=2) +
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
    all.cond.effects[["BeeAbundance.BeeAbundance_MeanCanopy"]]

babund.stand <- ggplot(babund, aes(x = MeanCanopy,
                                   y = estimate__)) +
    geom_line(aes(x = MeanCanopy, y=estimate__, size=1.5)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, 
                    alpha=0.3)) +
    scale_color_manual(values = c("black", "#999999")) +
    geom_point(data=new.net,
               aes(x=MeanCanopy, y=BeeAbundance), cex=2) +
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
               aes(x=VegAbundance, y=BeeAbundance), cex=2) +
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
   labs(x =  expression("Foraging distance (log km)"),
   y = expression("" * italic("Crithidia") * " prevalence ")) +
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


# ***********************************************************************
# scatterplot of canopy cover and dbh, highlighting thins
# ***********************************************************************

stands <- spec.orig[!duplicated(spec.orig$StandRoundYear), ]

standsplot <- stands %>%
  ggplot(aes(x=DomTreeDiam_cm, y=MeanCanopy, color=ThinStatus)) +
  geom_point() +
  labs(x="Dominant tree class DBH (cm)", y="Canopy openness") +
  xlim(20,60) +
  theme_classic()+
  theme(
    axis.text.x = element_text(color="black"),
    axis.text.y = element_text(color="black"),
    axis.ticks = element_line(color = "black"))

standsplot

ggsave(standsplot, file="figures/canopyDBH.pdf",
       height=2, width=4)
