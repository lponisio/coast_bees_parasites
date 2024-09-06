setwd("~/Dropbox (University of Oregon)/coast_bees_parasites")

## Prepares the data for model fitting (standardizes continuous
## variables, creates dummy variables to be used as weights to all
## different subsets of data to be used in different model levels),
## builds the models, and fits the models in brms. The model outputs
## are saved as tables, and chain diagnostic plots created.

rm(list=ls())

## set to the number of cores you would like the models to run on
ncores <- 3

load("data/spec_net_coast.Rdata")
source("src/init.R")
source("src/misc.R")
source("src/writeResultsTable.R")
source("src/runParasiteModels.R")
source("src/standardize_weights.R")
source("src/runPlotFreqModelDiagnostics.R")

## summaries of the data used
table(spec.net$Stand, spec.net$Year)
table(spec.net$Year)

## Load tree from : Henriquez Piskulich, Patricia Andrea; Hugall,
## Andrew F.; Stuart-Fox, Devi (2023).  A supermatrix phylogeny of the
## world’s bees (Hymenoptera: Anthophila) [Dataset].
## Dryad. https://doi.org/10.5061/dryad.80gb5mkw1
load("data/phylo.Rdata")

## parasite models only inlcude bombus and only species that were
## screened, so put NA for other species to avoid weird
## standardization
spec.net$rare.degree[spec.net$Genus != "Bombus"] <- NA
spec.net$MeanITD[spec.net$Genus != "Bombus"] <- NA

## **********************************************************
## formula for site effects on the bee community
## **********************************************************

## all of the variables that are explanatory variables and thus need
## to be centered. Because brms needs a single dataset, which must be
## at the indivudal-level for the parasite model, we need to carefully
## standardize the data at the correct level. 

## standardize by stand, year
vars_year <- c("MeanCanopy")

## standardize by stand, year, and sample round
vars_year_sr <- c(
    "DoyStart",
    "TempCStart",
    "VegAbundance",
    "VegDiversity")

## standardize by species
vars_sp <- c("ForageDist_km")

## standardize by stand, year, species
vars_sp_yearsr <- c("rare.degree")

## variables to log but add 1 first (due to zeros)
variables.to.log.p1 <- c(
    "VegAbundance",
    "BeeAbundance",
    "BeeDiversity"
)

## variables to log
variables.to.log <- c(
    "ForageDist_km",
    "rare.degree"
)

## create a dummy variable "Weight" to deal with the data sets being at
## different levels to get around the issue of having to pass in one
## data set into brms
## will need to think on the transect-site-stand stituation

## ********************************************************
## log variables here
## ********************************************************
dim(spec.net)
spec.orig <- prepDataSEM(spec.net, variables.to.log,
                         variables.to.log.p1,
                         standardize=FALSE)


## Make SEM weights and standardize data.
spec.net <- prepDataSEM(spec.net, variables.to.log, variables.to.log.p1, 
                        vars_yearsr=vars_year_sr,
                        vars_year=vars_year,
                        vars_sp=vars_sp,
                        vars_sp_yearsr=vars_sp_yearsr)

## **********************************************************
## Model 1.1: formula for forest effects on floral community
## **********************************************************
## define all the formulas for the different parts of the models

formula.flower.div <- formula(VegDiversity | subset(Weights) ~
                                  DoyStart + I(DoyStart^2) +
                                      MeanCanopy*ThinStatus +
                                      Year +
                                      (1|Stand) 
                              )

## flower abund with simpson div
formula.flower.abund <- formula(VegAbundance | subset(Weights) ~
                                    DoyStart +  I(DoyStart^2) +
                                        MeanCanopy*ThinStatus +
                                        ## I(MeanCanopy^2)*ThinStatus +
                                        Year +
                                        (1|Stand)
                                )

## **********************************************************
## Model 1.2: formula for forest effects on bee community
## **********************************************************

formula.bee.div <- formula(BeeDiversity | subset(Weights)~
                               VegDiversity +
                                   TempCStart +
                                   MeanCanopy*ThinStatus +
                                   Year +
                                   (1|Stand) 
                           )

formula.bee.abund <- formula(BeeAbundance | subset(Weights)~
                                 VegAbundance +
                                     TempCStart +
                                     MeanCanopy*ThinStatus +
                                     Year +
                                     (1|Stand)  
                             )



formula.bee.div.hu <- formula(hu ~
                                  VegDiversity +
                                  TempCStart +
                                  MeanCanopy +
                                  (1|Stand) 
                              )

formula.bee.abund.hu <- formula(hu ~
                                    VegAbundance +
                                    TempCStart +
                                    MeanCanopy +
                                    (1|Stand)  
                                )


## **********************************************************
## Model 1.3: formula for bee community effects on parasitism
## **********************************************************

xvars.coast <- c("BeeDiversity",
                 "BeeAbundance",
                 "VegDiversity",
                 "ForageDist_km",
                 "rare.degree",
                 "(1|Stand)",
                 "(1|gr(GenusSpecies, cov = phylo_matrix))"
                 )

## **********************************************************
## Crithidia 
## **********************************************************
## because we only screened bombus in the parasite models
spec.net$WeightsPar[spec.net$Genus != "Bombus"] <- 0

## summary stats of what is screened
sum(spec.net$WeightsPar)
table(spec.net$Year[spec.net$WeightsPar == 1])
parasites <- c("ApicystisSpp", "AscosphaeraSpp",
               "CrithidiaBombi", "CrithidiaExpoeki", "CrithidiaSpp",
               "NosemaBombi", "NosemaCeranae")
apply(spec.net[spec.net$WeightsPar == 1, parasites], 2, sum)

table(spec.net$ParasiteRichness[spec.net$WeightsPar == 1])

## the model will not run without having all the bee species in the
## phylogeny, enough though only bombus is in the model where the
## phylogeny is used. Setting all the non bombus species names to a
## random bombus so that the model with run because thouse rows will
## be dropped anyways by the parasite weights.

spec.net$GenusSpecies[!spec.net$GenusSpecies %in%
                      colnames(phylo_matrix)] <- "Bombus vosnesenskii"
spec.net$GenusSpecies <- as.character(spec.net$GenusSpecies)

formula.crithidia <-  runParasiteModels(spec.net, "bombus",
                                        "HasCrithidia", xvars.coast)

bf.fdiv <- bf(formula.flower.div, family="student")
bf.fabund <- bf(formula.flower.abund, family = "gaussian")
bf.bdiv <- bf(formula.bee.div, hu=formula.bee.div.hu,
              family="hurdle_gamma")
bf.babund <- bf(formula.bee.abund, hu=formula.bee.abund.hu,
                family = "hurdle_lognormal")

## convert to brms format
bf.par <- bf(formula.crithidia, family="bernoulli")
bform <-  bf.fdiv + bf.fabund + bf.babund + bf.bdiv + bf.par +
    set_rescor(FALSE)

## ## **********************************************************
## ## model assessment
## ## **********************************************************
## ## looks good
run_plot_freq_model_diagnostics(remove_subset_formula(formula.flower.div),
                                this_data=spec.net[spec.net$Weights == 1,],
                                this_family="students")

## looks great
run_plot_freq_model_diagnostics(remove_subset_formula(formula.flower.abund),
                                this_data=spec.net[spec.net$Weights == 1,],
                                this_family="gaussian")

## potentially an issue with homogeneity of variance, hard to say
## because no support for checking hurdle lognormal  models (only zero
## inflated)
run_plot_freq_model_diagnostics(remove_subset_formula(formula.bee.div),
                                this_data=spec.net[spec.net$Weights == 1,],
                                this_family="hurdle_gamma")

## potentially an issue with homogeneity of variance, hard to say
## because no support for checking hurdle models (only zero
## inflated)
run_plot_freq_model_diagnostics(remove_subset_formula(formula.bee.abund),
                                this_data=spec.net[spec.net$Weights == 1,],
                                this_family="hurdle_lognormal")

freq.formula <- as.formula(paste("HasCrithidia",
                                 paste(xvars.coast[-length(xvars.coast)],
                                       collapse=" + "),
                                 sep=" ~ "))
run_plot_freq_model_diagnostics(freq.formula,
                                this_data=spec.net[spec.net$WeightsPar == 1,],
                                this_family="bernoulli")

## **********************************************************

## run model
fit.bombus <- brm(bform, spec.net,
                  cores=ncores,
                  iter = 10^4,
                  chains =3,
                  thin=1,
                  init=0,
                  open_progress = FALSE,
                  control = list(adapt_delta = 0.999,
                                 stepsize = 0.001,
                                 max_treedepth = 20),
                  data2 = list(phylo_matrix=phylo_matrix)
                  )

write.ms.table(fit.bombus, "Crithidia_allbee_coast")
save(fit.bombus, spec.net, spec.orig,
     file="saved/CrithidiaFitAllBee_coast.Rdata")

load(file="saved/CrithidiaFitAllBee_coast.Rdata")

plot.res(fit.bombus, "Crithidia_allbee_coast")

summary(fit.bombus)

bayes_R2(fit.bombus)

plot(pp_check(fit.bombus, resp="VegDiversity"))
plot(pp_check(fit.bombus, resp="VegAbundance"))
plot(pp_check(fit.bombus, resp="BeeAbundance"))
plot(pp_check(fit.bombus, resp="BeeDiversity"))
