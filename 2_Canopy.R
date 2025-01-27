setwd("/coast_bees_parasites")

## Prepares the data for model fitting (standardizes continuous
## variables, creates dummy variables to be used as weights to all
## different subsets of data to be used in different model levels),
## builds the models, and fits the models in brms. The model outputs
## are saved as tables, and chain diagnostic plots created.

rm(list=ls())

## set to the number of cores you would like the models to run on
ncores <- 1

load("data/spec_net_coast.Rdata")
source("src/init.R")
source("src/misc.R")
source("src/writeResultsTable.R")
source("src/runParasiteModels.R")
source("src/standardize_weights.R")
source("src/runPlotFreqModelDiagnostics.R")

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

## variables to log but add 1 first (due to zeros)
variables.to.log.p1 <- c(
    "VegAbundance",
    "BeeAbundance",
    "BeeDiversity"
)

spec.net <- spec.net[spec.net$ThinStatus == "Y",]

spec.net <- spec.net[, c("Stand", "Year",
                         unique(c(vars_year, vars_year_sr,
                                variables.to.log.p1))
                         )]

spec.net$Key <- paste0(spec.net$Stand,
                       spec.net$Year,
                       spec.net$DoyStart)
spec.net <- spec.net[!duplicated(spec.net$Key),]

## ********************************************************
## log variables here
## ********************************************************

spec.orig <- prepDataSEM(spec.net, 
                         variables.to.log.1=variables.to.log.p1,
                         standardize=FALSE,
                         add.par.weights=FALSE,
                         data.is.multilevel=FALSE)

## Make SEM weights and standardize data.
dim(spec.net)
spec.net <- prepDataSEM(spec.net,
                        variables.to.log.1= variables.to.log.p1,
                        vars_yearsr=vars_year_sr,
                        vars_year=vars_year,
                        add.par.weights=FALSE,
                        data.is.multilevel=FALSE)
dim(spec.net)

## **********************************************************
## Model 1.1: formula for forest effects on floral community
## **********************************************************
## define all the formulas for the different parts of the models

formula.flower.div <- formula(VegDiversity | subset(Weights) ~
                                  DoyStart + I(DoyStart^2) +
                                      MeanCanopy +
                                      (1|Stand)
                              )

## flower abund with simpson div
formula.flower.abund <- formula(VegAbundance | subset(Weights) ~
                                    DoyStart +  I(DoyStart^2) +
                                        MeanCanopy +
                                        (1|Stand)
                                )

## **********************************************************
## Model 1.2: formula for forest effects on bee community
## **********************************************************

formula.bee.div <- formula(BeeDiversity | subset(Weights)~
                               VegDiversity +
                                   TempCStart +
                                   MeanCanopy +
                                   (1|Stand)
                           )

formula.bee.abund <- formula(BeeAbundance | subset(Weights)~
                                 VegAbundance +
                                     TempCStart +
                                     MeanCanopy +
                                     (1|Stand)
                             )


## hurdle models
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


bf.fdiv <- bf(formula.flower.div, family="student")
bf.fabund <- bf(formula.flower.abund, family = "gaussian")
bf.bdiv <- bf(formula.bee.div, hu=formula.bee.div.hu,
              family="hurdle_gamma")
bf.babund <- bf(formula.bee.abund, hu=formula.bee.abund.hu,
                family = "hurdle_gamma")

## convert to brms format
bform <-  bf.fdiv + bf.fabund + bf.babund + bf.bdiv +
    set_rescor(FALSE)

## ## **********************************************************
## ## model assessment
## ## **********************************************************

run_plot_freq_model_diagnostics(remove_subset_formula(formula.flower.div),
                                this_data=
                                    spec.net,
                                this_family="students")

run_plot_freq_model_diagnostics(remove_subset_formula(formula.flower.abund),
                                this_data=
                                    spec.net,
                                this_family="gaussian")

run_plot_freq_model_diagnostics(remove_subset_formula(formula.bee.div),
                                this_data=
                                    spec.net,
                                this_family="hurdle_gamma")

run_plot_freq_model_diagnostics(remove_subset_formula(formula.bee.abund),
                                this_data= spec.net,
                                this_family="hurdle_gamma")

## **********************************************************

## run model with only thinned stands
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

plot(pp_check(fit.bombus, resp="VegDiversity", ndraws=10^3))
plot(pp_check(fit.bombus, resp="VegAbundance", ndraws=10^3))
plot(pp_check(fit.bombus, resp="BeeAbundance", ndraws=10^3))
plot(pp_check(fit.bombus, resp="BeeDiversity", ndraws=10^3))
