library(glmmTMB)
library(lme4)
library(lmerTest)
library(performance)

remove_subset_formula <- function(form){
    char.form <- as.character(form)
    no.sub <-
        gsub("\\| subset\\(Weights[:alpha:⁠]*\\)",
             "", char.form[2])
    form.out <- formula(paste(no.sub, "~", char.form[3]))
    return(form.out)
}


run_plot_freq_model_diagnostics <- function(this_formula, #brms model formula
                                            this_data, #data frame, subsetted to correct weights!
                                            num_chains=1, 
                                            num_iter=10^4, 
                                            launch.shiny=FALSE,
                                            examine.pairs=FALSE,
                                            this_family, #model family
                                            fig.path =
                                                "figures/diagnostics",
                                            species.group="bombus", ...
                                            ){
    
    ## function to run frequentist version of brms models and plot diagnostics

    ## currently works with all families included in ?family, as well as
    ## zero inflated neg binomial, negbinomial, Gamma, inverse gaussian,
    ## poisson, quasibinomial will need to add other families as needed
    ## uses other glm packages to model frequentist versions of the
    ## bayesian models

    if (this_family == 'gaussian'){
        this_model_output <- lmer(this_formula, data=this_data)
        
        diagnostic.plots <- plot(check_model(this_model_output, panel
                                             = TRUE))
    } else if (this_family=='negbinomial') {
        
        this_model_output <- glmer.nb(this_formula, data=this_data)
        diagnostic.plots <- plot(check_model(this_model_output, panel = TRUE))
        
        
        

    } else if (this_family=='zero_inflated_binomial') {
        
        this_model_output <- glmmTMB(this_formula, data=this_data,
                                     ziformula = ~1,
                                     family = binomial)
        diagnostic.plots <- plot(check_model(this_model_output, panel = TRUE))
        
        
    } else if (this_family=='hurdle_poisson') {
        ## really zero inflated... but close
        this_model_output <- glmmTMB(this_formula, data=this_data,
                                     ziformula = ~.,
                                     family = truncated_poisson())
        diagnostic.plots <- plot(check_model(this_model_output, panel = TRUE))
        
    } else if (this_family=='hurdle_negbinomial') {
        ## really zero inflated... but close
        this_model_output <- glmmTMB(this_formula, data=this_data,
                                     ziformula = ~1,
                                     family = truncated_nbinom2())
        diagnostic.plots <- plot(check_model(this_model_output, panel = TRUE))
        

    } else if (this_family=='students') {
        
        this_model_output <- glmmTMB(this_formula, data=this_data,
                                     family = t_family(),
                                     control =
                                         glmmTMBControl(optimizer = optim,
                                                        optArgs = list(method="BFGS")))
        diagnostic.plots <- plot(check_model(this_model_output, panel = TRUE))
        
        

    } else if (this_family=='negbinomial') {
        this_model_output <- glmer.nb(this_formula, data=this_data, ...)
        diagnostic.plots <- plot(check_model(this_model_output, panel = TRUE))
        

    }else if (this_family=='bernoulli') {
        this_model_output <- glmmTMB(this_formula, data=this_data, family='binomial')
        diagnostic.plots <- plot(check_model(this_model_output, panel = TRUE))
        
        

    } else if (this_family == 'Gamma'){
        
        this_model_output <- glmer(this_formula, data=this_data, family=Gamma(link=log))
        
                                        # return a list of single plots
        diagnostic.plots <- plot(check_model(this_model_output, 
                                             panel = TRUE))


    } else if (this_family == 'inverse.gaussian'){
        
        this_model_output <- brms::brm(this_formula,
                                       data = this_data, 
                                       chains = num_chains, 
                                       iter = num_iter, family=this_family,
                                       thin=1,
                                       init=0,
                                       open_progress = FALSE,
                                       control = list(adapt_delta = 0.99),
                                       save_pars = save_pars(all = TRUE))
                                        # return a list of single plots
        diagnostic.plots <- plot(check_model(this_model_output, 
                                             panel = TRUE))


    } else if (this_family == 'poisson'){
        
        this_model_output <- brms::brm(this_formula,
                                       data = this_data, 
                                       chains = num_chains, 
                                       iter = num_iter, family=this_family,
                                       thin=1,
                                       init=0,
                                       open_progress = FALSE,
                                       control = list(adapt_delta = 0.99),
                                       save_pars = save_pars(all = TRUE))
                                        # return a list of single plots
        diagnostic.plots <- plot(check_model(this_model_output, 
                                             panel = TRUE), type = "discrete_dots")

    } else if (this_family == 'quasibinomial'){
        
        this_model_output <- brms::brm(this_formula,
                                       data = this_data, 
                                       chains = num_chains, 
                                       iter = num_iter, family=this_family,
                                       thin=1,
                                       init=0,
                                       open_progress = FALSE,
                                       control = list(adapt_delta = 0.99),
                                       save_pars = save_pars(all = TRUE))
                                        # return a list of single plots
        diagnostic.plots <- plot(check_model(this_model_output, 
                                             panel = TRUE))
    } else if (this_family=='hurdle_lognormal') {
        ## really zero inflated... but close
        this_model_output <- glmmTMB(this_formula, data=this_data,
                                     ziformula = ~.,
                                     family = lognormal())
        diagnostic.plots <- plot(check_model(this_model_output, 
                                             panel = TRUE))
    }

    if (launch.shiny == TRUE){shinystan::launch_shinystan(this_model_output)}
    if (examine.pairs == TRUE){pairs(this_model_output)}

    if(class(this_formula)[1] == "brmsformula"){
        file.name <- paste0(as.character(this_formula)[4], "_",
                            species.group, ".pdf")
    }else{
        file.name <- paste0(as.character(this_formula)[2], "_",
                            species.group, ".pdf")
    }
    print(summary(this_model_output))
    ggsave(diagnostic.plots, file= file.path(fig.path, file.name),
           height=10, width=15)
    return(diagnostic.plots)
}
