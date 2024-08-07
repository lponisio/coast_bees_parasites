
runParasiteModels <- function(spec.data,
                              species.group, parasite,
                              xvars){

  formula.parasite  <- as.formula(paste(
    paste(parasite, "| subset(WeightsPar)"),  #+ trials(1)
    paste(xvars,
          collapse=" + "),
    sep=" ~ "))

  return(formula.parasite)
}
