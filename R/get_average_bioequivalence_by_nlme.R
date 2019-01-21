#' Make contrast matrix.
#'
#' This function enables the user to set a reference treatment for
#' bioequivalence comparison using get_abe_bylme to set reference treatment.
#' @param df a dataset with factor variable to create contrast from
#' @param fact_var an unquote factor variable to create constrast
#' @param contr_variable a value of fact_var to use as a reference contrast
#' @return a contrast matrix
#' @examples
#' make_treatment_contrast(df=results_wide, fact_var = TREATMENT, contr_variable = "Mixtard 30/70")
#' @export
make_treatment_contrast<-function(df, fact_var, contr_variable)
{
  enfact_var<-rlang::enexpr(fact_var)
  factvar<-df%>%select(!!enfact_var)%>%mutate_all(.funs = as.factor)%>%unlist()
  fact_level<-levels(factvar)
  base_contrast<-which(fact_level==contr_variable)
  contrasts(factvar)<-contr.treatment(n=length(fact_level), base = base_contrast)
  contr_matrix<-contrasts(factvar)
  colnames(contr_matrix)<-fact_level[which(fact_level!=contr_variable)]
  return(contr_matrix)
}


#' Get average bioequivalence results.
#'
#' This function uses nlme::lme function to fit the linear mixed effects model.
#' The average bioequivalence results are calculated from the estimate of the
#' treatment effect and the corresponding 90% confidence interval
#' @param data a dataset with pharmacokinetic or pharmacodynamic parameters
#'   whose ABE are to be calculated
#' @param treatments unquoted variable name for treatment groups (Should be a factor variable)
#' @param sequences unquoted variable name for treatment sequences(Should be a factor variable)
#' @param periods unquoted variable name for treatment periods (Should be a factor variable)
#' @param idvar unquoted variable name for subject identification
#' @param abe_param unquoted variable name for a PK/PD parameter whose
#'   bioequivalence is sought
#' @param reference a value of the treatments variable to use as reference
#'   treatment
#' @return a list with 2 elements: lmefit = results from nlme::lme and ABEresult
#'   = a dataframe of ABE results
#'
#' @examples
#' fit<-get_abe_bylme(data=results_wide, treatments = TREATMENT, sequences = SEQUENCE, periods = PERIOD, idvar = ID, abe_param = cmax0_24, contr_variable = "Mixtard 30/70")
#' abe_res<-fit$ABEresult
#' modelfit<-fit$lmefit
#' @export
get_abe_bylme<-function(data, treatments, sequences, periods, idvar, abe_param, reference)
{
  #Capture symbols
  varData <- rlang::ensym(data)
  varTreatments<-rlang::ensym(treatments)
  varSequences<-rlang::ensym(sequences)
  varPeriods<-rlang::ensym(periods)
  varIdvar<-rlang::ensym(idvar)
  varParam<-rlang::ensym(abe_param)
  varContrVar<-rlang::enexpr(reference)
  #browser()
  #make expressions
  fixed<-rlang::eval_bare(expr = rlang::expr(log(!!varParam)~!!varTreatments+!!varSequences+!!varPeriods), env = rlang::caller_env())
  random<-rlang::eval_bare(expr = rlang::expr(~1|!!varIdvar), env = rlang::caller_env())
  #Figure out how to pass string into a function in a function and caller environment
  contrast_matrix<-rlang::eval_bare(expr = rlang::expr(
    make_treatment_contrast(df=!!varData, fact_var = !!varTreatments, contr_variable = !!varContrVar)),
    env = rlang::caller_env()
  )
  contrast_list<-list(contrast_matrix)
  names(contrast_list)<-rlang::as_string(varTreatments)
  ##Contrast list is bound to the caller environment, but is removed after the function executes.
  on.exit(rm(contrast_list, envir = rlang::caller_env()))
  rlang::env_bind(.env = rlang::caller_env(), contrast_list = contrast_list)
  ##Fit the lme model
  lme_model<-rlang::eval_bare(expr = rlang::expr(nlme::lme(fixed = !!fixed, random = !!random, data = !!varData,
                                              contrasts = contrast_list)), env = rlang::caller_env())
  ##Test string
  trts<-levels(data[[rlang::as_string(varTreatments)]])
  teststring<-paste0(varTreatments, trts[trts!=reference])
  fixef_test<-nlme::fixed.effects(lme_model)[[teststring]]
  se_test<-sqrt(vcov(lme_model)[teststring, teststring])
  DFtrt<-lme_model$fixDF$terms[[rlang::as_string(varTreatments)]]
  Zvalue<-qt(0.05,DFtrt)
  abe_res<-data.frame(
    GMR = exp(fixef_test),
    p5CI= exp(fixef_test+Zvalue*se_test),
    p95CI=exp(fixef_test-Zvalue*se_test),
    WSV=100*sqrt(exp(as.numeric(nlme::VarCorr(lme_model)["Residual","Variance"]))-1)
  )
  return(list(lmefit = lme_model, ABEresult = abe_res))
}
