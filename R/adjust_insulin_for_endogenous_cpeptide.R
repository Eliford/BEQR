#' Adjust for endogenous insulin production.
#'
#' This function calculates baseline insulin to C-Peptide ratio and adjust the
#' observed insulin for c-peptide (endogenous insulin) production
#'
#' @param df a dataset for one subject with insulin and cpeptide values provided
#'   in the same variable e.g. CONC
#' @param idv the time variable
#' @param dosing_time time in the idv variable at which insulin injection was
#'   administered
#' @param dv the variable containing insulin and c-peptide values
#' @param dv_type a flag variable identifying insulin and c-peptide values
#' @param dv_type_values list of expression indicating the values of the dv_type
#'   variable e.g. if values of dv_type are `ins` and `cp` then dv_type_values
#'   will take the expression list("INSULIN"="ins", "CPEPTIDE"="cp")
#' @return a dataframe with new variables INSCP_RAT
#'   (\eqn{\frac{insulin}{cpeptide}}) and INSULIN_ADJ (adjusted insulin
#'   concentration)
#' @examples
#' # Test the function using the \emph{wosmpkdata} dataframe.
#' pt1<-wosmpkdata%>%filter(ID==1, PERIOD==1)
#' pt1<-adjust_insulin(pt1)
#' @export
adjust_insulin<-function(df, idv="TAD", dosing_time = 0.01, dv="CONC", dv_type="TYPE", dv_type_values=list("INSULIN"="INSULIN", "CPEPTIDE"="CPEPTIDE"))
  {
  colnms  <- paste0(names(df)[!names(df)%in%c(dv, dv_type)], collapse = "+")
  formula_string <- paste0(colnms, "~", dv_type)
  df<-df%>%reshape2::dcast(eval(parse(text = formula_string)), value.var = dv)
  var_tad<-rlang::quo(!!rlang::sym(idv))
  sub_df<-df%>%filter(rlang::UQ(var_tad)<dosing_time)
  var_insulin<-rlang::quo(!!rlang::sym(dv_type_values[["INSULIN"]]))
  var_cpeptide<-rlang::quo(!!rlang::sym(dv_type_values[["CPEPTIDE"]]))
  var_idv<-rlang::quo(!!rlang::sym(idv))
  ##If either CPEPTIDE OR INSULIN is 0 before infusion.
  sub_df<-sub_df%>%mutate(RATIO=as.numeric(!!var_insulin/!!var_cpeptide))
  ##For subjects where RATIO!=0, do filter only
  sub_df<-sub_df%>%filter(RATIO>0&RATIO<Inf)
  df<-df%>%mutate(INSCP_RAT=mean(sub_df$RATIO))
  ##Change Nan to 0
  df<-df%>%mutate(INSCP_RAT=ifelse(is.nan(INSCP_RAT),0,INSCP_RAT))%>%arrange(!!var_idv)
  #Calculate exogenous insulin concentration by adjusting for C-peptide concentration during the insulin infusion
  df<-df%>%mutate(INSULIN_ADJ=round(!!var_insulin-(!!var_cpeptide*INSCP_RAT),2))
  #If exogenous insulin is calculated and found to be negative then impute 0
  df<-df%>%mutate(INSULIN_ADJ=ifelse(INSULIN_ADJ<0,0,INSULIN_ADJ))#%>%select(-!!var_cpeptide, -!!var_insulin)
  return(df)
}

