#' Make dose dataset for NCA analysis with PKNCA.
#'
#' This function makes a dosing dataset for use in NCA analysis by R PKNCA
#' package.
#' @param df a dataset
#' @param treatment_var a variable (unquoted name) with treatment groups
#' @param id_var a variable () with subjects identification numbers
#' @param time_var a variable () with time values
#' @param dose a dose value
#' @param dose_time a time of dose value
#' @param tau a value for interdose interval
#' @return a dataframe with dosing information
#' @examples
#' dose<-make_nca_dose_dataset(insulinpk, treatment_var = TREATMENT, id_var = ID, time_var = TIME, dose = 7287700, dose_time = 0, tau = 12)
#' @export

make_nca_dose_dataset<-function(df, treatment_var, id_var, time_var, dose, dose_time, tau)
{
  # Get a quosures for the unquoted arguments
  varGroup<-rlang::enquo(treatment_var)
  varID<-rlang::enquo(id_var)
  varTIME<-rlang::enquo(time_var)
  dose_data<-df%>%group_by(!!varGroup)%>%
    filter(!duplicated(!!varID))%>%
    select(!!varGroup, !!varID, !!varTIME)%>%
    mutate(DOSE=dose, TIME=dose_time, TAU = tau)
}


#' Create partial AUC interval object.
#'
#' A dataset with intended partial areas is required for NCA analysis using the
#' PKNCA package. This function creates such partial area dataset for PKNCA
#' package.
#' @param nca_params PK parameters to be calculated by NCA analysis
#' @param partial_aucs a list object with start (start time to partial AUC) and
#'   end (end time for partial AUC) elements
#' @param protocol_tlast a value of end of sampling time based on the protocol
#' @param compute_aucinf whether or not to compute AUCinf (TRUE or FALSE)
#' @return a dataframe with NCA parameters to be computed by PKNCA and partial
#'   areas
#' @examples
#' #Explore NCA parameters to select names appropriately
#' nca_params<-PKNCA::get.interval.cols()
#' names(nca_params)
#' #Create the dataset with correct NCA parameter names
#' testdf<-make_nca_par_dataset(nca_params = c("cmax","tmax","aumclast", "lambda.z","half.life","aucpext.obs"),
#'                             partial_aucs = list(start=c(0,0,0), end=c(4, 6, 12)),
#'                             protocol_tlast = 24,
#'                             compute_aucinf = FALSE)
#' @export
make_nca_par_dataset<-function(nca_params, partial_aucs=list(start, end), protocol_tlast, compute_aucinf=TRUE)
{
  create_df<-function(variables)
  {
    df<-data.frame(variables, value=TRUE)
    df<-t(df)%>%as.data.frame()
    colnames(df)<-as.character(unlist(df[1,]))
    df<-df[-1,]%>%mutate_all(.funs = function(x){as.logical(x)})
    rownames(df)<-NULL
    return(df)
  }

  partial_aucs<-data.frame( start=partial_aucs$start, end=partial_aucs$end, auclast=TRUE)
  complete_auc<-data.frame( start=0, end=protocol_tlast, auclast=TRUE)
  complete_auc<-bind_cols(complete_auc, create_df(nca_params))
  if(compute_aucinf){
    aucinf<-data.frame(start=0, end=Inf, `aucinf.obs`=TRUE)
  } else {
    aucinf<-NULL
  }
  intervals<-bind_rows(partial_aucs, complete_auc, aucinf)
  params<-names(intervals)[!names(intervals)%in%c("start", "end")]
  intervals<-intervals%>%mutate_at(.vars=params,.funs = function(x){as.logical(ifelse(is.na(x), FALSE, x))})
  return(intervals)
}


#' Extract and organize results from NCA analysis.
#'
#' This function is for extracting results from NCA analysis and put them in
#' appropriate format for bioequivalence analysis.
#'
#' @param pknca_results results from the function PKNCA::pk.nca functions
#' @param select_nca_param a character vector of PK parameters to be extracted
#'   from PKNCA::pk.nca results
#' @param design_var a character vector of column names for BEQ study design
#'   variables i.e ID, TREATMENT, PERIOD, and SEQUENCE
#' @return a dataframe with NCA parameter values in a wide format
#' @export
extract_nca_results<-function(pknca_results, select_nca_param, design_var=c("ID","TREATMENT","PERIOD","SEQUENCE"))
{
  Vardesign_var<-rlang::enquo(design_var)
  results<-pknca_results$result
  design<-myresults$data$conc$data%>%select(!!Vardesign_var)%>%distinct()
  results<-results%>%filter(PPTESTCD%in%select_nca_param)
  results<-results%>%mutate(PARNAME=paste0(PPTESTCD,start,"_",end))%>%select(-start, -end, -PPTESTCD)
  results_wide<-results%>%tidyr::spread(key = PARNAME, value = PPORRES)
  results_wide<-left_join(results_wide, design, by=names(results_wide)[names(results_wide)%in%names(design)])
  return(results_wide)
}



