#' Summarize statistics of continous variables contained in a dataframe.
#'
#' @param df a dataframe with variables to perform descriptive stats
#' @param group a unquoted name of a grouping variable
#' @param variables a character vector of variables to be summarized.
#' @param stats descriptive summary statistics to compute. (NOT YET IMPLEMENTED. USED DEFAULTS ARE: "mean", "sd", "CV", "median", "min", "max", "quantiles (0.25 and 0.75)", "geometric.mean" and "Goemetric.cv")
#' @return a dataframe of summary statistics
summarize_statz<-function(df, group, variables, stats=list("mean", "sd", "CV", "median", "min", "max", "geometric.mean")){
  ##Define basic function
  get_statz<-function(variables, df, group, stats=list("mean", "sd", "CV", "median", "min", "max", "geometric.mean")){
    # Get a quosure for a variables variable
    varParam<-rlang::quo(!!rlang::sym(variables))
    # Get a quosure for a grouping variable
    varGroup<-rlang::quo(!!rlang::sym(group))

    statz<-df%>% group_by(!!varGroup)%>%
      summarise(MEAN=mean(!!varParam, na.rm = TRUE), SD = sd(!!varParam, na.rm = TRUE), CV = SD*100/MEAN,
                MEDIAN = median(!!varParam, na.rm = TRUE),  MIN = min(!!varParam, na.rm = TRUE),
                MAX = max(!!varParam, na.rm = TRUE), LIQR = quantile(!!varParam, probs = 0.25, na.rm = TRUE),
                UIQR = quantile(!!varParam, probs = 0.75, na.rm = TRUE),
                GEOMEAN = PKNCA::geomean(!!varParam, na.rm = TRUE),
                GEOCV = PKNCA::geocv(!!varParam, na.rm = TRUE))%>%
      mutate(PARAMETERS = variables)

    statz<-statz%>%select(PARAMETERS, !!varGroup, MEAN, SD, CV, GEOMEAN, GEOCV, MEDIAN, LIQR, UIQR, MIN, MAX)

    return(statz)
  }

  #Make a list of statz

  statz_list<-lapply(variables, get_statz, df, group, stats=stats)

  return(bind_rows(statz_list))
}


#' Get median tmax and half-life
#'
#' @param df a result from PKNCA::pk.nca with tmax and half-life parameters
#' @param group a column name (string) for treatment groups
#' @param tmax a column name (string) for tmax variable
#' @param thalf a column name (string) for half-life variable
#' @return a dataframe of tmax and t-half or an error message
#' @examples
#' results_wide_pairs<-results_wide%>%group_by(ID)%>%mutate(N=n())%>%ungroup()%>%filter(N==2)%>%select(-N)
#' test<-table_median_tmax_thalf(results_wide_pairs, group = "TREATMENT")
table_median_tmax_thalf<-function(df, group, tmax="tmax0_24", thalf="half.life0_24")
{
  # Get a quosure for a grouping variable
  varGroup<-rlang::quo(!!rlang::sym(group))

  tmax_thalf_stats<-summarize_statz(df=df, group, variables=c(tmax, thalf))

  tmax_thalf_stats<-tmax_thalf_stats%>%mutate_if(is.numeric, .funs = function(x) round(x, 1))%>%
    mutate(MEDIAN_IQR = paste(MEDIAN, "(", LIQR, " - ", UIQR, ")"))

  tmax_thalf_median<-tmax_thalf_stats%>%select(PARAMETERS, !!varGroup, MEDIAN_IQR)%>%
    reshape2::dcast(PARAMETERS~eval(parse(text = group)), value.var = "MEDIAN_IQR")

  ##Before computing paired wilcoxon test make sure every subject has a pair of 2 values
  tmax_test<-tryCatch(
    wilcox.test(eval(parse(text = tmax))~eval(parse(text = group)), paired=TRUE, data = df, conf.level=0.90),
    error = function(cond)
    {
      if (stringr::str_detect(as.character(cond), "'x' and 'y' must have the same length"))
      {
        return("'x' and 'y' must have the same length")
      } else
      {
        return(cond)
      }
    }
  )

  thalf_test<-tryCatch(
    wilcox.test(eval(parse(text = thalf))~eval(parse(text = group)), paired=TRUE, data = df, conf.level=0.90),
    error = function(cond)
    {
      if (stringr::str_detect(as.character(cond), "'x' and 'y' must have the same length"))
      {
        return("'x' and 'y' must have the same length")
      } else
      {
        return(cond)
      }
    }
  )

  if(tmax_test[[1]]=="'x' and 'y' must have the same length"&thalf_test[[1]]=="'x' and 'y' must have the same length")
  {
    stop("Wilcoxon signed rank test failed because test or reference has more values than the other.
         Make sure every subject has a pair of two values only")
  } else
  {
    tmax_thalf_median<-try(
      tmax_thalf_median%>%mutate(`P-value` = ifelse(PARAMETERS==tmax, round(tmax_test$p.value,3), round(thalf_test$p.value,3))),
      silent = TRUE
    )
  }

  if(class(tmax_thalf_median)=="try-error")
  {
    stop(tmax_test)
  } else
  {
    return(tmax_thalf_median)
  }

}


#' Table of Tmax descriptive stats
#'
#' @param df a result from PKNCA::pk.nca with tmax and half-life parameters
#' @param group a column name (string) for treatment groups
#' @param tmax a column name (string) for tmax variable
#' @return a dataframe of tmax and t-half or an error message
#'
table_median_tmax<-function(df, group, tmax="tmax0_24")
{
  # Get a quosure for a grouping variable
  varGroup<-rlang::quo(!!rlang::sym(group))

  tmax_stats<-summarize_statz(df=df, group, variables=c(tmax))

  tmax_stats<-tmax_stats%>%mutate_if(is.numeric, .funs = function(x) round(x, 1))%>%
    mutate(MEDIAN_IQR = paste(MEDIAN, "(", LIQR, " - ", UIQR, ")"))

  tmax_median<-tmax_stats%>%select(PARAMETERS, !!varGroup, MEDIAN_IQR)%>%
    reshape2::dcast(PARAMETERS~eval(parse(text = group)), value.var = "MEDIAN_IQR")

  ##Before computing paired wilcoxon test make sure every subject has a pair of 2 values
  tmax_test<-tryCatch(
    wilcox.test(eval(parse(text = tmax))~eval(parse(text = group)), paired=TRUE, data = df, conf.level=0.90),
    error = function(cond)
    {
      if (stringr::str_detect(as.character(cond), "'x' and 'y' must have the same length"))
      {
        return("'x' and 'y' must have the same length")
      } else
      {
        return(cond)
      }
    }
  )

  if(tmax_test[[1]]=="'x' and 'y' must have the same length")
  {
    stop("Wilcoxon signed rank test failed because test or reference has more values than the other.
         Make sure every subject has a pair of two values only")
  } else
  {
    tmax_median<-try(
      tmax_median%>%mutate(`P-value` = round(tmax_test$p.value,3)),
      silent = TRUE
    )
  }

  if(class(tmax_median)=="try-error")
  {
    stop(tmax_test)
  } else
  {
    return(tmax_median)
  }

}


