#' Calculate standard errors of a vector
#'
#' @param x a vector whose standard error is required
#' @return standard error for the vector x
se<-function(x){
  n=length(x)
  sd=sd(x)
  se=sd/sqrt(n)
  return(se)
}


#' Plot mean concentration vs time profile.
#'
#' This function is a wrapper for ggplot and stat_summary to plot mean+-95%CI
#' concentration-vs-time profiles.
#' @param df a dataset with data for plotting mean profiles
#' @param treatment a variable name (string) for treatment groups
#' @param time a variable name for time after dose
#' @param dv a variable name for concentration column
#' @return a plot
#' @examples
#' base_mean_plot(wosmpkdata%>%filter(INCLUDED=="YES"))+
#'  #base_theme()+plot_theme()+
#'  labs(x="Time (hours)", y = "Concentrations (ng/mL)")+
#'  scale_color_manual(name=NULL, values = c("Mixtard 30/70" = "red", "Consegna 30/70" = "blue"))+
#'  facet_grid(PERIOD~TYPE, labeller = labeller(PERIOD=c("1"="Period 1", "2"= "Period 2")))
#' @export
base_mean_plot<-function(df, treatment = "TREATMENT", time = "TAD", dv = "CONC")
{
  ggplot2::ggplot(data = df, aes(x=eval(parse(text = time)), y=eval(parse(text = dv)), color=eval(parse(text = treatment))))+
    ggplot2::stat_summary(fun.y = mean, na.rm = TRUE, geom = "point", size=3)+
    ggplot2::stat_summary(fun.y = mean, na.rm = TRUE, geom = "line", size=1)+
    ggplot2::stat_summary(fun.data = function(x){data.frame(ymin=mean(x)-1.96*se(x), ymax=mean(x)+1.96*se(x))}, geom = "errorbar", width=1)
}

#' Smooth observations using the LOESS procedure.
#'
#' This is wrapper for stats::loess to create a loess model that is then used to
#' predict observations where they are not measured
#' @param df a dataset with independent and dependent variable
#' @param idv indpendent variable (string)
#' @param dv dependent variable (string)
#' @param ... other arguments to be passed to function stats::loess
#' @return returns a the original dataframe containing a new variable of
#'   predicted dv (smooth_dv)
#' @examples
#' wosmpddata<-read.csv("../data/secdata/wosm105_gir_pddata.csv", stringsAsFactors = FALSE)
#' id1pd<-wosmpddata%>%filter(ID==1, PERIOD==1)
#' id1pd<-id1pd%>%group_by(ID,PERIOD)%>%do(get_smooth_dv(.,idv = "TAD", dv= "GIR", span = 0.1))
#' ggplot(aes(x=TAD, y= GIR), data = id1pd)+
#' geom_point()+
#' geom_line(aes(y=smooth_dv))
#' @export
get_smooth_dv<-function(df, idv, dv,...)
{
  predict_smooth<-function(df, idv, dv, ...)
  {
    smoothing_model<-stats::loess(eval(parse(text = dv))~eval(parse(text = idv)), data = df, ...)
    smooth_dv<-stats::predict(smoothing_model, newdata = df)
    return(smooth_dv)
  }

  df<-df%>%mutate(smooth_dv=predict_smooth(df=df, idv=idv, dv = dv, ...))

  return(df)
}

#get_smooth_dv<-function(df, id, period, idv, dv, newdf=NULL, ...)
#{
#  predict_smooth<-function(df, id, period, idv, dv, newdf, ...)
#  {
#    #Get quote
#    varid<-rlang::quo(!!rlang::sym(id))
#    varperiod<-rlang::quo(!!rlang::sym(period))
#    smoothing_model<-stats::loess(eval(parse(text = dv))~eval(parse(text = idv)), data = df, ...)
#    if(is.null(newdf)){
#      smooth_dv<-predict(smoothing_model, newdata = df)
#    } else {
#      idval = unique(df[[id]])
#      perval = unique(df[[period]])
#      browser()
#      newdfi<-newdf%>%filter(!!varid==idval, !!varperiod==perval)
#      smooth_dv<-predict(smoothing_model, newdata = newdfi)
#    }
#    return(smooth_dv)
#  }
#
#  df<-df%>%mutate(smooth_dv=predict_smooth(df=df, id = id, period = period, idv=idv, dv = dv, newdf = newdf, ...))
#
#  return(df)
#}

#' Table subjects characteristcs.
#'
#' This function is a wrapper for tableone::CreateTableOne and creates a table
#' of subject characteristics by treatment groups and/or sequence groups
#' @param data a dataset with subjects characterics variables
#' @param vars a character vector of subject characteristics
#' @param factorVars a character vector of categorical variables
#' @param strata a character vector grouping variables
#' @param ... other arguments passed to function tableone::CreateTableOne
#' @return a dataframe of subject characteristics with descriptive and
#'   comparison statistics
#' @examples
#' tab_char<-table_characteristics(vars = c("GENDER", "AGE", "WEIGHT"), strata = c("SEQUENCE", "PERIOD"), includeNA = TRUE, data = demographics)
#' @export

table_characteristics<-function(data, vars, factorVars, strata, ...)
{
  tab_one<-tableone::CreateTableOne(data = data, vars = vars, factorVars = factorVars, strata = strata, ...)
  tab_one<-print(tab_one, showAllLevels = TRUE)%>%as.data.frame(.row.names=rownames(.))%>%
    mutate(Characteristics=rownames(.))%>%select(Characteristics, level:p)
  tab_one<-tab_one%>%mutate(Characteristics=recode(Characteristics, "n"="N"))

  return(tab_one)
}
