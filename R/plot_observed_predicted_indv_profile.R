#' Plot individual profiles.
#'
#' @param df data for a single patient per visit
#' @param id a variable name (string) for subject unique identifier
#' @param study a study identification ID
#' @param dose a variable name (string) for dose variable
#' @param dv a variable name (string) for dependent variable
#' @param idv a variable name (string) for independet (regressor) variable
#' @param xlab a label for x-axis
#' @param ylab a label for y-axis
#' @return a ggplot object
plot_dv_ipred_pred<-function(df, id="ID", study = "STUDY", dose="DOSE", dv = list("OBS"="DV", "PRED"="PRED", "IPRED" = "IPRED"), idv="TIME", ylab = "Concentration (ng/mL)", xlab = "Time (hrs)"){
  my_theme<-function(){
    theme(
      panel.background = element_rect(fill = "white", colour = "black", linetype = "solid"),
      plot.title = element_text(size = 10)
    )
  }

  # This expression below will avoid any attempt to plot if all concentrations for a given subject-period are null
  CONC = dv[["OBS"]]
  if(all(is.na(df[CONC]))) {
    dfn <- data.frame(matrix(ncol = ncol(df),nrow = 0))
    colnames(dfn) <- names(df)
    df <- dfn
  }
  #Get subject ID and dose type
  sid <- as.character(unique(df[id]))
  dosetype <- as.character(unique(df[dose]))
  studyid <- as.character(unique(df[study]))
  #Get quote
  varDV<-rlang::quo(!!rlang::sym(dv[["OBS"]]))

  p<-ggplot(df,aes(x=eval(parse(text = idv))))+
    geom_line(data = df%>%filter(!is.na(!!varDV)), aes(y=eval(parse(text = dv[["OBS"]])), colour="OBS"),size=1.5)+
    geom_point(data = df%>%filter(!is.na(!!varDV)), aes(y=eval(parse(text = dv[["OBS"]])), colour="OBS"),size=4)+
    geom_line(aes(y=eval(parse(text = dv[["PRED"]])), colour="PRED"),size=1.5)+
    geom_line(aes(y=eval(parse(text = dv[["IPRED"]])), colour="IPRED"),size=1.5)+
    scale_color_manual(name=NULL, values = c("OBS" = "black", "PRED" = "brown", "IPRED" = "red"))+
    scale_y_continuous(trans = "log", labels = function(x){round(x,2)})+
    labs(y=ylab, x=xlab, title = glue::glue('ID = {sid}, study = {studyid}, dose = {dosetype}'))+
    my_theme()+ theme(legend.position = c(0.7,0.8))
  return(p)
}

#' Get individual plots from a dataset of multiple subjects.
#'
#' @param .plotfun  a function to plot individual plots
#' @inheritParams plot_dv_ipred_pred
#' @return  a list of individual plots
get_individual_plots<-function(df, id = id, .plotfun = plot_dv_ipred_pred, study = study, dose=dose, dv = dv, idv=idv, ylab = ylab, xlab = xlab){
  ids<-unique(df[id]) %>% unlist() %>% unname()
  #browser()
  plots<-vector(mode = "list", length = length(ids))

  for(i in 1:length(ids)){
    id<-ids[i]
    #browser()
    dfi<-df%>%filter(ID == id)
    plots[[i]]<-plot_dv_ipred_pred(dfi)
  }
  return(plots)
}

