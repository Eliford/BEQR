#' Plot mutiple plots in one panel.
#'
#' This function was accessed on November 2015 at
#' http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/.
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE), then
#' plot 1 will go in the upper left, 2 will go in the upper right, and 3 will go
#' all the way across the bottom.
#'
#' @param ...: individula ggplot objects
#' @param plotlist: a list of ggplot objects
#' @param cols:   Number of columns in layout
#' @param layout: A matrix specifying the layout. If present, 'cols' is ignored.
multiplot <- function(..., plotlist=NULL, file, cols=cols, layout=NULL) {
  # cols=1 so that one column to have 2 plots per same subject. Eliford's choice
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



#' Plot individual subject's insulin profile by treatment period.
#'
#' @param df data for a single patient visit. e.g subject 1 period 1. Variable
#'   names should be as given in the function.
#' @param id a variable name (string) for subject unique identifier
#' @param visit a variable name for treatment period
#' @param form a variable name (string) for formulations variable
#' @param dv a variable name (string) for dependent variable
#' @param idv a variable name (string) for independet (regressor) variable
#' @param ylab  a label for y-axis
#' @param xlab a label for x-axis
#' @return a ggplot2 object
#' @examples
#' pt1<-insulinpk%>%filter(ID==1&PERIOD==1)
#' plot_insulin(pt1, visit = "PERIOD",form = "TREATMENT", idv = "TAD")
plot_insulin<-function(df, id="ID", visit = "PERIOD", form="TREATMENT", dv = list("CONC"="CONC"), idv="TIME", ylab = "Concentration (ng/mL)", xlab = "Time (hrs)"){
  my_theme<-function(){
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", colour = "black", linetype = "solid"),
      plot.title = ggplot2::element_text(size = 10)
    )
  }

  # This expression below will avoid any attempt to plot if all concentrations for a given subject-period are null
  CONC = dv[["CONC"]]
  if(all(is.na(df[CONC]))) {
    dfn <- data.frame(matrix(ncol = ncol(df),nrow = 0))
    colnames(dfn) <- names(df)
    df <- dfn
  }
  #Get subject ID and formulation type
 sid <- as.character(unique(df[id]))
 formtype <- as.character(unique(df[form]))
 visitid <- as.character(unique(df[visit]))

 p<-ggplot2::ggplot(df,aes(x=eval(parse(text = idv))))+
    ggplot2::geom_line(aes(y=eval(parse(text = dv[["CONC"]]))),colour="black",size=1.5)+
    ggplot2::geom_point(aes(y=eval(parse(text = dv[["CONC"]]))),size=4)+
    ggplot2::labs(y=ylab, x=xlab, title = glue::glue('ID = {sid}, Period = {visitid}, Formulation = {formtype}'))+
    my_theme()
  return(p)
}


#' Plot individual subject's glucose infusion rate profile by treatment period.
#'
#' @param adj_insert_fig_height adjustment for the height of the insert
#'   glucose-profile figure.
#' @inheritParams plot_insulin
#' @return a ggplot object
#' @examples
#' pt1<-pddata%>%filter(ID==1&PERIOD==1)
#' plot_infusionrate(pt1, adj_insert_fig_height = 20)
plot_infusionrate<-function(df, id="ID", idv="TAD", dv=list("GIR"="GIR", "GLUCOSE"="GLUCOSE", "CLAMP" = "GCLAMP"),
                   visit = "PERIOD", form="TREATMENT", ylab = "Glucose infusion rate (mL/h)", xlab = "Time (hrs)",
                   adj_insert_fig_height){
  sid<-as.character(unique(df[[id]]))
  formtype <- as.character(unique(df[form]))
  visitid<-as.character(unique(df[[visit]]))

  my_theme<-function(){
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", colour = "black", linetype = "solid"),
      plot.title = ggplot2::element_text(size = 10)
    )
  }

  #Define some values to use for plot arguments below
  GLUCOSE = dv[["GLUCOSE"]]
  GIR = dv[["GIR"]]
  CLAMP = dv[["CLAMP"]]

  max_rbg<-max(df[,GLUCOSE],na.rm = TRUE)+adj_insert_fig_height
  min_rbg<-min(df[,GLUCOSE],na.rm = TRUE)
  max_gir<-max(df[,GIR],na.rm = TRUE)
  min_time<-min(df[,idv],na.rm = TRUE)
  max_time<-max(df[,idv],na.rm = TRUE)
  seg_x<-min_time
  seg_xend<-min_time+2


  if(any(is.na(max_rbg), is.na(min_rbg))) {
    d<-0
  } else {
    d<-max_rbg-min_rbg + adj_insert_fig_height
  }
  #--end
  breaks_seq<-function(llm,ulm){
    return(round(seq(llm,ulm, by=(ulm-llm)/4),1))
  }

  labels_seq<-function(llm, ulm){
    return(as.character(round(seq(llm,ulm, by=(ulm-llm)/4),0)))
  }

  gir_plot<-ggplot2::ggplot(df,aes(x=eval(parse(text= idv))))+
    ggplot2::geom_line(aes(y=eval(parse(text = dv[["GIR"]]))),colour="black", size=1)+ylim(0,max_gir+d)+
    ggplot2::labs(y=ylab, x=xlab, title = glue::glue('ID = {sid}, Period = {visitid}, Formulation = {formtype}'))+
    my_theme()

  glucose_plot<-ggplot2::ggplot(df,aes(x=eval(parse(text= idv))))+
    ggplot2::geom_line(aes(y=eval(parse(text = dv[["GLUCOSE"]]))),colour="blue",size=1)+ylim(min_rbg,max_rbg+7)+
    ggplot2::geom_line(aes(y=eval(parse(text = dv[["CLAMP"]]))),size=1, linetype="dashed")+
    ggplot2::annotate("segment",x = seg_x , xend = seg_xend, y = max_rbg+5/30, yend = max_rbg+5/30, linetype="dashed", size=1)+
    ggplot2::annotate("text", size=3,  x=seg_xend+5, y=max_rbg+5/30, label= "Clamp value")+
    ggplot2::labs(y="BG (mg/dl)", x = "Time (hours)")+
    ggplot2::scale_y_continuous(breaks = breaks_seq(min_rbg, max_rbg), labels = labels_seq(min_rbg, max_rbg))+
    my_theme()

  q<-ggplot2::ggplotGrob(glucose_plot)

  p<-gir_plot + ggplot2::annotation_custom(grob = q, xmin = min_time, xmax = max_time, ymin = max_gir, ymax = max_gir+d)
  #coord_fixed()
  return(p)
}


#' Plot insulin profiles for all individuals and respective periods.
#'
#' This function uses the multiplot function and functions for plotting
#' individual profiles to create multiple plots for individuals.
#' @param df dataset containing individuals with analysable data. Variable names
#'   should be as given in the function.
#' @param nplots number of plots per panel
#' @param ncol number of plot-columns per panel
#' @param response_to_plot a response variable to be plotted (Insulin or Glucose
#'   infusion rate)
#' @param plot_function a function used to plot individual profiles
#'   (plot_insulin or plot_infusionrate)
#' @inheritParams plot_insulin
#' @return plots of individual profiles
#' @examples
#' get_individual_profiles(insulinpk%>%filter(ID%in%c(1,2,3)), id="ID", form = "PERIOD", dv = list("CONC"="CONC"), idv = "TAD", period = "PERIOD",
#'                  nplots = 6, ncol = 2, ylab = "Concentration (ng/mL)", xlab="Time (hrs)", response_to_plot = "INSULIN", plot_function = plot_insulin)
#' get_individual_profiles(pddata%>%filter(ID%in%c(1,2,3)), id="ID", form = "TREATMENT", dv = list("GIR"="GIR", "GLUCOSE"="GLUCOSE", "CLAMP" = "GCLAMP"),
#'                  idv = "TAD", period = "PERIOD", nplots = 6, ncol = 2, ylab = "Glucose infusion rate (mL/h)", xlab="Time (hrs)",
#'                  plot_function = plot_infusionrate, adj_insert_fig_height = 20, response_to_plot = "GIR")
get_individual_profiles<-function(df, id="ID", visit = "PERIOD", form = "TREATMENT", dv = list("CONC"="CONC"), idv = "TIME",
                           ylab = "Concentration (ng/mL)", xlab = "Time (hrs)", nplots = 4, ncol = 2,
                           response_to_plot = "INSULIN", plot_function,
                           adj_insert_fig_height){
  ids <- unique(df[id]) %>% unlist() %>% unname()

  multipleplot <- list()
  plots <- list()
  idplots <- list()
  visplots <- list()

  for(i in 1:length(ids)){
    varQ   <- rlang::quo(!!rlang::sym(id))
    iddata <- filter(df, rlang::UQ(varQ) == ids[i])
    visits <- unique(iddata[visit]) %>% unlist() %>% unname()

    for (j in 1:length(visits)){
      varR <- rlang::quo(!!rlang::sym(visit))
      idvisdata <- filter(iddata, rlang::UQ(varR) == visits[j])

      if(response_to_plot=="INSULIN"){
        r <- plot_function(df = idvisdata, id = id, visit = visit, form = form, dv = dv, idv = idv, ylab = ylab, xlab = xlab)
      } else if(response_to_plot=="GIR"){
        r <- plot_function(df = idvisdata, id = id, visit = visit, form = form, dv = dv, idv = idv, ylab = ylab, xlab = xlab,
                           adj_insert_fig_height)
      } else {
        stop("There is no response variable to plot")
      }

      visplots[[j]]<-r

    }

    idplots[[i]]<-visplots
  }

  plots<-unlist(idplots,recursive = FALSE)

  plot.matrix<-matrix(plots,nrow = nplots)  #nrow = 4 determines number of plots on same panel

  for(i in 1:ncol(plot.matrix)){
    plotlist<-plot.matrix[,i]
    multiplot(plotlist = plotlist, cols = ncol)
  }

}
