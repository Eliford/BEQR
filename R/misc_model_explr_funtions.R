#' Plot continous versus categorical variables
#'
#' The function was written for exploration of parameter versus categorical
#' covariates relationships. It can be used to customize plots produced by the
#' GGally package
#' @param data dataset containg continous and categorical variables for which to
#'   explore relationships
#' @param mapping aes mappings as in ggplot aes
#' @return a ggplot2 object to plot
#' @export
custom_combo <- function(data, mapping, ...) {
  x <- data[[deparse(mapping$x)]]
  y <- data[[deparse(mapping$y)]]
  ##Fit anova to get p value for continous vs categorical
  fit<-try(oneway.test(data=data, formula = get(deparse(mapping$y))~get(deparse(mapping$x))), silent = TRUE)

  if(class(fit)=="try-error"){
    pval=NA
  }else{
    pval<-round(fit$p.value,3)
  }
  ##Make plot using ggplot
  p<-ggplot2::ggplot(data = data, mapping = mapping)+
    ggplot2::geom_boxplot()+
    ggplot2::theme(axis.text.x = element_text(angle = 45))
  p+ggplot2::geom_label(
    data = data.frame(
      x = 0.5,
      y = max(y, na.rm = TRUE),
      lab = paste0("p=", round(pval, digits = 3))
    ),
    mapping = ggplot2::aes(x = x, y = y, label = lab, color = NULL),
    hjust = 0, vjust = 1
    #size = 5, fontface = "bold"
  )
  return(p)
}

#' Plot continous versus continous variables.
#'
#' The function was written for exploration of parameter versus continous
#' covariates relationships. It can be used to customize plots produced by the
#' GGally package
#' @param data dataset containg continous and continous variables for which to
#'   explore relationships
#' @param mapping aes mappings as in ggplot aes
#' @param ... other arguments passed to geom smooth
#' @return a ggplot2 object to plot
#' @export
lm_with_cor <- function(data, mapping, ..., method = "pearson") {
  x <- data[[deparse(mapping$x)]]
  y <- data[[deparse(mapping$y)]]
  ###Perform peasorn correlation
  corrtest <- stats::cor.test(x, y, method = method, use = "pairwise.complete.obs")
  cor<-unname(corrtest$estimate)
  pval<-unname(corrtest$p.value)
  ###Make a plot using Ggally
  GGally::ggally_smooth_lm(data, mapping, ...) +
    ggplot2::geom_label(
      data = data.frame(
        x = min(x, na.rm = TRUE),
        y = max(y, na.rm = TRUE),
        lab = paste0("r=", round(cor, digits = 3), " p=", round(pval, digits = 3))
      ),
      mapping = ggplot2::aes(x = x, y = y, label = lab, color = NULL),
      hjust = 0, vjust = 1
      #size = 5, fontface = "bold"
    )
}

#' Extract plots created by GGally package.
#'
#' @param plots A GGally object containing ggplot2 plot objects
#' @param ncolx number of variables specified as x variables when creating a plot using ggally
#' @param ncoly number of variables specified as y variables when creating a plot using ggally
#' @return a list of plots from the GGally object
#' @export
get_your_ggally_plots<-function(plots,ncolx,ncoly){
  plot_listy<-list()
  for(i in 1:ncoly){
    x<-i
    plot_listx<-list()
    for(j in 1:ncolx){
      y<-j
      p<-try(plots[x,y], silent = TRUE)
      plot_listx[[j]]<-p
    }
    plot_listy[[i]]<-plot_listx
  }
  plot_listy<-unlist(plot_listy, recursive = FALSE)
}
