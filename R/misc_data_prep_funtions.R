#' Insert panctuations in a string
#'
#' This function can be used to panctuate dates into a format that can be
#' modified with lubridate
#' @param string a string to be panctuated
#' @param panc_pos a position to insert panctuation
#' @param panctuation a panctuation to insert
#' @return a panctuated string
#' @export
str_panctuate <- function(string, panc_pos, panctuation) {
  paste0(stringr::str_trunc(string, panc_pos, "right", ellipsis = ""), panctuation, stringr::str_trunc(string, nchar(string)-panc_pos, "left", ellipsis = ""))
}

#' Edit date values after midnight
#'
#' @param df a dataframe with date column to edit
#' @param condition_col a column which provides a condition by which date-col
#'   should be edited
#' @param date_col a date column to edit
#' @return input dataframe df with date_col edited
#' @export
edit_Date<-function(df, condition_col="TIME", date_col="DATE"){
  for(i in 2:nrow(df)){
    if(df[[condition_col]][i] < df[[condition_col]][i-1]){
      df[[date_col]][i]<-df[[date_col]][i-1]+lubridate::days(1)
    } else {
      df[[date_col]][i]<-df[[date_col]][i-1]
    }
  }
  return(df)
}

#' Round time after dose to a specific base.
#'
#' This function rounds a value to a base value based on a cut-off value.
#' @param x a vector to round
#' @param base a base to round by
#' @param cutoff a cutoff (usually 0.5*base) to round by
#' @return a vector with values rounded
#' @examples
#' custom_round(18, 5, 2.5)
#' custom_round(20, 5, 2.5)
#' @export
custom_round<-function(x, base, cutoff){
  x <- ifelse(x%%base < cutoff, floor(x/base)*base, ceiling(x/base)*base)
  return(x)
}

#' Specify plot theme
#' @return ggplot2 theme object
#' @export
BEQR_theme<-function(){
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "white", colour = "black", linetype = "solid")
  )
}
