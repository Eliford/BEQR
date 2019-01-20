#' @import dplyr
#' @import rlang
#' @import ggplot2
NULL

#' demographics of health volunteers in the PKPD study
#'
#' A dataset of demographic characteristics
#'
#' @format a data-frame with 97 rows and 7 variables
#' \describe{
#'   \item{ID}{Unique subject identifier (1 -- 50)}
#'   \item{GENDER}{Biological sex, M = Male, F=Female}
#'   \item{AGE}{Age in years}
#'   \item{WEIGHT}{Weight in Kg}
#'   \iter{PERIOD}{Treatment periods}
#'   \item{SEQUENCE}{Assigned treatment sequences}
#'   \item{TREATMENT}{Treatments taken in each treatment period}
#' }
"beqrdemographic"

#' Pharmacodynamic dataset from the PKPD study
#'
#' A dataset providing glucose infusion rate and plasma glucose
#' levels overtime during the observation period.
#'
#' @format a data-frame with 21160 rows and 8 variables
#' \describe{
#'   \item{ID}{Unique subject identifier (1 -- 50)}
#'   \item{PERIOD} {Treatment periods}
#'   \item{TIME}{Time of the day in 24 hours format}
#'   \item{GIR}{Glucose infusion rate, milliter/hour}
#'   \item{GLUCOSE}{Plasma glucose concentration, milgram/deciliter}
#'   \item{SEQUENCE}{Assigned treatment sequences}
#'   \item{TREATMENT}{Treatments taken in each treatment period}
#'   \item{CLOCK}{Time of the day in 24 hours format}
#' }
#'
"beqrpddataset"

#' Pharmacokinetic dataset from the PKPD study
#'
#' A dataset providing insulin and c-peptide concentrations
#' over-time during the observation period
#'
#' @format a data-frame with 4656 rows and 9 variables
#' \describe{
#'   \item{ID}{Unique subject identifier (1 -- 50)}
#'   \item{PERIOD}{Treatment periods}
#'   \item{TIME}{Time after insulin injection}
#'   \item{CONC}{Insulin or c-peptide concentration}
#'   \item{TAD}{Time after insulin injection, hours}
#'   \item{TYPE}{Analyte type, INSULIN or CPEPTIDE}
#'   \item{INCLUDED}{Observations included for analysis}
#'   \item{SEQUENCE}{Assigned treatment sequences}
#'   \item{TREATMENT}{Treatments taken in each treatment period}
#' }
#'
"beqrpkdataset"
