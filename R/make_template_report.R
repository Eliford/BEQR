#' Create an example rmarkdown file report.
#'
#' This function has a template rmarkdown file. The file contains BEQR functions
#' for plotting, NCA and ABE analyis.
#' @param dir_path relative or absolute path to an existing file directory
#' @param file_name a name to be given to the template rmarkdown file (include
#'   *.rmd extension)
#' @return creates an rmarkdown file in an existing file directory
#' @export
make_template_report<-function(dir_path = "lab-notebook", file_name = "newfile.rmd")
{
  path<-paste0(dir_path,"/", file_name)
  file.create(path)
  writeLines(c("---",
               'title: "Untitled"',
               'author: "Eliford"',
               'date: "November 4, 2018"',
               "output: html_document",
               "---",
               " ",
               " ",
               "```{r}",
               "#Load the bioequivalence package",
               "library(beq)",
               "```",
               " ",
               " ",
               "```{r}",
               "base_mean_plot()",
               "```",
               " ",
               "```{r}",
               'pddata_smooth<-pddata%>%group_by(SEQUENCE,PERIOD,ID)%>%do(get_smooth_dv(., idv = "TAD", dv = "GIR", span = 0.25))',
               "```",
               " ",
               "```{r}",
               "##Prepare PKNCA for NCA analysis",
               "library(PKNCA)",
               "###Set method used to calculate AUCs",
               "##Method can be linear or log linear (default). Linear was chosen as required in FDA guidance",
               'PKNCA.options(auc.method="linear")',
               "###Check if method is set",
               'PKNCA.options(name = "auc.method")',
               "```",
               " ",
               "```{r}",
               "###Create partial AUC dummy dataset",
               'testdf<-make_nca_par_dataset(nca_params = c("cmax","tmax","aumclast", "lambda.z","half.life","aucpext.obs"), ',
               "                             partial_aucs = list(start=c(0,0,0), end=c(4, 6, 12)), ",
               "                             protocol_tlast = 24, ",
               "                             compute_aucinf = FALSE)",
               "###Create dose dataset",
               "dose<-make_nca_dose_dataset(df = insulinpk, treatment_var = TREATMENT, ",
               "                            id_var = ID, time_var = TIME, dose = 7287700, ",
               "                            dose_time = 0, tau = 12)",
               " ",
               "###Create Conc object",
               "myconc<-PKNCAconc(insulinpk, formula = CONC~TIME|TREATMENT+ID, ",
               '                  labels = c(TIME="Time (hrs)", CONC="Concentration (ng/mL)"),',
               '                  units = c(TIME="hrs", CONC="ng/mL"))',

               " ",
               "##The create the dose object",
               'mydose<-PKNCAdose(dose, formula = DOSE~TIME|TREATMENT+ID, units=c(DOSE="ng", TIME="hrs"), route="extravascular")',
               " ",
               "###Create data object",
               "mydata<-PKNCAdata(myconc, mydose, intervals=testdf)",
               "```",
               " ",
               "```{r}",
               "#Calculate NCA parameters",
               "myresults<-pk.nca(mydata)",
               "```",
               " ",
               " ",
               "```{r}",
               "#Extact calculated PK parameters",
               'results_wide<-extract_nca_results(myresults, select_nca_param = c("auclast", "aucinf.obs","cmax",',
               '                                                                  "tmax","aumclast", "lambda.z",',
               '                                                                  "half.life","aucpext.obs"))',
               "```",
               " ",
               "```{r}",
               "#Summarize",
               'nca_summary<-summarize_statz(results_wide, group = "TREATMENT", ',
               '                             variables = c("auclast0_24","cmax0_24","auclast0_6"))',
               "```",
               " ",
               "```{r}",
               "## Summarize tmax and half life",
               "#Make sure only subjects with PK parameters for both treatments are present in the dataset",
               "results_wide_pairs<-results_wide%>%group_by(ID)%>%mutate(N=n())%>%ungroup()%>%filter(N==2)%>%select(-N)",
               "## Summarize the PK parameters",
               'test<-table_median_tmax_thalf(results_wide_pairs, group = "TREATMENT")',
               "```",
               " ",
               " ",
               "```{r}",
               "###Get bioequivalence",
               "# First convert treatment variable to factor",
               "results_wide<-results_wide%>%mutate(TREATMENT=factor(TREATMENT), logCmax0_24=log(cmax0_24))",
               "# Fit model",
               "fit<-get_abe_bylme(data = results_wide, treatments = TREATMENT, sequences = SEQUENCE, periods = PERIOD,",
               '                   idvar = ID, abe_param = "logCmax0_24", contr_variable = "Mixtard 30/70")',
               "```",
               " ",
               " ",
               "```{r}",
               "##Extract BEQ table",
               "ABE_result<-fit$ABEresult",
               "```",
               " ",
               "```{r}",
               "## Investigate model fit ",
               "lmefit<-fit$lmefit",
               "## adjusted geometric mean for treatments",
               "fixed_effects<-fixef(lmefit)",
               'reference_mean<-exp(fixed_effects[["(Intercept)"]])',
               'test_mean<-exp(fixed_effects[["(Intercept)"]] + fixed_effects[["TREATMENTConsegna 30/70"]])',
               "```"),
             con = path )
}
