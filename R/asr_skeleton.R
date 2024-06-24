asr_skeleton <- function(){

  require(rstudioapi)

  args <- formals(asr)

  args$data <- "sae_data"

  args_helpers <- list(
    data = "# dataframe containing SAEs",
    target = "# word file as output\n\n    # TRIAL INFORMATION",
    trial_title = "",
    protocol_number = "",
    basec_number = "",
    snctp_number = "",
    swissmedic_number = "",
    ec_name = "# ethics committee",
    tr_number = "# Number for Transplantation Clinical Trials (FOPH number)",
    product_name = "# product/intervention name\n\n    # SPONSOR INFORMATION",
    sponsor_contact = "",
    inst_name_address = "\n\n    # TRIAL PROGRESS",
    n_centers_t = "# total number of participating centres",
    n_centers_p = "# planned number of participating centres",
    n_centers_c = "# number of closed centres",
    n_centers_o = "# number of open centres",
    n_pat_t = "# total target number of participants ",
    n_pat_e = "# actual number of participants enrolled",
    n_pat_c = "# number of participants completed",
    n_pat_p = "# number of participants prematurely terminated\n\n    # REPORT INFORMATION",
    n_centers_t_ch = '#total number of participating centres in CH',
    n_centers_p_ch = '#planned number of participating centres in CH',
    n_centers_c_ch = '#number of closed centres in CH',
    n_centers_o_ch = '#number of open centres in CH',
    n_pat_t_ch = "target number of participants in CH",
    n_pat_e_ch = "number of enrolled participants in CH",
    n_pat_c_ch = "number of completed participants in CH",
    n_pat_p_ch = "number of prematurely terminated participants in CH",
    n_per_arm  = "number of enrolled participants per arm, list with group 1 and 2, define here the names of your groups as in the data",
    report_date = "# normally Sys.Date()",
    period_from = "# date of previous report",
    period_to = "# probably Sys.Date()-1",
    template = "# Word file to use as the template",
    international = "# TRUE if the trial is international",
    trial_type = "# type of trial\n\n    # VARIABLES IN data",
    var_class = "# variable containing SAE class. Options allowed are 'SAE', 'SADR', 'SUSAR'",
    var_sae_n = "# variable containing SAE ID",
    var_part_id = "# variable containing participant ID",
    var_age = "# variable containing participant age",
    var_sex = "# variable containing participant sex",
    var_country = "# variable containing participants country",
    var_site = "# variable containing participants site",
    var_sae = "# variable containing SAE type (description)",
    var_date_onset = "# variable containing the date the SAE occurred",
    var_trt = "# variable containing the treatment used to alleviate the SAE",
    var_date_trt_start = "# variable containing the date that the treatment started",
    var_date_trt_stop = "# variable containing the date that the treatment stopped",
    var_outcome = "# variable containing the outcome of the SAE",
    var_comment = "# variable containing any comment",
    var_relation = "# variable containing the relationship to randomized intervention",
    var_expected = "# variable saying whether the SAE was expected",
    var_devdef = "# variable containing whether the SAE is a device deficiency (MD trials only)",
    var_devattr = "# variable containing whether the SAE is attributable to the device (MD trials only)",
    var_devint = "# variable containing whether the SAE is attributable to an intervention in the trial",
    var_safetymeasure = "# variable containing whether the SAE required safety related measures",
    var_tx = "# variable indicating the intervention group. If provided, this variable will be included in the line listing")

  args <- lapply(1:length(args), function(x){
    arg <- names(args)[x]
    helptext <- args_helpers[[arg]]
    paste(arg, "=", deparse(args[[x]]), helptext)
  }) |>
    paste(collapse = ", \n    ")

  context <- getSourceEditorContext()
  id <- context$id
  insertText(text = paste0("asr(\n    ", args, "\n)"), id = id)

}
