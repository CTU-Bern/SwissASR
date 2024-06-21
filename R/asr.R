

#
#
# wdfile <- system.file("extdata/clino_annual_safety_report_bm.docx", package = "CTUtemplate")
#
#
# trial_title <- "My trial name"
# protocol_number <- "v1399"
# basec_number <- "whatever it is"
# snctp_number <- "SNCTP"
# swissmedic_number <- "SM number"
# ec_name <- "KEK Bern"
# tr_number <- "FOPH number"
# product_name <- "my drug"
# sponsor_contact <- "Me, My number, me@email.com"
# inst_name_address <- "me, my address"
# n_centers_t <- 20 # total
# n_centers_p <- 30 #planned
# n_centers_c <- 0 #closed
# n_centers_o <- 20 #open
# n_pat_t <- 16000 #target
# n_pat_e <- 4956 #enrolled
# n_pat_c <- 3678 #complete
# n_pat_p <- 21 #prematurely terminated
#
#
# last_report <- as.Date("2020-11-01")
#
# var_class <- "class" #sae/sadr/susar
# var_sae_n <- "sae_n" #sae ID
# var_part_id <- "record_id" #participant ID
# var_age <- "age"
# var_sex <-"sex"
# var_country <- "country"
# var_site <- "site"
# var_sae <- "sae"
# var_date_onset <- "sae_date"
# var_date_trt_start <- "sae_trtstart"
# var_date_trt_start <- "sae_trtstop"
# var_outcome <- "outcome"
# var_comment <- "comment"
#
#
# report_date <- format(Sys.Date(), format = "%d/%m/%Y")
# period_from <- "start_date"
# period_to <- "to_date"
#
#
#
#
#
# # prepare table specific variables
#
#
# asr(sae_data)
# asr(sae_data, var_class = "sae_type")
#
# with(sae_data, asr(class = sae_type, ))



#' Fill the Annual Safety Report template
#'
#' This function fills out the SwissEthics annual safety report template with a
#' much as possible. Minor (formatting) changes will still be required after generation of the
#' report.
#'
#' Use of the \code{var_tx} argument results in the indicated variable being included in the line listing.
#'
#' @param data dataframe containing safety data
#' @param target filename to save the report to
#' @param trial_title name of the trial
#' @param protocol_number trial code/protocol number
#' @param basec_number BASEC number
#' @param snctp_number SNCTP number
#' @param swissmedic_number Swissmedic number
#' @param ec_name EC name (Lead EC and/concerned EC)
#' @param tr_number Number for Transplantation Clinical Trials (FOPH number)
#' @param product_name product name or intervention
#' @param sponsor_contact contact details of sponsor(-investigator)
#' @param inst_name_address name and address of institute
#' @param n_centers_t total number of participating centres
#' @param n_centers_p planned number of participating centres
#' @param n_centers_c number of closed centres
#' @param n_centers_o number of open centres
#' @param n_pat_t target number of participants
#' @param n_pat_e number of enrolled participants
#' @param n_pat_c number of completed participants
#' @param n_pat_p number of prematurely terminated participants
#' @param n_centers_t_ch total number of participating centres in CH
#' @param n_centers_p_ch planned number of participating centres in CH
#' @param n_centers_c_ch number of closed centres in CH
#' @param n_centers_o_ch number of open centres in CH
#' @param n_pat_t_ch target number of participants in CH
#' @param n_pat_e_ch number of enrolled participants in CH
#' @param n_pat_c_ch number of completed participants in CH
#' @param n_pat_p_ch number of prematurely terminated participants in CH
#' @param n_per_arm number of enrolled participants per arm, list with group 1 and 2, define here the names of your groups as in the data
#' @param report_date report date
#' @param period_from start of reporting period
#' @param period_to end of reporting period
#' @param template path to template file
#' @param international internation or national trial (logical)
#' @param trial_type one of \code{imp}, \code{medical device}, \code{other}. Abbreviations OK.
#' @param var_class variable containing SAE class. Options allowed are "SAE", "SADR", "SUSAR"
#' @param var_sae_n variable containing SAE ID
#' @param var_part_id variable containing participant ID
#' @param var_age variable containing participant age
#' @param var_sex variable containing participant sex
#' @param var_country variable containing participants country
#' @param var_site variable containing participants site
#' @param var_sae variable containing SAE type (description)
#' @param var_date_onset variable containing the date the SAE occured
#' @param var_trt variable containing the treatment used to alleviate the SAE
#' @param var_date_trt_start variable containing the date that the treatment started
#' @param var_date_trt_stop variable containing the date that the treatment ended
#' @param var_outcome variable containing the outcome of the SAE
#' @param var_comment variable containing any comment
#' @param var_relation variable containing the relationship to randomized intervention
#' @param var_expected variable saying whether the SAE was expected
#' @param var_devdef variable containing whether the SAE is a device deficiency
#' @param var_devattr variable containing whether the SAE is attributable to the device
#' @param var_devint variable containing whether the SAE is attributable to an intervention in the trial
#' @param var_safetymeasure variable containing whether the SAE required safety related measures
#' @param var_tx variable indicating the intervention group. If provided, this variable will be included in the line listing
#'
#' @return nothing in R, creates a docx file in the \code{target} location
#' @export
#' @import officer
#' @import flextable
#' @importFrom magrittr %>%
#' @importFrom tibble tribble
#' @importFrom glue glue
#' @importFrom stringr str_to_lower str_to_sentence
#' @importFrom doconv docx_update
#' @examples
#'
#' data(asr_sae)
#' file <- tempfile("asr", fileext = ".docx")
#' asr(asr_sae, file)
#'
#' # # more usual use will require passing more information:
#' # asr(asr_sae, file,
#' #     # trial info
#' #     trial_title = "Example Trial Name",
#' #     protocol_number = "20221002130",
#' #     basec_number = "",
#' #     snctp_number = "202200458",
#' #     swissmedic_number = "....",
#' #     ec_name = "Kantonale Ethikskommision Bern",
#' #     tr_number = "",
#' #     product_name = "Drug name",
#' #     international = FALSE,
#' #     trial_type = "imp",
#' #     # Sponsor info
#' #     sponsor_contact = "Sponsor name, Sponsor phone number, Sponsor email",
#' #     inst_name_address = "Institute name, Institute address",
#' #     # site info
#' #     n_centers_t = 20,        # total number
#' #     n_centers_p = "default", # planned
#' #     n_centers_c = "default", # closed
#' #     n_centers_o = "default", # open
#' #     # participant info
#' #     n_pat_t = 1000,          # target
#' #     n_pat_e = 300,           # enrolled
#' #     n_pat_c = 0,             # complete
#' #     n_pat_p = 0,             # prematurely terminated
#' #     # report info
#' #     report_date = format(Sys.Date(), format = "%d/%m/%Y"),
#' #     period_from = as.Date("2020-11-02"),
#' #     period_to = as.Date("2020-11-17"),
#' #     # variable mapping
#' #     var_class = "class",
#' #     var_sae_n = "sae_n", #sae ID
#' #     var_part_id = "record_id", #participant ID
#' #     var_age = "age",
#' #     var_sex ="sex",
#' #     var_country = "country",
#' #     var_site = "site",
#' #     var_sae = "sae",
#' #     var_date_onset = "sae_date",
#' #     var_trt = "trt",
#' #     var_date_trt_start = "sae_trtstart",
#' #     var_date_trt_stop = "sae_trtstop",
#' #     var_outcome = "outcome",
#' #     var_comment = "comment",
#' #     var_relation = "related",
#' #     var_expected = "expected",
#' #     var_safetymeasure = "safetymeasure"
#' #     )
#'
#'
#'

asr <- function(data,
                target = "tmp.docx",
                trial_title = "TRIAL NAME"
                , protocol_number = "default"
                , basec_number = "default"
                , snctp_number = "default"
                , swissmedic_number = "default"
                , ec_name = "default"
                , tr_number = "default"
                , product_name = "default"
                , sponsor_contact = "default name, default number, default email"
                , inst_name_address = "default name, default address"
                , n_centers_t = "default" # total
                , n_centers_p = "default" #planned
                , n_centers_c = "default" #closed
                , n_centers_o = "default" #open
                , n_pat_t = 500 #target
                , n_pat_e = 300 #enrolled
                , n_pat_c = 100 #complete
                , n_pat_p = 15 #prematurely terminated
                , n_centers_t_ch = "default" # total in CH
                , n_centers_p_ch = "default" #planned in CH
                , n_centers_c_ch = "default" #closed in CH
                , n_centers_o_ch = "default" #open in CH
                , n_pat_t_ch = 500 #target in CH
                , n_pat_e_ch = 300 #enrolled in CH
                , n_pat_c_ch = 100 #complete in CH
                , n_pat_p_ch = 15 #prematurely terminated in CH
                , n_per_arm = c(grp1 = 150, grp2 = 150) #number of patients per arm, use NA if study is blinded
                , report_date = format(Sys.Date(), format = "%d/%m/%Y")
                , period_from = as.Date("2020-11-02")
                , period_to = as.Date("2020-11-17")
                , template = system.file("extdata/clino_annual_safety_report_fm.docx", package = "SwissASR")
                , international = FALSE
                , trial_type = "imp"
                # dataframe variables
                , var_class = "class"
                , var_sae_n = "sae_n" #sae ID
                , var_part_id = "record_id" #participant ID
                , var_age = "age"
                , var_sex ="sex"
                , var_country = "country"
                , var_site = "site"
                , var_sae = "sae"
                , var_date_onset = "sae_date"
                , var_trt = "trt"
                , var_date_trt_start = "sae_trtstart"
                , var_date_trt_stop = "sae_trtstop"
                , var_outcome = "outcome"
                , var_comment = "comment"
                , var_relation = "related"
                , var_expected = "expected"
                , var_devdef = "devdef"
                , var_devattr = "devattr"
                , var_devint = "devint"
                , var_safetymeasure = "safetymeasure"
                , var_tx = NULL
){

  # general housekeeping ----
  trial_type <- match.arg(trial_type, c("imp", "medical device", "other"))

  # ## all line listing variables exist ----
  # if(!all(c(var_class, var_sae_n, var_part_id, var_age, var_sex,
  #           var_country, var_site, var_sae, var_trt, var_date_onset,
  #           var_date_trt_start, var_date_trt_stop, var_outcome,
  #           var_comment) %in% names(data))) stop("one or more variables for line listing missing")
  #
  # ## rename variables for easier code ----
  # names(data)[names(data) == var_class] <- "class"
  # names(data)[names(data) == var_sae_n] <- "sae_n"
  # names(data)[names(data) == var_part_id] <- "record_id"
  # names(data)[names(data) == var_age] <- "age"
  # names(data)[names(data) == var_sex] <- "sex"
  # if(international) names(data)[names(data) == var_country] <- "country"
  # names(data)[names(data) == var_site] <- "site"
  # names(data)[names(data) == var_sae] <- "sae"
  # names(data)[names(data) == var_date_onset] <- "sae_date"
  # names(data)[names(data) == var_date_trt_start] <- "sae_trtstart"
  # names(data)[names(data) == var_date_trt_stop] <- "sae_trtstop"
  # names(data)[names(data) == var_outcome] <- "outcome"
  # names(data)[names(data) == var_comment] <- "comment"
  # names(data)[names(data) == var_relation] <- "related"
  # names(data)[names(data) == var_trt] <- "trt"
  #
  # ## relevant variables exist
  # if(trial_type == "imp"){
  #   ## relatedness ----
  #   check_fac_char_log_int(data$related)
  #   if(is.character(data$related) | is.factor(data$related)){
  #     if(!all(str_to_lower(data$related) %in% c("yes", "no")))
  #       stop("unexpected 'related' types encountered (allowed are TRUE, FALSE, yes, no, 0, 1)")
  #     data$related <- str_to_lower(as.character(data$related)) == "yes"
  #   }
  #   if(is.integer(data$related)){
  #     if(!all(data$related %in% 0:1))
  #       stop("unexpected 'related' types encountered (allowed are TRUE, FALSE, yes, no, 0, 1)")
  #     data$related <- data$related == 1
  #   }
  #   related_data <- data[data$related, ]
  #
  # }
  # if(trial_type == "medical device"){
  #   names(data)[names(data) == var_expected] <- "expected"
  #   names(data)[names(data) == var_devdef] <- "devdef"
  #   names(data)[names(data) == var_devattr] <- "devattr"
  #   names(data)[names(data) == var_devint] <- "devint"
  #   names(data)[names(data) == var_safetymeasure] <- "safetymeasure"
  #
  #   vars <- c("expected", "devdef", "devattr", "devint", "safetymeasure")
  #   if(!all(vars %in% names(data))){
  #     stop(paste(vars[!vars %in% names(data)], collapse = ", "), " not found in data")
  #   }
  #
  #   ## safetymeasure ----
  #   check_log(data$safetymeasure)
  # }
  # if(trial_type == "other"){
  #
  # }
  #
  #
  # ## class ----
  # check_fac_char(data$class)
  # if(!all(data$class %in% c("SAE", "SADR", "SUSAR")))
  #   stop("unexpected SAE types encountered (allowed are SAE, SADR and SUSAR)")
  # data$class <- as.character(data$class)
  #
  # ## age/sex ----
  # if(!"numeric" %in% class(data$age))
  #   stop("'age' should be numeric")
  # check_fac_char(data$sex)
  # data$sex <- as.character(data$sex)
  # ## recode
  # data$sex[grepl("^[WwFf]", data$sex)] <- "F"
  # data$sex[grepl("^[Mm]", data$sex)] <- "M"
  # if(!all(data$sex %in% c("M", "F")))
  #   stop("unexpected sexes encountered")
  # data$agesex <- glue("{sprintf('%1.0f', data$age)}/{data$sex}")
  #
  # ## country/site ----
  # check_fac_char(data$site)
  # data$site <- as.character(data$site)
  # if(international){
  #   check_fac_char(data$country)
  #   data$country <- as.character(data$country)
  #   data$country_site <- glue("{data$country}, {data$site}")
  # } else {
  #   data$country_site <- glue("{data$site}")
  # }
  #
  # ## dates ----
  # check_date(data$sae_date)
  # check_date(data$sae_trtstart)
  # check_date(data$sae_trtstop)
  # check_date(period_from)
  # check_date(period_to)
  #
  #
  # period_data <- subset(data, sae_date >= period_from & sae_date <= period_to)
  #
  #
  # data$sae_date <- format(data$sae_date, format = "%d/%m/%Y")
  # data$sae_trtstart <- format(data$sae_trtstart, format = "%d/%m/%Y")
  # data$sae_trtstop <- format(data$sae_trtstop, format = "%d/%m/%Y")
  # data$trt_dates <- ifelse(is.na(data$sae_trtstop),
  #                          glue("{data$sae_trtstart}", .na = ""),
  #                          glue("{data$sae_trtstart} - {data$sae_trtstop}", .na = ""))
  #
  # ## outcome ----
  # check_fac_char(data$outcome)
  # data$outcome <- as.character(data$outcome)
  # data$outcome <- str_to_sentence(data$outcome)
  # # if(!all(data$outcome %in% c("Resolved", "Fatal", "Improved", "Sequel", "Unknown")))
  # #   stop("unexpected outcomes encountered (Resolved, Fatal, Improved, Sequel and Unknown are allowed)")
#
#   ## comment ----
#   check_fac_char(data$comment)
#   data$comment <- as.character(data$comment)
#
#   ## sae ----
#   check_fac_char(data$sae)
#   data$sae <- as.character(data$sae)
#
#   ## trt ----
#   check_fac_char(data$trt)
#   data$trt <- as.character(data$trt)
  #
  dfs <- asr_dataprep(data,
                      period_from = period_from,
                      period_to = period_to
                      , trial_type = trial_type
                      , var_class = var_class
                      , var_sae_n = var_sae_n #sae ID
                      , var_part_id = var_part_id #participant ID
                      , var_age = var_age
                      , var_sex =var_sex
                      , var_country = var_country
                      , var_site = var_site
                      , var_sae = var_sae
                      , var_date_onset = var_date_onset
                      , var_trt = var_trt
                      , var_date_trt_start = var_date_trt_start
                      , var_date_trt_stop = var_date_trt_stop
                      , var_outcome = var_outcome
                      , var_comment = var_comment
                      , var_relation = var_relation
                      , var_expected = var_expected
                      , var_devdef = var_devdef
                      , var_devattr = var_devattr
                      , var_devint = var_devint
                      , var_safetymeasure = var_safetymeasure
                      , var_tx = var_tx)
  data <- dfs$data
  period_data <- dfs$period_data
  tx_var <- !is.null(var_tx)


  # report itself ----
  doc <- read_docx(template)



  # ..general info, details of trial ----
  #
  # for(i in c("trial_title",
  #            "protocol_number",
  #            "basec_number",
  #            "snctp_number",
  #            "swissmedic_number",
  #            "ec_name",
  #            "tr_number",
  #            "product_name",
  #            "sponsor_contact",
  #            "inst_name_address",
  #            "report_date",
  #            "period",
  #            "n_centers_t",
  #            "n_centers_p",
  #            "n_centers_c",
  #            "n_centers_o",
  #            "n_pat_t",
  #            "n_pat_e",
  #            "n_pat_c",
  #            "n_pat_p")){
  #   x <- get(i)
  #   doc <- doc %>%
  #     cursor_bookmark(i) %>%
  #     body_add_par(x, pos = "after")
  # }


  period <- glue("{period_from} to {period_to}")
## New code to insert the fields instead of bookmarks
    doc <- doc %>%
      set_doc_properties(trial_title = get("trial_title"),
                                  protocol_number = get("protocol_number"),
                                  basec_number = get("basec_number"),
                                  snctp_number = get("snctp_number"),
                                  swissmedic_number = get("swissmedic_number"),
                                  ec_name = get("ec_name"),
                                  tr_number = get("tr_number"),
                                  product_name = get("product_name"),
                                  sponsor_contact = get("sponsor_contact"),
                                  inst_name_address = get("inst_name_address"),
                                  report_date = get("report_date"),
                                  period = get("period"),
                                  n_centers_t = as.character(get("n_centers_t")),
                                  n_centers_p = as.character(get("n_centers_p")),
                                  n_centers_c = as.character(get("n_centers_c")),
                                  n_centers_o = as.character(get("n_centers_o")),
                                  n_centers_t_ch = as.character(get("n_centers_t_ch")),
                                  n_centers_p_ch = as.character(get("n_centers_p_ch")),
                                  n_centers_c_ch = as.character(get("n_centers_c_ch")),
                                  n_centers_o_ch = as.character(get("n_centers_o_ch")),
                                  n_pat_t = as.character(n_pat_t),
                                  n_pat_e = as.character(n_pat_e),
                                  n_pat_c = as.character(n_pat_c),
                                  n_pat_p = as.character(n_pat_p),
                                  n_pat_t_ch = as.character(n_pat_t_ch),
                                  n_pat_e_ch = as.character(n_pat_e_ch),
                                  n_pat_c_ch = as.character(n_pat_c_ch),
                                  n_pat_p_ch = as.character(n_pat_p_ch))


  #}  ## participant safety ----
  summ <- asr_safety_summary(data = data,
                             period_data = period_data,
                             trial_type = trial_type,
                             n_pat_e = n_pat_e,
                             n_per_arm = n_per_arm)

  # doc <- doc %>%
  #   cursor_bookmark("partsafety_text")
  # for(i in 1:length(summ$txt)){
  #   txt <- summ$txt[i]
  #   doc <- doc %>% body_add_par(txt, style = "Text")
  # }

  doc <- doc %>%
    officer::set_doc_properties(partsafety_text = paste0(summ$txt, collapse = " ") )

  doc <- doc %>%
    officer::set_doc_properties(partsafety_text_all = paste0(summ$txt_all, collapse = " ") )

  ft <- summ$tab %>%
    flextable() %>%
    set_header_df(mapping = summ$tab_map, key = "col_key") %>%
    valign(part = "head", valign = "top") %>%
    border(border = fp_border(color = "#4FB4E0"), part="all") %>%
    fontsize(size = 8, part = "all") %>%
    font(fontname = "Helvetica", part = "all") %>%
    width(1:ncol(summ$tab), (18.57/ncol(summ$tab))*.393)


  doc <- doc %>%
    cursor_bookmark("partsafety_tab") %>%
    body_add_flextable(ft)



  ## line listing ----
  if(!tx_var){
    llist <- data[, c("class", "sae_n", "record_id", "agesex", "country_site",
                      "sae", "trt", "sae_date", "trt_dates", "outcome", "comment")]
    ## llist <- llist[c(1,1:nrow(llist)), ]
    names(llist) <- c("SAE/\nSADR/\nSUSAR",
                      "Serious adverse event/ reaction No.",
                      "Participants ID",
                      "Age / Sex (F=female, M=male)",
                      "Country and site in which participant is/was enrolled (for multicentre, international trials)",
                      "AE term",
                      "Description of intervention (dosage, schedule, route, if applicable)",
                      "Date of onset",	"Date of treatment (start and stop)",
                      "Outcome (e.g. resolved, fatal, improved, sequel, unknown)",
                      "Comments, if relevant (e.g. causality assessment, relationship)"
    )
  } else {
    llist <- data[, c("class", "sae_n", "record_id", "intervention", "agesex", "country_site",
                      "sae", "trt", "sae_date", "trt_dates", "outcome", "comment")]
    ## llist <- llist[c(1,1:nrow(llist)), ]
    names(llist) <- c("SAE/\nSADR/\nSUSAR",
                      "Serious adverse event/ reaction No.",
                      "Participants ID",
                      "Intervention",
                      "Age / Sex (F=female, M=male)",
                      "Country and site in which participant is/was enrolled (for multicentre, international trials)",
                      "AE term",
                      "Description of intervention (dosage, schedule, route, if applicable)",
                      "Date of onset",
                      "Date of treatment (start and stop)",
                      "Outcome (e.g. resolved, fatal, improved, sequel, unknown)",
                      "Comments, if relevant (e.g. causality assessment, relationship)"
    )
  }
  llist_ft <- flextable(llist) %>%
    #add_header_lines("Line listing of SAEs, SADRs and SUSARs, including international cases
#(code and version of used standard (e.g. MedDRA or CTCAE) should be indicated, details on SUSARs will be attached as appendices)") %>%
    border(border = fp_border(color = "#4FB4E0"), part="all") %>%
    fontsize(size = 8, part = "all") %>%
    font(fontname = "Helvetica", part = "all")


  doc <- doc %>%
    cursor_bookmark("llist") %>%
    body_add_flextable(llist_ft)

  print(doc, target = target)


   doc <- doc %>%
     doc_properties()

  docx_update(input = target)

}







