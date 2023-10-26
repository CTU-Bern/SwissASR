


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
# n <- 20
# msample <- function(x, ...) sample(x, n, TRUE, ...)
# sae_data <- data.frame(sae_date = last_report + msample(-20:20),
#     record_id = msample(1:300),
#     age = round(runif(n, 45, 75)),
#     sex = factor(msample(c(c("M", "F")))),
#     country = factor(msample("CH")),
#     site = factor(msample(letters[1:15])),
#     sae = factor(msample(c("headache", "diarhea",
#     "spontaneous unconsciousness", "death", "poor sleep"))),
#     intervention = factor(msample(c("grp1", "grp2"))),
#     outcome = factor(msample(c("resolved", "fatal", "improved",
#     "sequel", "unknown"))),
#     comment = msample(c("", "fribble", "foo", "bar", "foobar")),
#     trt = msample(c("", "trt1", "trt2", "trt3", "trt4")),
#     class = msample(c("SAE", "SUSAR", "SADR")),
#     expected = msample(c(TRUE, FALSE)),
#     devdef = msample(c(TRUE, FALSE)),
#     devattr = msample(c(TRUE, FALSE)),
#     devdef = msample(c(TRUE, FALSE)),
#     devint = msample(c(TRUE, FALSE)),
#     safetymeasure = msample(c(TRUE, FALSE))
# )
# sae_data <- sae_data[order(sae_data$sae_date), ]
# sae_data$sae_n <- 1:nrow(sae_data)
# sae_data$sae_trtstop <- as.Date(ifelse(sae_data$outcome %in% c("resolved", "fatal") &
#   sae_data$trt != "", sae_data$sae_date + runif(2:10, 1), NA), origin = "1970-01-01")
# sae_data$sae_trtstart <- as.Date(ifelse(sae_data$trt != "", sae_data$sae_date + 1, NA),
#   origin = "1970-01-01")
# sae_data$related <- rbinom(n, 1, .25)
# sae_data$sae_date
#
# x <- asr_dataprep(sae_data, period_from = as.Date("2020-10-10"), period_to = as.Date("2020-12-31"))

# data <- sae_data
#
#
#
#
#
# # prepare table specific variables
#


#' Prepare data for the \code{asr} function
#' This function allows the use of the \code{asr_safety_summary} function outside
#' of the normal \code{asr} function, which might be desirable to get an overview
#' of the information in a different format to MS Word.
#' @inheritParams asr
#' @return a list of 2 dataframes (\code{data} containing all data submitted to
#' the function and \code{period_data} which contains only the data for the relevant
#' period) with variables renamed to those expected by \code{asr_safety_summary}
#'
#' @export
#' @importFrom glue glue
#' @importFrom stringr str_to_sentence
#' @examples
#' data(asr_sae)
#' # IMP
#' asr_dataprep(asr_sae, period_from = as.Date("2020-10-10"), period_to = as.Date("2021-10-10"))
#' asr_dataprep(asr_sae, period_from = min(asr_sae$sae_date), period_to = Sys.Date())
asr_dataprep <- function(data
                         , trial_type = "imp"
                         , international = FALSE
                         , period_from = NA
                         , period_to = NA
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
                         , var_tx = NULL){

  # general housekeeping ----
  trial_type <- match.arg(trial_type, c("imp", "medical device", "other"))

  ## all line listing variables exist ----
  listvars <- c(var_class, var_sae_n, var_part_id, var_age, var_sex,
            var_country, var_site, var_sae, var_trt, var_date_onset,
            var_date_trt_start, var_date_trt_stop, var_outcome,
            var_comment)
  if(any(listvars == "")){
    stop("variable names (var_*) cannot be blank (ie '')")
  }
  if(!all(listvars %in% names(data))){
    stop(
      paste(
        paste(listvars[!listvars %in% names(data)], collapse = ", "),
        "variables missing for line listing"
        )
    )}
  tx_var <- !is.null(var_tx)
  if(tx_var){
    if(!var_tx %in% names(data)){
      stop(
        paste(
          var_tx,
          "variable missing for line listing"
        )
      )
    }
  }

  ## rename variables for easier code ----
  names(data)[names(data) == var_class] <- "class"
  names(data)[names(data) == var_sae_n] <- "sae_n"
  names(data)[names(data) == var_part_id] <- "record_id"
  names(data)[names(data) == var_age] <- "age"
  names(data)[names(data) == var_sex] <- "sex"
  if(international) names(data)[names(data) == var_country] <- "country"
  names(data)[names(data) == var_site] <- "site"
  names(data)[names(data) == var_sae] <- "sae"
  names(data)[names(data) == var_date_onset] <- "sae_date"
  names(data)[names(data) == var_date_trt_start] <- "sae_trtstart"
  names(data)[names(data) == var_date_trt_stop] <- "sae_trtstop"
  names(data)[names(data) == var_outcome] <- "outcome"
  names(data)[names(data) == var_comment] <- "comment"
  names(data)[names(data) == var_relation] <- "related"
  names(data)[names(data) == var_trt] <- "trt"
  if(tx_var){
    names(data)[names(data) == var_tx] <- "intervention"
  }

  ## relevant variables exist
  if(trial_type == "imp"){
    ## relatedness ----
    check_fac_char_log_int(data$related)
    if(is.character(data$related) | is.factor(data$related)){
      if(!all(str_to_lower(data$related) %in% c("yes", "no")))
        stop("unexpected 'related' types encountered (allowed are TRUE, FALSE, yes, no, 0, 1)")
      data$related <- str_to_lower(as.character(data$related)) == "yes"
    }
    if(is.integer(data$related)){
      if(!all(data$related %in% 0:1))
        stop("unexpected 'related' types encountered (allowed are TRUE, FALSE, yes, no, 0, 1)")
      data$related <- data$related == 1
    }
    related_data <- data[data$related, ]

  }

  if(trial_type == "medical device"){
    names(data)[names(data) == var_expected] <- "expected"
    names(data)[names(data) == var_devdef] <- "devdef"
    names(data)[names(data) == var_devattr] <- "devattr"
    names(data)[names(data) == var_devint] <- "devint"
    names(data)[names(data) == var_safetymeasure] <- "safetymeasure"

    vars <- c("expected", "devdef", "devattr", "devint", "safetymeasure")
    if(!all(vars %in% names(data))){
      stop(paste(vars[!vars %in% names(data)], collapse = ", "), " not found in data")
    }

    ## safetymeasure ----
    check_log(data$safetymeasure)

    # convert logicals to yes/no?
  }
  if(trial_type == "other"){

  }


  ## class ----
  check_fac_char(data$class)
  if(!all(data$class %in% c("SAE", "SADR", "SUSAR")))
    stop("unexpected SAE types encountered (allowed are SAE, SADR and SUSAR)")
  data$class <- as.character(data$class)

  ## age/sex ----
  if(!"numeric" %in% class(data$age))
    stop("'age' should be numeric")
  check_fac_char(data$sex)
  data$sex <- as.character(data$sex)
  ## recode
  data$sex[grepl("^[WwFf]", data$sex)] <- "F"
  data$sex[grepl("^[Mm]", data$sex)] <- "M"
  if(!all(data$sex %in% c("M", "F")))
    stop("unexpected sexes encountered")
  data$agesex <- glue("{sprintf('%1.0f', data$age)}/{data$sex}")

  ## country/site ----
  check_fac_char(data$site)
  data$site <- as.character(data$site)
  if(international){
    check_fac_char(data$country)
    data$country <- as.character(data$country)
    data$country_site <- glue("{data$country}, {data$site}")
  } else {
    data$country_site <- glue("{data$site}")
  }

  ## dates ----
  check_date(data$sae_date)
  check_date(data$sae_trtstart)
  check_date(data$sae_trtstop)
  # check_date(period_from)
  # check_date(period_to)


  period_data <- subset(data, data$sae_date >= as.Date(period_from) & data$sae_date <= as.Date(period_to))

  data$sae_date <- format(data$sae_date, format = "%d/%m/%Y")
  data$sae_trtstart <- format(data$sae_trtstart, format = "%d/%m/%Y")
  data$sae_trtstop <- format(data$sae_trtstop, format = "%d/%m/%Y")
  data$trt_dates <- ifelse(is.na(data$sae_trtstop),
                           glue("{data$sae_trtstart}", .na = ""),
                           glue("{data$sae_trtstart} - {data$sae_trtstop}", .na = ""))

  ## outcome ----
  check_fac_char(data$outcome)
  data$outcome <- as.character(data$outcome)
  data$outcome <- str_to_sentence(data$outcome)
  # if(!all(data$outcome %in% c("Resolved", "Fatal", "Improved", "Sequel", "Unknown")))
  #   stop("unexpected outcomes encountered (Resolved, Fatal, Improved, Sequel and Unknown are allowed)")

  ## comment ----
  check_fac_char(data$comment)
  data$comment <- as.character(data$comment)

  ## sae ----
  check_fac_char(data$sae)
  data$sae <- as.character(data$sae)

  ## trt ----
  check_fac_char(data$trt)
  data$trt <- as.character(data$trt)

  attr(data, "asr") <- "prepped"
  attr(period_data, "asr") <- "prepped"

  return(list(data = data,
              period_data = period_data))

}
