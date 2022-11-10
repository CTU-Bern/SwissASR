#' Summarize safety data
#'
#' This function creates the text and summary table for the report.
#'
#' @param data SAE data
#' @param period_data SAE data restricted to a specific period
#' @param trial_type trial type (\code{imp}, \code{medical device} or \code{other})
#' @param n_pat_e Number of enrolled participants
#'
#' @return
#' \describe{
#'   \item{txt}{The sentences required for the report}
#'   \item{tab}{The summary table required for the report}
#'   \item{tab_map}{Mapping between the variable name in \code{tab} and a nicer label}
#' }
#' @export
#'
#' @examples
#' data(asr_sae)
#' # IMP
#' prepped <- asr_dataprep(asr_sae, period_from = as.Date("2020-10-10"), period_to = as.Date("2021-10-10"))
#' asr_safety_summary(data = prepped$data, period_data = prepped$period_data, "imp", 60)
#' # medical devices
#' prepped <- asr_dataprep(asr_sae, period_from = as.Date("2020-10-10"), period_to = as.Date("2021-10-10"), trial_type = "m")
#' summ <- asr_safety_summary(data = prepped$data, period_data = prepped$period_data, "m", 60)
#' doc <- read_docx()
#' for(i in 1:length(summ$txt)){
#'   txt <- summ$txt[i]
#'   doc <- doc %>% body_add_par(txt, style = "Text")
#' }
#' # other trial
#' asr_safety_summary(data = prepped$data, period_data = prepped$period_data, "o", 60)
asr_safety_summary <- function(data, period_data, trial_type, n_pat_e){

  if(!attr(period_data, "asr") == "prepped") stop("has period_data been prepped by `asr_dataprep`?")
  if(!attr(data, "asr") == "prepped") stop("has data been prepped by `asr_dataprep`?")

  trial_type <- match.arg(trial_type, c("imp", "medical device", "other"))

  if(trial_type == "imp"){
    # FOR IMP TRIALS

    tab <- table(data$sae[data$related == TRUE])
    tab <- sort(tab, decreasing = TRUE)[1:3]
    most_freq <- paste0(names(tab), " (", tab, ")", collapse = ", ")

    txt <- c(glue("During the reporting period, {length(unique(period_data$record_id))} of {n_pat_e} participants ({sprintf('%1.1f', length(unique(period_data$record_id))/n_pat_e*100)} %) reported a total of {nrow(period_data)} serious adverse events (SAEs)."),
             glue("{sum(data$related)} of {nrow(data)} SAEs ({sprintf('%1.1f', sum(data$related)/nrow(data)*100)} %) were classified 'related' to the IMP. The most frequent related SAEs documented were {most_freq}."),
             glue("{sum(data$class == 'SUSAR')} Suspected Unexpected Serious Adverse Reactions (SUSARs) occurred during the reporting period, which have been notified to the Swiss competent authorities."))

    tab <- tribble(
      ~desc, ~fatal, ~sae, ~sadr, ~susar,
      # row 1
      'Number of cases (during reporting period)',
      sum(period_data$outcome == "Fatal"),
      nrow(period_data),
      sum(period_data$class == "SADR"),
      sum(period_data$class == "SUSAR"),
      # row 2
      'Number of cases (cumulative) since the start of the clinical trial',
      sum(data$outcome == "Fatal"),
      nrow(data),
      sum(data$class == "SADR"),
      sum(data$class == "SUSAR")
    )

    tab_map <- data.frame(
      col_key = c("desc", "fatal", "sae", "sadr", "susar"),
      name = c('', 'Fatal cases', 'Serious Adverse Events, SAEs', 'Serious Adverse Drug Reactions, SADRs (only for IMPs)', 'Suspected Unexpected Serious Adverse Reactions, SUSARs (only for IMPs)'),
      stringsAsFactors = FALSE)

  }
  if(trial_type == "medical device"){
    # FOR MEDICAL DEVICES

    tab <- table(data$sae)
    tab <- sort(tab, decreasing = TRUE)[1:3]
    most_freq <- paste0(names(tab), " (", tab, ")", collapse = ", ")

    txt <- c(glue("During the reporting period, a total of {nrow(period_data)} serious adverse events (SAEs) have been reported."),
             glue("In {sum(data$dev_attr)} of {nrow(data)} SAEs ({sprintf('%1.1f', sum(data$dev_attr)/nrow(data)*100)} %) it cannot be excluded that the events are attributable to the medical device under investigation."),
             glue("In {sum(data$dev_int)} of {nrow(data)} SAEs ({sprintf('%1.1f', sum(data$dev_int)/nrow(data)*100)} %) it cannot be excluded that the events are attributable to an intervention undertaken in the clinical trial."),
             glue("The most frequent SAEs documented were {most_freq}."),
             glue("With respect to the expectedness of the event, {sum(data$expected)} ({sprintf('%1.1f', sum(data$expected)/nrow(data)*100)} %) of the SAEs were expected and {sum(!data$expected)} ({sprintf('%1.1f', sum(!data$expected)/nrow(data)*100)} %) were classified as unexpected."),
             glue("{sum(data$devdef)} device deficiencies were observed."),
             glue("{sum(data$safetymeasure)} health hazards that required safety-related measures occurred."))

    tab <- tribble(
      ~desc, ~fatal, ~sae,
      # row 1
      'Number of cases (during reporting period)',
      sum(period_data$outcome == "Fatal"),
      nrow(period_data),
      # row 2
      'Number of cases (cumulative) since the start of the clinical trial',
      sum(data$outcome == "Fatal"),
      nrow(data)
    )

    tab_map <- data.frame(
      col_key = c("desc", "fatal", "sae"),
      name = c('', 'Fatal cases', 'Serious Adverse Events, SAEs'),
      stringsAsFactors = FALSE)

  }
  if(trial_type == "other"){
    # FOR OTHER TRIALS
    tab <- table(data$sae)
    tab <- sort(tab, decreasing = TRUE)[1:3]
    most_freq <- paste0(names(tab), " (", tab, ")", collapse = ", ")

    txt <- c(glue("During the reporting period, {length(unique(period_data$record_id))} of {n_pat_e} participants ({sprintf('%1.1f', length(unique(period_data$record_id))/n_pat_e*100)} %) reported a total of {nrow(period_data)} serious adverse events (SAEs; with possible relationship to the intervention)."),
             glue("The most frequent SAEs documented were {most_freq}."))

    tab <- tribble(
      ~desc, ~fatal, ~sae,
      # row 1
      'Number of cases (during reporting period)',
      sum(period_data$outcome == "Fatal"),
      nrow(period_data),
      # row 2
      'Number of cases (cumulative) since the start of the clinical trial',
      sum(data$outcome == "Fatal"),
      nrow(data)
    )

    tab_map <- data.frame(
      col_key = c("desc", "fatal", "sae"),
      name = c('', 'Fatal cases', 'Serious Adverse Events, SAEs'),
      stringsAsFactors = FALSE)

  }

  return(
    list(
      txt = txt,
      tab = tab,
      tab_map = tab_map
    ))

}

