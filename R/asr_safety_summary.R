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
#' @importFrom tibble tribble
#' @importFrom glue glue
#'
#' @examples
#' data(asr_sae)
#' # IMP
#' prepped <- asr_dataprep(asr_sae,
#'                         period_from = as.Date("2020-10-10"),
#'                         period_to = as.Date("2021-10-10"))
#' asr_safety_summary(data = prepped$data, period_data = prepped$period_data,
#'                    "imp", 60)
#'
#' # medical devices
#' prepped <- asr_dataprep(asr_sae, period_from = as.Date("2020-10-10"),
#'                         period_to = as.Date("2021-10-10"), trial_type = "m")
#' summ <- asr_safety_summary(data = prepped$data,
#'                            period_data = prepped$period_data, "m", 60)
#'
#' # other trial
#' asr_safety_summary(data = prepped$data, period_data = prepped$period_data, "o", 60)
asr_safety_summary <- function(data, period_data, trial_type, n_pat_e){

  n_pat_e <- as.numeric(n_pat_e)

  if(!attr(period_data, "asr") == "prepped") stop("has period_data been prepped by `asr_dataprep`?")
  if(!attr(data, "asr") == "prepped") stop("has data been prepped by `asr_dataprep`?")

  trial_type <- match.arg(trial_type, c("imp", "medical device", "other","trp"))

  if(trial_type == "imp"){
    # FOR IMP TRIALS

    tab <- table(data$sae[data$related == TRUE])
    tab <- sort(tab, decreasing = TRUE)[1:3]
    most_freq <- paste0(names(tab), " (", tab, ")", collapse = ", ")

    txt <- c(glue("IMP (or transplant products):"),
             glue("During the reporting period, {length(unique(period_data$record_id))} of {n_pat_e} participants ({sprintf('%1.1f', length(unique(period_data$record_id))/n_pat_e*100)} %) reported a total of {nrow(period_data)} serious adverse events (SAEs)."),
             glue("{sum(period_data$related)} of {nrow(period_data)} SAEs ({sprintf('%1.1f', sum(period_data$related)/nrow(period_data)*100)} %) were classified 'related' to the IMP. These events are called Serious Adverse Drug Reaction (SADR)."),
             glue("Most frequent related SAEs (i.e., SADR.) documented were {most_freq}."),
             glue("{sum(period_data$class == 'SUSAR')} Suspected Unexpected Serious Adverse Reactions (SUSARs) occurred during the reporting period, which have been notified to the Swiss competent authorities."))

    tab <- tribble(
      ~desc, ~fatal, ~sae, ~sadr, ~susar,
      # row 1
      'Number of cases (during reporting period)',
      sum(period_data$outcome == "Fatal"),
      sum(period_data$outcome != "Fatal"),
      sum(period_data$class == "SADR"),
      sum(period_data$class == "SUSAR"),
      # row 2
      'Number of cases (cumulative) since the start of the clinical trial',
      sum(data$outcome == "Fatal"),
      sum(data$outcome != "Fatal"),
      sum(data$class == "SADR"),
      sum(data$class == "SUSAR")
    )

    tab_map <- data.frame(
      col_key = c("desc", "fatal", "sae", "sadr", "susar"),
      name = c('IMP', 'Serious Adverse Events, SAEs, with fatal outcome', 'Other Serious Adverse Events, non-fatal SAEs',
               'Serious Adverse Drug Reactions, SADRs (only for IMPs)',
               'Suspected Unexpected Serious Adverse Reactions, SUSARs (only for IMPs)'),
      stringsAsFactors = FALSE)

  }
  if(trial_type == "medical device"){
    # FOR MEDICAL DEVICES

    tab <- table(data$sae)
    tab <- sort(tab, decreasing = TRUE)[1:3]
    most_freq <- paste0(names(tab), " (", tab, ")", collapse = ", ")

    txt <- c(glue("MD or IVD devices:"),
             glue("During the reporting period, a total of {nrow(period_data)} serious adverse events (SAEs) have been reported."),
             glue("{sum(period_data$related)} out of {nrow(period_data)} SAEs ({sprintf('%1.1f', sum(period_data$related)/{nrow(period_data)}*100)} %) were classified ''related'' to the MD or to an intervention (procedure) undertaken in the clinical trial. Such events are also defined Serious Adverse Device Effects (SADE)."),
             glue("In {sum(data$dev_attr)} of {nrow(data)} SAEs ({sprintf('%1.1f', sum(data$dev_attr)/nrow(data)*100)} %) it cannot be excluded that the events are attributable to the medical device under investigation."),
             glue("In {sum(data$dev_int)} of {nrow(data)} SAEs ({sprintf('%1.1f', sum(data$dev_int)/nrow(data)*100)} %) it cannot be excluded that the events are attributable to an intervention undertaken in the clinical trial."),
             glue("The most frequent SAEs documented were {most_freq}."),
             glue("Occurrence of SAE in the trial arm versus control arm (if applicable)."),
             glue("With respect to the expectedness of the event, {sum(data$expected)} ({sprintf('%1.1f', sum(data$expected)/nrow(data)*100)} %) of the SADEs were expected/anticipated and {sum(!data$expected)} ({sprintf('%1.1f', sum(!data$expected)/nrow(data)*100)} %) were classified as unexpected/unanticipated."),
             glue("{sum(data$devdef)} device deficiencies were observed (includes malfunctions, use errors, inadequacies in the information supplied by the manufacturer including labelling)."),
             glue("[tbd] out of {sum(data$devdef)} device deficiencies ({sprintf('%1.1f', sum(data$devdef)/sum(data$devdef)*100)} %) could have led to serious adverse events if suitable action had not been taken, intervention had not been made, or circumstances had been less fortunate (device deficiencies with a SAE potential)."),
             glue("{sum(data$safetymeasure)} health hazards that required safety-related measures occurred."),
             glue("Safety and protective measures taken by the investigator/sponsor (including those requested by the ethics committee and Swissmedic and authorities abroad) taken in Switzerland and abroad: [free text]"))

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
      name = c('MD/IVD Device', 'Fatal cases', 'Serious Adverse Events, SAEs'),
      stringsAsFactors = FALSE)

  }
  if(trial_type == "other"){
    # FOR OTHER TRIALS
    tab <- table(data$sae)
    tab <- sort(tab, decreasing = TRUE)[1:3]
    most_freq <- paste0(names(tab), " (", tab, ")", collapse = ", ")

    txt <- c(glue("During the reporting period, {length(unique(period_data$record_id))} of {n_pat_e} participants ({sprintf('%1.1f', length(unique(period_data$record_id))/n_pat_e*100)} %) reported a total of {nrow(period_data)} serious adverse events (SAEs) with possible relationship to the study intervention)."),
             glue("The most frequent documented SAEs with possible relationship to the intervention were {most_freq}."))

    tab <- tribble(
      ~desc, ~fatal, ~sae,
      # row 1
      'Number of cases (during reporting period)',
      sum(period_data$outcome == "Fatal" & period_data$related==1),
      sum(period_data$outcome != "Fatal" & period_data$related==1),
      # row 2
      'Number of cases (cumulative) since the start of the clinical trial',
      sum(data$outcome == "Fatal" & period_data$related==1),
      sum(data$outcome != "Fatal" & period_data$related==1),
    )

    tab_map <- data.frame(
      col_key = c("desc", "fatal", "sae"),
      name = c('Other clinical trial', 'SAEs with fatal outcome where a causality to the intervention cannot be excluded',
               'Other SAEs where a causality to the intervention cannot be excluded'),
      stringsAsFactors = FALSE)

  }
  if(trial_type == "trp"){
    # FOR OTHER TRIALS
    tab <- table(data$sae)
    tab <- sort(tab, decreasing = TRUE)[1:3]
    most_freq <- paste0(names(tab), " (", tab, ")", collapse = ", ")

    txt <- c(glue("During the reporting period, a total of {nrow(period_data)} Serious Adverse Events (SAEs) occurred."),
             glue("{sum(period_data$related)} of {nrow(period_data)} SAEs ({sprintf('%1.1f', sum(period_data$related)/nrow(period_data)*100)} %) were classified as Serious Adverse Drug Reaction (SADR) i.e., serious adverse events with possible relationship to the TrP/GT/GMO administered."),
             glue("The most frequent SADR documented were {most_freq}."),
             glue("In [xx] out of {sum(period_data$class == 'SADR')} SADRs ({sprintf('%1.1f', sum(period_data$related)/sum(period_data$class == 'SADR')*100)} %) it cannot be excluded that the events are attributable to the TrP/GT/GMO itself."),
             glue("In [xx] out of {sum(period_data$class == 'SADR')} SADRs ({sprintf('%1.1f', sum(period_data$related)/sum(period_data$class == 'SADR')*100)} %) it cannot be excluded that the events are attributable to other factors like quality defects, contaminations, administration and preparation procedures of TrP/GT/GMO, etc."),
             glue("{sum(data$class == 'SUSAR')} Suspected Unexpected Serious Adverse Reactions (SUSARs) occurred during the reporting period."),
             glue("Other new relevant safety aspects (including details regarding exposure): "))

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

