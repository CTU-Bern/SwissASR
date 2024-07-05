#' Summarize safety data
#'
#' This function creates the text and summary table for the report.
#'
#' @param data SAE data
#' @param period_data SAE data restricted to a specific period
#' @param trial_type trial type (\code{imp}, \code{medical device} or \code{other})
#' @param n_pat_e Number of enrolled participants
#' @param n_per_arm Number of participants enrolled per arm
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
#'                         period_to = as.Date("2021-10-10")
#'                         )
#' asr_safety_summary(data = prepped$data, period_data = prepped$period_data,
#'                    "imp", 60, n_per_arm = list(grp1 = 150, grp2 = 150))
#'
#' # medical devices
#' prepped <- asr_dataprep(asr_sae, period_from = as.Date("2020-10-10"),
#'                         period_to = as.Date("2021-10-10"), trial_type = "m")
#' asr_safety_summary(data = prepped$data,
#'                            period_data = prepped$period_data, "m", 60,
#'                             n_per_arm = list(grp1 = NA, grp2 = NA))
#'
#' # other trial
#' asr_safety_summary(data = prepped$data, period_data = prepped$period_data, "o", 60,
#'  n_per_arm = list(grp1 = 150, grp2 = 150))
#'
#' # tpr trial
#' asr_safety_summary(data = prepped$data, period_data = prepped$period_data, "t", 60,
#'  n_per_arm = list(grp1 = 150, grp2 = 150))
asr_safety_summary <- function(data, period_data, trial_type, n_pat_e, n_per_arm){

  n_pat_e <- as.numeric(n_pat_e)

  if(!attr(period_data, "asr") == "prepped") stop("has period_data been prepped by `asr_dataprep`?")
  if(!attr(data, "asr") == "prepped") stop("has data been prepped by `asr_dataprep`?")

  trial_type <- match.arg(trial_type, c("imp", "medical device", "other","trp"))

  if(trial_type == "imp"){
    # FOR IMP TRIALS

    tab <- table(period_data$sae[period_data$related == TRUE])
    tab <- sort(tab, decreasing = TRUE)[1:3]
    tab <- tab[tab>0]
    most_freq <- paste0(names(tab), " (", tab, ")", collapse = ", ")

    tab <- table(data$sae[data$related == TRUE])
    tab <- sort(tab, decreasing = TRUE)[1:3]
    tab <- tab[tab>0]
    most_freq_all <- paste0(names(tab), " (", tab, ")", collapse = ", ")

    txt <- c(glue("During the reporting period, {length(unique(period_data$record_id))} of {n_pat_e} participants ({sprintf('%1.1f', length(unique(period_data$record_id))/n_pat_e*100)} %) reported a total of {nrow(period_data)} serious adverse events (SAEs)."),
             glue("{sum(period_data$related)} of {nrow(period_data)} SAEs ({sprintf('%1.1f', sum(period_data$related)/nrow(period_data)*100)} %) were classified 'related' to the IMP."),
             if(sum(period_data$related)>0){glue("Most frequent related SAEs (i.e., SADR.) documented were {most_freq}.")}else{glue(" ")},
             glue("{sum(period_data$class == 'SUSAR')} Suspected Unexpected Serious Adverse Reactions (SUSARs) occurred during the reporting period, which have been notified to the Swiss competent authorities."))

    txt_all <- c(glue("Since the beginning of the study, {length(unique(data$record_id))} of {n_pat_e} participants ({sprintf('%1.1f', length(unique(data$record_id))/n_pat_e*100)} %) reported a total of {nrow(data)} serious adverse events (SAEs)."),
             glue("{sum(data$related)} of {nrow(data)} SAEs ({sprintf('%1.1f', sum(data$related)/nrow(data)*100)} %) were classified 'related' to the IMP."),
             if(sum(data$related)>0){glue("Most frequent related SAEs (i.e., SADR.) documented were {most_freq_all}.")}else{glue(" ")},
             glue("{sum(data$class == 'SUSAR')} Suspected Unexpected Serious Adverse Reactions (SUSARs) occurred during the reporting period, which have been notified to the Swiss competent authorities."))

    tab_map <- data.frame(
      col_key = c("desc", "fatal", "sae", "sadr", "susar"),
      name = c('IMP', 'Serious Adverse Events, SAEs, with fatal outcome', 'Other Serious Adverse Events, non-fatal SAEs',
               'Serious Adverse Drug Reactions, SADRs (only for IMPs)',
               'Suspected Unexpected Serious Adverse Reactions, SUSARs (only for IMPs)'),
      stringsAsFactors = FALSE)

    if(all(!is.na(n_per_arm))){
      ### define the values for the two interventional groups
      grp <- names(n_per_arm)


      tab <- tribble(
        ~desc, ~fatal, ~sae, ~sadr, ~susar,
        # row 1
        'Number of cases (during reporting period)',
        paste0("N = ", sum(period_data$outcome == "Fatal"),
        " (",sum(period_data$outcome == "Fatal" & period_data$intervention==grp[1]),",",
        sum(period_data$outcome == "Fatal" & period_data$intervention==grp[2]),")"),
        paste0("N = ", sum(period_data$outcome != "Fatal"),
               " (",sum(period_data$outcome != "Fatal" & period_data$intervention==grp[1]),",",
               sum(period_data$outcome != "Fatal" & period_data$intervention==grp[2]),")"),
        paste0("N = ", sum(period_data$class == "SADR"),
               " (",sum(period_data$class == "SADR" & period_data$intervention==grp[1]),",",
               sum(period_data$class == "SADR" & period_data$intervention==grp[2]),")"),
        paste0("N = ", sum(period_data$class == "SUSAR"),
               " (",sum(period_data$class == "SUSAR" & period_data$intervention==grp[1]),",",
               sum(period_data$class == "SUSAR" & period_data$intervention==grp[2]),")"),
        # row 2
        'Number of cases (cumulative) since the start of the clinical trial',
        paste0("N = ", sum(data$outcome == "Fatal"),
               " (",sum(data$outcome == "Fatal" & data$intervention==grp[1]),",",
               sum(data$outcome == "Fatal" & data$intervention==grp[2]),")"),
        paste0("N = ", sum(data$outcome != "Fatal"),
               " (",sum(data$outcome != "Fatal" & data$intervention==grp[1]),",",
               sum(data$outcome != "Fatal" & data$intervention==grp[2]),")"),
        paste0("N = ", sum(data$class == "SADR"),
               " (",sum(data$class == "SADR" & data$intervention==grp[1]),",",
               sum(data$class == "SADR" & data$intervention==grp[2]),")"),
        paste0("N = ", sum(data$class == "SUSAR"),
               " (",sum(data$class == "SUSAR" & data$intervention==grp[1]),",",
               sum(data$class == "SUSAR" & data$intervention==grp[2]),")"),
      )
    } else {

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

    }


  }
  if(trial_type == "medical device"){
    # FOR MEDICAL DEVICES

    tab <- table(period_data$sae)
    tab <- sort(tab, decreasing = TRUE)[1:3]
    most_freq <- paste0(names(tab), " (", tab, ")", collapse = ", ")

    tab <- table(data$sae)
    tab <- sort(tab, decreasing = TRUE)[1:3]
    most_freq_all <- paste0(names(tab), " (", tab, ")", collapse = ", ")

    txt <- c(glue("During the reporting period, a total of {nrow(period_data)} serious adverse events (SAEs) have been reported."),
             glue("{sum(period_data$related)} out of {nrow(period_data)} SAEs ({sprintf('%1.1f', sum(period_data$related)/{nrow(period_data)}*100)} %) were classified ''related'' to the MD or to an intervention (procedure) undertaken in the clinical trial."),
             glue("In {sum(period_data$devattr)} of {nrow(period_data)} SAEs ({sprintf('%1.1f', sum(period_data$devattr)/nrow(period_data)*100)} %) it cannot be excluded that the events are attributable to the medical device under investigation."),
             glue("In {sum(period_data$devint)} of {nrow(period_data)} SAEs ({sprintf('%1.1f', sum(period_data$devint)/nrow(period_data)*100)} %) it cannot be excluded that the events are attributable to an intervention undertaken in the clinical trial."),
             if(sum(period_data$class=="SAE")>0){glue("The most frequent SAEs documented were {most_freq}.")}else{glue(" ")},
             glue("Occurrence of SAE in the trial arm versus control arm (if applicable)."),
             glue("With respect to the expectedness of the event, {sum(period_data$expected & period_data$related)} ({sprintf('%1.1f', sum(period_data$expected & period_data$related)/sum(period_data$related)*100)} %) of the SADEs were expected/anticipated and {sum(!period_data$expected & period_data$related)} ({sprintf('%1.1f', sum(!period_data$expected)/sum(period_data$related)*100)} %) were classified as unexpected/unanticipated."),
             glue("{sum(period_data$devdef)} device deficiencies were observed (includes malfunctions, use errors, inadequacies in the information supplied by the manufacturer including labelling)."),
             if(sum(period_data$devdef)>0){glue("{sum(period_data$devdefcouldlead)} out of {sum(period_data$devdef)} device deficiencies ({sprintf('%1.1f', sum(period_data$devdef)/sum(period_data$devdef)*100)} %) could have led to serious adverse events if suitable action had not been taken, intervention had not been made, or circumstances had been less fortunate (device deficiencies with a SAE potential).")}else{glue(" ")},
             glue("{sum(period_data$safetymeasure)} health hazards that required safety-related measures occurred."),
             glue("Safety and protective measures taken by the investigator/sponsor (including those requested by the ethics committee and Swissmedic and authorities abroad) taken in Switzerland and abroad: [free text]"))

    txt_all <- c(glue("During the reporting period, a total of {nrow(data)} serious adverse events (SAEs) have been reported."),
             glue("{sum(data$related)} out of {nrow(data)} SAEs ({sprintf('%1.1f', sum(data$related)/{nrow(data)}*100)} %) were classified ''related'' to the MD or to an intervention (procedure) undertaken in the clinical trial."),
             glue("In {sum(data$devattr)} of {nrow(data)} SAEs ({sprintf('%1.1f', sum(data$devattr)/nrow(data)*100)} %) it cannot be excluded that the events are attributable to the medical device under investigation."),
             glue("In {sum(data$devint)} of {nrow(data)} SAEs ({sprintf('%1.1f', sum(data$devint)/nrow(data)*100)} %) it cannot be excluded that the events are attributable to an intervention undertaken in the clinical trial."),
             if(sum(data$class=="SAE")>0){glue("The most frequent SAEs documented were {most_freq_all}.")}else{glue(" ")},
             glue("Occurrence of SAE in the trial arm versus control arm (if applicable)."),
             glue("With respect to the expectedness of the event, {sum(data$expected & data$related)} ({sprintf('%1.1f', sum(data$expected & data$related)/sum(data$related)*100)} %) of the SADEs were expected/anticipated and {sum(!data$expected & data$related)} ({sprintf('%1.1f', sum(!data$expected)/sum(data$related)*100)} %) were classified as unexpected/unanticipated."),
             glue("{sum(data$devdef)} device deficiencies were observed (includes malfunctions, use errors, inadequacies in the information supplied by the manufacturer including labelling)."),
             if(sum(data$devdef)>0){glue("{sum(data$devdefcouldlead)} out of {sum(data$devdef)} device deficiencies ({sprintf('%1.1f', sum(data$devdef)/sum(data$devdef)*100)} %) could have led to serious adverse events if suitable action had not been taken, intervention had not been made, or circumstances had been less fortunate (device deficiencies with a SAE potential).")}else{glue(" ")},
             glue("{sum(data$safetymeasure)} health hazards that required safety-related measures occurred."),
             glue("Safety and protective measures taken by the investigator/sponsor (including those requested by the ethics committee and Swissmedic and authorities abroad) taken in Switzerland and abroad: [free text]"))

    tab_map <- data.frame(
      col_key = c("desc", "sade","fatal", "sae"),
      name = c('MD/IVD Device', 'Serious Adverse Device Effects SADE',
               'Device Deficiencies that could have led to an SAE (serious deficiencies)',
               'Safety and protective measures taken in Switzerland and abroad.'),
      stringsAsFactors = FALSE)

    if(all(!is.na(n_per_arm))){

    ### define the values for the two interventional groups
    grp <- names(n_per_arm)

    tab <- tribble(
      ~desc, ~sade ,~attr, ~measures,
      # row 1
      'Number of cases (during reporting period)',
      paste0("N = ", sum(period_data$class == "SADE"),
             " (",sum(period_data$class == "SADE" & period_data$intervention==grp[1]),",",
             sum(period_data$class == "SADE" & period_data$intervention==grp[2]),")"),
      paste0("N = ", sum(period_data$devattr),
             " (",sum(period_data$devattr & period_data$intervention==grp[1]),",",
             sum(period_data$devattr & period_data$intervention==grp[2]),")"),
      paste0("N = ", sum(period_data$safetymeasure),
             " (",sum(period_data$safetymeasure & period_data$intervention==grp[1]),",",
             sum(period_data$safetymeasure & period_data$intervention==grp[2]),")"),
      # row 2
      'Number of cases (cumulative) since the start of the clinical trial',
      paste0("N = ", sum(data$class == "SADE"),
             " (",sum(data$class == "SADE" & data$intervention==grp[1]),",",
             sum(data$class == "SADE" & data$intervention==grp[2]),")"),
      paste0("N = ", sum(data$devattr),
             " (",sum(data$devattr & data$intervention==grp[1]),",",
             sum(data$devattr & data$intervention==grp[2]),")"),
      paste0("N = ", sum(data$safetymeasure),
             " (",sum(data$safetymeasure & data$intervention==grp[1]),",",
             sum(data$safetymeasure & data$intervention==grp[2]),")"),
    )
    }else{
      tab <- tribble(
        ~desc, ~sade ,~attr, ~measures,
        # row 1
        'Number of cases (during reporting period)',
        sum(period_data$class == "SADE"),
        sum(period_data$devattr),
        sum(period_data$safetymeasure),
        # row 2
        'Number of cases (cumulative) since the start of the clinical trial',
        sum(data$class == "SADE"),
        sum(data$devattr),
        sum(data$safetymeasure)
      )
    }

  }
  if(trial_type == "other"){
    # FOR OTHER TRIALS
    tab <- table(period_data$sae[period_data$related == TRUE])
    tab <- sort(tab, decreasing = TRUE)[1:3]
    most_freq <- paste0(names(tab), " (", tab, ")", collapse = ", ")

    tab <- table(data$sae[data$related == TRUE])
    tab <- sort(tab, decreasing = TRUE)[1:3]
    most_freq_all <- paste0(names(tab), " (", tab, ")", collapse = ", ")

    txt <- c(glue("During the reporting period, {length(unique(period_data$record_id))} of {n_pat_e} participants ({sprintf('%1.1f', length(unique(period_data$record_id))/n_pat_e*100)} %) reported a total of {nrow(period_data)} serious adverse events (SAEs) with possible relationship to the study intervention)."),
             if(sum(period_data$related)>0){glue("The most frequent documented SAEs with possible relationship to the intervention were {most_freq}.")}else{glue(" ")})

    txt_all <- c(glue("Since the beginning of the study, {length(unique(data$record_id))} of {n_pat_e} participants ({sprintf('%1.1f', length(unique(data$record_id))/n_pat_e*100)} %) reported a total of {nrow(data)} serious adverse events (SAEs) with possible relationship to the study intervention)."),
               if(sum(data$related)>0){glue("The most frequent documented SAEs with possible relationship to the intervention were {most_freq}.")}else{glue(" ")})

    tab_map <- data.frame(
      col_key = c("desc", "fatal", "sae"),
      name = c('Other clinical trial', 'SAEs with fatal outcome where a causality to the intervention cannot be excluded',
               'Other SAEs where a causality to the intervention cannot be excluded'),
      stringsAsFactors = FALSE)

    if(all(!is.na(n_per_arm))){

      ### define the values for the two interventional groups
      grp <- names(n_per_arm)

      tab <- tribble(
        ~desc, ~fatal, ~nfatal,
        # row 1
        'Number of cases (during reporting period)',
        paste0("N = ", sum(period_data$outcome == "Fatal" & period_data$related==1),
               " (",sum(period_data$outcome == "Fatal" & period_data$related==1 & period_data$intervention==grp[1]),",",
               sum(period_data$outcome == "Fatal" & period_data$related==1 & period_data$intervention==grp[2]),")"),
        paste0("N = ", sum(period_data$outcome != "Fatal" & period_data$related==1),
               " (",sum(period_data$outcome != "Fatal" & period_data$related==1 & period_data$intervention==grp[1]),",",
               sum(period_data$outcome != "Fatal" & period_data$related==1 & period_data$intervention==grp[2]),")"),
        # row 2
        'Number of cases (cumulative) since the start of the clinical trial',
        paste0("N = ", sum(data$outcome == "Fatal" & data$related==1),
               " (",sum(data$outcome == "Fatal" & data$related==1 & data$intervention==grp[1]),",",
               sum(data$outcome == "Fatal" & data$related==1 & data$intervention==grp[2]),")"),
        paste0("N = ", sum(data$outcome != "Fatal" & data$related==1),
               " (",sum(data$outcome != "Fatal" & data$related==1 & data$intervention==grp[1]),",",
               sum(data$outcome != "Fatal" & data$related==1 & data$intervention==grp[2]),")"),
      )

    }else{
      tab <- tribble(
        ~desc, ~fatal, ~nfatal,
        # row 1
        'Number of cases (during reporting period)',
        sum(period_data$outcome == "Fatal" & period_data$related==1),
        sum(period_data$outcome != "Fatal" & period_data$related==1),
        # row 2
        'Number of cases (cumulative) since the start of the clinical trial',
        sum(data$outcome == "Fatal" & data$related==1),
        sum(data$outcome != "Fatal" & data$related==1),
      )
    }

  }
  if(trial_type == "trp"){
    # FOR OTHER TRIALS
    tab <- table(period_data$sae)
    tab <- sort(tab, decreasing = TRUE)[1:3]
    most_freq <- paste0(names(tab), " (", tab, ")", collapse = ", ")

    tab <- table(data$sae)
    tab <- sort(tab, decreasing = TRUE)[1:3]
    most_freq_all <- paste0(names(tab), " (", tab, ")", collapse = ", ")

    txt <- c(glue("During the reporting period, a total of {nrow(period_data)} Serious Adverse Events (SAEs) occurred."),
             glue("{sum(period_data$related)} of {nrow(period_data)} SAEs ({sprintf('%1.1f', sum(period_data$related)/nrow(period_data)*100)} %) were classified as Serious Adverse Drug Reaction (SADR) i.e., serious adverse events with possible relationship to the TrP/GT/GMO administered."),
             if(sum(period_data$related)>0){glue("The most frequent SADR documented were {most_freq}.")}else{glue(" ")},
             glue("In {sum(period_data$related==1 & period_data$class== 'SADR')} out of {sum(period_data$class == 'SADR')} SADRs ({sprintf('%1.1f', sum(period_data$related==1 & period_data$class== 'SADR')/sum(period_data$class == 'SADR')*100)} %) it cannot be excluded that the events are attributable to the TrP/GT/GMO itself."),
             glue("In {sum(period_data$related==0 & period_data$class== 'SADR')} out of {sum(period_data$class == 'SADR')} SADRs ({sprintf('%1.1f', sum(period_data$related==0 & period_data$class== 'SADR')/sum(period_data$class == 'SADR')*100)} %) it cannot be excluded that the events are attributable to other factors like quality defects, contaminations, administration and preparation procedures of TrP/GT/GMO, etc."),
             glue("{sum(data$class == 'SUSAR')} Suspected Unexpected Serious Adverse Reactions (SUSARs) occurred during the reporting period."),
             glue("Other new relevant safety aspects (including details regarding exposure): "))

    txt_all <- c(glue("Since the beginning of the study, a total of {nrow(data)} Serious Adverse Events (SAEs) occurred."),
             glue("{sum(data$related)} of {nrow(data)} SAEs ({sprintf('%1.1f', sum(data$related)/nrow(data)*100)} %) were classified as Serious Adverse Drug Reaction (SADR) i.e., serious adverse events with possible relationship to the TrP/GT/GMO administered."),
             if(sum(data$related)>0){glue("The most frequent SADR documented were {most_freq_all}.")}else{glue(" ")},
             glue("In {sum(data$related==1 & data$class== 'SADR')} out of {sum(data$class == 'SADR')} SADRs ({sprintf('%1.1f', sum(data$related==1 & data$class== 'SADR')/sum(data$class == 'SADR')*100)} %) it cannot be excluded that the events are attributable to the TrP/GT/GMO itself."),
             glue("In {sum(data$related==0 & data$class== 'SADR')} out of {sum(data$class == 'SADR')} SADRs ({sprintf('%1.1f', sum(data$related==0 & data$class== 'SADR')/sum(data$class == 'SADR')*100)} %) it cannot be excluded that the events are attributable to other factors like quality defects, contaminations, administration and preparation procedures of TrP/GT/GMO, etc."),
             glue("{sum(data$class == 'SUSAR')} Suspected Unexpected Serious Adverse Reactions (SUSARs) occurred during the reporting period."),
             glue("Other new relevant safety aspects (including details regarding exposure): "))

    tab_map <- data.frame(
      col_key = c("desc", "fatal", "nfatal", "nsadr","sadr", "susar"),
      name = c('', 'SAEs with fatal outcome', 'Other Serious Adverse Events (non-fatal SAEs)',
               'Non-Serious Adverse Drug Reactions, NSADRs',
               'Serious Adverse Drug Reactions, SADRs', 'Suspected Unexpected Serious Adverse Reactions, SUSARs'),
      stringsAsFactors = FALSE)

    if(all(!is.na(n_per_arm))){

    grp <- names(n_per_arm)

    tab <- tribble(
      ~desc, ~fatal, ~nfatal, ~nsadr, ~sadr, ~susar,
      # row 1
      'Number of cases (during reporting period)',
      paste0("N = ", sum(period_data$outcome == "Fatal" ),
             " (",sum(period_data$outcome == "Fatal" & period_data$intervention==grp[1]),",",
             sum(period_data$outcome == "Fatal"& period_data$intervention==grp[2]),")"),
      paste0("N = ", sum(period_data$outcome != "Fatal" & period_data$related==1),
             " (",sum(period_data$outcome != "Fatal" & period_data$intervention==grp[1]),",",
             sum(period_data$outcome != "Fatal" & period_data$intervention==grp[2]),")"),
      paste0("N = ", sum(period_data$class != "NSADR" ),
             " (",sum(period_data$class != "NSADR" & period_data$intervention==grp[1]),",",
             sum(period_data$class != "NSADR" & period_data$intervention==grp[2]),")"),
      paste0("N = ", sum(period_data$class != "SADR" ),
             " (",sum(period_data$class != "SADR" & period_data$intervention==grp[1]),",",
             sum(period_data$class != "SADR" & period_data$intervention==grp[2]),")"),
      paste0("N = ", sum(period_data$class != "SUSAR" ),
             " (",sum(period_data$class != "SUSAR" & period_data$intervention==grp[1]),",",
             sum(period_data$class != "SUSAR" & period_data$intervention==grp[2]),")"),
      # row 2
      'Number of cases (cumulative) since the start of the clinical trial',
      paste0("N = ", sum(data$outcome == "Fatal" ),
             " (",sum(data$outcome == "Fatal" & data$intervention==grp[1]),",",
             sum(data$outcome == "Fatal"& data$intervention==grp[2]),")"),
      paste0("N = ", sum(data$outcome != "Fatal" ),
             " (",sum(data$outcome != "Fatal" & data$intervention==grp[1]),",",
             sum(data$outcome != "Fatal"& data$intervention==grp[2]),")"),
      paste0("N = ", sum(data$class != "NSADR" ),
             " (",sum(data$class != "NSADR" & data$intervention==grp[1]),",",
             sum(data$class != "NSADR" & data$intervention==grp[2]),")"),
      paste0("N = ", sum(data$class != "SADR" ),
             " (",sum(data$class != "SADR" & data$intervention==grp[1]),",",
             sum(data$class != "SADR" & data$intervention==grp[2]),")"),
      paste0("N = ", sum(data$class != "SUSAR" ),
             " (",sum(data$class != "SUSAR" & data$intervention==grp[1]),",",
             sum(data$class != "SUSAR" & data$intervention==grp[2]),")"),
    )
    }else{
      tab <- tribble(
        ~desc, ~fatal, ~nfatal, ~nsadr, ~sadr, ~susar,
        # row 1
        'Number of cases (during reporting period)',
        sum(period_data$outcome == "Fatal"),
        sum(period_data$outcome != "Fatal"),
        sum(period_data$class == "NSADR"),
        sum(period_data$class == "SADR"),
        sum(period_data$class == "SUSAR"),
        # row 2
        'Number of cases (cumulative) since the start of the clinical trial',
        sum(data$outcome == "Fatal"),
        sum(data$outcome != "Fatal"),
        sum(data$class == "NSADR"),
        sum(data$class == "SADR"),
        sum(data$class == "SUSAR")
      )
    }

  }
  return(
    list(
      txt = txt,
      txt_all = txt_all,
      tab = tab,
      tab_map = tab_map
    ))
}

