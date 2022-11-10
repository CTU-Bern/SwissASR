#' Demonstration data set
#'
#' Simulated recruitment data from three sites. Each row represents an SAE. Sites one and two started on 2020-07-01, site three on 2020-09-01.
#'
#' @format A data frame with the following variables:
#' \describe{
#'   \item{sae_date}{The date that the SAE occured}
#'   \item{record_id}{Participant ID}
#'   \item{age}{Participant age}
#'   \item{sex}{Participant sex}
#'   \item{country}{Participant's country}
#'   \item{site}{Which site the participant was recruited into}
#'   \item{sae}{Description of the SAE}
#'   \item{intervention}{Intervention arm}
#'   \item{outcome}{SAE outcome (e.g. fatal, sequel, improved, resolved)}
#'   \item{comment}{A comment about the SAE}
#'   \item{trt}{How was the participant treated?}
#'   \item{class}{SAE classification (SUSAR, SADR, ...)}
#'   \item{expected}{Was the SAE expected}
#'   \item{devdef}{(For device trials) was the SAE a device deficiency?}
#'   \item{devattr}{(For device trials) was the SAE attributable to the device?}
#'   \item{devint}{(For device trials) was the SAE attributable to the intervention?}
#'   \item{safetymeasure}{(For device trials) was the SAE a health hazards that required safety-related measures?}
#'   \item{sae_n}{SAE number/identifier}
#'   \item{sae_trtstop}{SAE treatment stop}
#'   \item{sae_trtstart}{SAE treatment start}
#'   \item{related}{Was the SAE related to the intervention}
#' }
#'
"asr_sae"
