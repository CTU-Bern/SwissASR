
# helpers for checking classes and issuing errors

check_fac_char <- function(x){
  var <- as.character(sys.call())[2]
  var <- sub("data$", "", var, fixed = TRUE)
  if(!any(c("factor", "character") %in% class(x))) stop("'", var, "' should be factor or character")
}
check_fac_char_log_int <- function(x){
  var <- as.character(sys.call())[2]
  var <- sub("data$", "", var, fixed = TRUE)
  if(!any(c("factor", "character", "logical", "integer") %in% class(x))) stop("'", var, "' should be factor, character, logical or integer")
}
check_date <- function(x){
  var <- as.character(sys.call())[2]
  var <- sub("data$", "", var, fixed = TRUE)
  if(!any(c("Date", "POSIXt") %in% class(x))) stop("'", var, "' should be Date or POSIX")
}
check_fac <- function(x){
  var <- as.character(sys.call())[2]
  var <- sub("data$", "", var, fixed = TRUE)
  if(!any(c("factor") %in% class(x))) stop("'", var, "' should be factor")
}
check_log <- function(x){
  var <- as.character(sys.call())[2]
  var <- sub("data$", "", var, fixed = TRUE)
  if(!any(c("logical") %in% class(x))) stop("'", var, "' should be logical")
}



check_listing_names <- function(names
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
                                # , var_relation = "related"
                                # , var_expected = "expected"
                                # , var_devdef = "devdef"
                                # , var_devattr = "devattr"
                                # , var_devint = "devint"
                                # , var_safetymeasure = "safetymeasure"
){

}

check_field_characters <- function(x){
  if(grepl("&|<br>", x))
    warning("Special character found in '", x, "'\n",
            "  Special characters or strings such as '&' and '<br>' may cause problems with Word.\n",
            "  We advise replacing them with other characters.")
  return(NULL)
}
