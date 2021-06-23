
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
