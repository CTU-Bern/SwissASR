asr_skeleton <- function(){

  args <- formals(asr)

  args$data <- "sae_data"

  args <- lapply(1:length(args), function(x){
    paste(names(args)[x], "=", deparse(args[[x]]))
  }) |>
    paste(collapse = ", \n    ")

  context <- rstudioapi::getSourceEditorContext()
  id <- context$id
  rstudioapi::insertText(text = paste0("asr(\n    ", args, "\n)"), id = id)

}
