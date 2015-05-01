# bsActionButton has been removes from shinyBS
bsActionButton = function(...) {
  res =  try(shinyBS::bsActionButton, silent=TRUE)
  if (is(res,"try-error")) {
    args = list(...)
    if (!is.null(args$size)) {
      if (args$size == "mini") args$size="extra-small"
    }
    return(do.call(shinyBS::bsButton,args))
  }
  shinyBS::bsActionButton(...) 
}