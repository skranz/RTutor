# bsActionButton has been removes from shinyBS
bsActionButton = function(...) {
  res =  try(shinyBS::bsActionButton, silent=TRUE)
  if (is(res,"try-error")) return(shinyBS::bsButton(...))
  
  shinyBS::bsActionButton(...) 
}