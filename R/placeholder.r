get.placeholder = function(ps=get.ps()) {
  ph = ps$rps$placeholder
  if (is.null(ph)) return("___")
  ph
}