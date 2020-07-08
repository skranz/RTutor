get.placeholder = function(ps=get.ps()) {
  ph = ps$rps$placeholder
  if (is.null(ph)) return("___")
  ph
}

has.call.placeholder = function(call) {
  if (!is.character(call)) {
    call = deparse1(call)
  }
  has.substr(call,".PH_._")
}