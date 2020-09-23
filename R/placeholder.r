get.placeholder = function(ps=get.ps()) {
  ph = ps$rps$placeholder
  if (is.null(ph)) return("___")
  ph
}



get.placeholder.sym = function() {
  as.name(".PH_._")
}

has.call.placeholder = function(call) {
  if (!is.character(call)) {
    call = deparse1(call)
  }
  has.substr(call,".PH_._")
}

is.call.placeholder = function(call) {
  if (is.null(call)) return(FALSE)
  if (!is.character(call)) {
    call = deparse1(call)
  }
  isTRUE(call == ".PH_._")
}