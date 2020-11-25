example.scramble.call.diffs = function() {
  stud.code = "seq(1:(4+1),___,by=___)"
  stud.code = gsub("___", ".PH_._", stud.code)
  stud.call = parse(text=stud.code)[[1]]
  check.call = quote(seq(1:5,10,by=10))
  scramble.call.diffs(stud.call, check.call, share=1, val.env = .GlobalEnv)

  ind = 2
  stud.call[[ind]] = scramble.call.object(stud.call[[ind]], share=1)
  str = deparse1(stud.call)
  str
  
  
  stud.call[[2]]
}


# Helper function for hint()
# Looks where stud.call and check.call differ and 
# only replaces the differences with scramble.text
#
# By default only the first placeholder will be replaced
scramble.call.diffs = function(stud.call, check.call, compare.vals = !is.null(val.env), val.env=NULL, ph.max=1,  share=0.5, keep.char=c(" ","(",")",",","+","[","]","="), placeholder=get.placeholder()) {
  
  stud.call = match.call.object(stud.call)
  #check.call = match.call.object(check.call)
  
  restore.point("scramble.call.diffs")
  res = scramble.call.diffs.inner(stud.call, check.call, compare.vals, val.env,share=share, keep.char=keep.char, ph.max=ph.max, ph.count=0)
  res$ph.max = ph.max
  
  scode = deparse1(res$scall)
  scode = gsub('\\"','"',scode, fixed=TRUE)
  if (!res$same) {
    scode = gsub('"YyQ','',scode,fixed=TRUE)
    scode = gsub('QyY"','',scode,fixed=TRUE)
  }
  if (!is.null(placeholder)) {
    scode = gsub(".PH_._",placeholder, scode,fixed=TRUE)
  }
  res$scode = scode
  res
}


scramble.call.diffs.inner = function(stud.call, check.call, compare.vals = !is.null(val.env), val.env=NULL,ph.max=1, ph.count=0,...) {
  
  restore.point("scramble.call.diffs.inner")
  
  
  if (identical(stud.call,check.call)) {
    return(list(same=TRUE, scall=check.call, ph.count = ph.count))
  }

  is.ph = (is.call.placeholder(stud.call))
  if (is.ph) ph.count = ph.count+1

  if (is.ph & ph.count > ph.max) {
    return(list(same=FALSE, scall=get.placeholder.sym(), ph.count = ph.count))
  }
  
  if (compare.vals) {
    is.err = try({
      stud.val <- eval(stud.call,val.env)
      check.val <- eval(check.call, val.env)
    },silent = TRUE)
    if (!is(is.err, "try-error")) {
      if (identical(stud.val, check.val))
        return(list(same=TRUE, scall=check.call, ph.count = ph.count))
      if (isTRUE(all.equal(stud.val, check.val)))
        return(list(same=TRUE, scall=check.call, ph.count = ph.count))
    }
  }
  
  differs = FALSE
  
  
  if (is.symbol(stud.call) | is.symbol(check.call) | is.atomic(stud.call) | is.atomic(check.call)) {
    differs = !identical(stud.call, check.call)
  } else {
    differs = length(stud.call) != length(check.call)
  }
  if (differs) {
    scall = scramble.call.object(check.call, ...)
    return(list(same=FALSE, scall=scall, ph.count=ph.count))
  }

  scall = check.call
  same = TRUE
  # Both stud.call and check.call are function calls with same length
  for (i in seq_along(check.call)) {
    res = scramble.call.diffs.inner(stud.call[[i]], check.call[[i]],
      compare.vals, val.env,ph.count=ph.count, ph.max=ph.max,...)
    ph.count = res$ph.count
    scall[[i]] = res$scall
    same = same & res$same
  }
  list(same=same, scall=scall, ph.count=ph.count)
}

scramble.call.object = function(call, share=0.501, keep.char=c(" ","(",")",",","+","[","]","=")) {
  restore.point("scramble.call.object")
  if(is.character(call)) {
    scall = scramble.text(call,share = share,keep.char = keep.char)
  } else {
    str = deparse1(call)
    #cat("\ncall to scramble = ", str)
    scall = scramble.text(str,share = share,keep.char = keep.char)
    scall = paste0("YyQ",scall,"QyY")
  }
  scall
}
