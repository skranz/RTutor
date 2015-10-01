
rtutor.make.whitelist = function(wl.funs=NULL, wl.calls=NULL, bl.funs=NULL, bl.vars=c(".GlobalEnv",".BaseNamespaceEnv")) {
  nlist(wl.funs, wl.calls, bl.funs, bl.vars)
}


rtutor.check.whitelist = function(call, ps = get.ps()) {
  if (!isTRUE(ps$check.whitelist)) {
    return(list(ok=TRUE, msg=""))
  }
  restore.point("rtutor.check.whitelist")
  
  wl = ps$wl
  WhitelistEval::check.whitelist(call, wl.funs=wl$wl.funs,wl.vars = wl$wl.vars,wl.calls = wl$wl.calls,bl.funs = wl$bl.funs, bl.vars = wl$bl.vars)
}