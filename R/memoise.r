example.memoise = function() {

  li = memoise.fun.li(rtutor.default.memoise.funs())
  
}

# Creates a list of memoised functions that have the original function names
memoise.fun.li = function(funs) {
  restore.point("memoise.fun.li")
  
  fun.li = lapply(funs, function(str) {
    restore.point("inner.memoise.fun.li")

    if (has.substr(str,":::")) {
      fun.name = str.right.of(str,":::")
      pkg = str.left.of(str,":::")
    } else if (has.substr(str,"::")) {
      fun.name = str.right.of(str,"::")
      pkg = str.left.of(str,"::")
    } else {
      fun.name = str
      fun = get(fun.name)
      pkg = getPackageName(environment(fun))
    }
    
    call = substitute(memoise(pkg::fun.name), list(pkg=as.name(pkg), fun.name=as.name(fun.name)))
    li = list(eval(call))
    names(li) = fun.name
    li
  })
  fun.li = do.call(c, fun.li)
  fun.li
}

rtutor.default.memoise.funs = function() {
  c("read.csv","read.table", "foreign::read.dta")
}