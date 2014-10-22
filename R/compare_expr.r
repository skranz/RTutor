examples.describe.call = function() {
  x = 1:5
  describe.call(runif(10,1,2))
  describe.call(2*3+4)
  describe.call(x[1:4])

  df = data.frame(x=1:100)
  
  describe.call(df %.% filter(x>80))
}

examples.describe.call = function() {
  f = function(x) {
    y = substitute(x)
    describe.call(call.obj=y)
  }
  f(2*x)
  f(plot(1))
  f("Hi")
  f(x)
  f(3)
}

describe.call = function(call, call.obj=NULL, call.str=NULL) {
  if (!is.null(call.obj)) {
    call = call.obj
  } else if (!is.null(call.str)) {
    call = parse(call.str,srcfile=NULL)
  } else {
    call = substitute(call)
  }
  restore.point("describe.call")
  call
  na = name.of.call(call)
  
  type = "fun"
  if (na %in% c("+","-","*","/","%*%","(")) {
    type="math"
  } else if (na == "%.%") { 
    type="chain"
  } else if (na == "<-" | na =="=") { 
    type="assign"
  } else if (na == "[" | na=="$" | na == "[[") {
    type="subset"
  } else if (na=="==" | na=="<" | na =="!=" | na=="<=" | na==">" | na==">=") {
    type="comp"
  } else if (is.name(call)) {
    type="var"
  } else if (!is.call(call)) {
    type=class(call)
  }

  if (type=="chain") {
    return(describe.chain.call(call))
  }
#   if (type == "fun") {
#     if (is.null(call.str)) {
#       call.str = deparse1(call)
#     }
#     if (!has.substr(call.str,"(")) {
#       res = suppressWarnings(as.numeric(call.str))
#       if (is.na(res)) {
#         type = "var"
#       } else {
#         type = "numeric"
#       }
#     }
#   }
  
  args = args.of.call(call) 
  list(name=na,type=type, args = args)
}

describe.chain.call = function(call.obj) {
  restore.point("describe.chain.call")
  call = call.obj
  na = name.of.call(call)
  args = recursive.args.of.call(call, na)
  list(name=na,type="chain", args = args)
}

#' Checks whether arguments of stud.call are correct given the specification in check.call 
check.call.args = function(stud.call, check.call, compare.vals = !is.null(val.env), val.env=NULL, allow.extra.arg=FALSE, ignore.arg=NULL, check.values=NULL) {
  restore.point("compare.call.args")
   
  sarg = args.of.call(stud.call, name.empty.arg=TRUE)
  carg = args.of.call(check.call, name.empty.arg=TRUE)

  missing.arg = setdiff(names(carg), c(names(sarg), ignore.arg))
  if (length(missing.arg)>0)
    return(FALSE)
  if (!allow.extra.arg) {
    extra.arg = setdiff(names(sarg), c(names(carg), ignore.arg))
    if (length(extra.arg)>0)
      return(FALSE)
  }
  overlap.arg = setdiff(intersect(names(sarg), names(carg)), ignore.arg)
  if (length(overlap.arg)==0)
    return(TRUE)
  
  differs = sapply(overlap.arg, function(na) !identical(sarg[[na]],carg[[na]]))
  if (sum(differs)==0)
    return(TRUE)
  if (!compare.vals)
    return(FALSE)
  differ.arg = overlap.arg[differs]
   
  if (compare.vals) {
    for (var in differ.arg) {
      stud.val = eval(sarg[[var]],val.env)
      check.val = eval(carg[[var]],val.env)
      if (!is.same(stud.val,check.val))
        return(FALSE)
    }
  }
  return(TRUE)
}

remove.names = function(x) {
  try({
    if (!is.null(names(x)))
      names(x) = NULL
    if (!is.null(rownames(x)))
      rownames(x) = NULL
    if (!is.null(colnames(x)))
      catnames(x) = NULL
  }, silent=TRUE)
  x  
}

is.same = function(x,y, tol=1e-9, check.all.equal=TRUE, check.names=FALSE, check.attributes=FALSE) {
  restore.point("is.same")
  
  if(identical(x,y))
    return(TRUE)
  
  if (length(x)!=length(y))
    return(FALSE)
  
  if (check.all.equal) {
    if (is.data.frame(x) & is.data.frame(y)) {
      if ((NROW(x) != NROW(y)) | (NCOL(x) != NCOL(y)))
        return(FALSE)
      if (length(x)==0)
        return(TRUE)
      eq = sapply(1:NCOL(x), function(i) isTRUE(all.equal(x[[i]],y[[i]],tol=tol, check.names=check.names, check.attributes=check.attributes) ))
      if (all(eq))
        return(TRUE)
    } else {
      if (isTRUE(all.equal(x,y, tol=tol, check.names=check.names, check.attributes=check.attributes)))
        return(TRUE)
      
    }
  }
  if (is.numeric(x) & is.numeric(y)) {
    if (max(abs(x-y), na.rm=TRUE)>tol )
      return(FALSE)
    if (!identical(is.na(x),is.na(y)))
      return(FALSE)        
    return(TRUE)
  }
  return(FALSE)
}

compare.call.args = function(stud.call, check.call, compare.vals = !is.null(val.env), val.env=NULL, ...) {
  restore.point("compare.call.args")
  
  stud.call = match.call.object(stud.call, ...)
  check.call = match.call.object(check.call, ...)
  
  sarg = args.of.call(stud.call, name.empty.arg=TRUE)
  carg = args.of.call(check.call, name.empty.arg=TRUE)

  missing.arg = setdiff(names(carg), names(sarg))
  extra.arg = setdiff(names(sarg), names(carg))
  overlap.arg = intersect(names(sarg), names(carg))
  
  if (length(overlap.arg)>0) {
    differs = sapply(overlap.arg, function(na) !identical(sarg[[na]],carg[[na]]))
    differ.arg = overlap.arg[differs]
  } else {
    differ.arg = same.arg = overlap.arg
  }
  
  if (length(differ.arg)>0) {
    if (compare.vals) {
      differ.detail = lapply(differ.arg, function(var) {
        stud.val = eval(sarg[[var]],val.env)
        check.val = eval(carg[[var]],val.env)
        paste0(compare.values(stud.val, check.val), collapse=", ")
      })
      names(differ.detail) = differ.arg
      differs = sapply(differ.detail, function(x) nchar(x)>0)
      differ.detail = unlist(differ.detail[differs])
      differ.arg = names(differ.detail)
    } else {
      differ.detail = replicate(length(differ.arg),c("code"),simplify=FALSE)
      names(differ.detail) = differ.arg
    }
  } else {
    differ.detail = NULL
  }  
  same.arg = setdiff(overlap.arg, differ.arg)
  
  # Make a description that is used by hint functions.
  s = NULL
  if (length(differ.arg)>0) {
    s = c(s,paste0("Your argument ", differ.arg, " = ", sarg[differ.arg], " differs in its ", differ.detail, " from my solution."))
  }
  if (length(extra.arg)>0) {
    s = c(s,paste0("In my solution I don't use the argument ", extra.arg))
  }
  if (length(missing.arg)>0) {
    s = c(s,paste0("You don't use the argument ", missing.arg))
  }

  
  nlist(differ.arg,differ.detail,missing.arg,extra.arg,same.arg, overlap.arg, stud.arg=sarg, check.arg=carg, descr=s)  
}


compare.values = function(var.stud,var.sol, class=TRUE, length=TRUE, dim=TRUE, names=TRUE, values=TRUE, tol=1e-12, details = TRUE, check.all.equal=TRUE) {
  wrong = NULL
  
  if (is.same(var.stud, var.sol))
    return(NULL)
  
  if (class != FALSE) {
    class.stud = class(var.stud)[1]
    class.sol = class(var.sol)[1]
    if (class.stud == "integer") class.stud = "numeric"
    if (class.sol == "integer") class.sol = "numeric"
    
    if (class.stud != class.sol) {
      if (details) {
        wrong = c(wrong,paste0("class (is ", class.stud, " but shall be ", class.sol,")"))
      } else {
        wrong = c(wrong,"class")
      }
    }
  }  
  if (!is.null(wrong))
    return(wrong)
  
  if (length != FALSE) {
    if (!length(var.stud)==length(var.sol)) {
      wrong = c(wrong,"length")
    }
  }
  if (!is.null(wrong))
    return(wrong)
  if (dim != FALSE) {
    if (!identical(dim(var.stud),dim(var.sol))) {
      wrong = c(wrong,"dim")
    }
  }  
  if (!is.null(wrong))
    return(wrong)

  if (names != FALSE) {
    if (!identical(names(var.stud),names(var.sol))) {
      wrong = c(wrong,"names")
    }
  }  
  if (values != FALSE) {
    if (is.list(var.sol) | is.environment(var.sol)) {
      if (!identical(var.sol, var.stud, ignore.environment=TRUE)) {
        wrong = c(wrong,"values")
      }
    } else if (is.numeric(var.stud) & is.numeric(var.sol)) {
      if (max(abs(var.stud-var.sol), na.rm=TRUE)>tol ) {
        wrong = c(wrong,"values")
      } else if (!identical(is.na(var.stud),is.na(var.sol))) {
        wrong = c(wrong,"values")
      }        
    } else {
        wrong = c(wrong,"values")
    }
  }
  wrong
}


examples.match.call.object = function() {
  match.call.object(quote(t.test(extra ~ group, data=sleep)),s3.method=stats:::t.test.formula)
  match.call.object(quote(t.test(extra ~ group, data=sleep)))

  match.call.object(quote(stats:::t.test.formula(extra ~ group, data=sleep)))

  match.call.object(quote(t.test(formula=extra ~ group, data=sleep)))

  f()
}

match.call.object = function(call, envir=parent.frame(), s3.method=NULL) {
  restore.point("match.call.object")
  #browser()
  if (length(call)==1)
    return(call)
  ret = call
  env = new.env(parent=envir)
  env$call = call
  
  if (!is.null(s3.method)) {
      s3.method = substitute(s3.method)
      #restore.point("match.call.object2")
      match.expr = substitute(match.call(fun, call=call), list(fun=s3.method))    
  } else {
    match.expr = substitute(match.call(fun, call=call), list(fun=call[[1]]))
  }
  try(ret <- eval(match.expr, envir=env), silent=TRUE)
  ret
}

name.of.call = function(call) {
  if (is.symbol(call))
    return(as.character(call))
  as.character(call[[1]])  
}


recursive.args.of.call = function(call,expand.names=NULL) {
  args = args.of.call(call)
  names = sapply(args, name.of.call)
  do.expand = names %in% expand.names
  li = lapply(seq_along(args), function(i){
    if (do.expand[i])
      return(recursive.args.of.call(args[[i]],expand.names))
    return(args[i])
  })
  do.call("c",li)
}

examples.args.of.call = function() {
  args.of.call(quote(t.test(extra ~ group, data=sleep)))
  match.call.object(quote(t.test(extra ~ group, data=sleep)))
  
}

args.of.call = function(call, name.empty.arg = FALSE, prefix="") {
  #restore.point("args.of.call")
  if (is.symbol(call))
    return(NULL)
  li = as.list(call[-1])
  if (name.empty.arg & length(li)>0) {
    if (is.null(names(li))) {
      is.empty = seq_along(li)
    } else {
      is.empty = which(names(li)=="")
    }
    names(li)[is.empty] <- paste0(prefix,is.empty) 
  }
  li
}

examples.code.has.call = function() {
  code.str = "
  plot(5,y=3)
  x*2
  x$a
  x[['a']]
  "
  call.str = "plot(x=5,y=3,main='Hi')"
  
  call.str = 'x[["a"]]'
  
  find.matching.calls(code.str, call.str)
}


find.matching.calls = function(code.str, call.str, call = parse(text=call.str, srcfile=NULL)[[1]]) {
  
  code.li = as.list(parse(text=code.str, srcfile=NULL))
  call = 

  code.names = sapply(code.li, name.of.call)
  call.name = name.of.call(call)
  
  ind = which(code.names %in% call.name)
  if (length(ind)==0) {
    return(NULL)
  }
  return(code.li[ind])
  
  as.list(code)
  
  co = code[[3]]
  co
  co = match.call.object(co)
  
  names(co)
  as.character(co[[1]])
  co[[2]]
  co[[3]]
  class(co[[1]])
  call. = co
  f()
  
  
  names(co)
  
  args(co)
  call_tree(co)
  standardise_call(co)
  str(co)
  class(co)
  co[[2]]
}