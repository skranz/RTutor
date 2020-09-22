# This file contains code used 
# for creating automatic hint for dplyr chains

inner.hint.for.call.chain = function(stud.expr.li, cde, ps = get.ps(), ce=NULL, assign.str="",start.char="\n", end.char="\n", env=ps$stud.env, details.of.wrong..call = TRUE, compare.vals= !(isTRUE(ps$noeval) | isTRUE(ps$hint.noeval)), call=NULL,...) {

  restore.point("inner.hint.for.call.chain")

  op = cde$name
  chain.na = sapply(cde$arg, name.of.call)

  
  sde.li = lapply(stud.expr.li, function(se) describe.call(call.obj=se))
  
  if (length(sde.li)>0) {
    is.chain = sapply(sde.li, function(sde) sde$type=="chain")
    # Select student expressions that are chains
    sde.li = sde.li[is.chain]
    stud.expr.li = stud.expr.li[is.chain]
  }  
  # The student has no chain command yet
  if (length(sde.li)==0) {
    chain.str = paste0(chain.na, " ...", collapse = paste0(" ",op,"\n  "))
    chain.str = paste0(assign.str, chain.str)
    
    txt = paste0("My solution consists of a pipe chain of the form:\n\n", chain.str,"\n\nI have not seen any pipe operator ", op," in your solution, yet.")
    display(txt)
    return(invisible())
    #return(list(same=FALSE, fail.step=1, same.until=0, descr=txt))
    
  }
  
  # Cannot correctly evaluate if there are errors
  has.place.holders = sapply(sde.li, has.call.placeholder)
  if (any(has.place.holders)) {
    txt = paste0("Here is a scrambled solution:\n\n", scramble.call.chain(cde, assign.str))
    display(txt)
    return(invisible())
  }

  if (compare.vals) {
    check.res.li = eval.chain.steps(de=cde, envir=env)
  } else {
    check.res.li = NULL
  }
  
  has.error = FALSE
  compare.li = vector("list", length(sde.li))
  
  for (i in seq_along(sde.li)) {
    res = try(compare.pipe.chains(
      check.chain = ce, cde=cde,
      stud.chain = stud.expr.li[[i]],
      sde = sde.li[[i]],
      envir = env,
      check.res.li = check.res.li
    ),silent = TRUE)
    if (is(res, "try-error")) {
     txt = paste0("I could not evaluate all your code without error. Here is a scrambled solution:\n\n", scramble.call.chain(cde, assign.str))
      display(txt)
      return(invisible())
    }
    compare.li[[i]] = res
  }
  
  # compare.li = lapply(seq_along(sde.li), function(i) {
  #   res = try(compare.pipe.chains(
  #     check.chain = ce, cde=cde,
  #     stud.chain = stud.expr.li[[i]],
  #     sde = sde.li[[i]],
  #     envir = env,
  #     check.res.li = check.res.li
  #   ))
  # })
  
  # If student wrote several pipe chains
  # select student solution that fails last
  if (length(compare.li)>1) {
    fail.steps = sapply(compare.li,function(comp) {
      comp$fail.step
    })
    compare.li = compare.li[[which.max(fail.steps)]]
  }
  
  txt = compare.li[[1]]$descr
  display(txt)
  return(invisible())
}

compare.pipe.chains = function(check.chain, stud.chain, cde=describe.call(check.chain), sde=describe.call(stud.chain), compare.vals = TRUE, envir=parent.frame(), check.res.li = if (compare.vals) eval.chain.steps(de=cde, envir=envir), eval.fun = eval ) {
  
  restore.point("compare.pipe.chains")
  
  check.names = sapply(cde$arg, name.of.call)
  stud.names = sapply(sde$arg, name.of.call)
  
  same.until = same.until.pos(check.names, stud.names)
  if (same.until==0) {
    return(list(same=FALSE, fail.step=1, same.until=0, descr=paste0("Please start your pipe chain with ", check.names[1], ", as in the sample solution.")))
  }
  
  j = 1
  ccall = cde$arg[[j]]
  scall = sde$arg[[j]]
  
  if (compare.vals) {
    sres = eval.fun(scall, envir)
    ok = identical(sres, check.res.li[[j]]) 
  } else {
    res = compare.calls(stud.call = scall,check.call = ccall,compare.vals = FALSE)
    ok = res$same
  }
  if (!ok) {
    return(get.chain.failure.results(j, sde, cde,compare.vals = compare.vals, same.until=same.until))
  }

  while (j < same.until) {
    j = j+1
    ccall = cde$arg[[j]]
    scall = sde$arg[[j]]
    
    if (compare.vals) {
      sres = eval.next.chain.call(sres, scall, envir)
      ok = identical(sres, check.res.li[[j]]) 
    } else {
      res = compare.calls(stud.call = scall,check.call = ccall,compare.vals = FALSE)
      ok = res$same
    }
    if (!ok) {
      return(get.chain.failure.results(j, sde, cde,compare.vals = compare.vals, same.until=same.until))
    }
  }
  
  if (same.until < length(cde$args)) {
    step = same.until+1
    if (length(sde$args)>=step) {
      return(get.chain.failure.results(step, sde, cde,compare.vals = FALSE, same.until=same.until)) 
    } else if (length(sde$args)<step) {
      return(list(same=FALSE, fail.step=step, same.until=same.until, descr=paste0("You have to add another element to your pipe chain in which you call the function ", check.names[step],".")))
    }
  } else if (length(sde$args) > same.until) {
    return(list(same=FALSE, fail.step=step, same.until=same.until, descr=paste0("You have too many elements in your pipe chain. Please stop after step ", same.until,", i.e. after the call ", deparse1(sde$args[[same.until]]))))
  }
  return(list(same=TRUE, fail.step=Inf, same.until=same.until, descr=paste0("Great, you entered the correct pipe chain.")))
  
}


get.chain.failure.results = function(step=1, sde, cde, compare.vals=TRUE, same.until=NULL, call.comp.descr = NULL, pipe.op = "%>%") {
  restore.point("get.chain.failure.results")
  
  
  fail = step
  stud.na = name.of.call(sde$arg[[step]])
  check.na = name.of.call(sde$arg[[step]])

  child.sde = describe.call(call.obj = sde$arg[[step]])
  
  is.fun = child.sde$type == "fun"

  if (is.fun) {
    txt = paste0("In your following pipe chain, I detect an error in the ", to_ordinal(step)," element:\n")
  } else {
    txt = paste0("In your following pipe chain, I detect an error in the ", to_ordinal(step)," element: ", deparse1(sde$arg[[step]]),":\n")
  }
  sna = sapply(sde$arg, deparse1)
  err.code = rep("", length(sna))
  err.code[fail] = " !! WRONG !!"
  op.str = rep(pipe.op,NROW(sna))
  op.str[length(op.str)] = ""
  
  scall.str = paste0(sna[1]," ",op.str[1],err.code[1],paste0("\n   ", sna[-1]," ", op.str[-1],err.code[-1], collapse=""))
  
  txt = paste0(txt,"\n",scall.str)

  if (is.null(call.comp.descr)) {
    if (stud.na == check.na & is.fun) {
      comp.call = compare.call.args(stud.call = sde$args[[step]], check.call = cde$args[[step]],compare.vals = FALSE, from.pipe=TRUE)
      call.comp.descr = paste0("It is correct to call ", check.na,". But: ", paste0(comp.call$descr,collapse="\nAlso: "))
    } else if (is.fun) {
      call.comp.descr = paste0("You call the function ", stud.na, " but the sample solution calls the function ", check.na,".")
    } else {
      call.comp.descr = paste0("The sample solution for the ", to_ordinal(step)," element is ", deparse1(cde$arg[[step]]))
    }
  }
  
  txt = paste0(txt, "\n\n", call.comp.descr)
  
  return(list(
    same=FALSE,
    fail.step = step,
    same.until = same.until,
    descr= txt
  ))
  
  if (wrong.call.na=="group_by") {
    display("\nNote: For group_by(...) RTutor requires the groups and their order to be equal to the sample solution. You must call ", deparse1(ccall[[1]][[fail]]),".")
  }


}

# Returns an integer describing until which positions to vectors are the same
same.until.pos = function(x,y) {
  rows = seq_len(min(length(x), length(y)))
  if (length(rows)==0) return(0)
  same = which(x[rows] != y[rows])[1]
  if (is.na(same)) return(length(rows))
  same-1
}

example.eval.next.chain.call = function() {
  x = data.frame(x=1:10)
  call = quote(mutate(y=x^2))
  eval.next.chain.call(x,call)
}

eval.chain.steps = function(chain=NULL, de=describe.call(chain), envir = parent.frame(), eval.fun = eval) {
  restore.point("eval.chain.steps")
  if (length(de$args)==0) return(NULL)
  
  res.li = vector("list", length(de$args))
  res.li[[1]] = eval.fun(de$args[[1]], envir)
  
  for (j in setdiff(seq_along(res.li),1)) {
    res.li[[j]] = eval.next.chain.call(res.li[[j-1]], de$args[[j]],envir=envir, eval.fun = eval.fun, pipe.fun = de$name)
  }
  res.li
}

# Eval next chain call
eval.next.chain.call = function(x, call, envir=parent.frame(), eval.fun = eval, pipe.fun = "%>%") {
  new.call = substitute(._._x %>% call, list(call=call))
  restore.point("eval.next.chain.call")
  
  cenv = new.env(parent=envir)
  cenv$._._x = x
  
  eval.fun(new.call, cenv)
  
}

scramble.call.chain = function(cde, assign.str="") {
  parts = sapply(cde$args, function(call) {
    call.str = deparse1(call)
    scramble.text(call.str,"?",0.5, keep.char=c(" ","\n",">","%","(",")","=", "\t"))
  })
  str = paste0(parts, collapse = " %>%\n\t")
  str = paste0(assign.str, str)
  str
}