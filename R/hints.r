
hint.for.call = function(call, ex=get.ex(), env = get.ex()$stud.env, stud.expr.li = ex$stud.expr.li, part=NULL, from.assign=!is.null(lhs), lhs = NULL, call.obj = NULL) {
  if (!is.null(call.obj)) {
    call = call.obj
  } else {
    call = substitute(call)
  }
  restore.point("hint.for.call")

  part.str = ifelse(is.null(part),"",paste0(" in part ", part))
  
  ce = match.call.object(call)
  cde = describe.call(call.obj=ce)
  check.na = cde$name
    
  stud.na = sapply(stud.expr.li,  name.of.call)
  stud.expr.li = stud.expr.li[which(stud.na == check.na)]

  assign.str = ifelse(from.assign,paste0(" ",lhs, " ="),"")
  if (cde$type == "fun") {
    
    # Special cases
    if (check.na=="library") {
      lib =  as.character(cde$arg[[1]])
      has.lib = suppressWarnings(require(lib, character.only=TRUE, quietly=TRUE))
      if (has.lib) {
        display('Add the command ', deparse1(ce), ' to load the the package ',lib,'. It contains functions that we need.')
      } else {
        display('Add the command ', deparse1(ce), ' to load the the package ',lib,'. It contains functions that we need.\n First you must install the package, however. For packages that are on the CRAN or for which you have a local zip or tar.gz file, you can do the installation in RStudio using the menu Tools -> Install Packages.\n(For packages that are only on Github, first load and install the package devtools from CRAN and then use its function install_github.)') 
      }
      return(invisible())
    }
    
    if (length(stud.expr.li)==0) {
      if (!from.assign)
        display("You must call the function '", check.na,"'", part.str,".")
      if (from.assign)
        display("You must assign to ", lhs, " a call to the function '", check.na,"'", part.str,".")
      return(invisible())
    }
    
    # Environment in which argument values shall be evaluated. Is a data frame
    # if the function is a dplyer function like mutate(dat,...)
    val.env = env
    if (is.dplyr.fun(check.na)) {
      val.env = eval(cde$arg[[".data"]],env)
    }
    
    analyse.str = lapply(stud.expr.li, function(se) {
      ret = compare.call.args(stud.call=se, check.call=ce, val.env = val.env)
      s = NULL
      if (length(ret$differ.arg)>0) {
        s = c(s,paste0("Your argument ", ret$differ.arg, " = ", ret$stud.arg[ret$differ.arg], " differs in its ", ret$differ.detail, " from my solution."))
      }
      if (length(ret$extra.arg)>0) {
        s = c(s,paste0("In my solution I don't use the argument ", ret$extra.arg))
      }
      if (length(ret$missing.arg)>0) {
        s = c(s,paste0("You don't use the argument ", ret$missing.arg))
      }
      if (length(s)>0) {
        s = paste0("     - ",s, collapse="\n")
      }
      if (!is.null(s)) {
        str = paste0("  ",assign.str, deparse1(se),":\n",s)
      } else {
        str = paste0("  ",assign.str, deparse1(se),": is ok.")        
      }
      str
      
    })
    analyse.str = paste0(analyse.str, collapse = "\n")
    
    if (!from.assign)
      display("I don't see a correct call to the function '", check.na, "'",part.str,".\nLet me compare your call(s) with my solution:\n", analyse.str)
    if (from.assign)
      display("I don't see a correct assignment to ", lhs, ", which should call the function '", check.na, "'",part.str,".\nLet me compare your assignments with my solution:\n", analyse.str)
    
  }
  
  if (cde$type == "chain") {
    restore.point("hint.for.call.chain")
    op = cde$name
    chain.na = sapply(cde$arg, name.of.call)
    comb.chain.na = paste0(chain.na,collapse=";")
    
    sde.li = lapply(stud.expr.li, function(se) describe.call(call.obj=se))    
    scomb.chain.na = sapply(sde.li, function(sde){
       paste0(sapply(sde$arg, name.of.call), collapse=";")
    })
    
    correct.calls = which(scomb.chain.na == comb.chain.na)
    chain.str = paste0(chain.na, "...", collapse = paste0(" ",op,"\n  "))
    chain.str = paste0(assign.str, chain.str)
    display("My solution consists of a chain of the form:\n\n", chain.str,"\n\nThe ... may stand for some function arguments wrapped in () that you must figure out.")
    if (length(correct.calls)==0) {
      return(invisible())
    }
    sde.li = sde.li[correct.calls]
    stud.expr.li = stud.expr.li[correct.calls]
    # Stepwise check all results

    ccode = deparse1(cde$arg[[1]])
    ccall = cde$arg[[1]] 
    scode.li = lapply(sde.li, function(sde) deparse1(sde$arg[[1]]))
    scall.li = lapply(sde.li, function(sde) sde$arg[[1]])
    correct = rep(TRUE, length(sde.li))
    i = 1
    
    while (TRUE) {
      cval = eval(ccall, env)
      new.correct = sapply(seq_along(scall.li), function(j) {
        if (!correct) return(FALSE)
        is.same(eval(scall.li[[j]],env),cval)
      })
      if (!any(new.correct)) {
        fail = i
        best.ind = which(correct)
        break
      }
      i = i+1
      correct = new.correct
      # Some user solution is completely correct
      if (i > length(cde$arg)) {
        fail = FALSE
        best.ind = which(correct)
        break
      }
      
      ccode = paste(ccode, op, deparse1(cde$arg[[i]]))
      ccall = parse(text=ccode,srcfile=NULL) 
      sde.li = sde.li[correct]
      stud.expr.li = stud.expr.li[correct]
      scode.li = scode.li[correct]
      scode.li = lapply(seq_along(sde.li), function(j) 
        paste(scode.li[[j]], op, deparse1(sde.li[[j]]$arg[[i]]))
      )
      scall.li = lapply(scode.li, function(scode) parse(text=scode,srcfile=NULL))
    }
    
    if (fail == 1) {
      display("None of your commands has even the first element of the chain correct!")
      return(invisible())
    } else if (fail > 1) {
      wrong.call.na = name.of.call(cde$arg[[fail]])
      if (fail == 2) {
        display("Your following commands havs the first element of the chain correct, but seem wrong already in the second element '", wrong.call.na,"':")
      } else {
        display("Your following commands have the first ", fail-1," elements of the chain correct, but seem wrong already in element ", fail,", the call to '", wrong.call.na,"':")
      }
      scall.str = sapply(sde.li, function(sde) {
        sna = sapply(sde$arg, deparse1)
        err.code = rep("", length(sna))
        err.code[fail] = " !! WRONG !!"
        paste0(sna[1]," ",op,err.code[1],paste0("\n   ", sna[-1]," ", op,err.code[-1], collapse=""))
      })
      display(scall.str)
      
      if (!is.null(dim(cval)) & FALSE) {
        display("\nHere are the first two lines of the results when calling my solution up to the wrong call to '", wrong.call.na,"'")
        print(head(cval,2))
        display("...")
      }
      
      return(invisible())
    } else if (fail==0) {
        display("Hmm, it actually looks like you have a correct command. It is strange that the test did not pass...")
    }
  }  
  return(invisible())  

}

hint.for.assign = function(expr, ex=get.ex(), env = get.ex()$stud.env, stud.expr.li = ex$stud.expr.li, part=NULL) {
  expr = substitute(expr)
  restore.point("hint.for.assign")

  ce = match.call.object(expr)
  ce = standardize.assign(ce)                              

  ce.rhs = match.call.object(ce[[3]])
  dce.rhs = describe.call(call.obj=ce.rhs)

  stud.expr.li = lapply(stud.expr.li, standardize.assign)
  stud.expr.li = stud.expr.li[(!sapply(stud.expr.li,is.null))]
  
  # Check names
  var = deparse1(ce[[2]])
  stud.var = sapply(stud.expr.li, function(e) deparse1(e[[2]]))
  stud.expr.li = stud.expr.li[stud.var == var]
  
  se.rhs.li = lapply(stud.expr.li, function(e) match.call.object(e[[3]]))

  hint.for.call(call.obj=ce.rhs, ex=ex, env=env, stud.expr.li=se.rhs.li,part=part, lhs=var)  
}

is.dplyr.fun = function(na) {
  na %in% c("mutate","filter","select","arrange","summarise","summarize")
}
