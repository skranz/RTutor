info = function(info.name, ps = get.ps()) {
  restore.point("info")
  if (is.null(ps)) {
    display("Please check your problem set once with Ctrl-Alt-R. Then you can see infos.")
    return()
  }
  infos = ps$rps$infos
  if (! info.name %in% names(infos)) {
    display("You problem set has not the info '", info.name, "'. Only the following infos are stored: ", paste0("'", names(infos),"'", collapse=", "))
    return()
  }
  htmlFile <- tempfile(fileext=".html")
  writeLines(infos[[info.name]]$html,htmlFile )
  if (require(rstudioapi)) {
    rstudioapi::viewer(htmlFile)
  } else {
    cat("Info boxes can only be shown from RStudio. Please install the package rstudioapi.")
  }

}


#' Shows a hint for the current problem.
#' @export
hint = function(..., ps=get.ps()) {
  restore.point("hint")

  if (is.null(ps$chunk.ind)) {
    cat("Please test the chunk before you ask for a hint.")
    return(invisible(""))
  }


  # In old RTutor versions there was no chunk.hint
  if ("chunk.hint" %in% colnames(ps$cdt)) {
    chunk.hint = ps$cdt$chunk.hint[[ps$chunk.ind]]
  } else {
    chunk.hint = NULL
  }

  do.log = TRUE

  if (isTRUE(ps$use.secure.eval) & !isTRUE(ps$hint.noeval)) {
    eval.fun = function(call, envir=parent.frame(),...) {
      if (is.expression(call)) call = call[[1]]
      new.call = substitute(capture.output(call), list(call=call))
      txt = RTutor::rtutor.eval.secure(new.call, envir=envir, silent.check=TRUE)
      cat(paste0(txt, collapse="\n"))
    }
  } else {
    eval.fun = base::eval
  }
  
  # Set expression to first expression
  # if code cannot be evaluated without error
  e.ind = ps$e.ind
  if (is.null(e.ind)) e.ind = 0
  if (e.ind == 0 & isTRUE(ps$cdt$num.e[[ps$chunk.ind]]>0)) {
    e.ind = 1
  }

  
  # No expression set
  if (e.ind == 0) {
    if (!is.null(chunk.hint)) {
      eval.fun(chunk.hint)
      log.event(type="hint",chunk=ps$chunk.ind, ex=ps$ex.ind, e.ind=e.ind)
      if (ps$cdt$num.e[[ps$chunk.ind]]>0) {
        cat("\nI can't give you a more specific hint, since I can't run your code, due to an error.")
      }
    } else {
      do.log = FALSE
      if (ps$cdt$num.e[[ps$chunk.ind]]==0) {
        cat("Sorry, but there is no hint for your current problem.")
      } else {
        cat("There is an error in your code chunk so that RTutor cannot evaluate your code. Before you can get a more detailed hint, write code that runs without error when you manually run your chunk. (One way to get no syntax error is to remove all your own code in your chunk.)")
      }
    }

  # hint for expression ps$e.ind
  } else {
    hint.expr = ps$cdt$hint.expr[[ps$chunk.ind]][[e.ind]]
    if (length(hint.expr)==0) {
      if (!is.null(chunk.hint)) {
        res = try(eval.fun(chunk.hint))
        if (is("res","try-error")) {
          if (ps$e.ind==0) {
            cat("\nI could not evaluate your chunk without error. The hint may therefore not have worked properly.")
          } else {
            cat("\nUps, there was some error when evaluating the hint.")
          }
        }
        log.event(type="hint",chunk=ps$chunk.ind, ex=ps$ex.ind, e.ind=e.ind)
      } else {
        do.log = FALSE
        cat("Sorry, but there is no hint for your current problem.")
      }
    } else {
      res = try(eval.fun(hint.expr))
      if (is("res","try-error")) {
        if (ps$e.ind==0) {
          cat("\nI could not evaluate your chunk without error. The hint may therefore not have worked properly.")
        } else {
          cat("\nUps, there was some error when evaluating the hint.")
        }
      }
      if (!is.null(chunk.hint)) {
        res = try(eval.fun(chunk.hint))
        if (is("res","try-error")) {
          if (ps$e.ind==0) {
            cat("\nI could not evaluate your chunk without error. The hint may therefore not have worked properly.")
          } else {
            cat("\nUps, there was some error when evaluating the hint.")
          }
        }
      }
    }
    
  }

  
  if (do.log)
    log.hint(chunk.ind=ps$chunk.ind, ps=ps)
  
  #ps$chunk.ind
  #ps$e.ind
  invisible("")
}




log.hint = function(chunk.ind = ps$chunk.ind, ex.ind = ps$ex.ind, e.ind = ps$e.ind, ps=get.ps()) {
  log.event(type="hint",chunk=ps$chunk.ind, ex=ps$ex.ind, e.ind=ps$e.ind)

  # Update ups statistics
  if (isTRUE(ps$e.ind>0)) {
    ups = get.ups()
    update = isTRUE(!ups$cu$solved[chunk.ind])
    
    if (update) {
      ups$cu$num.hint[chunk.ind] = ups$cu$num.hint[chunk.ind]+1
      update.ups(hint=chunk.ind)
    }
  }
}

#' Default hint for a call
#' @export
hint.for.function = function(code, ..., ps = get.ps()) {
  code = substitute(code)
  restore.point("hint.for.function")

  if (isTRUE(ps$noeval) | isTRUE(ps$hint.noeval)) {
    display("Sorry, the default hint for your function requires to evaluate your code, but this is forbidden for security reasons on this server.")
    return()
  }

  #stop()
  part.str = ""
  stud.env = ps$stud.env
  env = new.env(parent=ps$stud.env)
  eval(code,env)
  fun.name = ls(env)[1]
  sol.fun = get(fun.name,env)

  if (!exists(fun.name, stud.env)) {
    display("You must assign a function to the variable ", fun.name)
    return()
  }
  stud.fun = get(fun.name, stud.env)
  if (!is.function(stud.fun)) {
    display("You must assign a function to the variable ", fun.name)
    return()
  }
  display("\nTake a look at the variables test.your.res and test.sol.res to compare the results of your function with the official solution from the test call that your function has failed.")

  args.sol = names(formals(sol.fun))
  args.stud = names(formals(stud.fun))
  if (!identical(args.sol, args.stud)) {
    display("\nYour function ", fun.name, " has different arguments than the official solution:")
    display("   Your fun.     :", paste0(args.stud, collapse=", "))
    display("   Solution fun. :", paste0(args.sol, collapse=", "))
  }
  has.codetools = suppressWarnings(require(codetools, quietly=TRUE, warn.conflicts=FALSE))
  if (has.codetools) {
    #sol.glob = findGlobals(sol.fun, merge=FALSE)$variables
    stud.glob = findGlobals(stud.fun, merge=FALSE)$variables
    if (length(stud.glob)>0) {
      stud.glob =  paste0(stud.glob,collapse=", ")
      display("\nWarning: Your function uses the global variable(s) \n    ",stud.glob,
              "\nOften global variables in a function indicate a bug and you just have forgotten to assign values to ", stud.glob, " inside your function. Either correct your function or make sure that you truely want to use these global variables inside your function.")
    }
  } else {
    display("Please install from CRAN the package 'codetools' to get more information about possible errors in your function. After you have installed the package, type hint() again.")
    return()
  }
}

#' Default hint for a call
#' @export
hint.for.call = function(call, ps=get.ps(), env = ps$stud.env, stud.expr.li = ps$stud.expr.li, part=ps$part, from.assign=!is.null(lhs), lhs = NULL, call.obj = NULL,s3.method=NULL, start.char="\n", end.char="\n") {
  if (!is.null(call.obj)) {
    call = call.obj
  } else {
    call = substitute(call)
  }
  restore.point("hint.for.call")

  part.str = ifelse(is.null(part),"",paste0(" in part ", part))

  ce = match.call.object(call, envir=match.call.object.env(),s3.method=s3.method)
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
        display("You must correctly call the function '", check.na,"'", part.str,".", start.char=start.char, end.char=end.char)
      if (from.assign)
        display("You must assign to '", lhs, "' a correct call to the function '", check.na,"'", part.str,".", start.char=start.char, end.char=end.char)
      return(invisible())
    }

    # Environment in which argument values shall be evaluated. Is a data frame
    # if the function is a dplyer function like mutate(dat,...)
    if (isTRUE(ps$noeval) | isTRUE(ps$hint.noeval)) {
      val.env = NULL
    } else {
      val.env = env
      if (is.dplyr.fun(check.na)) {
        val.env = eval(cde$arg[[".data"]],env)
      }
    }

    analyse.str = lapply(stud.expr.li, function(se) {
      ret = compare.call.args(stud.call=se, check.call=ce, compare.vals = !is.null(val.env), val.env = val.env, s3.method=s3.method)
      s = NULL
      if (length(ret$differ.arg)>0) {
        s = c(s,paste0("Your argument ", ret$differ.arg, " = ", ret$stud.arg[ret$differ.arg], " differs in its ", ret$differ.detail, " from my solution."))
      }
      if (length(ret$extra.arg)>0) {
        s = c(s,paste0("In my solution I don't use the argument '", ret$extra.arg,"'"))
      }
      if (length(ret$missing.arg)>0) {
        s = c(s,paste0("You don't use the argument '", ret$missing.arg,"'"))
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
      display("Let's take a look at your call to the function '", check.na, "'",part.str,":\n", analyse.str,start.char=start.char, end.char=end.char)
    if (from.assign)
      display("Let's take a look at your assignment to '", lhs, "', which should call the function '", check.na, "'",part.str,":\n", analyse.str,start.char=start.char, end.char=end.char)

  } else if (cde$type == "chain") {
    return(inner.hint.for.call.chain(stud.expr.li=stud.expr.li, cde=cde,ce=ce, assign.str=assign.str, ps = ps, env=env))
  }  else if (cde$type == "math") {
    restore.point("math.fail")
    hint.str = scramble.text(deparse(call),"?",0.4, keep.char=" ")

    if (from.assign) {
      display("You have to assign a correct formula to the variable '", lhs, "'. Here is a scrambled version of my solution with some characters being hidden by ?:\n\n ",lhs ," = ", hint.str, start.char=start.char, end.char=end.char)
    } else {
      display("You have to enter a correct formula... Here is a scrambled version of my solution with some characters being hidden by ?:\n\n  ", hint.str, start.char=start.char, end.char=end.char)
    }

  }  else if (cde$type == "var") {
    if (!from.assign)
      display("You shall simply show the variable '",cde$na, "' by typing the variable name in your code.", start.char=start.char, end.char=end.char)
  } else {
    display("Sorry... I actually do not have a hint for you.", start.char=start.char, end.char=end.char)
  }

  return(invisible())

}

scramble.text = function(txt, scramble.char="?", share=0.5, keep.char=" ") {
  vec = strsplit(txt, "")[[1]]

  keep = which(!(vec %in% keep.char))

  n = length(keep)
  ind = sample.int(n,round(n*share), replace=FALSE)
  vec[keep[ind]] = scramble.char
  paste0(vec, collapse="")
}

#' Default hint for an assignment
#' @export
hint.for.assign = function(expr, ps=get.ps(), env = ps$stud.env, stud.expr.li = ps$stud.expr.li, part=ps$part, s3.method=NULL, expr.object=NULL,start.char="\n", end.char="\n",...) {
  if (!is.null(expr.object)) {
    expr = expr.object
  } else {
    expr = substitute(expr)
  }
  restore.point("hint.for.assign")

  ce = match.call.object(expr,s3.method=s3.method, envir=match.call.object.env())
  ce = standardize.assign(ce)

  ce.rhs = match.call.object(ce[[3]],s3.method=s3.method, envir=match.call.object.env())
  dce.rhs = describe.call(call.obj=ce.rhs)

  stud.expr.li = lapply(stud.expr.li, standardize.assign)
  stud.expr.li = stud.expr.li[(!sapply(stud.expr.li,is.null))]

  # Check names
  var = deparse1(ce[[2]])
  stud.var = sapply(stud.expr.li, function(e) deparse1(e[[2]]))
  stud.expr.li = stud.expr.li[stud.var == var]

  se.rhs.li = lapply(stud.expr.li, function(e) match.call.object(e[[3]], envir=match.call.object.env(),s3.method=s3.method))

  hint.for.call(call.obj=ce.rhs, ps=ps,env=env, stud.expr.li=se.rhs.li,part=part, lhs=var,s3.method=s3.method, start.char=start.char, end.char=end.char)
}


#' Default hint for a compute block
#' @export
hint.for.compute = function(expr, hints.txt=NULL,var="", ps=get.ps(), env = ps$stud.env, stud.expr.li = ps$stud.expr.li, part=ps$part,start.char="\n", end.char="\n",...) {
  expr = substitute(expr)
  restore.point("hint.for.compute")

  if (isTRUE(ps$noeval) | isTRUE(ps$hint.noeval)) {
    display("Sorry, the default hint requires to evaluate your code, but this is forbidden for security reasons on this server. I show you the solution instead:")
    sol.txt = ps$cdt$sol.txt[[ps$chunk.ind]]
    display(sol.txt)
    return()
  }


  expr.li = as.list(expr[-1])
  i = 1
  if (length(expr.li)>1) {
    cat("You can compute ", var, " in different ways: hint() will guide you through the ", length(expr.li), " steps used in the sample solution.\n",sep="")
  }
  i=1
  for (i in seq_along(expr.li)) {
    e = expr.li[[i]]
    ret = FALSE
    if (!is.null(hints.txt[[i]])) {
      display("Step ", i,". ",hints.txt[[i]],"...", end.char="")
    }

    var = deparse1(e[[2]],collapse="\n")
    exists = check.var.exists(var)
    if (!exists) {
      break
    }
    tryCatch(ret <-  check.assign(call.object = e),
      error = function(e) {ex$failure.message <- as.character(e)}
    )
    if (!ret) {
      #message = ps$failure.message
      cat("\n\nYou have not yet correctly created '",var,"'. ",sep="")
      #display(ps$failure.message)
      hint.for.assign(expr.object=e, start.char="")
      break
    } else {
      cat(" looks good!\n")
      #message = ps$success.message
      #display(message)
    }
  }
  if (ret==FALSE & i < length(expr.li) & !isTRUE(ps$is.shiny)) {
    display("Note: If you have finished this step and want a hint for the next step. Check your problem set with Ctrl-Alt-R before you type hint() again.")
  }
  if (ret==TRUE) {
    display("Great, all steps seem correct. Check your solution to proceed.")
  }
}


is.dplyr.fun = function(na) {
  na %in% c("mutate","filter","select","arrange","summarise","summarize")
}

inner.hint.for.call.chain = function(stud.expr.li, cde, ps = get.ps(), ce=NULL, assign.str=assign.str,start.char="\n", end.char="\n", env=ps$stud.env,...) {

  restore.point("inner.hint.for.call.chain")


  compare.vals = ! (isTRUE(ps$noeval) | isTRUE(ps$hint.noeval))

  # if (isTRUE(ps$noeval)) {
  #   display("Sorry, the default hint requires to evaluate your code, but this is forbidden for security reasons on this server. I show you the complete solution instead:")
  #   sol.txt = ps$cdt$sol.txt[[ps$chunk.ind]]
  #   display(sol.txt)
  #   return()
  # }

  op = cde$name
  chain.na = sapply(cde$arg, name.of.call)
  comb.chain.na = paste0(chain.na,collapse=";")

  sde.li = lapply(stud.expr.li, function(se) describe.call(call.obj=se))
  scomb.chain.na = sapply(sde.li, function(sde){
     paste0(sapply(sde$arg, name.of.call), collapse=";")
  })

  correct.calls = which(scomb.chain.na == comb.chain.na)
  chain.str = paste0(chain.na, "???", collapse = paste0(" ",op,"\n  "))
  chain.str = paste0(assign.str, chain.str)


  if (length(correct.calls)==0) {
    display("My solution consists of a chain of the form:\n\n", chain.str,"\n\nThe ??? may stand for some function arguments wrapped in () that you must figure out.", start.char=start.char, end.char=end.char)
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

    # compare with checking values
    if (compare.vals) {
      cval = eval(ccall, env)
      new.correct = sapply(seq_along(scall.li), function(j) {
        #if (!correct[j]) return(FALSE)
        is.same(eval(scall.li[[j]],env),cval)
      })

    # compare without checking values
    } else {
      new.correct = sapply(seq_along(sde.li), function(j) {
        #if (!correct[j]) return(FALSE)
        compare.calls(stud.call = sde.li[[j]]$args[[i]], check.call = cde$args[[i]],compare.vals = FALSE,val.env = NULL)$same
      })
    }

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
    ccall = base::parse(text=ccode,srcfile=NULL)
    sde.li = sde.li[correct]
    stud.expr.li = stud.expr.li[correct]
    scode.li = scode.li[correct]
    scode.li = lapply(seq_along(sde.li), function(j)
      paste(scode.li[[j]], op, deparse1(sde.li[[j]]$arg[[i]]))
    )
    scall.li = lapply(scode.li, function(scode) parse(text=scode,srcfile=NULL))
  }

  if (!compare.vals) {
    if (fail == 1) {
      display("You don't have even the first element of the chain correct.")
      return(invisible())

    } else if (fail > 1) {

      wrong.call.na = name.of.call(cde$arg[[fail]])

      display("Your element ", fail,", the call to '", wrong.call.na,"', is different from the sample solution:")

      scall.str = sapply(sde.li, function(sde) {
        sna = sapply(sde$arg, deparse1)
        err.code = rep("", length(sna))
        err.code[fail] = " !!"
        paste0(sna[1]," ",op,err.code[1],paste0("\n   ", sna[-1]," ", op,err.code[-1], collapse=""))
      })
      display(scall.str)
      display("You must call ", deparse1(cde$args[[fail]]),".")
      return(invisible())
    } else if (fail==0) {
        display("Hmm, it actually looks like you have the correct commands. The test should pass pass...")
    }


  } else if (compare.vals) {

    if (fail == 1) {
      display("You don't have even the first element of the chain correct.")
      return(invisible())
    } else if (fail > 1) {
      wrong.call.na = name.of.call(cde$arg[[fail]])
      if (fail == 2) {
        display("In your following chain, I can detect wrong results already after the second element '", wrong.call.na,"':")
      } else {
        display("In your following chains, I can detect wrong results already after element ", fail,", the call to '", wrong.call.na,"':")
      }
      scall.str = sapply(sde.li, function(sde) {
        sna = sapply(sde$arg, deparse1)
        err.code = rep("", length(sna))
        err.code[fail] = " !! WRONG RESULTS !!"
        paste0(sna[1]," ",op,err.code[1],paste0("\n   ", sna[-1]," ", op,err.code[-1], collapse=""))
      })
      display(scall.str)
      if (wrong.call.na=="group_by") {
        display("\nNote: For group_by(...) RTutor requires the groups and their order to be equal to the sample solution. You must call ", deparse1(ccall[[1]][[fail]]),".")
      }

      return(invisible())
    } else if (fail==0) {
        display("Hmm, it actually looks like you have a correct command. It is strange that the test did not pass...")
    }
  }

}
