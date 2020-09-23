info = function(info.name, ps = get.ps()) {
  restore.point("info")
  if (is.null(ps)) {
    hdisplay("Please check your problem set once. Then you can see infos.")
    return()
  }
  infos = ps$rps$infos
  if (! info.name %in% names(infos)) {
    hdisplay("You problem set has not the info '", info.name, "'. Only the following infos are stored: ", paste0("'", names(infos),"'", collapse=", "))
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

  ps$shown.custom.hints = 0
  ps$stud.expr.li.info = NULL
  
  if (is.null(ps$chunk.ind)) {
    cat("\nPlease test the chunk before you ask for a hint.")
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
      hdisplay(paste0(txt, collapse="\n"))
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

  .stud.code = ps$cdt$stud.code[[ps$chunk.ind]]
  
  hint.env = new.env(parent=parent.env(ps$stud.env))
  copy.into.env(ps$stud.env, hint.env)
  hint.env$.stud.env = ps$stud.env
  hint.env$.stud.code = .stud.code
  
  # No expression set
  if (e.ind == 0) {
    if (!is.null(chunk.hint)) {
      eval.fun(chunk.hint, hint.env)
      log.event(type="hint",chunk=ps$chunk.ind, ex=ps$ex.ind, e.ind=e.ind)
      if (ps$cdt$num.e[[ps$chunk.ind]]>0) {
        cat("\nI can't give you a more specific hint, since I can't run your code, due to an error.")
      }
    } else {
      do.log = FALSE
      if (ps$cdt$num.e[[ps$chunk.ind]]==0) {
        cat("\nSorry, but there is no hint for your current problem.")
      } else {
        cat("\nThere is an error in your code chunk so that RTutor cannot evaluate your code. Before you can get a more detailed hint, write code that runs without error when you manually run your chunk. (One way to get no syntax error is to remove all your own code in your chunk.)")
      }
    }

  # hint for expression ps$e.ind
  } else {
    hint.expr = ps$cdt$hint.expr[[ps$chunk.ind]][[e.ind]]
    if (length(hint.expr)==0) {
      if (!is.null(chunk.hint)) {
        res = try(eval.fun(chunk.hint, hint.env))
        if (is(res,"try-error")) {
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
      res = try(eval.fun(hint.expr, hint.env))
      if (is(res,"try-error")) {
        if (ps$e.ind==0) {
          cat("\nI could not evaluate your chunk without error. The hint may therefore not have worked properly.")
        } else {
          cat("\nUps, there was some error when evaluating the hint.")
        }
      }
      if (!is.null(chunk.hint)) {
        res = try(eval.fun(chunk.hint, hint.env))
        if (is(res,"try-error")) {
          if (ps$e.ind==0) {
            display("\nI could not evaluate your chunk without error. The hint may therefore not have worked properly.")
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

#' A robust implementation of isTRUE
#' 
#' Returns FALSE if evaluation expr yields an
#' error or is not TRUE.
#' 
#' Useful for customized hints were evaluating
#' an expression may often cause errors,
#' e.g. if a user did not define a variable.
true = function(expr, envir=parent.frame()) {
  expr = substitute(expr)
  res = try(eval(expr, envir),silent = TRUE)
  isTRUE(res)
}




log.hint = function(chunk.ind = ps$chunk.ind, ex.ind = ps$ex.ind, e.ind = ps$e.ind, ps=get.ps()) {
  log.event(type="hint",chunk=ps$chunk.ind, ex=ps$ex.ind, e.ind=ps$e.ind)

  # Update ups statistics
  if (isTRUE(ps$e.ind>0)) {
    ups = get.ups()
    update = isTRUE(try(!ups$cu$solved[chunk.ind], silent=TRUE))
    
    if (update) {
      ups$cu$num.hint[chunk.ind] = ups$cu$num.hint[chunk.ind]+1
      update.ups(hint=chunk.ind)
    }
  }
}

#' Default hint for a function
#' @export
hint.for.function = function(code, ..., ps = get.ps()) {
  code = substitute(code)
  restore.point("hint.for.function")

  if (isTRUE(ps$noeval) | isTRUE(ps$hint.noeval)) {
    hdisplay("Sorry, the default hint for your function requires to evaluate your code, but this is forbidden for security reasons on this server.")
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
    hdisplay("You must assign a function to the variable ", fun.name)
    return()
  }
  stud.fun = get(fun.name, stud.env)
  if (!is.function(stud.fun)) {
    hdisplay("You must assign a function to the variable ", fun.name)
    return()
  }
  hdisplay("\nYou may want to take a look at the variables test.your.res and test.sol.res to compare the results of your function with the official solution from the test call that your function has failed.")

  args.sol = names(formals(sol.fun))
  args.stud = names(formals(stud.fun))
  if (!identical(args.sol, args.stud)) {
    hdisplay("\nYour function ", fun.name, " has different arguments than the official solution:")
    hdisplay("   Your fun.     :", paste0(args.stud, collapse=", "))
    hdisplay("   Solution fun. :", paste0(args.sol, collapse=", "))
  }
  has.codetools = suppressWarnings(require(codetools, quietly=TRUE, warn.conflicts=FALSE))
  if (has.codetools) {
    #sol.glob = findGlobals(sol.fun, merge=FALSE)$variables
    stud.glob = findGlobals(stud.fun, merge=FALSE)$variables
    if (length(stud.glob)>0) {
      stud.glob =  paste0(stud.glob,collapse=", ")
      hdisplay("Warning: Your function uses the global variable(s) \n    ",stud.glob,
              "\nOften global variables in a function indicate a bug and you just have forgotten to assign values to ", stud.glob, " inside your function. Either correct your function or make sure that you truely want to use these global variables inside your function.")
    }
    stud.locals = findFuncLocals(NULL,body = body(stud.fun))
    overwritten = intersect(stud.locals, args.stud)
    if (length(overwritten)>0) {
      hdisplay("Warning: Inside your function you assign new values to the function arguments ", paste0(overwritten, collapse=","),". This is typically a mistake.")
    }
    
  } else {
    hdisplay("Please call\n\ninstall.packages('codetools')\n\nin order to install from CRAN the package 'codetools'. This allows RTutor to get more information about possible errors in your function. After you have installed the package, type hint() again.")
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

  has.place.holder = any(sapply(stud.expr.li, has.call.placeholder))
  
  # For filter we show a scrambled version of the 
  # sample solution since arguments are not named
  # and the order does not matter
  
  # We also show a scrambled version if there
  # are placeholders
  scramble.fun = isTRUE(check.na == "filter") | has.place.holder
  
  ggplot.chain = FALSE
  # Check possible ggplot chain
  if (isTRUE(cde$type == "math" & identical(cde$name,"+"))) {
    check.code = deparse1(call)
    if (has.substr(check.code, "ggplot(") | has.substr(check.code, "+ geom_")) {
      ggplot.chain = TRUE
    }
  }
  if (ggplot.chain) {
    cde$type = "fun"
    scramble.fun = TRUE
  }

  assign.str = ifelse(from.assign,paste0(" ",lhs, " = "),"")
  if (cde$type == "fun" & !scramble.fun) {

    # Special cases
    if (check.na=="library") {
      lib =  as.character(cde$arg[[1]])
      has.lib = suppressWarnings(require(lib, character.only=TRUE, quietly=TRUE))
      if (has.lib) {
        hdisplay('Add the command ', deparse1(ce), ' to load the package ',lib,'. It contains functions that we need.')
      } else {
        hdisplay('Add the command ', deparse1(ce), ' to load the package ',lib,'. It contains functions that we need.\n First you must install the package, however. For packages that are on the CRAN or for which you have a local zip or tar.gz file, you can do the installation in RStudio using the menu Tools -> Install Packages.\n(For packages that are only on Github, first load and install the package devtools from CRAN and then use its function install_github.)')
      }
      return(invisible())
    }

    if (length(stud.expr.li)==0) {
      if (!from.assign)
        hdisplay("You must correctly call the function '", check.na,"'", part.str,".", start.char=start.char, end.char=end.char)
      if (from.assign)
        hdisplay("You must assign to '", lhs, "' a correct call to the function '", check.na,"'", part.str,".", start.char=start.char, end.char=end.char)
      return(invisible())
    }

    # Environment in which argument values shall be evaluated. 
    # Is a data frame if the function is a dplyr function
    # like mutate(dat,...)
    if (isTRUE(ps$noeval) | isTRUE(ps$hint.noeval)) {
      val.env = NULL
    } else {
      val.env = env
      if (is.dplyr.fun(check.na)) {
        val.env = eval(cde$arg[[".data"]],env)
      }
    }

    analyse.str = sapply(stud.expr.li, function(se) {
      ret = compare.call.args(stud.call=se, check.call=ce, compare.vals = !is.null(val.env), val.env = val.env, s3.method=s3.method)
      s = ret$descr
      if (length(s)>0) {
        s = gsub("\n","\n   ", s, fixed=TRUE)
        s = paste0("  - ",s, collapse="\n")
      }
      if (!is.null(s)) {
        s = paste0(trimws(assign.str)," ", deparse1(se),"\n\n",s)
      } else {
        s = ""
      }
      s
    })
    analyse.str = analyse.str[nchar(analyse.str)>0]
    if (length(analyse.str)==0) {
      return()
    } else if (length(analyse.str)==1) {
      cat("\nI found problems with your call\n")
    } else {
      cat("\nI found problems with ", length(analyse.str)," of your calls:\n\n")
    }
    analyse.str = paste0(analyse.str, collapse = "\n")
    ph = get.placeholder(ps)
    analyse.str = gsub(".PH_._",ph, analyse.str, fixed=TRUE)
    if (!from.assign)
      #hdisplay("Let's take a look at your call to the function '", check.na, "'",part.str,"\n", analyse.str,start.char=start.char, end.char=end.char)
      cat(analyse.str,"\n")
    if (from.assign)
      #hdisplay("Let's take a look at your assignment to '", lhs, "', which should call the function '", check.na, "'",part.str,":\n", analyse.str,start.char=start.char, end.char=end.char)
      cat(analyse.str,"\n")
  } else if ( (cde$type == "fun" | (cde$type=="chain" & length(stud.expr.li)==1)) & scramble.fun) {
    restore.point("jdhfjhdkfslfj")
    # Only scramble wrong parts if we have a single student
    # expression
    ph.txt = ""
    if (length(stud.expr.li)==1) {
      if (isTRUE(ps$noeval) | isTRUE(ps$hint.noeval)) {
        val.env = NULL
      } else {
        val.env = env
        if (is.dplyr.fun(check.na)) {
          val.env = eval(cde$arg[[".data"]],env)
        }
      }
      
      stud.call = stud.expr.li[[1]]
      res = scramble.call.diffs(stud.call = stud.call,check.call = call,val.env = val.env,ph.max = 1,share = 0.5)
      
      if (res$ph.count > res$ph.max) {
        ph.txt = paste0("and ", get.placeholder())
      }
      
      hint.str = res$scode
      if (cde$type == "chain") {
        hint.str = gsub("%>%","%>%\n\t",hint.str)
      }
    } else {
      hint.str = scramble.text(deparse(call),"?",0.5, keep.char=c(" ",",","(",")","=", "\n"))
    }
    if (from.assign) {
      hdisplay("You have to make a correct assignment to '",lhs,"'. Here is a scrambled solution with some unrevealed ? ",ph.txt,":\n\n ",lhs ," = ", hint.str, start.char=start.char, end.char=end.char)
    } else {
      if (ggplot.chain | cde$type=="chain") {
        start.str = ""
      } else {
        start.str = paste0("You have to enter a correct call to '", check.na,"'. ")
      }
      hdisplay(start.str,"Here is a scrambled solution with some unrevealed ? ",ph.txt,":\n\n  ", hint.str, start.char=start.char, end.char=end.char)
    }
  } else if (cde$type == "chain") {
    return(inner.hint.for.call.chain(stud.expr.li=stud.expr.li, cde=cde,ce=ce, assign.str=assign.str, ps = ps, env=env, call=call))
  }  else if (cde$type == "math" | cde$type == "formula") {
    #restore.point("math.fail")
    hint.str = scramble.text(deparse(call),"?",0.5, keep.char=c(" ","\n","+","(",")"))
    
    if (from.assign) {
      hdisplay("You have to assign a correct formula to the variable '", lhs, "'. Here is a scrambled version of my solution with some characters being hidden by ?:\n\n ",lhs ," = ", hint.str, start.char=start.char, end.char=end.char)
    } else {
      hdisplay("You have to enter a correct formula... Here is a scrambled version of my solution with some characters being hidden by ?:\n\n  ", hint.str, start.char=start.char, end.char=end.char)
    }
  }  else if (cde$type == "var") {
    if (!from.assign)
      hdisplay("You shall simply show the variable '",cde$na, "' by typing the variable name in your code.", start.char=start.char, end.char=end.char)
  }  else if (cde$type == "subset") {
    hint.str = scramble.text(deparse(call),"?",0.6, keep.char=c("[","]", "$",",","\n"))
    if (from.assign) {
      hdisplay("Here is a scrambled version of my solution with some characters being hidden by ?:\n\n ",lhs ," = ", hint.str, start.char=start.char, end.char=end.char)
    } else {
      hdisplay("Here is a scrambled version of my solution with some characters being hidden by ?:\n\n  ", hint.str, start.char=start.char, end.char=end.char)
    }
  
  } else {
    hdisplay("Sorry... I actually do not have a hint for you.", start.char=start.char, end.char=end.char)
  }

  return(invisible())

}

scramble.text = function(txt, scramble.char="?", share=0.5, keep.char=c(" ","\n")) {
  #restore.point("scramble.text")
  txt = merge.lines(txt,"\n")
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

  #ce.rhs = match.call.object(ce[[3]],s3.method=s3.method, envir=match.call.object.env())
  ce.rhs = ce[[3]]
  #dce.rhs = describe.call(call.obj=ce.rhs)

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
    hdisplay("Sorry, the default hint requires to evaluate your code, but this is forbidden for security reasons on this server. I show you the solution instead:")
    sol.txt = ps$cdt$sol.txt[[ps$chunk.ind]]
    hdisplay(sol.txt)
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
      hdisplay("Step ", i,". ",hints.txt[[i]],"...", end.char="")
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
      #hdisplay(ps$failure.message)
      hint.for.assign(expr.object=e, start.char="")
      break
    } else {
      cat(" looks good!\n")
      #message = ps$success.message
      #hdisplay(message)
    }
  }
  if (ret==FALSE & i < length(expr.li) & !isTRUE(ps$is.shiny)) {
    hdisplay("Note: If you have finished this step and want a hint for the next step. Check your problem set before you type hint() again.")
  }
  if (ret==TRUE) {
    hdisplay("Great, all steps seem correct. Check your solution to proceed.")
  }
}

hdisplay = function (..., collapse = "\n", sep = "", start.char="\n",end.char="\n") 
{
  restore.point("hdisplay")
  
  str = paste(start.char, paste(..., collapse = collapse, sep = sep), end.char, sep = "")
  
  # Substitute placeholder back to original term.
  ph = get.placeholder()
  str = gsub(".PH_._",ph, str, fixed=TRUE)
  invisible(cat(str))
}


is.dplyr.fun = function(na) {
  na %in% c("mutate","filter","select","arrange","summarise","summarize")
}

make.expr.li.info = function(expr.li, do.unlist=FALSE) {
  restore.point("make.expr.li.info")
  
  if (do.unlist)
    expr.li = unlist(as.list(expr.li))
  n = length(expr.li)
  is.assign = sapply(expr.li, is.assignment)
  
  matched.expr.li = vector("list",n)
  var = rep("",n)
  
  for (row in seq_len(n)) {
    expr = expr.li[[row]]
    if (is.assign[row]) {
      matched.expr.li[[row]] = match.call.object(expr[[3]])
      var[row] = deparse.assign.var(expr)                         
    } else {
      matched.expr.li[[row]] = match.call.object(expr)
    }
  }

  tibble(
    matched.expr = matched.expr.li,
    var = var,
    is.assign = is.assign
  )
}

deparse.assign.var = function(call) {
  deparse1(call[[2]],collapse="\n")
}

#' Show the hint if the student made the specified wrong assignment
#' 
#' @param var name of the to be assigned variable as character
#' @param call an unquoted call that we check whether the student makes it
#' @param msg a string that shall be shown as hint if the student made the call in his code
hint.stud.assign = function(var, call,msg , ps=get.ps(), env=parent.frame()) {
  call = substitute(call)
  hint.stud.call(qcall=call, msg=msg, ps=ps, env=env, var=var)
}

#' Show the hint if the student calls a specific function
#' 
#' Show the hint message if the student has called
#' a certain function (not nested in another function)
#' somewhere in the chunk. If you also want to consider
#' the call arguments use hint.stud.call or hint.stud.assign
#' instead. 
#' 
#' @param fun.name the function name as string.
#' @param msg a string that shall be shown as hint if the student made the call in his code
hint.stud.fun = function(fun.name, msg, ps=get.ps(), env=parent.frame()) {
  restore.point("hint.stud.fun")
  has.fun = FALSE
  for (call in ps$stud.expr.li) {
    if (!is.call(call)) next
    stud.fun = as.character(call[1])
    if (stud.fun == "=" | stud.fun == "<-") {
      call = call[[3]]
      if (!is.call(call)) next
      stud.fun = as.character(call[1])
    }
    if (identical(fun.name,stud.fun)) {
      has.fun = TRUE
      break
    }
  }
  if (has.fun) {
    cat(paste0("\n",msg,"\n"))
    ps$shown.custom.hints = ps$shown.custom.hints+1 
  }

}

#' Show the hint if the student made the specified wrong call
#' 
#' @param call an unquoted call that we check whether the student makes it
#' @param msg a string that shall be shown as hint if the student made the call in his code
hint.stud.call = function(call, msg="", ps=get.ps(), env=parent.frame(), qcall, var=NULL) {
  if (missing(qcall)) {
    call = substitute(call)
  } else {
    call = qcall
  }
  call = match.call.object(call)
  restore.point("hint_stud_call_inner")
  
  if (is.null(ps$stud.expr.li.info)) {
    ps$stud.expr.li.info =  make.expr.li.info(ps$stud.expr.li)
  }
  
  stud.expr.li.info = ps$stud.expr.li.info
  from.assign = !is.null(var)
  if (from.assign) {
    rows = which(stud.expr.li.info$is.assign & stud.expr.li.info$var == var)
  } else {
    rows = which(!stud.expr.li.info$is.assign)
  }
  if (length(rows)==0) return(invisible())

  stud.expr.li = ps$stud.expr.li.info$matched.expr[rows]

  has.call = FALSE
  for (scall in stud.expr.li) {
    if (identical(call, scall)) has.call = TRUE
  }
  if (has.call) {
    cat(paste0("\n",msg,"\n"))
    ps$shown.custom.hints = ps$shown.custom.hints+1 
  }
}

#' Get or set whether hint.else or
#' auto.hint.else would be triggered.
#' 
#' If a hint.stud.call or hint.stud.assign is shown
#' then a hint.else or auto.hint.else would not be
#' triggered. This function returns TRUE if hint.else
#' would still be triggered or otherwise FALSE.
#' 
#' If you set the argument activate you can change this status. 
hint.else.active = function(activate=NULL, ps = get.ps()) {
  if (is.null(activate)) {
     return(true(ps$shown.custom.hints==0))
  } else {
    if (activate) {
      ps$shown.custom.hints=0
    } else {
      ps$shown.custom.hints=1
    }
    return(activate)
  }
}

#' Show a hint only if no hint.stud.call or hint.stud.assign 
#' was triggered.
#' 
#' It says that the automatic hint should be shown unless
#' some hint with hint.stud.call has been shown (or ps$shown.custom.hints has been manually assigned a value above 0.)
hint.else = function(msg,add.line.breaks=TRUE, ps = get.ps()) {
  if (hint.else.active()) {
    cat(paste0(if (add.line.breaks) "\n",msg, if (add.line.breaks) "\n")) 
  }
}


#' This is just a place holder in a hint block
#' 
#' Only used inside a hint block.
#' 
#' It says that the automatic hint should be shown unless
#' some hint with hint.stud.call has been shown (or ps$shown.custom.hints has been manually assigned a value above 0.)
auto.hint.else = function() {
 cat("\nOh, this looks like an error. The function auto.hint.else() should have been replaced in the hint block. If you design the problem set, make sure that you write it in a separate line and have no spaces between the ().") 
}

#' This is just a place holder in a hint block
#' 
#' Only used inside a hint block.
#' 
#' It says that the automatic hint shall be shown. This
#' makes sense if you want to show the automatic hint in
#' addition to a custom hint. Also see auto_hint_else()
auto.hint = function() {
 cat("\nOh, this looks like an error. The function call auto.hint() should have been replaced in the hint block.  If you design the problem set, make sure that you write it in a separate line and have no spaces between the ().") 
}

