# Functions for checking student solutions
# Tests are implemented in in tests_for_ps.r

#' Checks a student problem set
#'
#' The command will be put at the top of a student's problem set. It checks all exercises when the problem set is sourced. If something is wrong, an error is thrown and no more commands will be sourced.
#'@export
check.problem.set = function(ps.name,stud.path, stud.short.file, reset=FALSE, set.warning.1=TRUE, user.name="GUEST", do.check=interactive(), verbose=FALSE, catch.errors=TRUE, from.knitr=isTRUE(getOption('knitr.in.progress')) | !interactive(), use.null.device=TRUE, just.init=FALSE, stud.code=NULL) {

  restore.point("check.problem.set", deep.copy=FALSE)


  if (from.knitr) {
    # Allows knitting to HTML even when there are errors
    knitr::opts_chunk$set(error = TRUE)
    ps = NULL
    try(ps <- get.or.init.ps(ps.name,user.name, stud.path, stud.short.file, reset), silent=TRUE)

    # Copy extra code into globalenv
    if (!is.null(ps$rps$extra.code.env)) {
      copy.into.env(source=ps$rps$extra.code.env, dest = globalenv())
    }
    return()
  }

  # If called from knitr, I don't want to check by default
  if (!do.check) return("not checked")



  if (set.warning.1) {
    if (options()$warn<1)
      options(warn=1)
  }
  if (!isTRUE(try(file.exists(stud.path),silent = TRUE))) {
    str= paste0("I could not find your problem set directory '", stud.path,"'.  Please set in the first code chunk of the your problem set the variable 'ps.dir' to the directory in which you have saved your problem set.

Note: use / instead of \\ to separate folders in 'ps.dir'")
    stop(str,call. = FALSE)
  }
  if (!file.exists(paste0(stud.path,"/", stud.short.file))) {
    str= paste0("I could not find your file '", stud.short.file,"' in your problem set folder '",stud.path,"'. Please set the variables ps.dir and ps.file to the right values in the first chunk of your problem set. The variable 'ps.file' must have the same file name than your problem set file.")
    stop(str,call. = FALSE)
  }

  setwd(stud.path)

  if (user.name=="ENTER A USER NAME HERE") {
    stop("You have not picked a user name. Change the variable user.name in the first chunk on the top of your your problem set file from 'ENTER A USER NAME HERE' to your user name that you can freely pick. E.g. write there\n\nuser.name = 'Erin Solstice'",call. = FALSE)
  }

  log.event(type="check_ps")

  if (verbose)
    display("get.or.init.ps...")

  ps = get.or.init.ps(ps.name,user.name,stud.path, stud.short.file, reset)
  ps$catch.errors = catch.errors
  ps$use.null.device = use.null.device

  set.ps(ps)
  ps$warning.messages = list()

  cdt = ps$cdt
  edt = ps$edt

  if (is.null(stud.code)) {
    ps$stud.code = readLines(ps$stud.file, warn=FALSE)
  } else {
    ps$stud.code = stud.code
  }
  cdt$stud.code = get.stud.chunk.code(ps=ps)
  cdt$code.is.task = cdt$stud.code == cdt$task.txt
  cdt$chunk.changed = cdt$stud.code != cdt$old.stud.code
  cdt$old.stud.code = cdt$stud.code

  #test.code.df = data.frame(stud.code = cdt$stud.code, old.stud.code = cdt$old.stud.code)

  ex.changed = summarise(group_by(as.data.frame(cdt),ex.ind), ex.changed = any(chunk.changed))$ex.changed

  ex.check.order = unique(c(which(ex.changed),ps$ex.last.mod, which(!edt$ex.solved)))

  if (! any(cdt$chunk.changed)) {
    #code.change.message = "\nBTW: I see no changes in your code... did you forget to save your file?"
    code.change.message = NULL
  } else {
    code.change.message = NULL
  }

  ps$cdt = cdt

  if (just.init) return(invisible())

  # Check exercises
  i = 1
  # i = 8
  for (i in ex.check.order) {
    ps$ex.last.mod = i
    ex.name = edt$ex.name[i]
    ret <- FALSE
    if (verbose) {
      display("### Check exercise ", ex.name ," ######")
    }

    if (!is.false(ps$catch.errors)) {
      ret = tryCatch(check.exercise(ex.ind=i, verbose=verbose),
                   error = function(e) {ps$failure.message <- as.character(e)
                                        return(FALSE)})
    } else {
      ret = check.exercise(ex.ind=i, verbose=verbose)
    }
    # Copy variables into global env
    copy.into.envir(source=ps$stud.env,dest=.GlobalEnv, set.fun.env.to.dest=TRUE)
    save.ups()
    if (ret==FALSE) {
      edt$ex.solved[i] = FALSE
      if (cdt$code.is.task[ps$chunk.ind]) {
        
        message = paste0("\nYou have not yet started with chunk ", cdt$chunk.name[ps$chunk.ind],"\nIf you have no clue how to start, try hint().")
        if (isTRUE(identical(cdt$sol.txt[ps$chunk.ind], cdt$stud.code[ps$chunk.ind]))) {
          message = paste0(message,"\n\nUps, it also looks like there is an error in the sample solution of chunk ", cdt$chunk.name[ps$chunk.ind],". Running it yields:\n\n", paste0(ps$failure.message, collapse="\n"))
        }
        cat(paste0(message,"\n"))
        return()
        #stop.without.error(message)
      }

      message = ps$failure.message
      if (isTRUE(ps$current.hint.on.fail)) {
        hint.txt = tryCatch(merge.lines(capture.output(hint(ps=ps))), error = function(e) {merge.lines(as.character(e))})
        message = paste0(message,"\nHint:", hint.txt)
      } else {
        message = paste0(message,"\nFor a hint, type hint() in the console and press Enter.")
      }
      message = paste(message,code.change.message)


      stop(message, call.=FALSE, domain=NA)
    } else if (ret=="warning") {
      message = paste0(ps$warning.messages,collapse="\n\n")
      message(paste0("Warning: ", message))
    }
    edt$ex.solved[i] = TRUE
  }

  if (all(edt$ex.solved)) {
    display("\n****************************************************")
    stats()
    msg = "\nYou solved the problem set. Congrats!"
    cat(msg)
    return()
    #stop.without.error(msg)
  }
  stop("There were still errors in your solution.")
}

check.exercise = function(ex.ind, verbose = FALSE, ps=get.ps(), check.all=FALSE) {
  restore.point("check.exercise")

  ck.rows = ps$cdt$ex.ind == ex.ind
  cdt = ps$cdt
  ex.name = ps$edt$ex.name[ex.ind]

  if (check.all) {
    min.chunk = min(which(ck.rows))
  } else {
    rows = which(ck.rows & ((!cdt$is.solved) | cdt$chunk.changed))
    # All chunks solved and no chunk changed
    if (length(rows)==0) {
      cat(paste0("\nAll chunks were correct and no change in exercise ",ex.name,"\n"))
      return(TRUE)
    }
    

    # Find chunks that are skipped because they are optional
    # and have not been changed.
    skip.rows = !cdt$chunk.changed[rows] & cdt$optional[rows]
    skip.rows = cummin(skip.rows)==1 # Only skip from beginning
    skip.rows = rows[skip.rows]
    
    if (length(skip.rows)>0) {
      rows = setdiff(rows, skip.rows)
      # If only optional chunks remain, try to solve them.
      if (length(rows)==0) {
        #cat(paste0("\nOnly optional chunks remain unsolved and unchecked in exercise ",ex.name,". To check an optional chunk, edit it, e.g. just enter a space. Then check the problem set again.\n"))
        #return(TRUE)
        cat(paste0("\nThere are some unsolved optional chunks left in exercise ",ex.name,". If you want to check the next exercise, just change a chunk in the next exercise. E.g. just enter a space.\n"))
        rows = skip.rows
      } else {
        cat(paste0("\nSkip unchanged optional chunk(s) ", paste0(cdt$chunk.name[skip.rows], collapse=", ","...\n")))
      }
    }
    min.chunk = min(rows)
    
  }
  chunks = min.chunk:max(which(ck.rows))
  chunk.ind = chunks[1]
  for (chunk.ind in chunks) {
    cat(paste0("Check chunk ",ps$cdt$chunk.name[[chunk.ind]],"..."))
    ret = check.chunk(chunk.ind,ps=ps, verbose=verbose)
    if (ret==FALSE) {
      return(FALSE)
    }
  }
  #if (NROW(ps$edt)==1) {
    # otherwise data.table throws strange error
    ps$edt$ex.final.env[[ex.ind]] = list(copy(ps$stud.env))
  #}
  return(TRUE)
}


# Check the student's solution of an exercise contained in stud.file
# @param ex.name The name of the exercise
# @param stud.code The code of the student's solution as a string (or vector of strings)
# @export
check.chunk = function(chunk.ind,ps=get.ps(), verbose=FALSE,stud.code=ps$cdt$stud.code[[chunk.ind]], stud.env=make.chunk.stud.env(chunk.ind, ps), expect.change = FALSE, store.output=TRUE, noeval = isTRUE(ps$noeval), precomp=isTRUE(ps$precomp)) {
  restore.point("check.chunk")

  ck = ps$cdt[chunk.ind,]
  chunk.name = ck$chunk.name
  ps$stud.env = stud.env
  
  ps$current.hint.on.fail = isTRUE(ps$rps$hint.on.fail)
  
  ps$cdt$old.stud.code[chunk.ind] = stud.code
  ps$cdt$is.solved[chunk.ind] = FALSE


  ps$ex.ind = ck$ex.ind
  ps$chunk.ind = chunk.ind
  ps$chunk.name = chunk.name
  ps$e.ind = 0

  #stop("analyse below")
  test.li = ck$test.expr[[1]]

  if (expect.change)  {
    if (stud.code == ck$task.txt) {
      ck$chunk.changed = FALSE
      ps$failure.message = paste0("You have not yet changed chunk ", chunk.name)
      return(FALSE)
    }
  }
  if (verbose)
    display("Check chunk ", chunk.name," ...")

  ps$success.log = ps$test.log = NULL
  ps$failure.message  = "No failure message recorded"
  ps$warning.messages = list()
  ps$check.date = Sys.time()

  has.error = FALSE
  ps$stud.expr.li = NULL
  if (verbose) {
    display("parse stud.code...")
  }
  if (!is.false(ps$catch.errors)) {
    tryCatch( ps$stud.expr.li <- base::parse(text=stud.code, srcfile=NULL),
              error = function(e) {
                ps$failure.message=paste0("parser error: ",geterrmessage())
                has.error <<- TRUE
              })
    # Try again with replacing placeholders
    if (has.error) {
      placeholder = get.placeholder(ps)
      if (!is.null(placeholder)) {
        if (has.substr(stud.code, placeholder)) {
          has.error = FALSE
          stud.code = gsub(placeholder, ".PH_._", stud.code)
   tryCatch( ps$stud.expr.li <- base::parse(text=stud.code, srcfile=NULL),
              error = function(e) {
                ps$failure.message=paste0("parser error: ",geterrmessage())
                has.error <<- TRUE
              })
          
        }
      }
    }
    
  } else {
    ps$stud.expr.li <- base::parse(text=stud.code, srcfile=NULL)
  }
  if (has.error) 
    return(FALSE)


  if (isTRUE(ps$check.whitelist)) {
    if (verbose)
      display("check whitelist")
    res = rtutor.check.whitelist(ps$stud.expr.li,ps=ps)
    if (!res$ok) {
      ps$failure.message=paste0("security error: ",res$msg)
      return(FALSE)
    }
  }


  has.error = FALSE
  if (!isTRUE(ps$precomp))
    ps$cdt[["stud.env"]][[chunk.ind]] = stud.env


  has.error = FALSE
  ps$stud.seed = as.integer(Sys.time())
  set.seed(ps$stud.seed)

  if (verbose) {
    display("eval stud.code...")
  }

  #eval(ps$stud.expr.li, ps$stud.env)
  ps$e.ind = 0

  # run student code in
  if (!isTRUE(ps$noeval)) {
    # We may not store output for speed reasons
    # storing output slows down checking of chunk if large
    # data frame is shown
    if (!store.output) ps$chunk.console.out=""
    has.error = !stepwise.eval.stud.expr(stud.expr=ps$stud.expr.li,stud.env=stud.env, store.output=store.output)

    # Update 19.07.2019: Don't stop after error
    # if we have multiple expressions to test
    # The error may have occured in later expressions only
    if (has.error & length(ck$e.li[[1]])==1) {
      log.event(type="check_chunk",chunk=chunk.ind, ex=ck$ex.ind,e.ind=0,code=stud.code, ok=FALSE,message=ps$failure.message)

      update.ups.chunk.check(passed=FALSE,chunk.ind=chunk.ind, save=TRUE, ps=ps)

      return(FALSE)
    }
  }

  had.warning = FALSE
  if (verbose) {
    display("run tests...")
  }

  e.ind = 1

  tdt.ind = which(ps$tdt$chunk.ps.ind == chunk.ind)[1]-1

  # Turn graphics device off during check
  if (isTRUE(ps$use.null.device)) {
    try(png("NUL"), silent=TRUE)
    # Back to normal graphics device
    on.exit(try(dev.off(), silent=TRUE),add = TRUE)
  }


  for (e.ind in seq_along(ck$e.li[[1]])) {
    ps$e.ind = e.ind
    tests = ck$test.expr[[1]][[e.ind]]
    test.ind = 1
    for (test.ind in seq_along(tests)){
      tdt.ind = tdt.ind +1
      ps$tdt.ind = tdt.ind
      test = tests[[test.ind]]
      ps$success.message = NULL
      passed.before = ps$tdt$test.passed[tdt.ind]
      if (verbose) {
        display("  Test #", test.ind, ": ",deparse1(test))
      }
      ret = eval(test,ps$ps.basenv)
      ps$tdt$test.passed[tdt.ind] = ret
      #update.log.test.result(ret,ups, ck, ps)

      if (ret==FALSE) {
        log.event(type="check_chunk",chunk=chunk.ind, ex=ck$ex.ind,e.ind=e.ind,code=stud.code, ok=FALSE,message=ps$failure.message)

        ps$test.log = c(ps$test.log, ps$failure.message)
        # Back to normal graphics device
        #if (isTRUE(ps$use.null.device))
        #  try(dev.off(), silent=TRUE)
        update.ups.chunk.check(passed=FALSE,chunk.ind=chunk.ind, save=TRUE, ps=ps)

        return(FALSE)
      } else if (ret=="warning") {
        had.warning = TRUE
        ps$test.log = c(ps$test.log, ps$warning.message)
      } else {
        ps$test.log = c(ps$test.log, ps$success.message)
        if (!is.null(ps$success.message) & !passed.before) {
          ps$success.log = c(ps$success.log,ps$success.message)
          cat(paste0(ps$success.message,"\n"))
        }
      }
    }
  }

  # Back to normal graphics device
  #if (isTRUE(ps$use.null.device))
  #  try(dev.off(), silent=TRUE)

  ps$cdt$is.solved[[chunk.ind]] = TRUE

  if (!is.na(ck$award.name)) {
    give.award(ck$award.name, ps=ps)
    if (isTRUE(ps$is.shiny))
      show.shiny.award(ck$award.name)
  }

  log.event(type="check_chunk",chunk=chunk.ind, ex=ck$ex.ind,e.ind=0,code=stud.code, ok=TRUE,message="")
  update.ups.chunk.check(passed=TRUE,chunk.ind=chunk.ind, save=TRUE, ps=ps)

  if (had.warning) {
    return("warning")
  } else {
    return(TRUE)
  }
}


update.ups.chunk.check = function(passed, chunk.ind = ps$chunk.ind, ups=get.ups(), ps=get.ps(), save=TRUE) {
  restore.point("update.ups.chunk.check")

  update = isTRUE(try(!ups$cu$solved[chunk.ind], silent=TRUE))

  if (update) {
    if (is.na(ups$cu$first.check.date[chunk.ind]))
      ups$cu$first.check.date[chunk.ind] = Sys.time()

    if (passed) {
      ups$cu$solved.date[[chunk.ind]] <- Sys.time()
      ups$cu$solved[chunk.ind] = TRUE
    } else {
      ups$cu$num.failed[chunk.ind] = ups$cu$num.failed[chunk.ind]+1
    }
  }
  update.code = isTRUE(ps$ups.save$code)
  if (update.code) {
    ups$cu$stud.code[[chunk.ind]] = ps$cdt$stud.code[[chunk.ind]]
  }
  if (passed) {
    ups.chunk.ind = chunk.ind +1
    if (ups.chunk.ind > NROW(ps$cdt)) ups.chunk.ind = 1
  } else {
    ups.chunk.ind = chunk.ind
  }
  
  if (update | update.code | isTRUE(ps$ups.save$chunk.ind)) {
    update.ups(ups, 
      chunk = if (update) chunk.ind else NULL,
      code  = if (update.code) chunk.ind else NULL,
      chunk.ind = ups.chunk.ind
    )
  }
  
}

update.log.test.result = function(...) {
  return()
}

make.chunk.stud.env = function(chunk.ind, ps = get.ps()) {
  restore.point("make.chunk.stud.env")

  # return emptyenv if no student code
  # shall ever be evaluated
  if (isTRUE(ps$noeval)) {
    return(emptyenv())
  }


  # return precomputed chunkenv
  if (isTRUE(ps$precomp)) {
    stud.env = copy.stud.env(ps$cdt[["stud.env"]][[chunk.ind]], chunk.ind)
    return(stud.env)
  }


  ck = ps$cdt[chunk.ind,]

  cdt = ps$cdt

  # Find index of closest non-optional parent
  ex.ind = ck$ex.ind
  non.optional = which(cdt$ex.ind == ex.ind & !cdt$optional)
  non.optional = non.optional[non.optional < chunk.ind]

  if (length(non.optional)==0) {
    start.ex = TRUE
  } else {
    parent.ind = max(non.optional)
    start.ex = FALSE
  }


  if (start.ex) {
    # First chunk in exercise: generate new stud.env
    stud.env = new.stud.env(chunk.ind)
    import.var.into.stud.env(ps$edt$import.var[[ck$ex.ind]], stud.env,ps)

  } else {
    # Later chunk in an exercise: simply copy previous stud.env
    stud.env = copy.stud.env(ps$cdt[["stud.env"]][[parent.ind]], chunk.ind)
  }
  stud.env
}

# Import variables from other exercises stud.env's
import.var.into.stud.env = function(import.var, dest.env, ps = get.ps()) {
  restore.point("import.var.into.stud.env")
  if (is.null(import.var))
    return(NULL)
  restore.point("import.var.into.stud.env2")

  #stop("jbhgbhbdgh")
  i = 1
  edt = ps$edt
  ex.names = edt$ex.name

  for (i in seq_along(import.var)) {
    ex.name = names(import.var)[i]
    if (!ex.name %in% ex.names) {
      ind = str.starts.with(ex.names,ex.name)
      if (!any(ind)) {
        stop(paste0("\nWrong import variable statement in solution file of exercise  ",ex.names[i], ": exercise ", ex.name, " not found"))
      } else {
        ex.name = ex.names[ind][1]
      }
    }

    vars = import.var[[i]]
    ex.ind = which(edt$ex.name==ex.name)
    source.env = edt$ex.final.env[[ex.ind]]
    if (is.list(source.env)) source.env = source.env[[1]]
    
    if (is.null(source.env)) {
      #str = paste0("\nYou must first solve and check exercise '", ex.name, " before you can solve this exercise.\n To check exercise ", ex.name, " enter somewhere an irrelevant space in it's code chunks.")
      ps$failure.message = str
      stop(str)
      return(FALSE)
    }
    for (var in vars) {
      if (!exists(var,source.env, inherits=FALSE)) {
        str = paste0("You first must correctly generate the variable '", var, "' in exercise ", ex.name, " before you can solve this exercise.")
        ps$failure.message = str
        stop(str)
      }
      val = get(var,source.env)
      # Set enclosing environments of functions to dest.env
      if (is.function(val))
        environment(val) = dest.env
      assign(var, val,dest.env)
    }
  }
  return(TRUE)
}


can.chunk.be.edited = function(chunk.ind, ps = get.ps()) {
  restore.point("can.chunk.be.edited")

  cdt = ps$cdt
  ck = cdt[chunk.ind,]
  ex.ind = ck$ex.ind


  non.optional = which(cdt$ex.ind == ex.ind & !cdt$optional)
  if (length(non.optional)==0) {
    start.ex = TRUE
  } else {
    first.ind = non.optional[1]
    start.ex = chunk.ind <= first.ind
  }

  if (start.ex) {
    if (ex.ind==1)
      return(TRUE)
    ex.names = names(ps$edt$import.var[[ck$ex.ind]])
    if (is.null(ex.names))
      return(TRUE)
    edt = ps$edt
    ex.inds = edt$ex.ind[match(ex.names,edt$ex.name)]

    chunks = which(ps$cdt$ex.ind %in% ex.inds)
    solved = all(ps$cdt$is.solved[chunks] | ps$cdt$optional[chunks])
    if (all(solved))
      return(TRUE)
    ps$failure.message = paste0("You must first solve and check all chunks in exercise(s) ", paste0(ex.names[ex.inds],collapse=", "), " before you can start this exercise.")
    return(FALSE)
  } else {
    ex.rows = which(cdt$ex.ind == ex.ind & cdt$chunk.ps.ind < chunk.ind)
    if (all(ps$cdt$is.solved[ex.rows] | ps$cdt$optional[ex.rows])) {
      return(TRUE)
    }

    ps$failure.message = paste0("You must first solve and check all previous, non-optional chunks in this exercise before you can edit and solve this chunk.")
    return(FALSE)
  }

}

# Extracts the stud's code of a given exercise
# @export
extract.exercise.code = function(ex.name,stud.code = ps$stud.code, ps=get.ps(),warn.if.missing=TRUE) {
  restore.point("extract.r.exercise.code")

  return(extract.rmd.exercise.code(ex.name,stud.code, ps,warn.if.missing))
}

extract.rmd.exercise.code = function(ex.name,stud.code = ps$stud.code, ps=get.ps(),warn.if.missing=TRUE) {
  restore.point("extract.rmd.exercise.code")
  txt = stud.code
  mr = extract.command(txt,paste0("## Exercise "))
  mr[,2] = str_trim(gsub("#","",mr[,2], fixed=TRUE))
  start.ind = which(mr[,2]==ex.name)
  start.row = mr[start.ind,1]
  if (length(start.row) == 0) {
    if (warn.if.missing)
      message(paste0("Warning: Exercise ", ex.name, " not found. Your code must have the line:\n",
                     paste0("## Exercise ",ex.name)))
    return(NA)
  }
  if (length(start.row)>1) {
    message("Warning: Your solution has ", length(start.row), " times exercise ", ex.name, " I just take the first.")
    start.row = start.row[1]
    start.ind = start.ind[1]
  }
  end.row = c(mr[,1],length(txt)+1)[start.ind+1]-1
  str = txt[(start.row+1):(end.row)]

  # Get all code lines with an R code chunk
  hf = str.starts.with(str,"```")
  str = str[cumsum(hf) %% 2 == 1 & !hf]

  paste0(str, collapse="\n")
}



stepwise.eval.stud.expr = function(stud.expr, ps=get.ps(), stud.env = ps$stud.env, seed=NULL, store.output = TRUE, source=NULL) {
  restore.point("stepwise.eval.stud.expr")
  if (!is.null(seed))
    set.seed(seed)
  has.error = FALSE

  err.fun = function(e) {
    msg = paste0("evaluation error in \n  ",
        deparse1(part.expr),"\n  ",adapt.console.err.message(as.character(e)))
    if (has.substr(msg,".PH_._")) {
      placeholder = get.placeholder(ps)
      #msg = paste0("You have not yet replaced all placeholders ", placeholder)
      msg = gsub(".PH_._",placeholder,msg)
    }
    ps$failure.message = msg
    has.error <<- TRUE
  }

  if (store.output) {
    ps$chunk.console.out = ""
    add = function(...) {
      str = paste0(..., collapse="\n")
      if (length(str)>0)
        ps$chunk.console.out = paste0(ps$chunk.console.out,str, sep="\n")
    }
  }

  i = 1
  for (i in seq_along(stud.expr)) {
    part.expr = stud.expr[[i]]

    if (!store.output) {
      tryCatch( eval(part.expr, stud.env),error = err.fun)
    } else {
      if (is.null(source)) {
        add("> ",deparse1(part.expr, collapse="\n+"))
      } else {
        add("> ",paste0(li$source[[i]], collapse="\n+ "))
      }
      out = NULL
      tryCatch(out <- capture.output(eval(part.expr, stud.env)),error = err.fun)
      if (length(out)>0) add(out)
    }
    if (has.error) {
      if (is.false(ps$catch.errors))
        stop(ps$failure.message)

      return(FALSE)
    }
  }
  #cat(ps$chunk.console.out)
  return(!has.error)
}





#' Used inside tests: adds a failure to an exercise
#'
#' @param message a longer description shown to the user
#' @param ... variables that will be rendered into messages that have whiskers
#' @export
add.failure = function(message,..., add.new.line=TRUE, ps= get.ps()) {
  message=replace.whisker(message,...)
  args = list(...)
  restore.point("add.failure")
  ps$failure.message = if (add.new.line) paste0("\n",message) else message
}

#' Used inside tests: adds a sucess message
#'
#' @param message a longer description shown to the user
#' @param ... variables that will be rendered into messages that have whiskers
#' @export
add.success = function(message,...,ps= get.ps()) {
  message=replace.whisker(message,...)
  ps$success.message = message
}


#' Used inside tests: adds a warning
#'
#' @param message a longer description shown to the user
#' @param ... variables that will be rendered into messages that have whiskers
#' @export
add.warning = function(message,...,ps= get.ps()) {
  message=replace.whisker(message,...)
  args = list(...)
  #restore.point("add.warning")
  ind = length(ps$warning.messages)+1
  ps$warning.messages[[ind]] = message
}


get.stud.chunk.code = function(txt = ps$stud.code,chunks = ps$cdt$chunk.name, ps = get.ps()) {
  restore.point("get.stud.chunk.code")
  chunk.start = which(str.starts.with(txt,"```{"))
  chunk.end   = setdiff(which(str.starts.with(txt,"```")), chunk.start)
  chunk.end = remove.verbatim.end.chunks(chunk.start,chunk.end)

  # remove all chunks that have no name (initial include chunk)
  chunk.name = str.between(txt[chunk.start],'"','"', not.found=NA)

  na.chunks  = is.na(chunk.name)
  chunk.start= chunk.start[!na.chunks]
  chunk.end  = chunk.end[!na.chunks]
  chunk.name = chunk.name[!na.chunks]

  chunk.txt = sapply(seq_along(chunk.start), function (i) {
      if (chunk.start[i]+1 > chunk.end[i]-1) return("")
      code = txt[(chunk.start[i]+1):(chunk.end[i]-1)]
      paste0(code, collapse="\n")
  })

  names(chunk.txt) = chunk.name
  chunk.txt = chunk.txt[chunk.name %in% chunks]

  if (!identical(names(chunk.txt), chunks)) {
    missing.chunks = paste0(setdiff(chunks, chunk.name),collapse=", ")
    stop("I miss chunks in your solution: ",missing.chunks,". You probably removed them or changed the title line or order of your chunks by accident. Please correct this!", call.=FALSE)
  }
  chunk.txt
}


adapt.console.err.message = function(str) {
  if (str.starts.with(str,"Error in eval(")) {
    str = paste0("Error: ",str.right.of(str,":"))
  }
  str
}
