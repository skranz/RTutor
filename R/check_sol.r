# Functions for checking student solutions
# Tests are implemented in in tests_for_ps.r

#' Checks a student problem set
#' 
#' The command will be put at the top of a student's problem set. It checks all exercises when the problem set is sourced. If something is wrong, an error is thrown and no more commands will be sourced.
#'@export
check.problem.set = function(ps.name,stud.path, stud.short.file, reset=FALSE, set.warning.1=TRUE, user.name="GUEST", do.check=interactive(), verbose=FALSE, catch.errors=TRUE) {
  
  restore.point("check.problem.set", deep.copy=FALSE)
  
  # If called from knitr, I don't want to check by default
  if (!do.check) return("not checked")
  
  if (set.warning.1) {
    if (options()$warn<1)
      options(warn=1)
  }
  if (!isTRUE(try(file.exists(stud.path),silent = TRUE))) {
    str= paste0("I could not find your problem set directory '", stud.path,"'.  Please set in the first code chunk of the your problem set the variable 'ps.dir' to the directory in which you have saved your problem set.
                 
Note: use / instead of \\ to separate folders in 'ps.dir'")
    stop(str)
  }
  if (!file.exists(paste0(stud.path,"/", stud.short.file))) {
    str= paste0("I could not find your file '", stud.short.file,"' in your problem set folder '",stud.path,"'. Please set the variables ps.dir and ps.file to the right values in the first chunk of your problem set. The variable 'ps.file' must have the same file name than your problem set file.")
    stop(str)    
  }
  
  setwd(stud.path)
  
  if (user.name=="ENTER A USER NAME HERE") {
    stop('You have not picked a user name. Change the variable "user.name" in your problem set file from "ENTER A USER NAME HERE" to some user.name that you can freely pick.')
  }
  
  if (verbose)
    display("get.or.init.ps...")
  
  ps = get.or.init.ps(ps.name,stud.path, stud.short.file, reset)
  ps$catch.errors = catch.errors
  
  set.ps(ps)
  ps$warning.messages = list()
  
  user = get.user(user.name)  
  cdt = ps$cdt
  edt = ps$edt
  
  ps$stud.code = readLines(ps$stud.file)
  cdt$stud.code = get.stud.chunk.code(ps=ps)
  cdt$code.is.task = cdt$stud.code == cdt$task.txt
  cdt$chunk.changed = cdt$stud.code != cdt$old.stud.code

  ex.changed = summarise(group_by(as.data.frame(cdt),ex.ind), ex.changed = any(chunk.changed))$ex.changed

  ex.check.order = unique(c(which(ex.changed),ps$ex.last.mod, which(!edt$ex.solved)))
  
  if (! any(cdt$chunk.changed)) {
    code.change.message = "\nBTW: I see no changes in your code... did you forget to save your file?"
  } else {
    code.change.message = NULL
  }
  
  ps$cdt = cdt
  # Check exercises
  i = 1
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
      message = ps$failure.message
      message = paste0(message,"\nFor a hint, type hint() in the console and press Enter.")
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
    msg = "You solved the problem set. Congrats!"
    stop.without.error(msg)
  }
  stop("There were still errors in your solution.")
}

check.exercise = function(ex.ind, verbose = FALSE, ps=get.ps(), check.all=FALSE) {
  restore.point("check.exercise")
  
  ck.rows = ps$cdt$ex.ind == ex.ind
  cdt = ps$cdt
  ex.name = ps$edt$ex.name[ex.ind]
  
  if (check.all) {
    min.chunk = min(ck.rows) 
  } else {
    rows = which(ck.rows & ((!cdt$is.solved) | cdt$chunk.changed)) 
    # All chunks solved and no chunk changed
    if (length(rows)==0) {
      cat(paste0("\nAll chunks were correct and no change in exercise ",ex.name,"\n"))
      return(TRUE)
    }
    min.chunk = min(rows)
  }
  chunks = min.chunk:max(which(ck.rows))
  chunk.ind = chunks[1]
  for (chunk.ind in chunks) {
    ret = check.chunk(chunk.ind,ps=ps, verbose=verbose)
    if (ret==FALSE) {
      return(FALSE)
    }
  }
  ps$edt$ex.final.env[[ex.ind]] = copy(ps$stud.env)
  return(TRUE)
}


#' Check the student's solution of an exercise contained in stud.file
#' @param ex.name The name of the exercise
#' @param stud.code The code of the student's solution as a string (or vector of strings)
#' @export 
check.chunk = function(chunk.ind,ps=get.ps(), verbose=FALSE,stud.code=ps$cdt$stud.code[[chunk.ind]], stud.env=make.chunk.stud.env(chunk.ind, ps), expect.change = FALSE) {
  restore.point("check.chunk")
  
  ck = ps$cdt[chunk.ind,]
  chunk.name = ck$chunk.name
  
  ps$cdt$old.stud.code[chunk.ind] = stud.code
  ps$cdt$is.solved[chunk.ind] = FALSE

  
  ps$ex.ind = ck$ex.ind
  ps$chunk.ind = chunk.ind
  ps$chunk.name = chunk.name
  
  #stop("analyse below")
  test.li = ck$test.expr[[1]]  
  
  if (expect.change)  {
    if (stud.code == ck$task.txt) {
      ck$chunk.changed = FALSE
      ps$failure.message = paste0("You have not yet changed chunk ", chunk.name)
      return(FALSE)
    }
  }
  display("Check chunk ", chunk.name," ...")
  
  ps$failure.message  = "No failure message recorded"
  ps$warning.messages = list()
  ps$check.date = Sys.time()
  
  has.error = FALSE
  ps$stud.expr.li = NULL
  if (verbose) {
    display("parse stud.code...")
  }
  if (!is.false(ps$catch.errors)) {  
    tryCatch( ps$stud.expr.li <- parse(text=stud.code, srcfile=NULL),
              error = function(e) {
                ps$failure.message=paste0("parser error: ",geterrmessage())
                has.error <<- TRUE
              })
  } else {
    ps$stud.expr.li <- parse(text=stud.code, srcfile=NULL)    
  }
  if (has.error)
    return(FALSE)


  if (verbose) {
    display("make.chunk.stud.env...")
  }
  has.error = FALSE
  ps$stud.env = stud.env
  ps$cdt$stud.env[[chunk.ind]] = stud.env

  
  has.error = FALSE    
  ps$stud.seed = as.integer(Sys.time())
  set.seed(ps$stud.seed)
  
  if (verbose) {
    display("eval stud.code...")
  }

  #eval(ps$stud.expr.li, ps$stud.env)
  ps$e.ind = 0   

  
  has.error = !stepwise.eval.stud.expr(stud.expr=ps$stud.expr.li,stud.env=stud.env)
#   tryCatch( eval(ps$stud.expr.li, stud.env),
#     error = function(e) {
#       # Evaluate expressions line by line and generate failure message
#       stepwise.eval.stud.expr(stud.expr=ps$stud.expr.li,stud.env=stud.env)
#       has.error <<- TRUE
#     }
#   )
  if (has.error)
    return(FALSE)
  
  
  had.warning = FALSE
  if (verbose) {
    display("run tests...")
  }
  ups = get.ups()
  ps$success.log = ps$test.log = NULL
  e.ind = 1

  tdt.ind = which(ps$tdt$chunk.ps.ind == chunk.ind)[1]-1 
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
      ret = eval(test,ps$stud.env)
      ps$tdt$test.passed[tdt.ind] = ret    
      update.ups.test.result(passed=ret,ps=ps)
      #update.log.test.result(ret,ups, ck, ps)
      
      if (ret==FALSE) {
        set.ups(ups)
        ps$test.log = c(ps$test.log, ps$failure.message)
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
  
  ps$cdt$is.solved[[chunk.ind]] = TRUE
  
  if (!is.na(ck$award.name)) {
    give.award(ck$award.name, ps=ps)
  }
  
  set.ups(ups)
  if (had.warning) {
    return("warning")
  } else {
    return(TRUE)
  }
}


update.ups.test.result = function(passed, tdt.ind = ps$tdt.ind, ups=get.ups(),ps=get.ps()) {
  passed.before = ps$tdt$test.passed[tdt.ind] 
  if (is.na(ups$tdt$first.call.date[tdt.ind]))
    ups$tdt$first.call.date[tdt.ind] = Sys.time()
  if (passed==FALSE) {
    if (!ups$tdt$success[tdt.ind])
      ups$tdt$num.failed[tdt.ind] = ups$tdt$num.failed[tdt.ind]+1
    set.ups(ups)
    return()
  }
  if (is.na(ups$tdt$success.date[tdt.ind])) {
    ups$tdt$success[tdt.ind] <- TRUE
    ups$tdt$success.date[tdt.ind] <- Sys.time()
  }
}

update.log.test.result = function(...) {
  return()
}

make.chunk.stud.env = function(chunk.ind, ps = get.ps()) {
  restore.point("make.chunk.stud.env")
  ck = ps$cdt[chunk.ind,]
  if (ck$chunk.ex.ind == 1) {
    # First chunk in exercise: generate new stud.env
    stud.env = new.stud.env(chunk.ind)
    import.var.into.stud.env(ps$edt$import.var[[ck$ex.ind]], stud.env,ps)

  } else {
    # Later chunk in an exercise: simply copy previous stud.env
    stud.env = copy.stud.env(ps$cdt$stud.env[[chunk.ind-1]], chunk.ind)
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
  
  ck = ps$cdt[chunk.ind,]
  ex.ind = ck$ex.ind
  if (ck$chunk.ex.ind == 1) {
    if (ex.ind==1)
      return(TRUE)
    ex.names = names(ps$edt$import.var[[ck$ex.ind]])
    if (is.null(ex.names))
      return(TRUE)
    edt = ps$edt
    ex.inds = edt$ex.ind[match(ex.names,edt$ex.name)]
    
    chunks = which(ps$cdt$ex.ind %in% ex.inds)
    solved = all(ps$cdt$is.solved[chunks])
    if (all(solved))
      return(TRUE)
    ps$failure.message = paste0("You must first solve and check all chunks in exercise(s) ", paste0(ex.names[ex.inds],collapse=", "), " before you can start this exercise.")
    return(FALSE)
  } else {
    if (ps$cdt$is.solved[chunk.ind-1]) {
      return(TRUE)
    }
    ps$failure.message = paste0("You must first solve and check the previous chunk before you can edit and solve this chunk.")
    return(FALSE) 
  }
  
}

#' Extracts the stud's code of a given exercise
#' @export
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



stepwise.eval.stud.expr = function(stud.expr, ps=get.ps(), stud.env = ps$stud.env, seed=NULL) {
  restore.point("stepwise.eval.stud.expr")
  if (!is.null(seed))
    set.seed(seed)
  has.error = FALSE
  
  i = 1
  for (i in seq_along(stud.expr)) {
    part.expr = stud.expr[[i]]
    tryCatch( eval(part.expr, stud.env),
              error = function(e) {        
                ps$failure.message = paste0("evaluation error in \n  ",deparse1(part.expr),"\n  ",adapt.console.err.message(as.character(e)))
                has.error <<- TRUE
              }
    )
    
    if (has.error) {
      if (is.false(ps$catch.errors))
        stop(ps$failure.message)

      return(FALSE)
    }
  }
  return(!has.error)
}



#' Used inside tests: adds a failure to an exercise
#' 
#' @param message a longer description shown to the user
#' @param ... variables that will be rendered into messages that have whiskers
#' @export
add.failure = function(message,..., ps= get.ps()) {
  message=replace.whisker(message,...)
  args = list(...)
  restore.point("add.failure")
  ps$failure.message = message
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
