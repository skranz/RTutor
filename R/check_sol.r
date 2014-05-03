# Functions for checking student solutions
# Tests are implemented in in tests_for_ps.r

#' Checks a student problem set
#' 
#' The command will be put at the top of a student's problem set. It checks all exercises when the problem set is sourced. If something is wrong an error is thrown and no more commands will be sourced.
#'@export
check.problem.set = function(name,stud.path, stud.short.file, reset=FALSE, set.warning.1=TRUE, user.name="GUEST", do.check=interactive(), verbose=FALSE) {  
  restore.point("check.problem.set", deep.copy=FALSE)
  if (!do.check) return("not checked")
  if (set.warning.1) {
    if (options()$warn<1)
      options(warn=1)
  }
  
  setwd(stud.path)
  
  if (user.name=="ENTER A USER NAME HERE") {
    stop('You have not picked a user name. Change the variable user.name in your problem set file from ""ENTER A USER NAME HERE" to some user.name that you can freely pick.')
  }
  
  
  #browser()
  
  if (verbose)
    display("get.or.init.problem.set...")
  ps = get.or.init.problem.set(name,stud.path, stud.short.file, reset)
  set.ps(ps)
  
  user = get.user(user.name)  
  
  
  
  old.code = sapply(ps$ex, function(ex) ex$stud.code[[1]])  
  ps$stud.code = readLines(ps$stud.file)
  
  ex.names = names(ps$ex)
  new.code = sapply(ex.names, extract.exercise.code, ps=ps)
  has.code = !is.na(new.code)
  names(has.code) = ex.names
  
  new.code = new.code[has.code]
  ex.names = ex.names[has.code]
  
  
  i = 1
  code.changed = sapply(seq_along(ex.names), function(i) {
    old.code[i]!=new.code[[i]]
  })
  names(code.changed) = names(new.code)  = ex.names
  
  code.check = code.changed

  if (verbose) {
    display("code.changed:")
    print(code.changed)
  }
  
  code.change.message = ""
  # If no code changed, check the last modified exercise
  if (!any(code.changed)) {
    if (!has.code[ps$ex.last.mod]) {
      ps$ex.last.mod=1
    }
    code.check[ps$ex.last.mod] = TRUE
    code.change.message = "\nBTW: I see no changes in your code... did you forget to save your file?"
   
  } else {
    ps$ex.last.mod = which(code.changed)[1]
  }
  
  # Check exercises
  sum.correct = 0
  sum.warning = 0
  i = 1
  any.false = FALSE
  was.checked = rep(FALSE,length(code.check))
  while(!all(was.checked)) {
    for (i in which(code.check)) {
      is.correct = FALSE
      ex.name = ex.names[i]
      ex = ps$ex[[ex.name]]
      ret <- FALSE
      
      if (verbose) {
        display("### Check exercise ", ex.name ," ######")
      }

      
      ret = tryCatch(check.exercise(ex.name,new.code[ex.name], verbose=verbose),
                     error = function(e) {ex$failure.message <- as.character(e)
                                          return(FALSE)})
            
      # Copy variables into global env
      copy.into.env(source=ex$stud.env,dest=.GlobalEnv, set.fun.env.to.dest=TRUE)
      log.exercise(ex)
      save.ups()
      if (ret==FALSE) {
        message = ex$failure.message
        if (!is.null(ex$hint.name))
          message = paste0(message,"\nFor a hint, type hint() in the console and press Enter.")
        message = paste(message,code.change.message)
        any.false=TRUE
        stop(message, call.=FALSE, domain=NA)
      } else if (ret=="warning") {
        message = paste0(ex$warning.messages,collapse="\n\n")
        message(paste0("Warning: ", message))
        sum.warning = sum.warning+1
      }
      is.correct = TRUE
      if (is.correct)
        sum.correct = sum.correct+1
      #.Internal(stop(FALSE,""))
    }
    was.checked[code.check]=TRUE
    # get.next exercise to check
    last.check = i
    old.code.check = code.check
    code.check  = !old.code.check & seq_along(code.check) > i
    if (!any(code.check))
      code.check = !old.code.check 
    
  }
  if (!any.false) {
    display("\n****************************************************",
            "\nYou solved the problem set, congrats!")
    stats()
    display("
To generate a zip file of your solution for submission, call

  zip.solution()
")
    
    msg = " I could not find any error in your problem set. Congrats!"
    stop(msg, call.=FALSE, domain="")
  }
  stop("There were still errors in your solution.")
}

#' Check the student's solution of an exercise contained in stud.file
#' @param ex.name The name of the exercise
#' @param stud.code The code of the student's solution as a string (or vector of strings)
#' @export 
check.exercise = function(ex.name,stud.code,ps=get.ps(), verbose=FALSE) {
  restore.point("check.exercise")
  
  cat(paste0("\n\n###########################################\n",
             "Check exercise ",ex.name,"...",
             "\n###########################################\n"))
  
  
  ex = ps$ex[[ex.name]]
  set.ex(ex)
  ex$solved = FALSE

  initial.code = ps$ex.initial.code[[ex.name]]
  if (any(initial.code==stud.code)) {
    ex$code.changed = FALSE
    ex$failure.message = ex$failure.short = paste0("You have not yet started with exercise ", ex.name)
    return(FALSE)
  }

  
  #message(capture.output(print(ex)))
  #message(capture.output(print(ps$ex[[ex.name]])))
  #message(capture.output(print(get.ex())))
  
  ex$failure.message = ex$failure.short = "No failure message recorded"
  ex$warning.messages = ex$warning.shorts = list()
  
  ex$check.date = Sys.time()
  ex$code.changed = !all(ex$stud.code == stud.code)
  if (ex$code.changed) {
    ex$stud.code = stud.code
    ex$checks = ex$checks +1
    if (!ex$was.solved)
      ex$attempts = ex$attempts+1
    
  }                           
  
  ex$stud.env = new.env(parent=.GlobalEnv)
  ex$stud.seed = as.integer(Sys.time())
  
  # Import variables from other exercises stud.env's
  has.error = FALSE

  if (verbose) {
    display("import.stud.env.var...")
  }

  tryCatch(
    import.stud.env.var(ex$settings$import.var, dest.env = ex$stud.env, ps = ps),
     error = function(e) {
      ex$failure.message=ex$failure.short=paste0("Error in check.exercise import.stud.env.var ",geterrmessage())
      has.error <<- TRUE
    })
    
  if (has.error)
    return(FALSE)

  has.error = FALSE
  ex$stud.expr.li = NULL
  if (verbose) {
    display("parse stud.code...")
  }

  tryCatch( ex$stud.expr <- parse(text=ex$stud.code, srcfile=NULL),
            error = function(e) {
              ex$failure.message=ex$failure.short=paste0("parser error: ",geterrmessage())
              has.error <<- TRUE
            })
  
  if (has.error)
    return(FALSE)
  
  
  has.error = FALSE
  
  set.seed(ex$stud.seed)
  if (verbose) {
    display("eval stud.code...")
  }

  #eval(ex$stud.expr, ex$stud.env)
  tryCatch( eval(ex$stud.expr, ex$stud.env),
            error = function(e) {
              # Evaluate expressions line by line and generate failure message
              #stepwise.eval.stud.expr(stud.expr=ex$stud.expr,stud.env=new.env(parent=.GlobalEnv))
              stepwise.eval.stud.expr(stud.expr=ex$stud.expr,stud.env=ex$stud.env)
              has.error <<- TRUE
            }
  )
  if (has.error)
    return(FALSE)
  
  
  # Evaluate official solution or recycle previous evaluation  
  if (is.null(ex$sol.env)) {
    if (verbose) {
      display("eval solution code...")
    }

    sol.env = new.env(parent=.GlobalEnv)    
    ex$sol.env = sol.env

    tryCatch( suppressWarnings(eval(ex$sol, sol.env)),
      error = function(e) {
        str = paste0("Uups, an error occured while running the official solution:\n",
                     as.character(e),
                     "\nI don't test your solution. You may contact the creator of the problem set.")
        stop(str)
      }
    )
  } else {
    sol.env = ex$sol.env
  }
  
  had.warning = FALSE
  if (verbose) {
    display("run tests...")
  }

  ups = get.ups()
  for (test.ind in seq_along(ex$tests)){
    test = ex$tests[[test.ind]]
    passed.before = ex$tests.stats[[test.ind]]$passed
    ex$success.message = NULL
    ex$test.ind = test.ind
    if (verbose) {
      display("  Test #", test.ind, ": ",deparse1(test))
    }

    ret = eval(test,ex$stud.env)
    
    if (is.na(ups$li[[ex$name]]$first.call.date[test.ind]))
      ups$li[[ex$name]]$first.call.date[test.ind] = Sys.time()
    
    if (ret==FALSE) {
      if (!ups$li[[ex$name]]$success[test.ind])
        ups$li[[ex$name]]$num.failed[test.ind] = ups$li[[ex$name]]$num.failed[test.ind]+1
      set.ups(ups)
      return(FALSE)
    } else if (ret=="warning") {
      had.warning = TRUE
    } else {
      if (!is.null(ex$success.message) & !passed.before) {
      #if (!is.null(ex$success.message)) {
        cat(paste0(ex$success.message,"\n"))
      }
    }
    if (is.na(ups$li[[ex$name]]$success.date[test.ind])) {
      ups$li[[ex$name]]$success[test.ind] <- TRUE
      ups$li[[ex$name]]$success.date[test.ind] <- Sys.time()
    }

  }
  set.ups(ups)
  if (had.warning) {
    message("\nHmm... overall, I am not sure if your solution is right or not, look at the warnings.")
    return(invisible("warning"))
  } else {
    cat(paste0("\nCongrats, I could not find an error in exercise ", ex$name,"!"))
    
    ex$solved = TRUE
    ex$was.solved = TRUE
    
    return(invisible(TRUE))
  }
}

# Import variables from other exercises stud.env's
import.stud.env.var = function(import.var.li, dest.env = get.ex()$stud.env, ps = get.ps()) {
  restore.point("import.stud.env.var")
  if (is.null(import.var.li))
    return(NULL)
  for (i in seq_along(import.var.li)) {
    ex.name = names(import.var.li)[i]
    vars = import.var.li[[i]]
    source.env = ps$ex[[ex.name]]$stud.env
    if (is.null(source.env)) {
      stop(paste0("\nWe first must correctly generate the variables '", paste0(vars, collapse=","), "' in exercise ", ex.name, " before you can solve this exercise.\n To check exercise ", ex.name, " enter somewhere an irrelevant space in the code."))
    }
    for (var in vars) {
      if (!exists(var,source.env, inherits=FALSE))
        stop(paste0("You first must correctly generate the variable '", var, "' in exercise ", ex.name, " before you can solve this exercise."))
        val = get(var,source.env)
        # Set enclosing environments of functions to dest.env
        if (is.function(val))
          environment(val) = dest.env
        assign(var, val,dest.env)
    }
  }
  return(NULL)
}


#' Extracts the stud's code of a given exercise
#' @export
extract.exercise.code = function(ex.name,stud.code = ps$stud.code, ps=get.ps(),warn.if.missing=TRUE) {
  restore.point("extract.r.exercise.code")
  
  if (ps$is.rmd.stud) {
    return(extract.rmd.exercise.code(ex.name,stud.code, ps,warn.if.missing))
  } else {
    return(extract.r.exercise.code(ex.name,stud.code, ps,warn.if.missing))    
  }

}


extract.r.exercise.code = function(ex.name,stud.code = ps$stud.code, ps=get.ps(),warn.if.missing=TRUE) {
  restore.point("extract.r.exercise.code")

  txt = stud.code
  mr = extract.command(txt,paste0("#' ## Exercise "))
  start.ind = which(str.starts.with(mr[,2],ex.name))
  start.row = mr[start.ind,1]
  if (length(start.row) == 0) {
    if (warn.if.missing)
      message(paste0("Warning: Exercise ", ex.name, " not found. Your code must have the line:\n",
                     paste0("#' ## Exercise ",ex.name)))
    return(NA)
  }
  if (length(start.row)>1) {
    message("Warning: Your solution has ", length(start.row), " times exercise ", ex.name, " I just take the first.")
    start.row = start.row[1]
    start.ind = start.ind[1]
  }
  end.row = c(mr[,1],length(txt)+1)[start.ind+1]-1
  str = txt[(start.row+1):(end.row)]
  paste0(str, collapse="\n")
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



stepwise.eval.stud.expr = function(stud.expr, ex=get.ex(), stud.env = ex$stud.env()) {
  restore.point("stepwise.eval.stud.expr")
  set.seed(ex$stud.seed)
  has.error = FALSE
  for (i in seq_along(stud.expr)) {
    part.expr = stud.expr[[i]]
    tryCatch( eval(part.expr, stud.env),
              error = function(e) {        
                ex$failure.message = ex$failure.short= paste0("evaluation error in \n  ",deparse1(part.expr),"\n  ",geterrmessage())
                has.error <<- TRUE
              }
    )
    if (has.error)
      return(FALSE)
  }
  return(!has.error)
}



#' Used inside tests: adds a failure to an exercise
#' 
#' @param ex the exercise (environment) that is currently tested (typically ex=get.ex())
#' @param short a short id of the error that will be stored in the log file
#' @param message a longer description shown to the user
#' @param ... variables that will be rendered into messages that have whiskers
#' @export
add.failure = function(ex,short,message,...) {
  short=replace.whisker(short,...)
  message=replace.whisker(message,...)
  args = list(...)
  restore.point("add.failure")
  ex$failure.message = message
  ex$failure.short = short
  
  ex$last.failed.test = ex$test.ind
  ts = ex$tests.stats[[ex$test.ind]]
  ts$times.failed.before.passed = ts$times.failed.before.passed + (!ts$ever.passed)
  ts$times.failed = ts$times.failed + 1
  ts$passed = FALSE
  ex$tests.stats[[ex$test.ind]] = ts
}

#' Used inside tests: adds a failure to an exercise
#' 
#' @param ex the exercise (environment) that is currently tested (typically ex=get.ex())
#' @param short a short id of the error that will be stored in the log file
#' @param message a longer description shown to the user
#' @param ... variables that will be rendered into messages that have whiskers
#' @export
add.success = function(ex,message,...) {
  message=replace.whisker(message,...)
  ex$success.message = message
  ex$last.passed.test = ex$test.ind
  
  ts = ex$tests.stats[[ex$test.ind]]
  ts$times.failed =0
  ts$passed = TRUE
  ex$tests.stats[[ex$test.ind]] = ts  
  
}


#' Used inside tests: adds a warning to an exercise
#' 
#' @param ex the exercise (environment) that is currently tested (typically ex=get.ex())
#' @param short a short id of the warning that will be stored in the log file
#' @param message a longer description shown to the user
#' @param ... variables that will be rendered into messages that have whiskers
#' @export
add.warning = function(ex,short,message,...) {
  short=replace.whisker(short,...)
  message=replace.whisker(message,...)
  args = list(...)
  #restore.point("add.warning")
  ind = length(ex$warning.messages)+1
  ex$warning.messages[[ind]] = message
  ex$warning.shorts[[ind]] = short
}
