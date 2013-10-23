# Functions for checking student solutions
# Tests are implemented in in tests_for_ps.r

#' Checks a student problem set
#' 
#' The command will be put at the top of a student's problem set. It checks all exercises when the problem set is sourced. If something is wrong an error is thrown and no more commands will be sourced.
#'@export
check.problem.set = function(name,stud.path, stud.short.file, reset=FALSE) {  
  restore.point("check.problem.set", deep.copy=FALSE)
  ps = get.or.init.problem.set(name,stud.path, stud.short.file, reset)
  
  ps$stud.code = readLines(ps$stud.file)
  
  
  ex.names = names(ps$ex)
  new.code = sapply(ex.names, extract.exercise.code, ps=ps)
  has.code = !is.na(new.code)
  names(has.code) = ex.names
  
  new.code = new.code[has.code]
  ex.names = ex.names[has.code]
  
  
  i = 2
  code.changed = sapply(seq_along(ex.names), function(i) {
    ps$ex[[i]]$stud.code[[1]]!=new.code[[i]]
  })
  names(code.changed) = names(new.code)  = ex.names
  
  code.check = code.changed
  
  # If no code changed, check the last modified exercise
  if (!any(code.changed)) {
    if (!has.code[ps$ex.last.mod]) {
      ps$ex.last.mod=1
    }
    code.check[ps$ex.last.mod] = TRUE
    message("I see no changes in your code... did you forget to save your file?")
  } else {
    ps$ex.last.mod = which(code.changed)[1]
  }
  
  # Check exercises
  sum.correct = 0
  sum.warning = 0
  i = 1
  for (i in which(code.check)) {
    is.correct = FALSE
    ex.name = ex.names[i]
    ex = ps$ex[[ex.name]]
    ret = check.exercise(ex.name,new.code[ex.name])
    log.exercise(ex)
    if (ret==FALSE) {
      message = ex$failure.message
      if (!is.null(ex$hint.name))
        message = paste0(message,"\nIf you want a hint, type hint() in the R console and press Enter.")
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
  if (sum.correct == length(ps$ex)) {
    cat("\nYou have solved all exercises and I could not detect any error!")
  }
}

#' Check the student's solution of an exercise contained in stud.file
#' @param ex.name The name of the exercise
#' @param stud.code The code of the student's solution as a string (or vector of strings)
#' @export 
check.exercise = function(ex.name,stud.code,ps=get.ps()) {
  restore.point("Exercise")
  
  cat(paste0("\n\n###########################################\n",
             "Check exercise ",ex.name,"...",
             "\n###########################################\n"))
  
  ex = ps$ex[[ex.name]]
  set.ex(ex)
  ex$solved = FALSE
  
  #message(capture.output(print(ex)))
  #message(capture.output(print(ps$ex[[ex.name]])))
  #message(capture.output(print(get.ex())))
  
  ex$failure.message = ex$failure.short = "No failure message recorded"
  ex$warning.messages = ex$warning.shorts = list()
  
  ex$check.date = Sys.time()
  ex$code.changed = ex$stud.code != stud.code
  if (ex$code.changed) {
    ex$stud.code = stud.code
    ex$checks = ex$checks +1
    if (!ex$was.solved)
      ex$attempts = ex$attempts+1
    
  }                           
  
  ex$stud.env = new.env(parent=.GlobalEnv)
  ex$stud.seed = as.integer(Sys.time())
  
  has.error = FALSE
  tryCatch( ex$stud.expr <- parse(text=ex$stud.code, srcfile=NULL),
            error = function(e) {
              ex$failure.message=ex$failure.short=paste0("parser error: ",geterrmessage())
              has.error <<- TRUE
            })
  if (has.error)
    return(FALSE)
  
  has.error = FALSE
  
  set.seed(ex$stud.seed)
  tryCatch( eval(ex$stud.expr, ex$stud.env),
            error = function(e) {
              # Evaluate expressions line by line and generate failure message
              stepwise.eval.stud.expr(stud.expr=ex$stud.expr,stud.env=new.env(parent=.GlobalEnv))
              has.error <<- TRUE
            }
  )
  if (has.error)
    return(FALSE)
  
  # Evaluate official solution or recycle previous evaluation  
  if (is.null(ex$sol.env)) {
    sol.env = new.env(parent=.GlobalEnv)
    eval(ex$sol, sol.env)
    ex$sol.env = sol.env
  } else {
    sol.env = ex$sol.env
  }
  
  had.warning = FALSE
  for (test.ind in seq_along(ex$tests)){
    test = ex$tests[[test.ind]]
    passed.before = ex$tests.stats[[test.ind]]$passed
    ex$success.message = NULL
    ex$test.ind = test.ind
    ret = eval(test,ex$stud.env)
    
    if (ret==FALSE) {
      return(FALSE)
    } else if (ret=="warning") {
      had.warning = TRUE
    } else {
      if (!is.null(ex$success.message) & !passed.before) {
        cat(paste0(ex$success.message,"\n"))
      }
    }
  }
  if (had.warning) {
    message("\n Hmm... overall, I am not sure if your solution is right or not, look at the warnings.")
    return(invisible("warning"))
  } else {
    cat(paste0("\nCongrats, I could not find an error in exercise ", ex$name,"!"))
    
    ex$solved = TRUE
    ex$was.solved = TRUE
    
    return(invisible(TRUE))
  }
}



#' Extracts the stud's code of a given exercise
#' @export
extract.exercise.code = function(ex.name,stud.code = ps$stud.code, ps=get.ps(),warn.if.missing=TRUE) {
  txt = stud.code
  start.command = extract.command(txt,paste0("#### Exercise ",ex.name))
  if (is.null(start.command)) {
    if (warn.if.missing)
      message(paste0("Warning: Exercise ", ex.name, " not found. Your code must have the line:\n",
                     paste0("#### Exercise ",ex.name)))
    return(NA)
  }
  end.command =  extract.command(txt,paste0("#### end exercise ",ex.name))
  if (is.null(start.command)) {
    message(paste0("Warning: Exercise ", ex.name, " could not be parsed, since I can't find the end of the exercise. Your code needs at the end of the exercise the line:\n",
                   paste0("#### end exercise ",ex.name)))
    return(NA)
  }
  
  start = start.command$line
  end =end.command$line
  
  # Return student's solution
  paste0(txt[(start+1):(end-1)],collapse="\n")
}



stepwise.eval.stud.expr = function(stud.expr, ex=get.ex(), stud.env = ex$stud.env()) {
  set.seed(ex$stud.seed)
  has.error = FALSE
  for (i in seq_along(stud.expr)) {
    part.expr = stud.expr[[i]]
    tryCatch( eval(part.expr, stud.env),
              error = function(e) {        
                ex$failure.message = ex$failure.short= paste0("evaluation error in \n  ",deparse(part.expr),"\n  ",geterrmessage())
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
