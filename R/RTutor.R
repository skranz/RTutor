#library(devtools); install_github(repo="RTutor", studname="skranz")

#' Init files for developing a new problem set with given name under the specified parent directory
#' @export 
develop.problem.set = function(name,parent.path) {
  restore.point("develop.problem.set")
  RTutor.path = path.package("RTutor")
  
  ps.path = paste0(parent.path,"/",name)
  message(paste0("Create directory ", ps.path))
  dir.create(ps.path, recursive=TRUE)
  
  dest.file = paste0(ps.path,"/make_student_ps.r")
  if (file.exists(dest.file)) {
    message(paste0("File ", dest.file, " already exists. I don't overwrite it."))
  } else {
    source.file = paste0(RTutor.path,"/problemsets/make_student_ps.Rtmpl")
    txt = readLines(source.file)
    txt = whisker.render(txt,list(ps_name=name,ps_path=ps.path))
    writeLines(txt,dest.file)
  }
  
  dest.file = paste0(ps.path,"/",name,"_struc.r")
  if (file.exists(dest.file)) {
    message(paste0("File ", dest.file, " already exists. I don't overwrite it."))
  } else {
    source.file = paste0(RTutor.path,"/problemsets/ps_struc.r")
    txt = readLines(source.file)
    txt[1] = paste0("#$ problem_set ", name)
    writeLines(txt,dest.file)
  }  
}

extract.command = function(txt,command) {
  lines = which(substring(txt,1,nchar(command))==command)
  if (length(lines)==0)
    return(NULL)
  val = str_trim(substring(txt[lines],nchar(command)+1))
  data.frame(line=lines, val=val)
} 

#' Add a hint to an exercise.
#' 
#' @param hint.name A name for the hint
#' @param code R code inside {} that will be evaluated in sol.env when the hint is shown. A hint could show messages via cat, but it could also show a plot. 
#' @export
add.hint = function(hint.name, code,cond=NULL, ex=get.ex()) {
  code = substitute(code)
  restore.point("add.hint")
  hint = list(name=hint.name,cond=cond,code=code,ex=ex)
  ex$hints[[hint.name]]=hint
}

#' Can be called by a student to see a hint for the specified exercise.
#' 
#' Preliminary. The procedure how hints are given may well change. 
#' @export
hint.for = function(ex.name,ps=get.ps()) {
  restore.point("hint.for")
  ex = ps$ex[[ex.name]]
  hint.id = min(ex$prev.hint + 1, length(ex$hints))
  if (hint.id==0) {
    message("Sorry, but I have no hints stored...")
  }
  hint = ex$hints[[hint.id]]
  cat(paste0("\nHint ", hint$name,":"))
  eval(hint$code,ex$sol.env)
}

#' Parse the structure of a problem set from a file
parse.ps.structure =  function(ps=get.ps(),file=ps$structure.file) {
  restore.point("parse.ps.structure", deep.copy=TRUE)
  txt = readLines(file)
  library(stringr)
  
  ps.name= extract.command(txt, "#$ problem_set")[,2]
  ex_start = extract.command(txt, "#$ exercise")
  ex_end = extract.command(txt,"#$ end_exercise")
  
  ex.df = data.frame(start=ex_start$line, end=ex_end$line, name=ex_start$val)
  
  ex.li =vector("list",NROW(ex.df))
  names(ex.li) = ex.df$name
  
  i = 1
  for (i in 1:NROW(ex.df)) {
    ex = new.env(parent=.GlobalEnv)
    set.ex(ex)
    ex$name = names(ex.li)[i]
    if (i == NROW(ex.df)) {
      ex$next.ex = NULL  
    } else {
      ex$next.ex = i+1        
    }
    ex.txt = txt[(ex.df[i,"start"]+1):(ex.df[i,"end"]-1)]
    com = extract.command(ex.txt,"#$")$line     
    #ex.task.txt = paste0(ex.txt[1:(com[1]-1)],collapse="\n")
    ex$task.txt = paste0(ex.txt[(com[1]+1):(com[2]-1)],collapse="\n")
    ex$sol.txt = paste0(ex.txt[(com[2]+1):(com[3]-1)],collapse="\n")
    ex$tests.txt = paste0(ex.txt[(com[3]+1):(com[4]-1)],collapse="\n")
    ex$hints.txt = paste0(ex.txt[(com[4]+1):length(ex.txt)],collapse="\n")
    
    # Replace whiskers
    ex$task.txt = whisker.render(ex$task.txt,list(ex_name=ex$name))
    
    
    ex$sol = parse(text=ex$sol.txt,srcfile=NULL)
    ex$tests = as.list(parse(text=ex$tests.txt,srcfile=NULL))     
    # Run the code that generates hints
    ex$prev.hint = 0
    ex$hints = list()
    eval(parse(text=ex$hints.txt,srcfile=NULL))
    
    ex$sol.env = NULL
    ex$stud.env = NULL
    ex$stud.code = ex$task.txt
    ex$checks = 0
    ex$attempts=0
    ex$solved = FALSE
    ex$was.solved = FALSE
    
    ex.li[[i]] = ex
  }
  #ps$name = ps.name
  ps$ex = ex.li
  return(invisible(ps))
}

add.exercise = function(ex,ps=get.ps()) {
  ex.code = get.empty.ex.code(ex)
  con = file(ps$stud.file,"a")
  writeLines(ex.code,con)
  close(con)
}

get.empty.ex.code = function(ex) {
  paste0("\n###########################################\n",
         "#### Exercise ", ex$name,"\n",
         "###########################################\n\n",
         ex$task.txt,"\n\n",
         "#### end exercise ", ex$name, "\n"
         #           'check.exercise("',ex$name,'")'
  )      
} 

#' Generate a problem set skeleton for a student and save it in a file
#' @export
create.stud.ps = function(ps, file = ps$stud.file, ps.dir="C:/...", game.mode = FALSE) {
  restore.point("create.stud.ps")

  # in game mode just show first exercise
  if (game.mode) {
    ex.str = list(paste0(get.empty.ex.code(ps$ex[[1]]),'
\n#Further exercises are shown, once you solve this correctly...'))
  } else { 
    ex.str = lapply(ps$ex, get.empty.ex.code)
  }

  str = paste0("#### Problemset ", ps$name,"\n\n",
'
# To check your solutions in RStudio save (Ctrl-s) and then source (Ctrl-Shift-s)
# If you check "Source on Save" in RStudio you just have to save (Ctrl-s)

ps.dir =  "',ps.dir,'" # your working directory
ps.file = "', ps$prefix, ps$name,'.r" # this file

library(RTutor)
check.problem.set("',ps$name,'", ps.dir, ps.file)
', paste0(ex.str,collapse="\n"))
  
  cat(str)
  writeLines(str,file)
  invisible(str)
}

print.Problemset = function(ps) {
  print(as.list(ps))
}

get.or.init.problem.set = function(name,stud.path, stud.short.file, reset=FALSE, ps=get.ps()) {
  restore.point("get.or.init.problem.set")
  
  # Init problem set new
  if (reset) {
    return(init.problem.set(name,stud.path,stud.short.file))
  }
  
  # Just take current problem set information
  if (!is.null(ps)) {
    if (ps$name == name & ps$stud.path==stud.path & ps$stud.short.file == stud.short.file)
      return(ps)
  }
  
  # Check if a previous instance of the problem set has been stored and load it
  
  # ... not yet implemented ...
  
  # Initialize problem set newly
  return(init.problem.set(name,stud.path,stud.short.file))
}

#' Checks a student problem set
#' 
#' The command will be put at the top of a student's problem set. It checks all exercises when the problem set is sourced. If something is wrong an error is thrown and no more commands will be sourced.
#'@export
check.problem.set = function(name,stud.path, stud.short.file, reset=FALSE, game.mode=FALSE) {  
  restore.point("check.problem.set", deep.copy=FALSE)
  ps = get.or.init.problem.set(name,stud.path, stud.short.file, reset)
  
  ps$stud.code = readLines(ps$stud.file)
  
  ex.names = names(ps$ex)
  new.code = sapply(ex.names, extract.exercise.code, ps=ps)
  has.code = !is.na(new.code)
  names(has.code) = ex.names
  
  new.code = new.code[has.code]
  ex.names = ex.names[has.code]
  
  
  code.changed = sapply(seq_along(ex.names), function(i) !identical(ps$ex[[i]]$stud.code, new.code[i]))
  names(code.changed) = names(new.code)  = ex.names
  
  code.check = code.changed
  
  # No code changed
  if (!any(code.changed)) {
    if (!has.code[ps$ex.last.mod]) {
      ps$ex.last.mod=1
    }
    code.check[ps$ex.last.mod] = TRUE
    message("I see no changes in your code... did you forget to save your file?")
  } else {
    ps$ex.last.mod = ex.names[which(code.changed)[1]]
  }
  
  # Check exercises
  sum.correct = 0
  sum.warning = 0
  i = 1
  for (i in seq_along(ex.names)) {
    is.correct = FALSE
    ex.name = ex.names[i]
    ex = ps$ex[[ex.name]]
    if (ex$solved & !code.check[i]) {
      cat(paste0("\nExercise ", ex.name, " is unchanged and seemed correct."))
      is.correct = TRUE
    } else if (!ex$solved & !code.check[i]) {
      cat(paste0("\nSkip exercise ", ex.name, ": unchanged but still had error!"))
    } else {      
      ret = check.exercise(ex.name,new.code[ex.name])
      log.exercise(ex)
      if (ret==FALSE) {
        stop(ex$failure.message, call.=FALSE, domain=NA)
      } else if (ret=="warning") {
        message = paste0(ex$warning.messages,collapse="\n\n")
        message(paste0("Warning: ", message))
        sum.warning = sum.warning+1
      }
      is.correct = TRUE
    }
    if (is.correct) {
      sum.correct = sum.correct+1
      
      # Add new exercises to the student's problem set
      if (game.mode & length(ex$next.ex)>0) {
        for (j in ex$next.ex){
          if (!has.code[j]) {
            message(paste0("Add next exercise... ", ps$ex[[j]]$name))
            add.exercise(ex=ps$ex[[j]],ps=ps)
          }
        }
      }
    }
    #.Internal(stop(FALSE,""))
  } 
  if (sum.correct == length(ps$ex)) {
    cat("\nYou have solved all exercises and I could not detect any error!")
  }
}

signif.or.round = function(val, digits=3) {
  if (val>10^digits)
    return(round(val))
  return(signif(val,digits))
}

replace.whisker = function(txt,...,signif.digits=3) {
  require(whisker)
  args = list(...)
  restore.point("replace.whisker")
  for (i in seq_along(args)) {
    if (is.numeric(args[[i]]))
      args[[i]] = signif.or.round(args[[i]],signif.digits)
  }
  whisker.render(txt,args)
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
  #browser()
  #as.list(ex)
  ex$failure.message = message
  ex$failure.short = short
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


#' Initialize a problem set for the student
#' @param name the name of the problem set
#' @param stud.path the path in which the stud has stored his file
#' @param stud.file the file name in which the stud has stored her files 
#' @export
init.problem.set = function(name,stud.path, stud.short.file=paste0(prefix,name,".r"),
                            log.file = paste0(prefix,name,".log"),
                            state.file = paste0(prefix,name,".Rdata"),
                            structure.path=stud.path,
                            structure.file = paste0(prefix,name,"_struc.r"), prefix = "") {
  restore.point("init.problem.set")
  ps = new.env()
  class(ps) = c("Problemset","environment")
  set.ps(ps)
  
  
  ps$name = name
  ps$prefix = prefix
  ps$stud.path = stud.path
  ps$stud.short.file = stud.short.file
  ps$stud.file = paste0(stud.path,"/",stud.short.file)
  ps$log.file = paste0(stud.path,"/",log.file)
  ps$state.file = paste0(stud.path,"/",state.file)
  ps$structure.path = structure.path
  ps$structure.file = paste0(structure.path,"/",structure.file)
  ps$ex.last.mod = 1
  
  setwd(stud.path)
  parse.ps.structure(ps=ps)
  return(invisible(ps))
}


#' @export
get.ps = function() {
  if (!exists(".__rtutor_ps",.GlobalEnv))
    return(NULL)
  get(".__rtutor_ps",.GlobalEnv)
}

#' @export
set.ps = function(ps) {
  assign(".__rtutor_ps", ps, .GlobalEnv)
}

#' @export
get.ex = function() {
  if (!exists(".__rtutor_ex",.GlobalEnv))
    return(NULL)
  get(".__rtutor_ex",.GlobalEnv)
}

#' @export
set.ex = function(ex) {
  assign(".__rtutor_ex", ex, .GlobalEnv)
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


log.exercise = function(ex=get.ex(), log.file = ps$log.file, ps = get.ps(), do.log = ex$code.changed) {
  restore.point("log.exercise")
  if (!do.log)
    return
  entry = list(ps.name = ps$name, ex.name=ex$name, date=as.character(ex$check.date), stud.seed = ex$stud.seed,code.changed=as.logical(ex$code.changed),failure.short = ex$failure.short,checks=ex$checks, attempts=ex$attempts, solved=ex$solved, was.solved=ex$was.solved, stud.code=paste0(ex$stud.code, collapse="\n"),
warnings=ex$warning.messages)
  
  library(RJSONIO)
  json.entry = toJSON(entry)
  con = file(log.file,open="at")
  
  writeLines(json.entry, con)
  close(con) 
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
  for (test in ex$tests){
    ret = eval(test,ex$stud.env)
    
    if (ret==FALSE) {
      #message(capture.output(print(ex)))
      #message(capture.output(print(ps$ex[[ex.name]])))
      #message(capture.output(print(get.ex())))
      
      return(FALSE)
    } else if (ret=="warning") {
      had.warning = TRUE
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

examples = function() {
  
  library(restorepoint)
  setwd("C:/libraries/RTutor")
  source("rtutor.r")
  
  ps = load.problem.set("First Steps")
  create.stud.ps(ps)
  
  check.exercise("1a")
  
}

#shell('START mailto:sebkranz@gmail.com?subject=Test_Mailto&body=see_attachment')

