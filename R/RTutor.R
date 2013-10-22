#library(devtools); install_github(repo="RTutor", studname="skranz")


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


print.Problemset = function(ps) {
  print(as.list(ps))
}

reset.ps = function(ps=get.ps()) {
  init.problem.set(ps$name,ps$stud.path,ps$stud.short.file)
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

examples = function() {
  
  library(restorepoint)
  setwd("C:/libraries/RTutor")
  source("rtutor.r")
  
  ps = load.problem.set("First Steps")
  create.stud.ps(ps)
  
  check.exercise("1a")
  
}

#shell('START mailto:sebkranz@gmail.com?subject=Test_Mailto&body=see_attachment')

