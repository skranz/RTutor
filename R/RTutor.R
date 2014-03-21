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

#' Shows a hint for the current problem.
#' @export
hint = function(hint.name = ex$hint.name,  ex=get.ex(), ps=get.ps()) {
  
  if (is.null(hint.name)) {
    message("Sorry, but there is no hint for your current problem.")
    return()
  }
  hint = ex$hints[[hint.name]]
  #cat(paste0("\nHint ", hint$name,":"))
  eval(hint$code,ex$sol.env)
}

#' Called by a test, sets the current hint
set.current.hint = function(hint.name=NULL, ex=get.ex()) {
  ex$hint.name = hint.name
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
    if (isTRUE(ps$name == name & ps$stud.path==stud.path & ps$stud.short.file == stud.short.file))
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

#' Create a zip file of a students solution that can be uploaded 
zip.solution = function(ps.name = get.ps()$name, user.name=get.user()$name, dir = get.ps()$stud.path, make.html=TRUE) {
  restore.point("zip.solution")
  
  old.dir = getwd()
  setwd(dir)
  files = paste0(ps.name,c(".log",".r",".rmd",".html"))
  zip.file = paste0(dir,"/solution_", gsub(" ","_",ps.name,fixed=TRUE), "_by_", user.name, ".zip")
  
  zip(zip.file, files)
  message(paste0("Created zipped solution ", zip.file))
  setwd(old.dir)
}

unzip.solutions = function(dir = getwd(), dest.dir = dir) {
  restore.point("unzip.solutions")
  files = c(list.files(path=dir,pattern=glob2rx("*.zip"), recursive=check.sub.dir, full.names=TRUE),
            list.files(path=dir,pattern=glob2rx("*.ZIP"), recursive=check.sub.dir, full.names=TRUE))

  lapply(files, function(file) {
    restore.point("zwsdhd")
    subdir =  paste0(dest.dir,"/solutions/",str.left.of(basename(file),"."))
    dir.create(subdir)
    unzip(file, exdir=subdir, overwrite=TRUE)
    message(paste0("Unzipped ", file))
  })
  
}

#shell('START mailto:sebkranz@gmail.com?subject=Test_Mailto&body=see_attachment')

