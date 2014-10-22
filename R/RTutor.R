#library(devtools); install_github(repo="RTutor", studname="skranz")


.onLoad = function(...)  {
  # If loaded as library overwrite restore.point to an empty function
  #assign("restore.point", function(...){}, envir=parent.env(environment()))
}


#' Add a hint to an exercise.
#' 
#' @param hint.name A name for the hint
#' @param code R code inside {} that will be evaluated in sol.env when the hint is shown. A hint could show messages via cat, but it could also show a plot. 
add.hint = function(hint.name, code,cond=NULL, ex=get.ex()) {
  code = substitute(code)
  restore.point("add.hint")
  hint = list(name=hint.name,cond=cond,code=code,ex=ex)
  ex$hints[[hint.name]]=hint
}

print.Problemset = function(ps) {
  print(as.list(ps))
}

reset.ps = function(ps=get.ps()) {
  init.ps(ps$name,ps$stud.path,ps$stud.short.file)
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


log.exercise = function(ex=get.ex(), log.file = ps$log.file, ps = get.ps(), do.log = isTRUE(ex$code.changed), part=ex$part) {
  restore.point("log.exercise")
  
  user.name = get.user()$name
  if (!do.log)
    return
  entry = list(ps.name = ps$name, ex.name=ex$name, test.ind=ex$test.ind, part=part, date=as.character(ex$check.date), user.name = user.name, stud.seed = ex$stud.seed,code.changed=as.logical(ex$code.changed),failure.short = ex$failure.short,checks=ex$checks, attempts=ex$attempts, solved=ex$solved, was.solved=ex$was.solved, stud.code=paste0(ex$stud.code, collapse="\n"),
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

#' Create a zip file of your solution that can be submitted
#' 
#' Only works after you have once checked your problem set!
#' @export
zip.solution = function(ps = get.ps(), user.name=get.user()$name, dir = ps$stud.path) {
  restore.point("zip.solution")
  
  if (is.null(ps)) {
    display("You must check your problem set before you can zip the solution")
    return()
  }
  
  old.dir = getwd()
  setwd(dir)
  #files = paste0(ps.name,c(".log",".r",".rmd",".html"))
  #files = c(ps$stud.file, ps$log.file, paste0(ps$stud.path,"/",user.name,"_",ps$name,".ups"))

  files = c(ps$stud.short.file, paste0(ps$name,".log"), paste0(user.name,"_",ps$name,".ups"))
  
  zip.file = paste0(dir,"/solution_", gsub(" ","_",ps$name,fixed=TRUE), "_by_", user.name, ".zip")
  
  zip(zip.file, files)
  display(paste0("Created zipped solution ", zip.file))
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

