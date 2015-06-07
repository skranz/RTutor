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

#' Get the current problem set
#' 
#' Either a globally stored problem set or
#' if RTutor runs as a web-app the associated problem set
#' with the current shiny session
#' 
#' @export
get.ps = function(force.global = FALSE) {
  if (!exists(".__rtutor_ps",.GlobalEnv))
    return(NULL)
  gps = get(".__rtutor_ps",.GlobalEnv)
  if (force.global)
    return(gps)
  
  if (isTRUE(gps$running.web.app)) {
    # if we have a local variant; get the local problem set
    app = getApp()
    if (!is.null(app[["ps"]]))
      return(app[["ps"]])    
  }
  return(gps)
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
zip.solution = function(ps = get.ps(), user.name=get.user()$name, dir = ps$stud.path, ask=TRUE) {
  restore.point("zip.solution")
  
  if (is.null(ps)) {
    display("You must check your problem set before you can zip the solution")
    return(invisible(NULL))
  }
  

  if (ask) {
    stats()
    cat("\nDo you want to zip your solution with the stats above?\n(If the stats seem wrong, cancel and check your problem set again.)")
    res = readline(prompt="Type y and Enter to continue: ")
    if (!tolower(res)=="y") {
      cat("\nCancelled zipping of solution.")
      return(invisible(NULL))
    }
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
  return(invisible(zip.file))
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

