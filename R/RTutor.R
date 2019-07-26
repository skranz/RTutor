#library(devtools); install_github(repo="RTutor", studname="skranz")


.onLoad = function(...)  {
  # If loaded as library overwrite restore.point to an empty function
  #assign("restore.point", function(...){}, envir=parent.env(environment()))
}



print.Problemset = function(ps) {
  omit = c("output","shiny.dt","cdt","tdt", "view.ui.li")
  omit = c("output")
  fields = setdiff(ls(ps),omit)
  print(str(as.list(ps)[fields], max.level=1, give.attr=FALSE))
  
  
  cat("\n Ommited: ", paste0(omit, collapse="\n"))
}

reset.ps = function(ps=get.ps()) {
  init.ps(ps$name,ps$user.name, ps$stud.path,ps$stud.short.file)
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
