# All sorts of functions to initialize working on a problem set

#' Returns a list of the names of all problem sets that are included in RTutor
#' @export
list.ps = function() {
  structure.path = paste0(find.package("RTutor"),"/problemsets")
  files = list.files(structure.path)
  pattern = ".rps"
  files = files[substring(files,nchar(files)-nchar(pattern)+1,)==pattern]
  names = substring(files,1,nchar(files)-nchar(pattern))
  return(names)
}
examples.list.ps = function() {
  list.ps()
}

examples.init.ps = function() {
  setwd("D:/libraries/RTutor/RTutor/vignettes/problemsets")
  ps = init.ps("Example2")
  ps$cdt
}

#' Makes a local copy of a problem set for a new shiny session
copy.ps.for.session = function(ps, empty.stud.env=TRUE) {
  if (!empty.stud.env)
    stop("Current version can only make copies of empty.stud.env")
  
  ops = ps
  ps = as.environment(as.list(ps))
  class(ps) = c("Problemset","environment")
  
  ps$cdt = copy(ops$cdt)
  ps$edt = copy(ops$edt)
  
  cdt = ps$cdt; edt = ps$edt
  cdt$stud.env = lapply(1:NROW(cdt), function(chunk.ind) {
    new.stud.env(chunk.ind)
  })
  env.li  = replicate(NROW(edt),new.stud.env(chunk.ind=0), simplify=FALSE)
  edt$ex.final.env = env.li  
  return(ps)  
}

#' Initialize a problem set for the student
#' @param ps.name the name of the problem set
#' @param dir the path in which the stud has stored his file
#' @param stud.hort.file the file name (without path) of the .rmd problem set file
#' @export
init.ps = function(ps.name,dir=getwd(), stud.short.file = paste0(ps.name,".Rmd"), rps.file = paste0(rps.dir,"/",ps.name,".rps"),log.file = paste0(dir,"/",ps.name,".log"), rps.dir=dir, save.nothing=FALSE) {
  restore.point("init.ps")
 
  rps = load.rps(file=rps.file)
  ps = new.env()
  set.ps(ps)
  ps$name = ps.name
  ps$rps = rps
  load.ps.libs(rps$libs)
  ps$save.nothing = save.nothing
  
  ps$ps.baseenv = new.env(parent=parent.env(globalenv()))
  #print(all.parent.env(ps$ps.baseenv))
  if (!is.null(rps$extra.code.env)) {
    copy.into.env(source=rps$extra.code.env, dest = ps$ps.baseenv) 
  }
  
  
  cdt = rps$cdt
  
  cdt$is.solved = FALSE
  cdt$chunk.changed = FALSE
  cdt$stud.env = lapply(1:NROW(cdt), function(chunk.ind) {
    new.stud.env(chunk.ind)
  })
  cdt$old.stud.code = cdt$task.txt
  #cdt$stud.code = cdt$task.txt
  
  ps$cdt = cdt  
  ps$tdt = rps$tdt
  
  edt = rps$edt
  edt$ex.solved = FALSE
  #env.li  = replicate(NROW(edt),new.env(parent=ps$ps.baseenv), simplify=FALSE)
  env.li  = replicate(NROW(edt),new.stud.env(chunk.ind=0), simplify=FALSE)
  edt$ex.final.env = env.li
  ps$edt = edt

  
  ps$num.ex = NROW(ps$edt)
  ps$num.chunks = NROW(cdt)
  ps$warning.messages = list()
  
  set.ps(ps)
  ps$stud.path = dir
  stud.file=paste0(dir,"/",stud.short.file)
  ps$stud.file = stud.file
  ps$stud.short.file = stud.short.file
  ps$log.file = log.file  
  class(ps) = c("Problemset","environment")
  return(ps)
}

load.ps.libs = function(libs) {
  if (length(libs)==0)
    return()
  for (i in seq_along(libs)) {
    lib = libs[i]
    display("load package ", lib, "...")
    ret = suppressWarnings(require(lib, quietly=TRUE, warn.conflicts =FALSE,character.only=TRUE))
    if (!ret) {
      stop(paste0("Please install the package '", lib,
                  "', which is required to solve this problem set."))
    }
    display("... all required packages loaded.") 
  }
}


get.or.init.ps = function(ps.name,dir, stud.short.file = paste0(ps.name,".Rmd"), reset=FALSE, ps=get.ps()) {
  restore.point("get.or.init.ps")
  
  # Just take current problem set information
  if (!is.null(ps) & !reset) {
    if (isTRUE(ps$name == ps.name & ps$stud.path==dir & ps$stud.short.file == stud.short.file)) {
      return(ps)
    }
  }
  
  # Initialize problem set newly
  return(init.ps(ps.name,dir,stud.short.file))
}
