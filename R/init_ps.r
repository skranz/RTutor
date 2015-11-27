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
init.ps = function(ps.name,dir=getwd(), stud.short.file = paste0(ps.name,".Rmd"), rps.file = paste0(rps.dir,"/",ps.name,".rps"),log.file = paste0(dir,"/",ps.name,".log"), rps.dir=dir, save.nothing=FALSE, check.whitelist=!is.null(wl), wl = NULL, use.memoise=NULL, noeval=FALSE, precomp=FALSE) {
  restore.point("init.ps")

  rps = load.rps(file=rps.file)

  if (!is.null(use.memoise))
    rps$use.memoise = use.memoise

  ps = new.env()
  set.ps(ps)
  ps$name = ps.name
  ps$rps = rps
  load.ps.libs(rps$libs)
  ps$save.nothing = save.nothing

  ps$precomp = precomp & isTRUE(rps$precomp)
  ps$preknit = isTRUE(rps$preknit)
  ps$noeval = noeval & ps$preknit
  if (precomp & !isTRUE(rps$precomp))
    stop("Cannot init.ps with precomp=TRUE, since rps was not precomputed.")
  if (noeval & !ps$preknit)
    stop("Cannot init.ps with noeval=TRUE, since rps was not preknitted.")
  if (noeval & precomp)
    stop("Cannot init.ps with both noeval=TRUE and precomp=TRUE.")

  ps$ps.baseenv = new.env(parent=parent.env(globalenv()))

  if (isTRUE(rps$use.memoise)) {
    copy.into.env(dest=ps$ps.baseenv, source=rps$memoise.fun.li)
  }


  # for backwards compatibility
    if (! ("optional" %in% colnames(rps$cdt)))
      rps$cdt$optional = FALSE

    if (! ("note.ind" %in% colnames(rps$shiny.dt))) {
      rps$shiny.dt$note.ind = 0
      rps$shiny.dt$note.label = ""
    }
  # end backwards compatibility

  #print(all.parent.env(ps$ps.baseenv))
  if (!is.null(rps$extra.code.env)) {
    copy.into.env(source=rps$extra.code.env, dest = ps$ps.baseenv)
  }


  cdt = as_data_frame(rps$cdt)



  cdt$is.solved = FALSE
  cdt$chunk.changed = FALSE

  # in noeval mode not all tests can be run
  # show sample solution for non-testable chunks
  if (noeval & !is.null(cdt$can.noeval.test)) {
    for (row in which(!cdt$can.noeval.test)) {
      if (cdt$task.txt[[row]] != cdt$sol.txt[[row]]) {
        if (cdt$num.e[[row]] >0) {
          add =  "# solution shown because chunk cannot be tested on this server\n"
        } else {
          add =  "# solution shown because chunk cannot be run on this server\n"
        }
        cdt$task.txt[[row]] = paste0(add, cdt$sol.txt[[row]])
      }
      cdt$test.expr[[row]] = list()
      cdt$has.test[[row]] = FALSE
      cdt$e.li[[row]] = cdt$e.source.li[[row]] = list()
      cdt$num.e[[row]] = 0
    }
  }

  if (!ps$precomp) {
    cdt$stud.env =lapply(1:NROW(cdt), function(chunk.ind) {
      new.stud.env(chunk.ind)
    })
  }
  cdt$old.stud.code = cdt$task.txt

  cdt = as.data.table(cdt)

  ps$cdt = rps$cdt = cdt
  ps$tdt = rps$tdt

  edt = rps$edt
  edt$ex.solved = FALSE
  #env.li  = replicate(NROW(edt),new.env(parent=ps$ps.baseenv), simplify=FALSE)

  if (!ps$precomp) {
    env.li  = replicate(NROW(edt),new.stud.env(chunk.ind=0), simplify=FALSE)
    edt$ex.final.env = env.li
  }

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

  log.event(type="init_ps")

  ps$check.whitelist = check.whitelist
  ps$wl = wl
  if (isTRUE(ps$check.whitelist))
    library(whitelistcalls)

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
