examples.hint.report = function() {
  library(RTutor)
  undebug(scramble.call.diffs)
  setwd("C:/libraries/RTutor/examples")
  ps.name = "myps"
  make.hint.report(ps.name)
  
  library(RTutor)
  setwd("C:/lehre/empecon/rtutor")
  ps.name = "ps1_wine"
  make.hint.report(ps.name)
  
}

#' Helper function when developing problem sets
#' 
#' Tries to check all chunks with given solution and shows
#' the corresponding hint.
make.hint.report = function(ps.name, out.file = paste0(ps.name, "_hint_report.Rmd")) {
  restore.point("make.hint.report")
  ps = init.ps(ps.name)
  set.ps(ps)
  chunk.ind = 1
  
  n = NROW(ps$cdt)
  hint.txt = rep("",n)
  org.code = ps$cdt$old.stud.code
  for (chunk.ind in 1:n) {
    stud.code = org.code[[chunk.ind]]
    #ps$stud.env = make.chunk.stud.env(chunk.ind, ps)
    check.chunk(chunk.ind = chunk.ind,stud.code = stud.code)
    copy.into.env(ps$stud.env, .GlobalEnv)
    hint.txt[chunk.ind] = merge.lines(capture.output(hint()))
    stud.code = ps$cdt$sol.txt[[chunk.ind]]
    check.chunk(chunk.ind = chunk.ind,stud.code = stud.code)
  }
  
  txt = paste0(
    "```{r \"sol_",ps$cdt$chunk.name,"\"}\n", ps$cdt$sol.txt,"\n```",
    "\n```{r \"",ps$cdt$chunk.name,"\"}\n", org.code,"\n```",
    "\nHint: ", hint.txt,
    collapse="\n---------------------------\n"
  )
  writeLines(txt,out.file)
  cat("\nWrote hint report to ", out.file)
  
}