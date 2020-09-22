examples.hint.report = function() {
  library(RTutor)
  setwd("C:/libraries/RTutor/examples")
  ps.name = "myps"
  make.hint.report(ps.name)
}

#' Helper function when developing problem sets
#' 
#' Tries to check all chunks with given solution and shows
#' the corresponding hint.
make.hint.report = function(ps.name, out.file = paste0(ps.name, "_hint_report.Rmd")) {
  ps = init.ps(ps.name)
  set.ps(ps)
  chunk.ind = 1
  
  n = NROW(ps$cdt)
  hint.txt = rep("",n)
  org.code = ps$cdt$old.stud.code
  for (chunk.ind in 1:n) {
    stud.code = org.code[[chunk.ind]]
    check.chunk(chunk.ind = chunk.ind,stud.code = stud.code)
    hint.txt[chunk.ind] = merge.lines(capture.output(hint()))
    stud.code = ps$cdt$sol.txt[[chunk.ind]]
    check.chunk(chunk.ind = chunk.ind,stud.code = stud.code)
  }
  
  txt = paste0(
    "```{r sol",1:n,"}\n", ps$cdt$sol.txt,"\n```",
    "\n```{r chunk",1:n,"}\n", org.code,"\n```",
    "\nHint: ", hint.txt,
    collapse="\n---------------------------\n"
  )
  writeLines(txt,out.file)
  cat("\nWrote hint report to ", out.file)
  
}