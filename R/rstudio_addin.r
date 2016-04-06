check.ps.addin = function(...) {
  library(rstudioapi)
  library(RTutor)
  doc = rstudioapi::getActiveDocumentContext()
  restore.point("check.ps.addin")
  file = basename(doc$path)
  dir = dirname(doc$path)
  txt = doc$contents
  
  cat("\n---------------------------\nSave and check problem set file ", doc$path," ...\n")
  
  # Save file
  writeLines(text = txt, doc$path)
  
  start.line = which(str.starts.with(txt,"```{r 'check_ps', include=FALSE}"))[1]
  if (is.na(start.line)) {
    message(paste0("Error: Your first chunk should contain the code to check your solution and start with the line:\n\n",
      "```{r 'check_ps', include=FALSE}",
      "You seemed to have changed this line. You must correct this first chunk, so that I can check your solution."))
    return()
  }
  end.line = which(str.starts.with(txt,"```") & seq_along(txt)>start.line)[1]
  code = txt[(start.line+1):(end.line-1)]
  
  .GlobalEnv$ps.dir = dir
  .GlobalEnv$ps.file = file
  
  expr = parse(text=code)
  err = NULL
  tryCatch(eval(expr, globalenv()), error=function(e) {err<<-paste0(as.character(e),collapse="\n")})
    
  if (!is.null(err)) {
    err = gsub("Error in stop.without.error(message): ","",err,fixed=TRUE)
    message(err)
  }
}

