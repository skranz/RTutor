# Not used anymore
rmd.to.latex.addin = function(...) {
  library(rstudioapi)
  library(RTutor)
  doc = rstudioapi::getActiveDocumentContext()
  restore.point("rmd.to.latex.addin")
  file = basename(doc$path)
  dir = dirname(doc$path)
  setwd(dir)

  ext = tools::file_ext(file)
  if (tolower(ext) == "rmd") {
    rmd.to.latex(input=doc$path)
  } else if (tolower(ext) == "md") {
    md.to.latex(input=doc$path)
  } else {
    cat("\nYou must select an .Rmd or an .md file...")
    return()
  }
}

rtutor.hint.addin = function(...) {
  library(RTutor)
  hint()
}

check.ps.addin = function(...) {
  library(rstudioapi)
  library(RTutor)
  doc = rstudioapi::getSourceEditorContext()
  restore.point("check.ps.addin")
  
  # Work around to get active Rmd file by focusing on console
  if (is.null(doc)) {
    sendToConsole("", execute = FALSE, echo = TRUE, focus = TRUE)
    doc = rstudioapi::getSourceEditorContext()
  }
  
  if (is.null(doc)) {
      return(check.ps.addin.no.active.doc())
  }
  
  file = basename(doc$path)
  if (tolower(tools::file_ext(file)) != "rmd") {
    message("No .Rmd file selected. Select your problem set file in the editor window and try again.")
    return()
  }
  dir = dirname(doc$path)
  
  cat("\n---------------------------\nSave and check problem set file ", doc$path," ...\n")
  
  # Save document via RStudio API
  # Benefits compared to writeLines:
  # Should ensure the same encoding as set in RStudio.
  # Also should avoid message that the content of
  # the file has been changed
  try(rstudioapi::documentSave(doc$id))

  #doc$contents does not yet work well with visual markdown
  #txt = doc$contents
  txt = readLines(doc$path, warn=FALSE)
  restore.point("check.ps.addin2")
  
  user.name.line = which(str.starts.with(str.trim(txt),"user.name ="))
  if (length(user.name.line)==0) {
    message = paste0("I could not find a line in your first chunk that starts with `user.name = ` to specify your user.name. I set the user.name to UNKNOWN.\n")
    message(message)
    user.name = "UNKNOWN"
  } else {
    eval(parse(text=txt[user.name.line]))
  }
  
  ps.name = guess.ps.name(txt,dir,file)
  if (is.na(ps.name)) return()

  err = NULL
  tryCatch(check.problem.set(ps.name, dir, file, user.name=user.name, stud.code=txt), error=function(e) {err<<-paste0(as.character(e),collapse="\n")})
    
  if (!is.null(err)) {
    err = gsub("Error in stop.without.error(message): ","",err,fixed=TRUE)
    message(err)
  }

}

check.ps.addin.no.active.doc = function() {
  msg = "The active problem set RMD file could not be detected. This can currently happen in the visual RMarkdown editor mode.\nPlease set the cursor inside a code chunk or in the R console and check your solution again."
  message(msg)
  return(invisible())
  
} 

guess.ps.name = function(txt, dir, file) {
  restore.point("guess.ps.name")
  rmd.guess = paste0(tools::file_path_sans_ext(file))
  if (file.exists(file.path(dir, paste0(rmd.guess,".rps")))) {
    return(rmd.guess)
  }
  
  line = which(str.starts.with(str.trim(txt),"check.problem.set("))
  if (length(line)==0) {
    message("I could not identify a problem set name. Try again to select your .Rmd file and choose Addins->Check Problemset. Also make sure that your directory contains the binary problem set file that has the same name as your .Rmd file but the file type .rps")
    return(NA)
  }
  ps.name = str.remove.ends(str.trim(str.between(txt[line[1]], "(",",")),1,1)
  return(ps.name)
}


check.ps.addin.old = function(...) {
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


