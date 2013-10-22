# All sorts of functions to create problem sets

#' Init files for developing a new problem set with given name under the specified parent directory
#' @export 
develop.problem.set = function(name,parent.path) {
  restore.point("develop.problem.set")
  RTutor.path = path.package("RTutor")
  
  ps.path = paste0(parent.path,"/",name)
  message(paste0("Create directory ", ps.path))
  dir.create(ps.path, recursive=TRUE)
  
  dest.file = paste0(ps.path,"/make_student_ps.r")
  if (file.exists(dest.file)) {
    message(paste0("File ", dest.file, " already exists. I don't overwrite it."))
  } else {
    source.file = paste0(RTutor.path,"/problemsets/make_student_ps.Rtmpl")
    txt = readLines(source.file)
    txt = whisker.render(txt,list(ps_name=name,ps_path=ps.path))
    writeLines(txt,dest.file)
  }
  
  dest.file = paste0(ps.path,"/",name,"_struc.r")
  if (file.exists(dest.file)) {
    message(paste0("File ", dest.file, " already exists. I don't overwrite it."))
  } else {
    source.file = paste0(RTutor.path,"/problemsets/ps_struc.r")
    txt = readLines(source.file)
    txt[1] = paste0("#$ problem_set ", name)
    writeLines(txt,dest.file)
  }  
}



add.exercise = function(ex,ps=get.ps()) {
  ex.code = get.empty.ex.code(ex)
  con = file(ps$stud.file,"a")
  writeLines(ex.code,con)
  close(con)
}

get.empty.ex.code = function(ex) {
  paste0("\n###########################################\n",
         "#### Exercise ", ex$name,"\n",
         "###########################################\n\n",
         ex$task.txt,"\n\n",
         "#### end exercise ", ex$name, "\n"
         #           'check.exercise("',ex$name,'")'
  )      
} 

#' Generate a problem set skeleton for a student and save it in a file
#' @export
create.stud.ps = function(ps, file = ps$stud.file, ps.dir="C:/...") {
  restore.point("create.stud.ps")
  
  ex.str = lapply(ps$ex, get.empty.ex.code)
  
  str = paste0("#### Problemset ", ps$name,"\n\n",
               '
# To check your solutions in RStudio save (Ctrl-s) and then source (Ctrl-Shift-s)
# If you check "Source on Save" in RStudio you just have to save (Ctrl-s)

ps.dir =  "',ps.dir,'" # your working directory
ps.file = "', ps$prefix, ps$name,'.r" # this file

library(RTutor)
check.problem.set("',ps$name,'", ps.dir, ps.file)
', paste0(ex.str,collapse="\n"))
  
  cat(str)
  writeLines(str,file)
  invisible(str)
}
