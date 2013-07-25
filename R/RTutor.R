extract.command = function(txt,command) {
  lines = which(substring(txt,1,nchar(command))==command)
  if (length(lines)==0)
    return(NULL)
  val = str_trim(substring(txt[lines],nchar(command)+1))
  data.frame(line=lines, val=val)
} 

#' Parse the structure of a problem set from a file
parse.ps.structure =  function(file) {
  txt = readLines(file)
  library(stringr)
  
  
  ps.name= extract.command(txt, "#$ problem_set")[,2]
  ex_start = extract.command(txt, "#$ exercise")
  ex_end = extract.command(txt,"#$ end_exercise")
  
  ex.df = data.frame(start=ex_start$line, end=ex_end$line, name=ex_start$val)
  
  ex.li =vector("list",NROW(ex.df))
  names(ex.li) = ex.df$name
  
  for (i in 1:NROW(ex.df)) {
    ex.txt = txt[(ex.df[i,"start"]+1):(ex.df[i,"end"]-1)]
    com = extract.command(ex.txt,"#$")$line 
    
    ex.task.txt = paste0(ex.txt[1:(com[1]-1)],collapse="\n")
    ex.sol.txt = paste0(ex.txt[(com[1]+1):(com[2]-1)],collapse="\n")
    ex.tests.txt = paste0(ex.txt[(com[2]+1):length(ex.txt)],collapse="\n")
    
    ex.sol = parse(text=ex.sol.txt,srcfile=NULL)
    ex.tests = as.list(parse(text=ex.tests.txt,srcfile=NULL))     
    
    ex.li[[i]] = list(name = names(ex.li)[i],txt=ex.txt, task.txt=ex.task.txt,sol.txt=ex.sol.txt, tests.txt = ex.tests.txt, sol=ex.sol, tests=ex.tests)
    
  }
  
  ps = list(name=ps.name,ex=ex.li)
  return(ps)
}

#' Generate a problem set skeleton for a student and save it in a file
#' @export
write.ps.skeleton = function(ps, file = paste0(get.problemset.user.path(),"ps_",ps$name,"_skeleton.r")) {
  restore.point("write.ps.skeleton")
  
  ex.str = lapply(ps$ex, function (ex){
    paste0("\n###########################################\n",
           "#### Exercise ", ex$name,"\n",
           "###########################################\n",
           ex$task.txt,"\n",
           "#### end exercise ", ex$name, "\n",
           'check.exercise("',ex$name,'")'
           )    
  })
  
  str = paste0("#### Problemset ", ps$name,"\n\n",
'
library(RTutor)
# Add the path where you store this file
setwd("C:/libraries/RTutor/RTutor/problemsets")
set.student.file("',file,'") # Path and name of this file
load.problem.set("',ps$name,'")
'             ,
               paste0(ex.str,collapse="\n"))
  
  cat(str)
  writeLines(str,file)
  invisible(str)
}


#' Load the structure of a problem set
#' @param name the name of the problem set
#' @param the r file in which the structure of the problem set is described 
#' @export
load.problem.set = function(name,file = paste0(get.problemset.structure.path(),"ps_",name,"_struc.r")) {
  ps = parse.ps.structure(file)
  set.ps(ps)
  invisible(ps)
}


get.rtutor.glob = function(name) {
  get(paste0(".__rtutor_",name),.GlobalEnv)
  
}
set.rtutor.glob = function( name,val) {
  assign(paste0(".__rtutor_",name),val,.GlobalEnv)
}


#' Gets the path for the stored problem sets
#' @export
get.problemset.structure.path = function() {
  get.rtutor.glob("problemset.structure.path")
}

#' Sets the path for problem sets
#' @export
set.problemset.structure.path = function(path=  paste0(find.package("RTutor"),"/problemsets/")
) {
  set.rtutor.glob("problemset.structure.path",path)  
}

#' Gets the path for the stored problem sets
#' @export
get.problemset.user.path = function() {
  get.rtutor.glob("problemset.user.path")
}

#' Sets the path for problem sets
#' @export
set.problemset.user.path = function(path=getwd()) {
  set.rtutor.glob("problemset.user.path",path)  
}


get.sol.env = function() {
  get(".__sol.env",.GlobalEnv)
}

set.sol.env = function(env) {
  assign(".__sol.env", env, .GlobalEnv)
}


get.student.file = function() {
  get(".__rtutor_student.file",.GlobalEnv)
}

set.student.file = function(student.file) {
  assign(".__rtutor_student.file", student.file, .GlobalEnv)
}

get.ps = function() {
  get(".__rtutor_ps",.GlobalEnv)
}

set.ps = function(ps) {
  assign(".__rtutor_ps", ps, .GlobalEnv)
}

get.student.env = function() {
  get(".__student.env",.GlobalEnv)
}

set.student.env = function(env) {
  assign(".__student.env", env, .GlobalEnv)
}

#' Check the student's solution of an exercise contained in student.file
#' @param ex.name The name of the exercise
#' @param ps The problem set object. By default the problem set loaded with load.problem.set
#' @param student.file The r file in which the students solution to the problem set is stored
#' @export 
check.exercise = function(ex.name,ps=get.ps(),student.file=get.student.file()) {
  restore.point("check.exercise")
  
  cat(paste0("\n\n###########################################\n",
             "Check exercise ",ex.name,"...",
             "\n###########################################\n"))
  
  ex= ps$ex[[ex.name]]
  txt = readLines(student.file)
  
  start = extract.command(txt,paste0("#### Exercise ",ex.name))$line
  end = extract.command(txt,paste0("#### end exercise ",ex.name))$line
  
  # Parse user's solution
  student.txt = paste0(txt[start:end])
  student.expr = parse(text=student.txt,srcfile=NULL)
  student.env = new.env(parent=.GlobalEnv)
  eval(student.expr, student.env)
  
  # Parse official solution  
  sol.env = new.env(parent=.GlobalEnv)
  eval(ex$sol, sol.env)
  
  set.student.env(student.env)
  set.sol.env(sol.env)

  # Run tests
  student.env = get.student.env()
  sol.env= get.sol.env()
  ls(sol.env)
  
  had.warning = FALSE
  for (test in ex$tests){
    ret = eval(test)
    if (ret==FALSE) {
      stop("You did not pass all my tests. Your solution seems not right.")
    } else if (ret=="warning") {
      had.warning = TRUE
    }
  }
  if (had.warning) {
    message("\n Hmm... overall, I am not sure if your solution is right or not, look at the warnings.")
    return(invisible("warning"))
  } else {
    cat(paste0("\nCongrats, I could not find any error in your solution!"))
    return(invisible(TRUE))
  }
}

examples = function() {
  
  library(restorepoint)
  setwd("C:/libraries/RTutor")
  source("rtutor.r")
  
  ps = load.problem.set("First Steps")
  write.ps.skeleton(ps)
  
  check.exercise("1a")
  
}
