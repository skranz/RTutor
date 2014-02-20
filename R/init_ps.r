# All sorts of functions to initialize working on a problem set

#' Returns a list of the names of all problem sets that are included in RTutor
#' @export
list.ps = function() {
  structure.path = paste0(find.package("RTutor"),"/problemsets")
  files = list.files(structure.path)
  pattern = "_struc.r"
  files = files[substring(files,nchar(files)-nchar(pattern)+1,)==pattern]
  names = substring(files,1,nchar(files)-nchar(pattern))
  return(names)
}
examples.list.ps = function() {
  list.ps()
}


#' Initialize a problem set for the student
#' @param name the name of the problem set
#' @param stud.path the path in which the stud has stored his file
#' @param stud.file the file name in which the stud has stored her files 
#' @export
init.problem.set = function(name,stud.path, stud.short.file=paste0(prefix,name,".r"),
                            log.file = paste0(prefix,name,".log"),
                            state.file = paste0(prefix,name,".Rdata"),
                            prefix = "", require.stud.file = TRUE) {
  restore.point("init.problem.set")
  
  stud.file = paste0(stud.path,"/",stud.short.file)
  if (require.stud.file & !file.exists(stud.file)){
    stop(paste0("I could not find the problem set file ", stud.file, ". Please check the file name and folder and make sure this problem set file does indeed exist!"))    
  }

  # Search for structure file first in problem set folder
  short.structure.file = paste0(prefix,name,"_struc.r")  
  structure.path= stud.path    
  structure.file = paste0(structure.path,"/",short.structure.file)

  # if not found search in library folder
  if (!file.exists(structure.file)) {
    structure.path = paste0(find.package("RTutor"),"/problemsets")
    structure.file = paste0(structure.path,"/",short.structure.file) 
    if (!file.exists(structure.file)) {   
      str = paste0("I could not find the problem set structure file '", short.structure.file, 
                  "', neither in your problem set folder ", stud.path, " nor in the RTutor library. Make sure you entered the correct name for the problem set. You can get a list of all problem sets that are included in RTutor by running 'list.ps()'. If you don't see the problem set, try updating your RTutor version.")
      stop(str)
    }  
  }
  
  ps = new.env()
  class(ps) = c("Problemset","environment")
  set.ps(ps)
  
  
  
  
  ps$name = name
  ps$prefix = prefix
  ps$stud.path = stud.path
  ps$stud.short.file = stud.short.file
  ps$stud.file = paste0(stud.path,"/",stud.short.file)
  ps$log.file = paste0(stud.path,"/",log.file)
  ps$state.file = paste0(stud.path,"/",state.file)
  ps$structure.path = structure.path
  ps$structure.file = structure.file
  ps$ex.last.mod = 1
  
  setwd(stud.path)
  parse.ps.structure(ps=ps)
  return(invisible(ps))
}


#' Parse the structure of a problem set from a file
parse.ps.structure =  function(ps=get.ps(),file=ps$structure.file) {
  restore.point("parse.ps.structure")
  txt = readLines(file)
  library(stringr)
  
  ps.name= extract.command(txt, "#$ problem_set")[,2]
  ex_start = extract.command(txt, "#$ exercise")
  # Exercise names: remove # and trailing " "
  ex_start$val = str.trim(gsub("#"," ", ex_start$val,fixed=TRUE))

  ex_end = extract.command(txt,"#$ end_exercise")
  
  ex.df = data.frame(start=ex_start$line, end=ex_end$line, name=ex_start$val)
  
  ex.li =vector("list",NROW(ex.df))
  names(ex.li) = ex.df$name
  
  i = 1
  for (i in 1:NROW(ex.df)) {
    ex.name = names(ex.li)[i]
    
    ex.txt = txt[(ex.df[i,"start"]+1):(ex.df[i,"end"]-1)]

    ex = parse.exercise(ex.name,ex.txt)
    
    if (i == NROW(ex.df)) {
      ex$next.ex = NULL  
    } else {
      ex$next.ex = i+1        
    }
    ex.li[[i]] = ex
  }
  #ps$name = ps.name
  ps$ex = ex.li
  return(invisible(ps))
}

parse.exercise = function(ex.name, ex.txt) {
  restore.point("parse.exercise")
  ex = new.env(parent=.GlobalEnv)
  ex$name = ex.name
  set.ex(ex)
  
  com = extract.command(ex.txt,"#$")
  str = gsub("#","",com$val,fixed=TRUE)
  str = gsub("-","",str,fixed=TRUE)
  com$name = str.trim(str)

  com$end.line = c(com$line[-1]-1,length(ex.txt))
  for (i in seq_along(com$name)) {
    com.name.txt = paste0(com$name[i],".txt")
    ex[[com.name.txt]] = paste0(ex.txt[(com$line[i]+1):com$end.line[i]],collapse="\n")
  }
  
  # Replace whiskers
  ex$task.txt = whisker.render(ex$task.txt,list(ex_name=ex$name))    
  
  if (length(ex$solution.txt)>0)
    ex$sol = parse(text=ex$solution.txt,srcfile=NULL)

  if (length(ex$tests.txt)>0)
    ex$tests = as.list(parse(text=ex$tests.txt,srcfile=NULL))     
  
  ex$tests.stats = lapply(seq_along(ex$tests), function (i) {
    list(
      times.failed.before.ever.passed = 0,
      times.failed = 0, # Times failed before last passed
      ever.passed = FALSE,
      passed = FALSE
    )
  })
  # Run the code that generates hints
  ex$prev.hint = 0
  ex$hints = list()
  if (length(ex$hints.txt)>0)
    eval(parse(text=ex$hints.txt,srcfile=NULL))
  
  ex$sol.env = NULL
  ex$stud.env = NULL
  ex$stud.code = paste0("###########################################\n\n",ex$task.txt,"\n")
  ex$checks = 0
  ex$attempts=0
  ex$solved = FALSE
  ex$was.solved = FALSE
  
  return(ex)
}