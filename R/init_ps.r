# All sorts of functions to initialize working on a problem set


#' Initialize a problem set for the student
#' @param name the name of the problem set
#' @param stud.path the path in which the stud has stored his file
#' @param stud.file the file name in which the stud has stored her files 
#' @export
init.problem.set = function(name,stud.path, stud.short.file=paste0(prefix,name,".r"),
                            log.file = paste0(prefix,name,".log"),
                            state.file = paste0(prefix,name,".Rdata"),
                            structure.path=stud.path,
                            structure.file = paste0(prefix,name,"_struc.r"), prefix = "") {
  restore.point("init.problem.set")
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
  ps$structure.file = paste0(structure.path,"/",structure.file)
  ps$ex.last.mod = 1
  
  setwd(stud.path)
  parse.ps.structure(ps=ps)
  return(invisible(ps))
}


#' Parse the structure of a problem set from a file
parse.ps.structure =  function(ps=get.ps(),file=ps$structure.file) {
  restore.point("parse.ps.structure", deep.copy=TRUE)
  txt = readLines(file)
  library(stringr)
  
  ps.name= extract.command(txt, "#$ problem_set")[,2]
  ex_start = extract.command(txt, "#$ exercise")
  ex_end = extract.command(txt,"#$ end_exercise")
  
  ex.df = data.frame(start=ex_start$line, end=ex_end$line, name=ex_start$val)
  
  ex.li =vector("list",NROW(ex.df))
  names(ex.li) = ex.df$name
  
  i = 1
  for (i in 1:NROW(ex.df)) {
    ex = new.env(parent=.GlobalEnv)
    set.ex(ex)
    ex$name = names(ex.li)[i]
    if (i == NROW(ex.df)) {
      ex$next.ex = NULL  
    } else {
      ex$next.ex = i+1        
    }
    ex.txt = txt[(ex.df[i,"start"]+1):(ex.df[i,"end"]-1)]
    com = extract.command(ex.txt,"#$")$line     
    #ex.task.txt = paste0(ex.txt[1:(com[1]-1)],collapse="\n")
    ex$task.txt = paste0(ex.txt[(com[1]+1):(com[2]-1)],collapse="\n")
    ex$sol.txt = paste0(ex.txt[(com[2]+1):(com[3]-1)],collapse="\n")
    ex$tests.txt = paste0(ex.txt[(com[3]+1):(com[4]-1)],collapse="\n")
    ex$hints.txt = paste0(ex.txt[(com[4]+1):length(ex.txt)],collapse="\n")
    
    # Replace whiskers
    ex$task.txt = whisker.render(ex$task.txt,list(ex_name=ex$name))
    
    
    ex$sol = parse(text=ex$sol.txt,srcfile=NULL)
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
    eval(parse(text=ex$hints.txt,srcfile=NULL))
    
    ex$sol.env = NULL
    ex$stud.env = NULL
    ex$stud.code = paste0("###########################################\n\n",ex$task.txt,"\n")
    ex$checks = 0
    ex$attempts=0
    ex$solved = FALSE
    ex$was.solved = FALSE
    
    ex.li[[i]] = ex
  }
  #ps$name = ps.name
  ps$ex = ex.li
  return(invisible(ps))
}
