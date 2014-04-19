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
init.problem.set = function(name,stud.path=NULL, stud.short.file=paste0(name,".r"),
                            log.file = paste0(name,".log"),
                            require.stud.file = TRUE, load.rps=TRUE) {
  restore.point("init.problem.set")
  
  #setwd(stud.path)
  
  # Search for a binary or text version of the structure file 
  structure.path= stud.path    
  structure.file = paste0(structure.path,"/",name,"_struc.r")
  found.file = TRUE
  # 1. r file in stud.path
  if (file.exists(structure.file)) {
    use.rps = FALSE
  } else {
    # 2. rps file in stud.path
    structure.file = paste0(structure.path,"/",name,".rps")
    # 3. rps file in library problem set path
    if (file.exists(structure.file)) {
      use.rps = TRUE
    } else {
      structure.path = paste0(find.package("RTutor"),"/problemsets")
      structure.file = paste0(structure.path,"/",name,".rps")
      if (file.exists(structure.file)) {
        use.rps = TRUE
      } else {
        found.file = FALSE
      }
    }
  }


  # if not found search in library folder
  if (!found.file) {
    str = paste0("I could not find a problem set structure file '", paste0(name,".rps"), ,"' or '", paste0(name,"_struc.r"),
                "', neither in your problem set folder ", stud.path, " nor in the RTutor library. Make sure you entered the correct name for the problem set. You can get a list of all problem sets that are included in RTutor by running 'list.ps()'. If you don't see the problem set, try updating your RTutor version.")
    stop(str) 
  }


  if (use.rps) {
    ps = load.binary.ps(file=structure.file)
  } else {
    ps = new.env()
    class(ps) = c("Problemset","environment")
    ps$structure.path = structure.path
    ps$structure.file = structure.file

    set.ps(ps)
    
    ps$name = name
    ps$ex.last.mod = 1  
    parse.ps.structure(ps=ps)
    
    # Save as rps
    structure.file = paste0(structure.path,"/",name,".rps")
    save.binary.ps(ps,structure.file)
  }
  ps$structure.path = structure.path
  ps$structure.file = structure.file
  ps$stud.path = stud.path

  ps$has.stud.file = FALSE
  if (require.stud.file) {
    set.ps.stud.file(ps,stud.path,stud.short.file, log.file = log.file)
  }
  
  return(invisible(ps))
}

# Assigns a student file to ps and does all corresponding intializations
set.ps.stud.file = function(ps, stud.path,stud.short.file, log.file = paste0(ps$name,".log") ) {
  stud.file = paste0(stud.path,"/",stud.short.file)
  if (!file.exists(stud.file)){
    stop(paste0("I could not find the problem set file ", stud.file, ". Please check the file name and folder and make sure this problem set file does indeed exist!"))    
  }
  ps$has.stud.file = TRUE
  
  ps$stud.path = stud.path
  ps$stud.short.file = stud.short.file
  ps$stud.file = paste0(stud.path,"/",stud.short.file)
  ps$log.file = paste0(stud.path,"/",log.file)
  ps$is.rmd.stud = str.ends.with(tolower(stud.short.file),".rmd")

  # Initialize stud.code once more
  ps$stud.code = readLines(ps$stud.file)
  ex.names = names(ps$ex)
  i = 1
  for (i in seq_along(ps$ex)) {
    ps$ex[[i]]$stud.code[[1]] = extract.exercise.code(ex.names[i],ps=ps)
  }
}

save.binary.ps = function(ps, file = paste0(ps$name,".rps")) {
  save(ps, file=file)
}


load.binary.ps = function(ps.name, file = paste0(ps.name,".rps")) {
  load(file)
  return(ps)
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
    ex$ind =i
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

  ex$settings = new.env(parent=.GlobalEnv)
  if (!is.null(ex$settings.txt))
    eval(parse(text=ex$settings.txt,srcfile=NULL),ex$settings)
  
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