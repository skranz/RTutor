# All sorts of functions to create problem sets


add.exercise = function(ex,ps=get.ps()) {
  ex.code = get.empty.ex.code(ex)
  con = file(ps$stud.file,"a")
  writeLines(ex.code,con)
  close(con)
}

get.empty.ex.code = function(ex) {
  restore.point("get.empty.ex.code")
  
  paste0("#' ## Exercise ", ex$name," ###########################\n",
         "#'\n",
         ex$task.txt,"\n\n"
         #"#' #### end of exercise ", ex$name, " ##################\n"
         #           'check.exercise("',ex$name,'")'
  )      
} 

#' Generate a problem set from a solution file
#' 
#' Generates  _struc.r file, .rps file, empty problem set .r and .rmd files
#' and a sample solution .rmd file (overwrites existing files)
#' @export
create.ps = function(sol.file, ps.name, user.name= "ENTER A USER NAME HERE", sol.user.name="Jane Doe", dir = getwd(), header="", footer="", libs="") {
  setwd(dir)
  create.struc(sol.file,ps.name)
  create.empty.ps.and.rps(ps.name=ps.name, user.name=user.name, dir=dir, header=header,footer=footer, libs=libs)
  create.sample.solution(sol.file=sol.file,ps.name=ps.name, user.name=sol.user.name, dir=dir, header=header,footer=footer, libs=libs)
  
}

#' Generate an empty problem set for a student in folder dir and the rps file
#' @export
create.empty.ps.and.rps = function(ps.name=get.ps()$name, dir = getwd(),user.name = "ENTER A USER NAME HERE", header="", footer="", libs=NULL, make.rps = TRUE) {
  restore.point("create.empty.ps")
  ps = init.problem.set(ps.name,dir, require.stud.file=FALSE, load.struc.file = TRUE)
  create.stud.ps.r(ps,ps.dir=dir, user.name=user.name, header=header,footer=footer)
  create.stud.ps.rmd(ps,ps.dir=dir, user.name=user.name, header=header, footer=footer,libs=libs)
  cat(paste0("I generated the empty problem set files ", ps$stud.file, " (an .r and a .Rmd version). You can open them in RStudio."))
  
  if (make.rps) {
    setwd(dir)
    ext = "rmd"
    rli = lapply(c("rmd","r"), function(ext) {
      ps$is.rmd.stud = ext=="rmd"
      ps$stud.file = paste0(ps.name,".",ext)
      ps$stud.code = readLines(ps$stud.file)
      ex.names = names(ps$ex)
      li = lapply(seq_along(ps$ex), function(i) {
        extract.exercise.code(ex.names[i],ps=ps)  
      })
      names(li) = ex.names
      li
    })
    ps$ex.initial.code.rmd = rli[[1]]
    ps$ex.initial.code.r = rli[[2]]
    ps$is.rmd.stud = ps$stud.file = ps$stud.code = NULL
    # Save as rps
    rps.file = paste0(ps.name,".rps")
    save.binary.ps(ps,rps.file)
  }

  
}



# create.empty.ps()

#' Generate default header text for a Rmd file
#' @export
install.header.txt = function() {
"
# Remove comments below if you need to install packages
# install.packages('devtools');install.packages('whisker');install.packages('stringr')
# install.packages('RJSONIO');
# library(devtools)
# install_github(repo = 'restorepoint', username = 'skranz')
# install_github(repo = 'RTutor', username = 'skranz')    
"  
}

#' Generate default footer text for a Rmd file
#' @export
zip.submit.footer.txt = function(ps.name) {
paste0("
#'
#'## Sumbitting your solution
#'
#' Submit your solution as a zip file with name
#'`solution_",ps.name,"_by_username.zip`
#' that contains the files
#' `",ps.name,".rmd, ", ps.name,".log, username_",ps.name,".ups`
#' (replace `username` by your user name)
#' 
#' If you have installed RTools (http://cran.r-project.org/bin/windows/Rtools/) and updated your Windows PATH variable you can also try calling
#' `zip.solution()` 
#' to generate the zip file automatically.
")
}

rmd.ps.header = function(ps.name,ps.dir = "C:/...", ps.file = paste0(ps.name,".Rmd"), header="", libs=NULL, user.name="") {
  if(!is.null(libs))
    libs = paste0("library(",libs,")", collapse=";")

  str = paste0("
#' # Problemset ", ps.name,"

#+ include=FALSE", header,"

# To check your solutions in RStudio save (Ctrl-S) and then run all chunks (Ctrl-Alt-R)

# Note: You must use / instead of \\ to separate folders
ps.dir =  '",ps.dir,"' # the folder in which this file is stored
ps.file = '", ps.file,"' # this file
user.name = '", user.name,"' # your user name


library(RTutor)
check.problem.set('",ps.name,"', ps.dir, ps.file, user.name=user.name, reset=FALSE)
", libs, "
#+ include=TRUE

cat('Name: ', user.name)

")  
  str
}

#' Internal function to generate a problem set skeleton for a student and save it in a file
#' @export
create.stud.ps.rmd = function(ps, file = paste0(ps$stud.path,"/",ps$name,".rmd"), ps.dir="C:/...", user.name = "ENTER A USER NAME HERE", header=NULL,libs = NULL, footer=NULL ) {
  restore.point("create.stud.ps.rmd")
  
  ex.str = lapply(ps$ex, get.empty.ex.code)
  
  
  file = paste0(str.left.of(file,"."),".Rmd")
  head = rmd.ps.header(ps.name=ps$name,ps.dir=ps.dir,header=header, libs=libs, user.name=user.name)
  str = paste0(head,paste0(ex.str,collapse="\n"), footer)
  
  library(knitr)  
  str = spin(text=str,knit=FALSE,format = "Rmd")  
  writeLines(str,file)
  invisible(str)
}


examples.create.sample.solution = function() {
  create.sample.solution(sol.file = "ps_1b1_ols_sol.r")
}

#' Creates a problem set structure file from a solution file
#' @param sol.file file name of the solution file
#' @param ps.name name of the problem set
#' @export
create.sample.solution = function(sol.file, target.file = NULL, ps.name=NULL, user.name="Jane Doe", dir = getwd(), libs=NULL, header="", footer="") {
  restore.point("create.sample.solution")  
  txt = readLines(sol.file)
  if (is.null(ps.name)) {
    ps.name = str.trim(extract.command(txt,"#$ problem_set")[1,2])
  }
  if (is.null(target.file)) {
    target.file = paste0(ps.name,"_sample_solution.Rmd")
  }
  
  row = 0
  empty.txt = "#§§§#"
  ignore = FALSE
  while (row<length(txt)) {
    row = row+1
    str = txt[row]
    ssub = substring(str,1,2)
    if (str.trim(str)=="#s" | str.trim(str)=="#e" |
        str.starts.with(str,"#< task") |
        str.starts.with(str,"#< notest") |
        str.starts.with(str,"#> task") |
        str.starts.with(str,"#> notest") ) {
      
      txt[row] = empty.txt
    } else if (str.starts.with(str,"#<")) {
      txt[row] = empty.txt
      ignore = TRUE
    } else if (str.starts.with(str,"#>")) {
      txt[row] = empty.txt
      ignore = FALSE
    } else if (str.starts.with(str,"#$ exercise")) {
      ex.name = str.trim(str.left.of(str.right.of(str,"#$ exercise"),"#"))
      txt[row] = paste0("#' ## Exercise ", ex.name)
    } else if (str.starts.with(str,"#$")) {
      txt[row] = empty.txt
    } else {
      if (ignore)
        txt[row] = empty.txt
    }
  }
  txt = txt[txt != empty.txt]
  head = rmd.ps.header(ps.name=ps.name,ps.file=target.file,ps.dir=dir,header=header, libs=libs, user.name=user.name)
  str = paste0(head,'\n', paste0(txt, collapse="\n"))

  
  library(knitr)  
  str = spin(text=str,knit=FALSE,format = "Rmd")  
  writeLines(str,paste0(dir,"/",target.file))
  invisible(str)
  
}


#' Internal function to generate a problem set skeleton for a student and save it in a file
#' @export
create.stud.ps.r = function(ps, file = paste0(ps$stud.path,"/",ps$name,".r"), ps.dir="C:/...",user.name = "ENTER A USER NAME HERE", header=NULL, footer=NULL) {
  restore.point("create.stud.ps")
  
  file = paste0(str.left.of(file,"."),".r")

  ex.str = lapply(ps$ex, get.empty.ex.code)
  
  str = paste0("
#' #############################################################
#' # Problemset ", ps$name,"
#' #############################################################

#+ include=FALSE", header,"

# To check your solutions in RStudio save (Ctrl-s) and then source (Ctrl-Shift-s)
# If you check 'Source on Save' in RStudio you just have to save (Ctrl-s)

ps.dir =  '",ps.dir,"' # the folder in which this file is stored
ps.file = '", ps$name,".r' # this file
user.name = '", user.name,"' # your user name


library(RTutor)
check.problem.set('",ps$name,"', ps.dir, ps.file, user.name=user.name, reset=FALSE)

#+ include=TRUE

cat('Name: ', user.name)

",paste0(ex.str,collapse="\n"), footer)
  
  #cat(str)
  writeLines(str,file)
  invisible(str)
}
