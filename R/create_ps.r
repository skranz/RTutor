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

#' Generate an problem set for a student in folder dir
#' @export
create.empty.ps = function(ps.name=get.ps()$name, dir = getwd(),user.name = "ENTER A USER NAME HERE", show.install = FALSE) {
  restore.point("create.empty.ps")
  ps = init.problem.set(ps.name,dir, require.stud.file=FALSE)
  create.stud.ps.r(ps,ps.dir=dir, user.name=user.name, show.install=show.install)
  create.stud.ps.rmd(ps,ps.dir=dir, user.name=user.name, show.install=show.install)
  message(paste0("I generated the empty problem set files ", ps$stud.file, " (an .r and a .Rmd version). You can open them in RStudio."))
  
}

# create.empty.ps()


#' Internal function to generate a problem set skeleton for a student and save it in a file
#' @export
create.stud.ps.rmd = function(ps, file = paste0(ps$stud.path,"/",ps$name,".rmd"), ps.dir="C:/...", user.name = "ENTER A USER NAME HERE", show.install = FALSE ) {
  restore.point("create.stud.ps.rmd")
  
  ex.str = lapply(ps$ex, get.empty.ex.code)
  
  file = paste0(str.left.of(file,"."),".Rmd")
  
  install.txt = ""
  if (show.install) {
    install.txt = "

# Remove comments below if you need to install packages
# install.packages('devtools');install.packages('whisker');install.packages('stringr')
# install.packages('RJSONIO');
# library(devtools)
# install_github(repo = 'restorepoint', username = 'skranz')
# install_github(repo = 'RTutor', username = 'skranz')    
"
  }
  
  str = paste0("
#' #############################################################
#' # Problemset ", ps$name,"
#' #############################################################

#+ include=FALSE", install.txt,"

# To check your solutions in RStudio save (Ctrl-S) and then run all chunks (Ctrl-Alt-R)

ps.dir =  '",ps.dir,"' # the folder in which this file is stored
ps.file = '", ps$name,".Rmd' # this file
user.name = '", user.name,"' # your user name


library(RTutor)
check.problem.set('",ps$name,"', ps.dir, ps.file, user.name=user.name, reset=FALSE)

#+ include=TRUE

cat('Name: ', user.name)

",paste0(ex.str,collapse="\n"))
  
  library(knitr)  
  str = spin(text=str,knit=FALSE,format = "Rmd")  
  writeLines(str,file)
  invisible(str)
}


#' Internal function to generate a problem set skeleton for a student and save it in a file
#' @export
create.stud.ps.r = function(ps, file = paste0(ps$stud.path,"/",ps$name,".r"), ps.dir="C:/...",user.name = "ENTER A USER NAME HERE", show.install = FALSE) {
  restore.point("create.stud.ps")
  
  file = paste0(str.left.of(file,"."),".r")

  install.txt = ""
  if (show.install) {
    install.txt = "

# Remove comments below if you need to install packages
# install.packages('devtools');install.packages('whisker');install.packages('stringr')
# install.packages('RJSONIO');
# library(devtools)
# install_github(repo = 'restorepoint', username = 'skranz')
# install_github(repo = 'RTutor', username = 'skranz')    
"
  }

  ex.str = lapply(ps$ex, get.empty.ex.code)
  
  str = paste0("
#' #############################################################
#' # Problemset ", ps$name,"
#' #############################################################

#+ include=FALSE", install.txt,"

# To check your solutions in RStudio save (Ctrl-s) and then source (Ctrl-Shift-s)
# If you check 'Source on Save' in RStudio you just have to save (Ctrl-s)

ps.dir =  '",ps.dir,"' # the folder in which this file is stored
ps.file = '", ps$name,".r' # this file
user.name = '", user.name,"' # your user name


library(RTutor)
check.problem.set('",ps$name,"', ps.dir, ps.file, user.name=user.name, reset=FALSE)

#+ include=TRUE

cat('Name: ', user.name)

",paste0(ex.str,collapse="\n"))
  
  cat(str)
  writeLines(str,file)
  invisible(str)
}
