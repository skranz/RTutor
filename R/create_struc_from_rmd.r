

examples.create.struc.from.rmd = function() {
  library(restorepoint)
  setwd("D:/lehre/empIOUlm/rtutor")
  create.struc.from.rmd("test_sol.rmd")

}



#' Creates a problem set structure file from a solution file
#' @param sol.file file name of the solution file
#' @param ps.name name of the problem set
#' @export
create.struc.from.rmd = function(sol.file, ps.name=NULL) {
  restore.point("create.struc.from.rmd")  

  txt = readLines(sol.file)
  if (is.null(ps.name)) {
    ps.name = str.trim(extract.command(txt,"# Problemset")[1,2])
  }
  
  ex.rows = extract.command(txt, "## Exercise ")
  ex.names = str.trim(ex.rows[,2])
  rows = c(ex.rows[,1], length(txt)+1)
  ex.txt = lapply(1:(length(rows)-1), function(i) txt[(rows[i]+1):(rows[i+1]-1)])
  
  ex.struc = sapply(seq_along(ex.txt), function(i) {
    create.ex.struc.from.rmd(ex.names[i], ex.txt[[i]])
  })
  
  file = paste0(ps.name,"_struc.r")  
  txt = paste0('#$ problem_set ', ps.name,'\n\n', paste0(ex.struc, collapse="\n"),
               '\n# create.empty.ps("',ps.name,'")')
  #writeLines(txt, file)
  
  remove.ups(ps.name)
  set.ps(NULL)
}

create.ex.struc.from.rmd = function(name, txt) {
  restore.point("create.ex.struc.from.rmd")

  te = new.env()
  te$sol.txt = NULL
  te$task.txt = NULL
  te$test.txt = NULL
  te$hint.txt = NULL
  te$code.txt = NULL
  te$part = NULL
  te$block.head = NULL
  te$last.e = NULL
  te$code.to.task = FALSE
  te$counter = 0
  te$code.to.task = FALSE;te$task.notest = FALSE; te$notest.notest=FALSE
  te$block.is.open = FALSE;
  
  code.stop = c("#<","#>","#$","#'")
  
  in.code.block = FALSE
  row = 0
  while (row<length(txt)) {
    row = row+1
    str = txt[row]
    ssub = substring(str,1,2)
    if (str.trim(str)=="#s" | str.trim(str)=="#< task") {
      add.struc.code(te)
      te$code.to.task = TRUE
      te$task.notest = FALSE
      te$notest = isTRUE(te$notest.notest)
    } else if (str.trim(str)=="#< task notest") {
      add.struc.code(te)
      te$task.notest = TRUE
      te$code.to.task = TRUE
      te$notest = TRUE
    } else if (str.trim(str)=="#e" | str.trim(str)=="#> task") {
      add.struc.code(te)
      te$code.to.task = FALSE
      te$task.notest = FALSE
      te$notest = isTRUE(te$notest.notest)
    } else if (str.trim(str)=="#< notest") {
      add.struc.code(te)
      te$notest = TRUE
      te$notest.notest = TRUE      
    } else if (str.trim(str)=="#> notest") {
      add.struc.code(te)
      te$notest.notest = FALSE
      te$notest = isTRUE(te$task.notest)
    } else if (ssub=="#'") {
      add.struc.code(te)
      add.struc.html.comment(te,str)
    } else if (ssub=="#<") {
      if (te$block.is.open)
        stop(paste0("In row ", row,"\n",txt[row],"\n You open a block with #<, but you have not closed the block before."))
      add.struc.code(te)
      
      te$block.head = str
      te$block.is.open = TRUE
    } else if (ssub=="#>") {
      add.struc.block(te)
      te$block.is.open = FALSE
      # Normal code
    } else {
      te$code.txt = c(te$code.txt, str)
    }
  }
  add.struc.code(te)

  
  ex.code = paste0('
#$ exercise ', name,' ############################################

#$ settings #######################################################
',paste0(te$settings.txt,collapse="\n"),'
#$ task #######################################################
',paste0(te$task.txt,collapse="\n"),'
#$ solution ###################################################
',paste0(te$sol.txt,collapse="\n"),'
#$ tests ######################################################
',paste0(te$test.txt,collapse="\n"),'
#$ hints ######################################################
',paste0(te$hint.txt,collapse="\n"),'

#$ end_exercise
')
 #cat(ex.code)
 ex.code
}

