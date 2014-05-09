

examples.create.struc.from.rmd = function() {
  library(restorepoint)
  setwd("D:/lehre/empIOUlm/rtutor")
  create.struc.from.rmd("test_sol.rmd")

}

#' Creates a problem set structure file from a .rmd solution file
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
  
  i = 1
  ex.struc = sapply(seq_along(ex.txt), function(i) {
    create.ex.struc.from.rmd(ex.names[i], ex.txt[[i]], start.row = rows[i])
  })
  
  file = paste0(ps.name,"_struc.r")  
  txt = paste0('#$ problem_set ', ps.name,'\n\n', paste0(ex.struc, collapse="\n"),
               '\n# create.empty.ps("',ps.name,'")')
  writeLines(txt, file)
  
  remove.ups(ps.name)
  set.ps(NULL)
}

create.ex.struc.from.rmd = function(name, txt, start.row=1) {
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
  te$added.code = FALSE
  
  code.stop = c("#<","#>","#$","#'")
  
  te$in.code = FALSE
  row = 0
  
  
  while (row<length(txt)) {
    row = row+1
    tryCatch(
      te <- inner.create.ex.struc.from.rmd(row=row,txt=txt,te=te),
      error = function(e) {
        str = paste0("in inner.create.ex.struc.from.rmd(...) in ex. ", name, ", row ", row+start.row-1, ".\n",txt[row],"\n",  as.character(e))
        stop(str)
      }
    )
  }
  #add.struc.code(te)
  
  
  # Adapt task.txt to have good empty lines in code
  if (length(te$task.txt)>1) {
    #restore.point("create.ex.struc.rmd.inner")

    s = te$task.txt
    comment.rows = str.trim(s) == "#'"
    next.row.no.comment = c(!comment.rows[-1],FALSE)
    s = s[!(comment.rows & next.row.no.comment)]
    
    empty.line.rows = str.trim(s) == "#'"
    html.rows = str.starts.with(s,"#'")
    part.rows = grepl("^#'[ ]?([a-z]|[ivx]*)\\)",s)
    next.part.rows = c(part.rows[-1],FALSE)
    rows = html.rows & (!empty.line.rows) & next.part.rows
    s[rows] = sc(s[rows],"\n#'")
    te$task.txt = s
  }

  
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


inner.create.ex.struc.from.rmd = function(row,txt, te) {
 restore.point("inner.create.ex.struc.from.rmd")    
 str = txt[row]
 ssub = substring(str,1,2)

 #display("\nrow = ", row,"\n", txt[row], "\n te$last.e = ", deparse1(te$last.e))
 
 if (!te$in.code) {
    if (str.starts.with(str,"```{r")) {
      te$in.code = TRUE
    } else {
      add.struc.html.comment(te,paste0("#' ",str))      
    }
  } else if (te$in.code) {
    if (str.starts.with(str,"```")) {
      te$in.code = FALSE
      add.struc.code(te)
    } else if (str.trim(str)=="#s" | str.trim(str)=="#< task") {
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
      if (str.starts.with(str,"#< task")) {
        stop(paste0("In row ", row," you have the unrecognized command:\n", str))
      }
      
      if (te$block.is.open)
        stop(paste0("In row ", row,"\n",txt[row],"\n You open a block with #<, but you have not closed the block before."))
      if (te$added.code)
        add.struc.code(te)
      #display("row: ", row, " ", txt[row])
      te$block.head = str
      te$block.is.open = TRUE
    } else if (ssub=="#>") {
      if (!te$block.is.open) {
        str = paste0("In row ", row,"\n",txt[row],"\n You close a block with #>, but you have no test or hint block open.")
        if (te$code.to.task)
          str = paste0(str, "\n To close a task block, you must type\n\n#> task\n")
        if (te$notest.notest)
          str = paste0(str, "\n To close a notest block, you must type\n\n#> notest\n")

        stop(str)
      }

      add.struc.block(te)
      te$block.is.open = FALSE
      te$added.code = FALSE
      # Normal code
    } else {
      if (str.trim(str)!="" | te$added.code) {
        te$added.code = TRUE
        te$code.txt = c(te$code.txt, str)
      }
    }
  }
  te
}


#' Creates a problem set structure file from a solution file
#' @param sol.file file name of the solution file
#' @param ps.name name of the problem set
#' @export
create.sample.solution.from.rmd = function(sol.file, target.file = NULL, ps.name=NULL, user.name="Jane Doe", dir = getwd(), libs=NULL, header="", footer="") {
  restore.point("create.sample.solution.from.rmd")  
  txt = readLines(sol.file)
  if (is.null(ps.name)) {
    ps.name = str.trim(extract.command(txt,"# Problemset")[1,2])
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
    } else if (str.starts.with(str,"#$")) {
      txt[row] = empty.txt
    } else if (str.starts.with(str,"# Problemset ")) {
      txt[row] = empty.txt
    } else {
      if (ignore)
        txt[row] = empty.txt
    }
  }
  txt = txt[txt != empty.txt]
  
  # Remove empty code blocks
  code.start = str.starts.with(txt,"```{r")
  code.end = (!code.start) & str.starts.with(txt,"```")
  code.ends.next = c(code.end[-1],FALSE)
  rows = which(code.start & code.ends.next)
  if (length(rows)>0) {
    txt[rows] = ""
    txt = txt[-(rows+1)]
  }
  
  
  head = rmd.rmd.ps.header(ps.name=ps.name,ps.file=target.file,ps.dir=dir,header=header, libs=libs, user.name=user.name)
  str = c(head,txt)
  #str = paste0(head, paste0(txt, collapse="\n"))

  name.rmd.chunks(target.file, txt = str) # also writes file
  
  invisible(str)
  
}


rmd.rmd.ps.header = function(ps.name,ps.dir = "C:/...", ps.file = paste0(ps.name,".Rmd"), header="", libs=NULL, user.name="") {
  if(!is.null(libs))
    libs = paste0("library(",libs,")", collapse=";")

  str = paste0("# Problemset ", ps.name,"

```{r include=FALSE}
",header,"

# To check your solutions in RStudio save (Ctrl-S) and then run all chunks (Ctrl-Alt-R)

# Note: You must use / instead of \\ to separate folders
ps.dir =  '",ps.dir,"' # the folder in which this file is stored
ps.file = '", ps.file,"' # this file
user.name = '", user.name,"' # your user name


library(RTutor)
check.problem.set('",ps.name,"', ps.dir, ps.file, user.name=user.name, reset=FALSE)
", libs, "
```
Name: `r user.name`
")  
  str
}



