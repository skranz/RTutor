

examples.create.struc = function() {
  setwd("D:/libraries/RTutor/problemsets/EmpIO1a")
  create.struc("dplyr_car_sol.r",ps.name="dplyr_car")

  create.empty.ps("dplyr_car", user.name = "seanbasti")
  
  
  setwd("D:/libraries/RTutor/problemsets/intro")
  create.struc("r_intro_sol.r",ps.name="r_intro")
  create.empty.ps("r_intro", user.name = "seanbasti")

  setwd("D:/libraries/RTutor/problemsets/intro")
  create.struc("r_intro_sol.r",ps.name="r_intro")
  create.empty.ps("r_intro", user.name = "seanbasti",footer=zip.submit.footer.txt())

  create.ps("example_sol.r","example")
}



#' Creates a problem set structure file from a solution file
#' @param sol.file file name of the solution file
#' @param ps.name name of the problem set
#' @export
create.struc = function(sol.file, ps.name=NULL) {
  restore.point("create.struc")  

  txt = readLines(sol.file)
  if (is.null(ps.name)) {
    ps.name = str.trim(extract.command(txt,"#$ problem_set")[1,2])
  }
    
  
  ex.rows = extract.command(txt, "#$ exercise ")
  ex.names = str.trim(str.left.of(ex.rows[,2],"#"))
  rows = c(ex.rows[,1], length(txt)+1)
  ex.txt = lapply(1:(length(rows)-1), function(i) txt[(rows[i]+1):(rows[i+1]-1)])
  
  ex.struc = sapply(seq_along(ex.txt), function(i) {
    create.ex.struc(ex.names[i], ex.txt[[i]])
  })
  
  file = paste0(ps.name,"_struc.r")  
  txt = paste0('#$ problem_set ', ps.name,'\n\n', paste0(ex.struc, collapse="\n"),
               '\n# create.empty.ps("',ps.name,'")')
  writeLines(txt, file)
  
  remove.ups(ps.name)
  set.ps(NULL)
}

# remove old ups files when new problem set structure is generated 
remove.ups = function(ps.name = get.ps()$name) {
  set.ups(NULL)

  files = list.files()
  files = files[str.ends.with(files,paste0("_",ps.name,".ups"))]
  if (length(files)>0) {
    file.remove(files)
  }
  set.ups(NULL)
}

create.ex.struc = function(name, txt) {
  restore.point("create.ex.struc")

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


add.struc.block = function(te) {
  restore.point("add.struc.block")

  type = str.trim(str.right.of(te$block.head,"#<"))
  # Add test code
  if (type == "") {
    te$test.txt = c(te$test.txt,te$code.txt)
  } else if (type == "test") {
     test.txt = paste0(te$code.txt, collapse="\n")
     te$test.txt[length(te$test.txt)] <- test.txt
     # Remove default hint for manual tests
     te$hint.txt[length(te$hint.txt)] <- ""
  } else if (type == "test.arg") {    
     test.txt = test.code.for.e(te$last.e, part=te$part, counter=te$counter, extra.arg = paste0(te$code.txt,collapse="\n"))  
     te$test.txt[length(te$test.txt)] <- test.txt
  } else if (type == "hint") {
     hint.name = hint.name.for.e(te$last.e, counter=te$counter)  
     hint.txt = paste0("add.hint('",hint.name,"',", 
      "{\n",  paste0(te$code.txt, collapse="\n"),"\n})"
     ,collapse="\n")
    te$hint.txt[length(te$hint.txt)] <- hint.txt
  } else if (str.starts.with(type,"hint")) { # hint with a name
    hint.name = str.trim(str.right.of(type,"hint "))  
    hint.txt = paste0("add.hint('",hint.name,"',", 
      "{\n",  paste0(te$code.txt, collapse="\n"),"\n})"
     ,collapse="\n")
    
    te$hint.txt[length(te$hint.txt)] <- paste0(te$hint.txt[length(te$hint.txt)],"\n",hint.txt)
    
  } else if (type == "add to hint") {
    hint.txt = hint.code.for.e(te$last.e, part=te$part, counter=te$counter, extra.code = te$code.txt)  
    te$hint.txt[length(te$hint.txt)] <- hint.txt
  } else if (type == "settings") {
    te$settings.txt = c(te$settings.txt,te$code.txt)    
  } else {
    display("unknown block.head: ", te$block.head)
  }
  
  te$code.txt = NULL
  te$block.head = NULL
}

add.struc.html.comment = function(te, str) {
  restore.point("add.struc.html.comment")
  part.rows = which(grepl("#'[ ]?([a-z]|[ivx]*)\\)",str))
  if (length(part.rows)>0)
    te$part = str.right.of(str.left.of(str,")"),"#' ")
  te$task.txt = c(te$task.txt, str)
  #te$sol.txt = c(te$task.text, str)
}

add.struc.code = function(te) {
  restore.point("add.struc.code")

  if (te$code.to.task) {
    te$task.txt = c(te$task.txt, te$code.txt)
  }

  if (isTRUE(te$notest)) {
    #te$last.e = NULL     
  } else {
    code.txt = str.trim(te$code.txt)
    code.txt = code.txt[nchar(code.txt)>0]
    
    if (length(code.txt)>0) {
      
      
      e.li = parse(text = te$code.txt, srcfile=NULL)
      
      if (length(e.li)>0) {
        test.txt = sapply(seq_along(e.li), function(i) test.code.for.e( e.li[[i]], part=te$part, counter=te$counter+i))   
        hint.txt = sapply(seq_along(e.li), function(i) hint.code.for.e( e.li[[i]], part=te$part, counter=te$counter+i))   
      
        te$counter = te$counter+length(e.li)
        te$test.txt = c(te$test.txt,test.txt)
        te$hint.txt = c(te$hint.txt,hint.txt)
        te$last.e = e.li[[length(e.li)]]
        enter.code.str =  "\n# enter your code here ...\n"
        if (!te$code.to.task & 
            !identical(te$task.txt[length(te$task.txt)], enter.code.str)) {
          te$task.txt = c(te$task.txt, enter.code.str)
        }
      }
    # Empty code.txt
    } else {
      te$last.e = NULL    
    }
  }  
  
  te$code.txt = NULL
} 

examples.test.code.for.e = function() {
  f = function(e) {
    e = substitute(e)
    test.code.for.e(e)
  }
  
  f(fun <- function(x) {x*x})
}

test.code.for.e = function(e, part="", extra.arg="", counter=0) {
  restore.point("test.code.for.e")
  
  part.str = ifelse(part=="","",paste0(",part='",part,")'"))
  extra.arg = ifelse(extra.arg=="","",paste0(",",extra.arg))
  
  if (is.assignment(e)) {
    var = deparse1(e[[2]],collapse="\n")
    rhs = deparse1(e[[3]],collapse="\n")
    estr = deparse1(e)
    call.name = name.of.call(e[[3]])
    
    if (call.name == "function") {
      restore.point("bfndq645by")
      code=paste0("\ncheck.function(", var, "<-",rhs, part.str,extra.arg,")")    
    } else {
      hint.name = paste0(var, "<- ", substring(rhs,1,10), "...", counter)
      code = paste0(
        paste0("\ncheck.assign(", var, "<- ",rhs,", hint.name = '",hint.name,"'", part.str,extra.arg,")")
      )
    }
  } else {
    estr = short = paste0(deparse(e),collapse="")
    if (nchar(estr)>23) 
      short = paste0(substring(estr,1,23),"...", counter)
    
    code = c(
      paste0("check.call(", estr,", hint.name = 'call ",short,"'",part.str,extra.arg,")")
    )
    
  }
  
  code  
}


hint.name.for.e = function(e, counter=0) {
  if (is.assignment(e)) {
    var = deparse1(e[[2]])
    rhs = deparse1(e[[3]])
    estr = deparse1(e)

    hint.name = paste0(var, "<-", substring(rhs,1,10), "...", counter)
  } else {
    estr = short = paste0(deparse(e),collapse="")
    if (nchar(estr)>23) 
      short = paste0(substring(estr,1,23),"...", counter)
    hint.name = paste0("call ",short)
  }
  hint.name  
}


hint.code.for.e = function(e, sol.env, part="", counter=0, extra.code = NULL) {
  
  part.str = ifelse(part=="","",paste0(",part='",part,")'"))

  if (!is.null(extra.code)) {
    extra.code = paste0("\n  ",paste0(extra.code,collapse="\n  "))
  }
  
  if (is.assignment(e)) {
    var = deparse1(e[[2]])
    rhs = deparse1(e[[3]])
    estr = deparse1(e)
    call.name = name.of.call(e[[3]])
    
    if (call.name == "function") {
      restore.point("jhjfndfnng")
      code=""    
    } else {
      hint.name = paste0(var, "<-", substring(rhs,1,10), "...", counter)
      code = paste0("add.hint('",hint.name,"',", 
        "{\n  hint.for.assign(",var ,"<-",rhs,part.str,")", extra.code,"\n})"
      )
    }
  } else {
    estr = short = paste0(deparse(e),collapse="")
    if (nchar(estr)>23) 
      short = paste0(substring(estr,1,23),"...", counter)
    hint.name = paste0("call ",short)
    code = paste0("add.hint('",hint.name,"',",
      "{\n  hint.for.call(",estr,part.str,")", extra.code,"\n})"
    )
  }
  code  
}

