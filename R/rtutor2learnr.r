example.rtutor2learnr = function() {
  library(RTutor)
  setwd("D:/libraries/RTutor/examples")

  ps.name = "learnr"; sol.file = paste0(ps.name,"_sol.Rmd")
  libs = c("ggplot2") # character vector of all packages you load in the problem set
  #create.ps(sol.file=sol.file, ps.name=ps.name, user.name="Test",libs=libs, stop.when.finished=FALSE,addons = "quiz", keep.fill.in.output.sol = TRUE)
  
  rtutor2learnr(ps.name)
  rmarkdown::run("learnr_learnr.Rmd")
  #rmarkdown::run("learnr_learnr.Rmd", shiny_args = list(launch.browser=rstudio::viewer))
}

rtutor2learnr = function(ps.name, sol.file = paste0(ps.name,"_sol.Rmd"), dest.file = paste0(ps.name,"_learnr.Rmd")) { 
  library(rmdtools)
  sol.file = paste0(ps.name,"_sol.Rmd")
  txt = readLines(sol.file)
  res = rtutor2learnr.txt(txt, title=ps.name)
  writeLines(res, dest.file)
  invisible(res)
}


rtutor2learnr.txt = function(txt, title="RTutor2Learnr", verbose=TRUE)   {
  txt = name.rmd.chunks(txt=txt,only.empty.chunks = TRUE)
  bi = rmdtools::find.rmd.nested(txt)
  bi = add.block.complements(bi,type="text",level=1,parent=0, end = NROW(txt)) %>%
    filter(level==1, type!="ignore")
  
  ignore.blocks = c("hint","add_to_hint","test","test_arg","notest")
  for (ib in ignore.blocks) {
    rows = which(bi$type == "hint")
    if (length(rows)>0) {
      bi = bi[-rows,]
      if (verbose) {
        cat(paste0("\nIgnore ", length(rows), " ", ib, " blocks in conversion from RTutor to learnr. Automatic conversion for this block type is not yet implemented.\n"))
      }
    }
  }
  
  bi$text = unlist(lapply(seq_len(NROW(bi)), function(row) {
    merge.lines(txt[bi$start[row]:bi$end[row]])    
  }))

  
  rows = which(bi$form == "chunk" & bi$level == 1)
  
  bi$text[rows] = unlist(lapply(rows, function(row) {
    rtutor2learnr.chunk(txt[bi$start[row]:bi$end[row]])    
  }))

  res = merge.lines(c(learnr.header(title),bi$text))
  res
}


learnr.header = function(title = "RTutorLearnr",rps=NULL) {
txt = paste0('
---
title: "', title, '"
output: learnr::tutorial
runtime: shiny_prerendered
---

```{r setup, include=FALSE}
library(learnr)
library(RTutor)
knitr::opts_chunk$set(echo = FALSE)
tutorial_options(exercise.checker = rt.exercise.checker)
```
')
}

rtutor2learnr.chunk = function(txt) {
  restore.point("rtutor2learnr.chunk")
  
  args = rmdtools::parse.chunk.args(txt[1])
  body = txt[-c(1, length(txt))]
  cbi = find.rmd.blocks(body)
  sol.pos = pos.complement(cbi, is.sorted=TRUE, end=NROW(body))
  
  if (length(sol.pos)>0) {
    sol.bi = tibble(start = sol.pos[,1], end=sol.pos[,2], type="sol", arg.str="") %>% filter(start<=end)
    cbi = bind_rows(cbi, sol.bi) %>% arrange(start)
  }
  task.rows = which(cbi$type %in% c("task","task_notest", "fill_in"))
  task.txt = merge.lines(unlist(lapply(task.rows,  function(row) {
    body[(cbi$start[row]+1):(cbi$end[row]-1)]
  })))
  
  res = paste0('```{r "', args$label, '", exercise=TRUE, eval=FALSE}\n',task.txt,"\n```")
  
  sol.rows = which(cbi$type %in% c("task","sol"))
  if (length(sol.rows)>0) {
    sol.txt = merge.lines(unlist(lapply(sol.rows,  function(row) {
      type = cbi$type[row]
      body[(cbi$start[row]+(1*(type!="sol"))):(cbi$end[row]-(1*(type!="sol")))]
    })))
    
    check.txt = merge.lines(make.learnr.checks.txt(sol.txt)$check)
    
    res = paste0(res,'\n\n```{r "', args$label, '-solution", eval=FALSE}\n',sol.txt,"\n```")
    
    res = paste0(res,'\n\n```{r "', args$label, '-check", eval=FALSE}\n',check.txt,"\n```")
    
  }
  res
  
}

make.learnr.checks.txt = function(code.txt, prefix = "rt.") {
  restore.point("make.learnr.checks.txt")
  test.txt = hint.txt = NULL
  code.txt = code.txt[nchar(code.txt)>0]

  ret = tryCatch(parse.text.with.source(code.txt),
    error = function(e) {
      e.str = paste0(as.character(e), collapse="\n")
      str = paste0(" when parsing your code\n",code.txt,"\n",str.right.of(e.str,":"))
      stop(str, call.=FALSE)
  })
  e.li = ret$expr
  e.source.li = ret$source

  if (length(e.li)>0) {
    test.txt = sapply(seq_along(e.li), function(i) paste0(prefix,test.code.for.e( e.li[[i]])))
    hint.txt = sapply(seq_along(e.li), function(i)  paste0(prefix,hint.code.for.e( e.li[[i]])))
  }
  return(list(check=test.txt, hint=hint.txt))
}

add.block.complements = function(blocks,..., end=max(blocks$end), only.level1 = "level" %in% colnames(blocks)) {
  args = list(...)
  restore.point("add.block.complements")
  
  all.blocks = blocks
  if (only.level1) {
    blocks = filter(blocks, level==1)
  }
  
  cpos = pos.complement(blocks, is.sorted=TRUE, end=end)
  if (NROW(cpos)>0) {
    cpos = as.tibble(cpos) %>% setNames(c("start","end")) %>%
      filter(start <= end)
    
    for (col in names(args)) {
      cpos[[col]] = args[[col]]      
    }
    all.blocks = bind_rows(all.blocks, cpos) %>% arrange(start)
  }
  all.blocks
}

rt.exercise.checker = function(label, user_code, check_code, solution_code, envir_result, evaluate_result, ...) {
  args = list(...)
  restore.point("rt.exercise.checker")
  check_calls = parse(text = check_code)
  ps = get.ps()
  if (is.null(ps)) {
    ps = new.env(parent = globalenv())
    set.ps(ps)
  }

  if (identical(ps$label, label) & identical(ps$user_code, user_code)) {
    ps$add.hint = TRUE
  } else {
    ps$add.hint = FALSE
  }
  ps$label = label
  ps$user_code = user_code

  
  
  has.error = FALSE  
  tryCatch(
    ps$stud.expr.li <- base::parse(text=user_code, srcfile=NULL),
    error = function(e) {
      ps$failure.message=paste0("parser error: ",geterrmessage())
    has.error <<- TRUE
    }
  )
  if (has.error) {
    return(list(message = ps$failure.message, correct = FALSE, type="error", location = "append"))    
  }
  
  counter = 0
  success.txt = ""
  for (call in check_calls) {
    ps$stud.env = envir_result
    res = eval(call)
    if (!isTRUE(res)) {
      type = if(isTRUE(ps$add.hint)) "info" else "warning"
      failure.html = paste0('<div class="alert alert-',type,'" role="alert">',paste0(c(success.txt,ps$failure.message),collapse="<br>"),'</div>')
      return(list(message = HTML(failure.html), correct = FALSE, type="custom", location = "append"))
    } else {
      success.txt = c(success.txt, ps$success.message)
    }
  }
  list(message = "Great, you solved the chunk correctly!", correct = TRUE, location = "append", type="success")
}

rt.check.assign = function(...) {
  res = check.assign(...)
  restore.point("rt.check.assign")
  if (!res) 
    adapt.learnr.check.by.hint(hint.for.assign,...)
  return(res)
}


rt.check.call = function(...) {
  res = check.call(...)
  restore.point("rt.check.call")
  if (!res) 
    adapt.learnr.check.by.hint(hint.for.call,...)
  return(res)
}


adapt.learnr.check.by.hint = function(hint.fun,...) {
  ps = get.ps()
  add.hint = isTRUE(ps$add.hint)
  if (add.hint) {
    hint.txt = capture.output(hint.fun(...))
    failure.message = paste0("Hint: ", merge.lines(hint.txt,collapse = "<br>"))
  } else {
    failure.message = paste0(ps$failure.message, " To get a hint, submit your solution again without changing your code.")
  }
  add.failure(failure.message)
}


old.rt.check.call = function(call, check.arg.by.value=TRUE, allow.extra.arg=FALSE, ignore.arg=NULL, success.message=NULL, failure.message = NULL,no.command.failure.message = NULL, ok.if.same.val = TRUE,s3.method=NULL,stud.env = ps$stud.env, stud.expr.li = ps$stud.expr.li, verbose=FALSE, noeval=FALSE, ps=get.ps(), add.hint = isTRUE(ps$add.hint),  ...) {

  expr = call = substitute(call)


  # restore.point can lead to error
  restore.point("rt.check.call")

  part.str = ""

  ce = match.call.object(expr, envir=match.call.object.env(), s3.method=s3.method)
  dce = describe.call(call.obj=ce)
  check.na = dce$name

  stud.na = sapply(stud.expr.li,  name.of.call)
  # Filter all student calls that have the same name of call
  stud.expr.li = stud.expr.li[which(stud.na == check.na)]


  # Check if a student call with the same name has the same return value
  if (ok.if.same.val) {
    check.val <- eval(ce, stud.env)
    ok = FALSE
    for (se in stud.expr.li) {
      tryCatch({
        sval <- eval(se,stud.env)
        if (is.same(check.val,sval)) {
          ok <- TRUE
          break
        }
      }, error = function(e){})
    }
    if (ok) {
      success.message = paste0("Great,",part.str," you correctly called the command: ",deparse1(se))
      add.success(success.message)
      return(TRUE)
    }
  }

  stud.expr.li = lapply(stud.expr.li, function(e) match.call.object(e, envir=match.call.object.env(), s3.method=s3.method))


  ret = internal.check.call(ce,dce, stud.expr.li,stud.env, allow.extra.arg=allow.extra.arg, ignore.arg=ignore.arg, check.arg.by.value=check.arg.by.value, noeval=noeval)
  if (ret[[1]]==TRUE) {
     success.message = paste0("Great,",part.str," you correctly called the command: ",ret[[2]])
     add.success(success.message)
     return(TRUE)
  } else {

    if (is.null(failure.message))
      failure.message = paste0("You have not yet entered all correct commands", part.str,".")
    if (add.hint) {
      hint.txt = capture.output(hint.for.call(call.obj=call))
      failure.message = paste0("Hint: ", merge.lines(hint.txt,collapse = "<br>"))
    } else {
      failure.message = paste0(failure.message, " To get a hint, submit your solution again without changing your code.")
    }
    
    add.failure(failure.message)
    return(FALSE)
  }
}