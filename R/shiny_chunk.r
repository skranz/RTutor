
# set all information for the initial chunk ui
# when a problem set is started
make.initial.chunk.ui = function(chunk.ind, ps=get.ps()) {
  restore.point("make.initial.chunk.ui")
  chunk.name = ps$cdt$chunk.name[chunk.ind]
  nali = make.chunk.nali(chunk.name) 
  ui = fluidRow(
    uiOutput(nali$chunkUI)   
  )
  ps$cdt$nali[[chunk.ind]] = nali
  ps$cdt$ui[[chunk.ind]] = ui
  invisible(ui)
}

# Set names for chunk widgets
make.chunk.nali = function(chunk.name, chunk.ind=which(ps$cdt$chunk.name==chunk.name), ps = get.ps()) {
  restore.point("make.chunk.nali")
  name = gsub(" ","_",chunk.name,fixed=TRUE)
  name = gsub(".","_",name,fixed=TRUE)
  name = gsub(")","",name,fixed=TRUE)
  name = paste0("chunk_",name)
  
  base.names = c(
    "chunkUI", "editor","console","chunkout",
    "runLineKey","runKey","checkKey","hintKey","helpKey",
    "runLineBtn","runBtn","checkBtn","hintBtn","helpBtn","dataBtn",
    "outputBtn","hideBtn","hideCodeBtn", "restoreBtn", "saveBtn", 
    "editBtn","solutionBtn","alertOut",
    "inputPanel","outputPanel"
  )
  nali = paste0(name,"_",base.names)
  names(nali) =  base.names
  nali = as.list(c(name=name, chunk.name=chunk.name, nali))
  nali$chunk.ind = chunk.ind
  nali
}

set.nali.names = function(x, nali) {
  restore.point("set.nali.names")
  ind = match(names(x), names(nali))
  names(x)[!is.na(ind)] = unlist(nali[ind[!is.na(ind)]])
  x
}

update.chunk.ui = function(chunk.ind, mode=ps$cdt$mode[chunk.ind], ps=get.ps(), session=ps$session) {
  restore.point("update.chunk.ui")
  #browser()
  cat("\nupdate.chunk.ui: ", chunk.ind)
  ps$cdt$mode[chunk.ind] = mode
  ui = get.chunk.ui(chunk.ind, ps=ps)
  nali = ps$cdt$nali[[chunk.ind]]
    
  updateUI(session,nali$chunkUI, ui)
  cat("\nend update.chunk.ui\n")
}

# returns the ui for a chunk based on its current mode 
# mode can be "input", "output", "hidden", "hidden.code"
# or "inactive"
get.chunk.ui = function(chunk.ind, ps=get.ps(),... ) {
  restore.point("make.chunkUI")
  mode = ps$cdt$mode[chunk.ind] 
  if (mode=="input") {
    return(make.chunk.input.ui(chunk.ind=chunk.ind,...))   
  } else if (mode=="output") {
    return(make.chunk.output.ui(chunk.ind=chunk.ind,...))  
  } else if (mode=="hidden") {
    return(make.chunk.hidden.ui(chunk.ind=chunk.ind,...))  
  } else if (mode=="hide.code") {
    return(make.chunk.hide.code.ui(chunk.ind=chunk.ind,...))  
    
  } else if (mode=="inactive") {
    fluidRow(
      HTML("You must first solve the earlier chunks...")   
    )    
  } else  {
    fluidRow(
      HTML("Not shown")   
    )    
  }
}


make.chunk.input.ui = function(chunk.ind, theme="textmate", height=NULL, code.lines=NULL, fontSize=13, console.height=height, ps = get.ps()) {
  restore.point("make.chunk.input.ui")
  
  nali = ps$cdt$nali[[chunk.ind]]
  code = ps$cdt$stud.code[[chunk.ind]]
  
  if (is.null(code.lines))
    code.lines = length(sep.lines(code))+1

  if (is.null(height)) {
    height = max((fontSize * 1.5) * code.lines,30)    
  }
  if (is.null(console.height)) {
    console.code.lines = max(code.lines,10)
    console.height = (fontSize * 1.5) * console.code.lines    
  }
  
  cat(paste0("\n",nali$name, " height = ", height))

  keys = list(runLineKey="Ctrl-Enter", helpKey="F1", runKey="Ctrl-R|Ctrl-Shift-Enter", hintKey="Ctrl-H", checkKey = "Ctrl-Alt-R|Ctrl-T")
  keys = set.nali.names(keys, nali)  

  if (ps$cdt$is.solved[chunk.ind]) {
    label = "was already solved"
  } else {
    label = "not yet solved"
  }
  
  solutionBtn  = NULL
  if (isTRUE(ps$show.solution.btn)) {
    solutionBtn=bsActionButton(nali$solutionBtn, "solution",size="mini")
  } else {
    solutionBtn  = NULL
  }

  button.row = fluidRow(
#     bsButtonGroup(nali$inoutBtnGroup, toggle="radio",value="input",
#        bsActionButton(nali$outputBtn, "output", size="mini"),
#        bsActionButton(nali$inputBtn, "input",size="mini"),
#        bsActionButton(nali$hideBtn, "hide",size="mini")    
#     ),              
    bsActionButton(nali$outputBtn, "output", size="mini"),
    bsActionButton(nali$checkBtn, "check",size="mini"),
    bsActionButton(nali$hintBtn, "hint", size="mini"),
    bsActionButton(nali$runBtn, "run chunk",size="mini"),
    bsActionButton(nali$dataBtn, "data", size="mini"),
    #bsActionButton(nali$restoreBtn, "restore", size="mini"),
    solutionBtn,
    bsActionButton(nali$saveBtn, "save", size="mini")
  )
  edit.row = fluidRow(
    aceEditor(nali$editor, code, mode="r",theme=theme, height=height, fontSize=13,hotkeys = keys, wordWrap=TRUE, debounce=10),
    aceEditor(nali$console, "", mode="r",theme="clouds", height=console.height, fontSize=13,hotkeys = NULL, wordWrap=TRUE, debounce=10, showLineNumbers=FALSE,highlightActiveLine=FALSE)
  )
  
  #aceAutocomplete(nali$editor)
  award.ui = make.chunk.award.ui(chunk.ind)

  fluidRow(
    button.row,
    bsAlert(nali$alertOut),
    edit.row,
    award.ui
  )
}

make.chunk.output.ui = function(chunk.ind, ps = get.ps()) {
  restore.point("make.chunk.output.ui")
  
  nali = ps$cdt$nali[[chunk.ind]]
  code = ps$cdt$stud.code[[chunk.ind]]
  
  button.row = fluidRow(
    bsActionButton(nali$editBtn, "edit",size="mini"),
    bsActionButton(nali$hideBtn, "hide",size="mini"),
    bsActionButton(nali$hideCodeBtn, "hide code",size="mini"),    
    bsActionButton(nali$dataBtn, "data", size="mini"),
    bsActionButton(nali$saveBtn, "save", size="mini")

  ) 
  is.solved = ps$cdt$is.solved[[chunk.ind]]
  mode = ps$cdt$mode[[chunk.ind]]
  cat("\nbefore if (is.solved) {")
  cat("code:\n", code)
  if (is.solved) {
    code = code
    html = chunk.to.html(code, chunk.ind, nali=nali)
  } else {
    html = chunk.to.html(code, chunk.ind, eval=FALSE, nali=nali)
  }
  html = HTML(html)
  
  cat("\nbefore fluidRow(")
  award.ui = make.chunk.award.ui(chunk.ind)

  fluidRow(
    button.row,
    bsAlert(nali$alertOut),
    html,
    award.ui
  )
}


make.chunk.hide.code.ui = function(chunk.ind, ps = get.ps()) {
  restore.point("make.chunk.output.ui")
  
  nali = ps$cdt$nali[[chunk.ind]]
  code = ps$cdt$stud.code[[chunk.ind]]  
  button.row = fluidRow(
    bsActionButton(nali$outputBtn, "show code",size="mini"),
    bsActionButton(nali$hideBtn, "hide all",size="mini"),
    bsActionButton(nali$editBtn, "edit",size="mini"),
    bsActionButton(nali$dataBtn, "data", size="mini"),
    bsActionButton(nali$saveBtn, "save", size="mini")
  )
  is.solved = ps$cdt$is.solved[[chunk.ind]]
  if (is.solved) {
    code = code
    html = HTML(chunk.to.html(code, chunk.ind, echo=FALSE, nali=nali))
  } else {
    html = HTML(chunk.to.html(code, chunk.ind, eval=FALSE, echo=TRUE,nali=nali))
  }
  award.ui = make.chunk.award.ui(chunk.ind)

  fluidRow(
    button.row,
    bsAlert(nali$alertOut),
    html,
    award.ui
  )
}


make.chunk.hidden.ui = function(chunk.ind, ps = get.ps()) {
  restore.point("make.chunk.output.ui")
  
  nali = ps$cdt$nali[[chunk.ind]]
  code = ps$cdt$stud.code[[chunk.ind]]  
  button.row = fluidRow(
    bsActionButton(nali$outputBtn, "hidden: show output",size="mini"),
    bsActionButton(nali$editBtn, "edit",size="mini"),
    bsActionButton(nali$dataBtn, "data", size="mini"),
    bsActionButton(nali$saveBtn, "save", size="mini")
  ) 
  award.ui = make.chunk.award.ui(chunk.ind)
  fluidRow(
    button.row,
    bsAlert(nali$alertOut),
    award.ui
  )
}

make.chunk.award.ui = function(chunk.ind, ps=get.ps(), user = get.user()) {
  ck = ps$cdt[chunk.ind,]
  #cdt$award.name
  if (is.na(ck$award.name)) return(NULL)
  html = user$awards[[ck$award.name]]$html
  if (is.null(html)) return(NULL)
  restore.point("make.chunk.award.ui")
  
  collapseId = paste0("collapse_award_",chunk.ind)
  collapsePanelId = paste0("collapse_panel_award_",chunk.ind) 
  ahtml = bsCollapse(open = NULL, id = collapseId,
    bsCollapsePanel(paste0("Award: ",ck$award.name),id=collapsePanelId, HTML(html) )
  )
  
  ahtml
}

make.chunk.task.ui = function(...) {
  make.chunk.output.ui(...)
}


make.chunk.handlers = function(chunk.ind, nali = ps$cdt$nali[[chunk.ind]], ps=get.ps()) {
  
  buttonHandler(nali$runBtn, run.shiny.chunk, chunk.ind=chunk.ind)
  aceHotkeyHandler(nali$runKey, run.shiny.chunk, chunk.ind=chunk.ind)
  
  aceHotkeyHandler(nali$runLineKey, run.line.shiny.chunk, chunk.ind=chunk.ind)
  
  buttonHandler(nali$checkBtn, check.shiny.chunk, chunk.ind=chunk.ind)
  aceHotkeyHandler(nali$checkKey, check.shiny.chunk, chunk.ind=chunk.ind)

  buttonHandler(nali$hintBtn, hint.shiny.chunk, chunk.ind=chunk.ind)
  aceHotkeyHandler(nali$hintKey, hint.shiny.chunk, chunk.ind=chunk.ind)

  #buttonHandler(nali$helpBtn, help.shiny.chunk, chunk.ind=chunk.ind)
  aceHotkeyHandler(nali$helpKey, help.shiny.chunk, chunk.ind=chunk.ind)

  buttonHandler(nali$saveBtn, save.shiny.chunk, chunk.ind=chunk.ind)
  buttonHandler(nali$dataBtn, data.shiny.chunk, chunk.ind=chunk.ind)
  #buttonHandler(nali$restoreBtn, restore.shiny.chunk, chunk.ind=chunk.ind)
  if (isTRUE(ps$show.solution.btn))
    buttonHandler(nali$solutionBtn, solution.shiny.chunk, chunk.ind=chunk.ind)

  
  buttonHandler(nali$outputBtn, output.shiny.chunk, chunk.ind=chunk.ind)
  buttonHandler(nali$editBtn, edit.shiny.chunk, chunk.ind=chunk.ind)
  buttonHandler(nali$hideBtn, hide.shiny.chunk, chunk.ind=chunk.ind)
  buttonHandler(nali$hideCodeBtn, hide.code.shiny.chunk, chunk.ind=chunk.ind)
}

run.shiny.chunk = function(chunk.ind,...,session=ps$session, ps=get.ps()) {
  set.shiny.chunk(chunk.ind)
  envir=ps$stud.env; in.R.console=is.null(ps$nali$console)
  restore.point("run.shiny.chunk")
  
  if (in.R.console) {
    eval.in.console(ps$code, envir=envir)
  } else {
    eval.in.ace.console(ps$code, envir=envir, consoleId=ps$nali$console,session=ps$session)
  }
}

run.line.shiny.chunk = function(chunk.ind, cursor=NULL, selection=NULL,...,session=ps$session,ps=get.ps()) {
  set.shiny.chunk(chunk.ind, cursor=cursor, selection=selection)
  envir=ps$stud.env; in.R.console=is.null(ps$nali$console)
  restore.point("run.line.shiny.chunk")

  if (ps$selection == "") {
    txt = sep.lines(ps$code)
    txt = txt[ps$cursor$row+1]
  } else {
    txt = ps$selection
  }
  if (in.R.console) {
    eval.in.console(txt, envir=envir)
  } else {
    eval.in.ace.console(txt, envir=envir, consoleId=ps$nali$console,session=ps$session)
  }
}

check.shiny.chunk = function(chunk.ind = ps$chunk.ind,...,session=ps$session, ps=get.ps(), internal=FALSE) {
  cat("\n check.shiny.chunk1")
  if (!internal)
    set.shiny.chunk(chunk.ind)
  cat("\n check.shiny.chunk2")
  #browser()
  restore.point("check.shiny.chunk")
  #cat("\n check.shiny.chunk3")

  if (!is.false(ps$catch.errors)) {  
    ret = tryCatch(check.chunk(chunk.ind=chunk.ind),
         error = function(e) {ps$failure.message <- as.character(e)
          return(FALSE)})
  } else {
    ret = check.chunk(chunk.ind=chunk.ind)   
  }
  if (!ret) {
    txt = merge.lines(c(ps$success.log, ps$failure.message,"Press Ctrl-H to get a hint."))
    updateAceEditor(ps$session, ps$nali$console, value=txt, mode="text")
    ps$cdt$is.solved[chunk.ind] = FALSE
  } else {
    #restore.point("success test shiny chunk")
    txt = merge.lines(c("You successfully solved the chunk!",
                        ps$chunk.console.out))
    updateAceEditor(ps$session, ps$nali$console, value=txt,mode="r")
    if (!internal) {
      proceed.with.successfuly.checked.chunk(chunk.ind)
    }
  }
  
  cat("\nend check.shiny.chunk.ui\n")
  return(ret)
}

proceed.with.successfuly.checked.chunk = function(chunk.ind, ps=get.ps()) {
  restore.point("proceed.with.successfuly.checked.chunk")
  
  ps$cdt$is.solved[chunk.ind] = TRUE
  if (is.last.chunk.of.ex(chunk.ind)) {
    ex.ind = ps$cdt$ex.ind[chunk.ind]
    ps$edt$ex.final.env[[ex.ind]] = copy.stud.env(ps$stud.env)
  }
  r.chunk.ui.mode = paste0("r.chunk_",chunk.ind,".ui.mode")
  ps$cdt$mode[[chunk.ind]]="output"
  update.chunk.ui(chunk.ind)

  
  # set the next chunk to edit mode
  if (chunk.ind < NROW(ps$cdt)) {
    if (ps$cdt$ex.ind[chunk.ind] == ps$cdt$ex.ind[chunk.ind+1] &
       !ps$cdt$is.solved[chunk.ind+1]) {
    
      cat("update next chunk...")
      ps$cdt$mode[chunk.ind+1] = "input"
      update.chunk.ui(chunk.ind+1)
    }
  }
  
}


hint.shiny.chunk = function(chunk.ind, ...,session=ps$session, ps=get.ps()) {
  set.shiny.chunk(chunk.ind)
  envir=ps$stud.env; in.R.console=is.null(ps$nali$console)
  restore.point("hint.shiny.chunk")

  check.shiny.chunk(chunk.ind, internal=TRUE)
  txt = tryCatch(merge.lines(capture.output(hint(ps=ps))),
         error = function(e) {merge.linesas.character(e)})
  txt = paste0("Hint: ", txt)
  updateAceEditor(ps$session, ps$nali$console, value=txt, mode="text")
}

help.shiny.chunk = function(chunk.ind, cursor=NULL, selection="",...,session=ps$session, ps=get.ps()) {
  set.shiny.chunk(chunk.ind, cursor=cursor, selection=selection)
  envir=ps$stud.env; in.R.console=is.null(ps$nali$console)
  restore.point("help.shiny.chunk")

  if (ps$selection == "") {
    txt = sep.lines(ps$code)
    txt = txt[ps$cursor$row+1]
    txt = word.at.pos(txt, pos=ps$cursor$column+1)
  } else {
    txt = ps$selection
  }
  isRStudio <- isTRUE(Sys.getenv("RSTUDIO") == "1")
  if (isRStudio) {
    str = paste0("To show help for '", txt,"' use the help in your RStudio window." )
    updateAceEditor(ps$session, ps$nali$console, value=str, mode="text")   
    browser.help(topic=txt)

  } else {
    str = paste0("The help for '", txt,"' is shown in a new browser tab." )
    updateAceEditor(ps$session, ps$nali$console, value=str, mode="text") 
    help(topic=txt, help_type="html")
  }

  #browser()
  #help(topic=txt, help_type="html")
  cat("help.shiny.chunk: ",txt)
}

restore.shiny.chunk = function(chunk.ind=ps$chunk.ind,...,session=ps$session,ps=get.ps()) {
  restore.point("restore.shiny.chunk")
  set.shiny.chunk(chunk.ind)

  ps$cdt$stud.code[[chunk.ind]] = ps$cdt$task.txt[[chunk.ind]]
  ps$cdt$is.solved[[chunk.ind]] = FALSE
  ps$stud.code = ps$cdt$stud.code[[chunk.ind]]
  
  updateAceEditor(ps$session, ps$nali$editor, value=ps$stud.code, mode="r")
  updateAceEditor(ps$session, ps$nali$console, value="restored original task code...", mode="text")
}
 

solution.shiny.chunk = function(chunk.ind=ps$chunk.ind,...,session=ps$session,ps=get.ps()) {
  restore.point("restore.shiny.chunk")
  set.shiny.chunk(chunk.ind)

  ps$cdt$stud.code[[chunk.ind]] = ps$cdt$sol.txt[[chunk.ind]]
  #ps$cdt$is.solved[[chunk.ind]] = FALSE
  ps$stud.code = ps$cdt$stud.code[[chunk.ind]]
  
  updateAceEditor(ps$session, ps$nali$editor, value=ps$stud.code, mode="r")
  updateAceEditor(ps$session, ps$nali$console, value="Sample solution shown", mode="text")
}
 

hide.shiny.chunk = function(chunk.ind, ...,session=ps$session, ps=get.ps()) {
  restore.point("hide.shiny.chunk")
  set.shiny.chunk(chunk.ind)
  update.chunk.ui(chunk.ind, mode="hidden")
}
 
hide.code.shiny.chunk = function(chunk.ind, ...,session=ps$session, ps=get.ps()) {
  restore.point("hide.code.shiny.chunk")
  set.shiny.chunk(chunk.ind)
  update.chunk.ui(chunk.ind, mode="hide.code")
}

output.shiny.chunk = function(chunk.ind, ...,session=ps$session, ps=get.ps()) {
  restore.point("hide.shiny.chunk")
  set.shiny.chunk(chunk.ind)
  update.chunk.ui(chunk.ind, mode="output")
}


edit.shiny.chunk = function(chunk.ind, ...,session=ps$session, ps=get.ps()) {
  restore.point("edit.shiny.chunk")
  #browser()
  if (can.chunk.be.edited(chunk.ind)) {
    update.chunk.ui(chunk.ind, mode="input")
    set.shiny.chunk(chunk.ind)
    session = ps$session
  } else {
    nali = ps$cdt$nali[[chunk.ind]]
    createAlert(session,inputId = nali$alertOut, 
        title = "Cannot edit chunk", 
        message= ps$failure.message,
        type = "info", append=FALSE
    )
  }
}
    
data.shiny.chunk = function(chunk.ind=ps$chunk.ind,session=ps$session,
                            ...,ps=get.ps()) {
  restore.point("data.shiny.chunk")
  set.shiny.chunk(chunk.ind, from.data.btn = TRUE)
  update.data.explorer.ui()
  updateTabsetPanel(session, inputId="exTabsetPanel",
                    selected = "dataExplorerTabPanel")
}

save.shiny.chunk = function(chunk.ind=ps$chunk.ind,session=ps$session,
                            ...,ps=get.ps()) {
  restore.point("data.shiny.chunk")
  #set.shiny.chunk(chunk.ind)
  save.sav()
  nali = ps$cdt$nali[[chunk.ind]]

  createAlert(session,inputId = nali$alertOut, 
    title = paste0("Saved as ", ps$sav.file), 
    message= "",
    type = "info", append=FALSE
  )
}


set.shiny.chunk = function(chunk.ind=NULL,selection=NULL, cursor=NULL,
                           input=session$input,session=ps$session,
                           ps=get.ps(),reload.env=FALSE, from.data.btn = FALSE) {
  restore.point("set.shiny.chunk")
  #browser()
  cat("start set.shiny.chunk\n")

  ps$selection = selection
  ps$cursor = cursor
  
  nali = ps$cdt$nali[[chunk.ind]]
  
  if (ps$cdt$mode[chunk.ind]=="input") {
    code = paste0(isolate(input[[nali$editor]]), collapse="\n")
    ps$stud.code = ps$code = code
    ps$cdt$stud.code[[chunk.ind]] = code
  }
  ps$session = session
  ps$nali = nali
  # Always reload env if chunk.ind has changed
  if (!reload.env)
      reload.env = !isTRUE(ps$chunk.ind == chunk.ind)  
  
  ps$chunk.ind = chunk.ind
  if (from.data.btn & ps$cdt$mode[chunk.ind]!="input") {
    reload.env = FALSE
    ps$stud.env = ps$cdt$stud.env[[ps$chunk.ind]]
  }
  
  if (reload.env) {
    stud.env = NULL
    if (!is.false(ps$catch.errors)) {  
      tryCatch(
        stud.env <- make.chunk.stud.env(chunk.ind, ps),
        error = function(e) {
          ps$failure.message = paste0(deparse(e), collapse="\n")  
        }
      )
    } else {
      stud.env <- make.chunk.stud.env(chunk.ind, ps)  
    }
    if (!is.null(stud.env)) {
      ps$cdt$stud.env[[ps$chunk.ind]] <- stud.env
    }
    if (is.null(stud.env)) {
      return(FALSE)
    }
    ps$stud.env = ps$cdt$stud.env[[ps$chunk.ind]]
  }
  return(TRUE)
}



update.all.chunk.ui = function(ps=get.ps()) {
  restore.point("update.all.chunks")
  for (chunk.ind in ps$cdt$chunk.ps.ind) {
    update.chunk.ui(chunk.ind, ps=ps)  
  }
} 

