


examples.show.shiny.ps = function() {
  library(restorepoint)
  set.restore.point.options(display.restore.point=!FALSE)
  #options(shiny.error=browser)
  library(RTutor)
  setwd("D:/libraries/RTutor/examples")
  ps.name = "Example"
  show.shiny.ps(ps.name, launch.browser=TRUE, import.rmd=TRUE)
  
  show.shiny.ps(ps.name, user.name="Seb", load.sav=FALSE, sample.solution=FALSE, import.rmd=TRUE, catch.errors=FALSE)
  
  show.shiny.ps(ps.name, user.name="Seb", load.sav=TRUE)
  show.shiny.ps(ps.name, launch.browser=TRUE)

  windows(title="Plots")

  setwd("D:/libraries/RTutor/examples")
  ps.name = "The Impact of Shrouded Fees 3"
  show.shiny.ps(ps.name, load.sav=FALSE, launch.browser=TRUE, sample.solution=TRUE, is.solved=TRUE)

  show.shiny.ps(ps.name, launch.browser=TRUE)
  
  options(shiny.error=traceback)
  show.shiny.ps(ps.name)
}


init.shiny.ps = function(ps.name,dir=getwd(), user.name="Seb",  sav.file=NULL, load.sav = !is.null(sav.file), ex.inds =NULL, sample.solution=FALSE, is.solved=load.sav, import.rmd=FALSE, rmd.file = paste0(ps.name,"_",user.name,"_export.rmd")) {
  restore.point("init.shiny.ps")
  setwd(dir)
  ps = init.ps(ps.name,dir)
  ps$is.shiny = TRUE
  ps$shiny.ex.inds = ex.inds
  ps$shiny.dt = ps$rps$shiny.dt 
  ps$chunk.ind = 0
  #ps$shiny.dt$code
  
  n = NROW(ps$cdt)
  ps$button.counter = list()
  ps$cdt$nali = replicate(n, list(), simplify=FALSE)
  ps$cdt$ui = replicate(n, list(), simplify=FALSE)
  
  ps$cdt$has.output.observer = rep(FALSE,n)
  ps$cdt$has.input.observer = rep(FALSE,n)
  ps$cdt$has.ui.renderer = rep(FALSE,n)
  ps$cdt$server = replicate(n, expression(), simplify=FALSE)
  
  for (chunk.ind in ps$cdt$chunk.ps.ind) {
    id = paste0("r.chunk_",chunk.ind,".ui.mode")
    ps[[id]] = reactiveValues(counter=0)
    # r.chunk.ui.mode = reactiveValues(counter=0)
  }
  
  
  if (sample.solution & !ps$rps$has.sol) {
    warning("I cannot show the sample solution, since the sample solution was not made available for the problem set.")
    sample.solution = FALSE
  }
  ps$cdt$is.solved = rep(FALSE,n)  

  if (is.null(sav.file)) {
    sav.file = paste0(user.name, "_", ps.name,".sav")
  }
  ps$sav.file = sav.file
  if (load.sav) {
    sav = load.sav(ps$sav.file)
    ps$cdt$mode = sav$mode
    ps$cdt$stud.code = sav$stud.code
    if (is.solved) {
      ps$cdt$is.solved = sav$is.solved  
      rerun.solved.chunks(ps)
    }
  } else {
    ps$cdt$mode = "output"
    ps$cdt$mode[1] = "input"
    if (sample.solution) {
      ps$cdt$stud.code = ps$cdt$sol.txt
      if (is.solved) {
        ps$cdt$is.solved = rep(TRUE,n)   
        rerun.solved.chunks(ps)
        ps$cdt$mode[1] = "output"
      }
    } else if (import.rmd) {
      ps$cdt$stud.code = import.stud.code.from.rmd(rmd.file, ps = ps)
    } else {
      ps$cdt$stud.code = ps$cdt$task.txt
    }
  }
  set.ps(ps)
  
}



#' Run a problem set in the webbroser (or in the viewer pane).
#' 
#' ... contains parameters specified in init.shiny.ps. They are explained here.
#' 
#' @param load.sav shall the last saved be loaded?
#' @param sample.solution shall the sample solution be shown
#' @param is.solved if sample.solution or load.sav shall the correct chunks be automatically run when the problem set is loaded?
#' @param import.rmd shall the solution be imported from the rmd file specificed in the argument rmd.file
#' @param lauch.browser if TRUE (default) show the problem set in the browser. Otherwise it is shown in the RStudio viewer pane
#' @param catch.errors by default TRUE only set FALSE for debugging purposes in order to get a more informative traceback()
show.ps = function(ps.name, user.name="Seb", sav.file=NULL, load.sav = !is.null(sav.file), sample.solution=FALSE, is.solved=load.sav, import.rmd=FALSE, rmd.file = paste0(ps.name,"_",user.name,"_export.rmd"), launch.browser=TRUE, catch.errors = TRUE, ...) {
  library(restorepoint)

  ps = init.shiny.ps(ps.name=ps.name, user.name=user.name,sav.file=sav.file, load.sav=load.sav, sample.solution=sample.solution, import.rmd=import.rmd, rmd.file=rmd.file, ...)
  ps$catch.errors = catch.errors
  
  restore.point("show.shiny.ps")
  n = NROW(ps$cdt)
  
  for (chunk.ind in 1:n) {
    make.initial.chunk.ui(chunk.ind=chunk.ind)  
  }
  
  ui = make.rtutor.ui(ps=ps)
  txt = as.character(ui)  
  server = make.rtutor.server()
  
  #server = function (input, output, session) {}
  if (!isTRUE(launch.browser))
    launch.browser = rstudio::viewer
  
  runApp(list(
    ui = ui,
    server=server)
  ,  launch.browser = launch.browser)
  
}

show.shiny.ps = show.ps

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
    "editBtn","labelBtn","alertOut",
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

make.chunkUI = function(chunk.ind, ps=get.ps(),... ) {
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
    bsActionButton(nali$restoreBtn, "restore", size="mini"),
    bsActionButton(nali$saveBtn, "save", size="mini"),

    bsActionButton(nali$labelBtn, label,size="mini")

  )
  edit.row = fluidRow(
    aceEditor(nali$editor, code, mode="r",theme=theme, height=height, fontSize=13,hotkeys = keys, wordWrap=TRUE, debounce=10),
    aceEditor(nali$console, "", mode="r",theme="clouds", height=console.height, fontSize=13,hotkeys = NULL, wordWrap=TRUE, debounce=10, showLineNumbers=FALSE,highlightActiveLine=FALSE)
  )
  
  fluidRow(
    button.row,
    bsAlert(nali$alertOut),
    edit.row
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
  fluidRow(
    button.row,
    bsAlert(nali$alertOut),
    html
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

  fluidRow(
    button.row,
    bsAlert(nali$alertOut),
    html
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
  fluidRow(
    button.row,
    bsAlert(nali$alertOut)

  )
}



make.chunk.task.ui = function(...) {
  make.chunk.output.ui(...)
}

make.initial.chunk.ui = function(chunk.ind, ps=get.ps()) {
  restore.point("make.initial.chunk.ui")
  chunk.name = ps$cdt$chunk.name[chunk.ind]
  nali = make.chunk.nali(chunk.name) 
  ui = fluidRow(
    uiOutput(nali$chunkUI)   
  )
  ps$cdt$nali[[chunk.ind]] = nali
  ps$cdt$ui[[chunk.ind]] = ui
}

key.observer = function(id, chunk.ind,input,session, shiny.env, r=input[[id]],reload.env=TRUE) {
  #input[[id]]
  set.chunk.ps(chunk.ind, r=r, input=input,session=session, reload.env=reload.env)    
}


create.chunk.observer = function(chunk.ind, env=parent.frame(), ps=get.ps()) {
  mode = ps$cdt$mode[chunk.ind]  
  if (mode == "input") {
    create.chunk.input.observer(chunk.ind, env, ps)
  } else {
    create.chunk.output.observer(chunk.ind, env, ps)    
  }
}


has.counter.increased = function(id, counter, ps=get.ps()) {

  restore.point("has.counter.increased")
  if (isTRUE(counter == 0) | is.null(counter) | isTRUE(counter<=ps$button.counter[[id]])) {
    ps$button.counter[[id]] = counter
    cat("\nno counter increase: ", id, " ",counter)
    return(FALSE)
  }
  ps$button.counter[[id]] = counter
  cat("\ncounter has increased: ", id, " ",counter)
  return(TRUE)  
}

check.trigger = function(chunk.ind, id, ps=get.ps()) {
  restore.point("check.trigger")
  ret = isTRUE(ps$run.observers)
  cat("\n check.trigger " ,id,": ", ret)
  return(ret)  
}

create.chunk.ui.renderer = function(chunk.ind, env = parent.frame, ps = get.ps()) {
  restore.point("create.chunk.ui.renderer")
  if (ps$cdt$has.ui.renderer[chunk.ind]) {
    return()
  }
  nali = ps$cdt$nali[[chunk.ind]]
  li = c(nali, list(is.suspended=TRUE, min.val = 1, 
    r.chunk.ui.mode = as.name(paste0("r.chunk_",chunk.ind,".ui.mode")),
    r.chunk.ui.mode.str = paste0("r.chunk_",chunk.ind,".ui.mode")
  ))
  
  expr = substitute(env=li, expr= {  
   # chunkUI
   #r.chunk.ui.mode = reactiveValues(counter=0)
    
   observe({
      if (has.counter.increased(hideBtn, input[[hideBtn]])) {
        ps$cdt$mode[[chunk.ind]] = "hidden"
        ps$r.chunk.ui.mode$counter=isolate(ps$r.chunk.ui.mode$counter+1)
      }
    }) 
    observe({
      if (has.counter.increased(hideCodeBtn, input[[hideCodeBtn]])) {
        ps$cdt$mode[[chunk.ind]] = "hide.code"
        ps$r.chunk.ui.mode$counter=isolate(ps$r.chunk.ui.mode$counter+1)
      }
    }) 

    observe({
      if (has.counter.increased(editBtn, input[[editBtn]])) {
        ps = get.ps()
        if (can.chunk.be.edited(chunk.ind)) {
          ps$cdt$mode[[chunk.ind]] = "input"
          ps$r.chunk.ui.mode$counter=isolate(ps$r.chunk.ui.mode$counter+1)
        } else {
          #restore.point("nkjgnkjgndhfu")
          createAlert(session,inputId = alertOut, 
              title = "Cannot edit chunk", 
              message= ps$failure.message,
              type = "info", append=FALSE
          )
          #updateAceEditor(ps$session, console, value=ps$failure.message, mode="text")
        }
      }
    }) 
    observe({
      if (has.counter.increased(outputBtn, input[[outputBtn]])) {
        ps = get.ps()
        ps$cdt$mode[[chunk.ind]] = "output"
        ps$r.chunk.ui.mode$counter=isolate(ps$r.chunk.ui.mode$counter+1)
      }
    })
 
    
    output[[chunkUI]] <- renderUI({
      cat("chunk_renderUI")
      
      restore.point(paste0("chunk_renderUI"))
      counter = ps$r.chunk.ui.mode$counter
      ps = get.ps()
      mode = ps$cdt$mode[[chunk.ind]]
      if (counter==0 | 
          has.counter.increased(r.chunk.ui.mode.str,counter)) {
        cat("\nmode is ", mode)
        create.chunk.observer(chunk.ind)
        if (mode=="input") {
          ret = isolate(make.chunk.input.ui(chunk.ind))
        } else if (mode=="output") {
          ret = isolate(make.chunk.output.ui(chunk.ind))
        } else if (mode=="hidden") {
          ret = isolate(make.chunk.hidden.ui(chunk.ind))
        } else if (mode=="hide.code") {
          ret = isolate(make.chunk.hide.code.ui(chunk.ind))
        }

        return(ret)
      }
    })
    
    
    # data button (in input & output ui)
    observe({
      if (has.counter.increased(dataBtn,input$dataBtn)) {
        key.observer(dataBtn, chunk.ind,input,session, shiny.env,r=NA,reload.env=FALSE)
        updateTabsetPanel(session, inputId="exTabsetPanel", selected = "dataExplorerTabPanel")
        r.data.counter$counter <- isolate(r.data.counter$counter+1)
      }
    })
    
    # save button (in input & output ui)
    observe({
      if (has.counter.increased(saveBtn,input$saveBtn)) {
        key.observer(saveBtn, chunk.ind,input,session, shiny.env,r=NA,reload.env=FALSE)
        save.sav()
        createAlert(session,inputId = alertOut, 
          title = paste0("Saved as ", ps$sav.file), 
          message= "",
          type = "info", append=FALSE
        )

      }
    }) 

   
  })
  ps$cdt$has.ui.renderer[chunk.ind] = TRUE
  eval(expr,env)
  
}

create.chunk.input.observer = function(chunk.ind, env=parent.frame(), ps=get.ps()) {
  restore.point("create.chunk.input.observer")
  if (ps$cdt$has.input.observer[chunk.ind]) {
    return()
  }

  nali = ps$cdt$nali[[chunk.ind]]
  li = c(nali, list(is.suspended=TRUE, min.val = 1, r.chunk.ui.mode = as.name(paste0("r.chunk_",chunk.ind,".ui.mode"))))
  
  cat("\ncreate.chunk.input.observer for chunk ", chunk.ind)
  expr = substitute(env=li, expr= {
    
   observe({
      cat("\n early input observer")
      ps = get.ps()
      input$labelBtn
      ps$run.observers = FALSE
    }, priority=1000)

    observe({
      cat("\n late input observer")
      ps = get.ps()
      input$labelBtn
      ps$run.observers = TRUE  
    }, priority=-1000) 
    
    observe({
      input$labelBtn
    }) 
    # Run line
    observe({
      input$runLineKey
      if (check.trigger(chunk.ind, runLineKey)) {
        key.observer(runLineKey, chunk.ind,input,session, shiny.env,reload.env=FALSE) 
        run.shiny.chunk.line()
      }
    })
    
    # Run chunk
    observe({
      input$runKey
      if (check.trigger(chunk.ind, runKey)) {
        key.observer(runKey, chunk.ind,input,session, shiny.env,r=NA,reload.env=TRUE)
        run.shiny.chunk()
       }
    }) 
    observe({    
      if (has.counter.increased(runBtn,input$runBtn)) {
        key.observer(runBtn, chunk.ind,input,session, shiny.env,r=NA,reload.env=TRUE)
        run.shiny.chunk()
      }
    }) 
    # Check chunk
    observe({
      input$checkKey
      if (check.trigger(chunk.ind, checkKey)) {
        key.observer(checkKey, chunk.ind,input,session, shiny.env,r=NA,reload.env=TRUE)
        ret = check.shiny.chunk()
        if (isTRUE(ret)) {
           ps = get.ps()
           ps$cdt$mode[[chunk.ind]]="output"
           ps$r.chunk.ui.mode$counter=isolate(ps$r.chunk.ui.mode$counter+1)
         }
      }      
    }) 
    observe({
      if (has.counter.increased(checkBtn,input$checkBtn)) {
        key.observer(checkBtn, chunk.ind,input,session, shiny.env,r=NA,reload.env=TRUE)
        ret = check.shiny.chunk()
        cat("Test result: ", ret)
         if (isTRUE(ret)) {
           ps = get.ps()
           ps$cdt$mode[[chunk.ind]]="output"
           ps$r.chunk.ui.mode$counter=isolate(ps$r.chunk.ui.mode$counter+1)
           
           # set the next chunk to edit mode
           if (chunk.ind < NROW(ps$cdt)) {
             if (ps$cdt$ex.ind[chunk.ind] == ps$cdt$ex.ind[chunk.ind+1] &
                 !ps$cdt$is.solved[chunk.ind+1]) {
              
                cat("update next chunk...")
                ps$cdt$mode[chunk.ind+1] = "input"
                update.chunk(chunk.ind+1)
             }
           }
         }
      }
    }) 
       
    # Hint
    observe({
      input$hintKey
      if (check.trigger(chunk.ind, hintKey)) {
        key.observer(hintKey, chunk.ind,input,session, shiny.env,r=NA)
        check.shiny.chunk()
        hint.shiny.chunk()
      }
    }) 
    observe({
      if (has.counter.increased(hintBtn,input$hintBtn)) {
        key.observer(hintBtn, chunk.ind,input,session, shiny.env,r=NA)
        check.shiny.chunk()
        hint.shiny.chunk()
      }
    })
   
    # restore original task btn
    observe({    
        if (has.counter.increased(restoreBtn,input$restoreBtn)) {
          key.observer(restoreBtn, chunk.ind,input,session, shiny.env,r=NA,reload.env=TRUE)
          restore.chunk.task()
        }
      }) 

    # Help Key (F1)
    observe({
      input$helpKey
      if (check.trigger(chunk.ind, helpKey)) {
        key.observer(helpKey, chunk.ind,input,session, shiny.env,reload.env=FALSE)
        help.shiny.chunk()
      }
    })
  })
  ps$cdt$has.input.observer[chunk.ind] = TRUE
  eval(expr,env)

  create.chunk.ui.renderer(chunk.ind, env)
}  

create.chunk.output.observer = function(chunk.ind, env=parent.frame(), ps=get.ps()) {
  restore.point("create.chunk.output.observer")
  if (ps$cdt$has.output.observer[chunk.ind]) {
    return()
  }
  nali = ps$cdt$nali[[chunk.ind]]
  li = c(nali, list(is.suspended=FALSE))
  expr = substitute(env=li, expr= { 
    # Currently all code is in create.chunk.ui.renderer...
  })
  #eval(expr,env)
  ps$cdt$has.output.observer[chunk.ind] = TRUE
  
  create.chunk.ui.renderer(chunk.ind, env)
}  


make.rtutor.ui = function(shiny.dt = ps$shiny.dt,cdt=ps$cdt, ps=get.ps()) {
  restore.point("make.rtutor.ui")
  

  ex.inds = setdiff(unique(cdt$ex.ind),0)
  if (!is.null(ps$shiny.ex.inds))
    ex.inds = intersect(ex.inds, ps$shiny.ex.inds)

  view.inds = setdiff(unique(ps$shiny.dt$view.ind),0)
  view.ui.li = lapply(view.inds, function(view.ind) {
    if (view.ind==1) {
      rows = which(shiny.dt$view.ind == view.ind | shiny.dt$view.ind == 0)
    } else {
      rows = which(shiny.dt$view.ind == view.ind)      
    }
    ui.li = lapply(rows, function(i) {
      if (shiny.dt$type[i]=="chunk") {
        chunk.ind = which(cdt$chunk.name==shiny.dt$chunk.name[i])
        #make.initial.chunk.ui(chunk.ind)
        return(ps$cdt$ui[[chunk.ind]])
      } else {
        return(shiny.dt$html[[i]])
      }
    })
    do.call("fluidRow", ui.li)
  })
  ps$view.ui.li = view.ui.li
  
  ex.li = lapply(ex.inds, function(ex.ind) {
    if (ex.ind==1) {
      rows = which(shiny.dt$ex.ind == ex.ind | shiny.dt$ex.ind == 0)
    } else {
      rows = which(shiny.dt$ex.ind == ex.ind)      
    }
    view.inds = unique(shiny.dt$view.ind[rows])
    ex.name = ps$edt$ex.name[ex.ind]
    li = lapply(view.inds, function(view.ind) {
      outputName = paste0("viewUI",view.ind)
      uiOutput(outputName)
    })
    do.call("tabPanel", c(list(title=ex.name, value=ex.ind), li))
  })
  ex.li[[1]]
  #ui.li = do.call("c",ui.li)
  
  dataExplorerPanel = tabPanel("Data Explorer",value="dataExplorerTabPanel", data.explorer.ui())
  loadSavePanel = tabPanel("File",value="loadSaveTabPanel", load.save.ui())
  
  doc = do.call("tabsetPanel", c(ex.li,list(dataExplorerPanel,loadSavePanel), list(id="exTabsetPanel")))
  
  ret = navbarPage("RTutor", header=
    tags$head(
      tags$script(src = 'http://yandex.st/highlightjs/7.3/highlight.min.js', type = 'text/javascript'),
      tags$script(src = 'http://yandex.st/highlightjs/7.3/languages/r.min.js', type = 'text/javascript'),
      tags$link(rel = 'stylesheet', type = 'text/css',
      href = 'http://yandex.st/highlightjs/7.3/styles/github.min.css')
    ),                   
    tabPanel(ps$name, withMathJax(doc))
  )
  
  return(ret)

}

make.rtutor.server = function(cdt=ps$cdt, ps=get.ps()) {
  restore.point("make.rtutor.server")
  nali = unlist(cdt$nali)
  times.obs = rep(0,length(nali))
  names(times.obs) = nali
  shiny.env = new.env()
  shiny.env$times.obs = times.obs
  
  server.base = as.list(quote({
    r.chunk <- reactiveValues(ind = 0, counter=0)
    #r.chunk.correct <- reactiveValues(ind = 0, counter=0)
    r.data.counter <- reactiveValues(counter=0)
    observe({
      cat("\nr.chunk$ind = ", r.chunk$ind)
    })
    
    # We use early and late input observer in check.trigger to avoid 
    # running button and key observers when ui is created
    observe({
      cat("\n early input observer")
      ps = get.ps()
      ps$run.observers = FALSE
    }, priority=1000)

    observe({
      cat("\n late input observer")
      ps = get.ps()
      ps$run.observers = TRUE  
    }, priority=-1000) 
  })[-1])
  
  # Generate the renderUI for all viewUI
  view.inds = setdiff(unique(ps$shiny.dt$view.ind),0)
  # Generate the function body of server from all chunks
  view.ind =1
  view.server.li = lapply(view.inds, function(view.ind) {
    ca = substitute(
      env=list(.view.ind=view.ind, viewUI=paste0("viewUI",view.ind), view.created=as.name(paste0("view.created.",view.ind))),{
      
      load.save.observer()
        
        
      view.created = reactiveValues(counter=0)  
        
      output[[viewUI]] <- renderUI({
        cat("\nviewUI",.view.ind, " renderUI")

        ps = get.ps()
        chunk.inds = setdiff(unique(ps$shiny.dt$chunk.ind[ps$shiny.dt$view.ind==.view.ind]),0)
        view.created$counter = isolate(view.created$counter+1)
        for (chunk.ind in chunk.inds) {
          create.chunk.output.observer(chunk.ind,ps=ps)  
        }
        ps$view.ui.li[[.view.ind]]
      })
      # An observe that creates the view's observers in a delayed fashion
      observe({
        cat("view ",.view.ind, " observer creator")
        if (view.created$counter>0) {
          ps = get.ps()
          chunk.inds = setdiff(unique(ps$shiny.dt$chunk.ind[ps$shiny.dt$view.ind==.view.ind]),0)
          for (chunk.ind in chunk.inds) {
            if (ps$cdt$mode[chunk.ind]=="input") {
              #create.chunk.input.observer(chunk.ind,ps=ps)
            }
          }
        }
      }, priority=-10)
    })
    as.list(ca[-1])
  })
  view.server.li = do.call("c",view.server.li)

  #li = c(server.base,view.server.li, li, data.explorer.server())
  li = c(server.base,view.server.li, data.explorer.server())

  ca = quote({})
  ca[2:(length(li)+1)] = li
  #ca
  server = function(input, output, session) {}
  body(server) <- ca
  server
}



eval.to.string = function(code, envir=parent.frame(), convert=TRUE) {
  restore.point("eval.to.string")
  txt = sep.lines(code)
  li = parse.text.with.source(txt)
  all.str = NULL
  add = function(...) {
    str = paste0(..., collapse="\n")
    all.str <<- paste0(all.str,str, sep="\n")
  }
  i = 1
  for (i in seq_along(li$expr)) {
    add("> ",paste0(li$source[[i]], collapse="\n+ "))
    out = tryCatch(capture.output(eval(li$expr[[i]], envir=envir)),
                   error = function(e) {
                     adapt.console.err.message(as.character(e))
                   })                
    if (length(out)>0) {
      add(out)
    }
  }
  # convert special characters that cause JSON errors when shown in 
  # HTML output or in ace console
  if (convert) {
    all.str = iconv(all.str, "LATIN2", "UTF-8")
    all.str = gsub("[\u0091\u0092]","'",all.str)

  }
  all.str
}

eval.in.ace.console = function(code,envir=parent.frame(), consoleId, session) {
  restore.point("eval.in.ace.console")
  out = eval.to.string(code,envir, convert=TRUE)

  #iconv(out,"UTF-8", "LATIN2")

  
  if (length(out)==0)
    out = ""

  #browser()
  # remove special characters that cause errors in ACE console

  tryCatch(updateAceEditor(session, consoleId, value=out,mode="r"),
           error = function(e) {message(e)}
  )
  cat("\n ace console was successfuly updated!")
}


eval.in.console = function(code, envir=parent.frame()) {
  restore.point("eval.in.console")
  out = eval.to.string(code,envir)
  cat(out)
}

set.chunk.ps = function(chunk.ind=NULL,input,session, ps=get.ps(),reload.env=FALSE,r=NA) {
  restore.point("set.chunk.ps")
  cat("start set.chunk.ps\n")
  if (is.null(r))
    return(FALSE)
  if (!is.list(r)) {
    ps$selection = NULL    
    ps$cursor = NULL
  } else {
    ps$selection = r$selection    
    ps$cursor = r$cursor
  }
  nali = ps$cdt$nali[[chunk.ind]]
  code = paste0(isolate(input[[nali$editor]]), collapse="\n")
  ps$session = session
  ps$stud.code = ps$code = code
  ps$nali = nali
  ps$cdt$stud.code[[chunk.ind]] = code
  # Always reload env if chunk.ind has changed
  if (!reload.env)
      reload.env = !isTRUE(ps$chunk.ind == chunk.ind)  
  
  ps$chunk.ind = chunk.ind
  if (reload.env) {
    stud.env = NULL
    if (!is.false(ps$catch.errors)) {  
      tryCatch(stud.env <- make.chunk.stud.env(chunk.ind, ps))
    } else {
      stud.env <- make.chunk.stud.env(chunk.ind, ps)  
    }
    if (!is.null(stud.env)) {
      ps$cdt$stud.env[[ps$chunk.ind]] <- stud.env
    }
    ps$stud.env = ps$cdt$stud.env[[ps$chunk.ind]]
  }
  return(TRUE)
}

run.shiny.chunk = function(envir=ps$stud.env, in.R.console=is.null(ps$nali$console), ps=get.ps()) {
  #restore.point("run.shiny.chunk")
  if (in.R.console) {
    eval.in.console(ps$code, envir=envir)
  } else {
    eval.in.ace.console(ps$code, envir=envir, consoleId=ps$nali$console,session=ps$session)
  }
}

run.shiny.chunk.line = function(envir=ps$stud.env, in.R.console=is.null(ps$nali$console), ps=get.ps()) {
  restore.point("runLine")
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


help.shiny.chunk = function(ps=get.ps()) {
  restore.point("help.shiny.chunk")
    if (ps$selection == "") {
      txt = sep.lines(ps$code)
      txt = txt[ps$cursor$row+1]
      txt = word.at.pos(txt, pos=ps$cursor$column+1)
    } else {
      txt = ps$selection
    }
    help(topic=txt, help_type="html")
    isRStudio <- isTRUE(Sys.getenv("RSTUDIO") == "1")
    if (isRStudio) {
      str = paste0("To show help for '", txt,"' use the help in your RStudio window." )
      updateAceEditor(ps$session, ps$nali$console, value=str, mode="text")   
    } else {
      str = paste0("The help for '", txt,"' is shown in a new browser tab." )
      updateAceEditor(ps$session, ps$nali$console, value=str, mode="text")   
    }

    #browser()
    #help(topic=txt, help_type="html")
    cat("help.shiny.chunk: ",txt)
}


restore.chunk.task = function(ps=get.ps()) {
  restore.point("restore.chunk.task")
  chunk.ind = ps$chunk.ind
  ps$cdt$stud.code[[chunk.ind]] = ps$cdt$task.txt[[chunk.ind]]
  ps$cdt$is.solved[[chunk.ind]] = FALSE
  ps$stud.code = ps$cdt$stud.code[[chunk.ind]]
  
  updateAceEditor(ps$session, ps$nali$editor, value=ps$stud.code, mode="r")
  updateAceEditor(ps$session, ps$nali$console, value="restored original task code...", mode="text")
}
 

hint.shiny.chunk = function(ps=get.ps()) {
  restore.point("hint.shiny.chunk")
  txt = tryCatch(merge.lines(capture.output(hint(ps=ps))),
         error = function(e) {merge.linesas.character(e)})
  txt = paste0("Hint: ", txt)
  updateAceEditor(ps$session, ps$nali$console, value=txt, mode="text")
}
  
check.shiny.chunk = function(ps=get.ps()) {
  restore.point("check.shiny.chunk")
  chunk.ind = ps$chunk.ind
  
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
    restore.point("success test shiny chunk")
    txt = merge.lines(c("You successfully solved the chunk!",eval.to.string(ps$code, ps$stud.env)))
    updateAceEditor(ps$session, ps$nali$console, value=txt,mode="r")
    ps$cdt$is.solved[chunk.ind] = TRUE
    if (is.last.chunk.of.ex(chunk.ind)) {
      ex.ind = ps$cdt$ex.ind[chunk.ind]
      ps$edt$ex.final.env[[ex.ind]] = copy.stud.env(ps$stud.env)
    }
  }
  return(ret)
}

is.last.chunk.of.ex = function(chunk.ind, ps=get.ps()) {
  ex.ind = ps$cdt$ex.ind[chunk.ind]
  chunk.ind == max(which(ps$cdt$ex.ind==ex.ind))
}

rerun.solved.chunks = function(ps = get.ps()) {
  inds = which(ps$cdt$is.solved)
  ok = TRUE
  for (chunk.ind in inds) {
    cat("\n rerun chunk", chunk.ind)
    ps$chunk.ind = chunk.ind
    ps$stud.env = make.chunk.stud.env(chunk.ind, ps)
    if (is.null(ps$stud.env)) {
      stop(ps$failure.message)
    }
    ps$cdt$stud.env[[chunk.ind]] <- ps$stud.env
    code = ps$cdt$stud.code[[chunk.ind]]
    
    if (!is.false(ps$catch.errors)) {  
      ok = tryCatch({
        out <- eval.to.string(code,ps$stud.env)
        TRUE
      }, error = function(e) {
        message(as.character(e))
        FALSE
      })
    } else {
      out <- eval.to.string(code,ps$stud.env)      
    }
    if (!ok) 
      break
    if (is.last.chunk.of.ex(chunk.ind)) {
      ex.ind = ps$cdt$ex.ind[chunk.ind]
      ps$edt$ex.final.env[[ex.ind]] = copy.stud.env(ps$stud.env)
    }
  }
  # Could not rerun a chunk that was supposed to be solved
  # flag all later chunks as not solved
  if (!ok) {
    inds = which((1:NROW(ps$cdt$is.solved))>=chunk.ind)
    ps$cdt$is.solved[inds] = FALSE
  } 
}
 

chunk.to.html = function(txt, chunk.ind, name=paste0("out_",ps$cdt$nali[[chunk.ind]]$name), ps = get.ps(), eval=TRUE, success.message=isTRUE(ps$cdt$is.solved[[chunk.ind]]), echo=TRUE, nali=NULL) {
  restore.point("chunk.to.html")
  if (is.null(txt))
    return("")
  
  
  if (paste0(txt,collapse="\n") == "")
    txt = "# Press 'edit' to enter your code."
  
  if (success.message) {
    txt = c("# Great, solved correctly!",txt)
  } else {
    txt = c("# Not yet solved...",txt)
    echo = TRUE
  }
  
  opt = default.out.chunk.options()
  copt = ps$cdt$chunk.opt[[chunk.ind]]
  if (length(copt)>0) {
    opt[names(copt)] = copt
  }
  opt$eval = eval
  opt$echo = echo
  
  
  header = paste0("```{r '",name,"'",chunk.opt.list.to.string(opt,TRUE),"}")
  
  
  library(knitr)
  library(markdown)
  txt = c(header,sep.lines(txt),"```")
  
  #stop("stop in chunk.to.html")
  stud.env = ps$cdt$stud.env[[chunk.ind]]
  #all.parent.env(stud.env)
  html = knitr::knit2html(text=txt, envir=stud.env,fragment.only = TRUE)
  
   # Add syntax highlightning
  if (!is.null(nali$chunkUI)) {
    html = paste0(paste0(html,collapse="\n"),"\n",
     "<script>$('#",nali$chunkUI," pre code').each(function(i, e) {hljs.highlightBlock(e)});</script>")
  }

  #file = tempfile(fileext=".html")
  #writeLines(html,file)
  #browseURL(file)
  
  #html= markdownToHTML(text=ktxt)
  html
  #ktxt
}

chunk.opt.list.to.string = function(li, add.comma=!TRUE) {
  if (length(li)==0)
    return("")
  is.char = sapply(li, is.character)
  quotes = ifelse(is.char,"'","")
  str = paste0(names(li),"=",quotes,li,quotes, collapse=", ")
  if (add.comma)
    str = paste0(", ", str)
  str
}
default.out.chunk.options = function() {
  list(fig.width=6.5, fig.height=4.5, fig.align='center', "warning"=FALSE, cache=FALSE, collapse=TRUE, comment=NA)  
}


