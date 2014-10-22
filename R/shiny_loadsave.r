
load.save.ui = function(ps=get.ps()) {
  restore.point("load.save.ui")
  
  pattern = paste0(".*\\Q_",ps$name,".sav\\E")
  
  files = list.files(pattern=pattern)
  
  # if default sav file does not exist, create the sav file
  if (!ps$sav.file %in% files) {
    save.sav(ps=ps)  
    files = list.files(pattern=pattern)
  }
  
  file.end = paste0("_",ps$name,".sav")
  fluidRow(
    fluidRow(
      bsActionButton("loadBtn","Load"),
      selectizeInput('loadFileInput',"",choices=files,multiple=FALSE, width="80%", selected=ps$sav.file),
      bsAlert("loadSaveAlert")
    ),
    fluidRow(
      bsActionButton("saveAsBtn","Save as"),
      textInput('saveFileInput',"",value=ps$sav.file),
      helpText(paste0('(file name must end with "',file.end,'")')),
      bsAlert("saveAsAlert")
    ),
    fluidRow(
      bsActionButton("exportBtn","Export to Rmd"),
      bsActionButton("importBtn","Import from Rmd"),
      textInput('exportFileInput',"",value=paste0("exported_",get.user()$name,"_",ps$name,".Rmd")),
      helpText(paste0('(file name must end with ".Rmd")')),
      bsAlert("exportAlert")
    )
  )
}

load.save.observer = function(server.env = parent.frame()) {
  expr = substitute(env=list(session = server.env$session), expr= {
    observe({
      if (has.counter.increased("loadBtn",input$loadBtn)) {
        ps = get.ps()
        file = isolate(input$loadFileInput)
        
        
        ok = load.and.set.sav(file, ps=ps)
        if (ok) {
          update.all.chunks()
          createAlert(session,inputId = "loadSaveAlert",
            title = "Successfully loaded.",
            message="",
            type = "success", append=FALSE
          )

        } else {
          createAlert(session,inputId = "loadSaveAlert", 
            title = "Could not load saved solution.",
            message = ps$failure.message,
            type = "warning", append=FALSE
          )          
        }
      }
    }) 
   observe({
      if (has.counter.increased("saveAsBtn",input$saveAsBtn)) {
        
        ps = get.ps()
        file = isolate(input$saveFileInput)
        restore.point("saveAs")

        file.end = paste0("_",ps$name,".sav")
        if (!str.ends.with(file, file.end)) {
          createAlert(session,inputId = "saveAsAlert", 
            title = "Invalid file name",
            message = paste0('Your file name must end with "', file.end,'".'),
            type = "warning", append=FALSE
          )          
        } else {
          ps$sav.file = file
          save.sav(ps=ps)
          
          pattern = paste0(".*\\Q_",ps$name,".sav\\E")
          files = list.files(pattern=pattern)
          updateSelectizeInput(session,'loadFileInput',choices=files,selected=ps$sav.file)
          
          createAlert(session,inputId = "saveAsAlert",
            title = paste0("Saved as ", ps$sav.file),
            message="",
            type = "success", append=FALSE
          )
        }
      }
    }) 

   
    # export button
    observe({
      if (has.counter.increased("exportBtn",input$exportBtn)) {
        rmd.file = isolate(input$exportFileInput)
        export.solution(rmd.file)
        createAlert(session,inputId = "exportAlert", 
          title = paste0("Exported to ", rmd.file), 
          message= "",
          type = "info", append=FALSE
        )
      }
    })
   
    # import button
    observe({
      if (has.counter.increased("importBtn",input$importBtn)) {
        rmd.file = isolate(input$exportFileInput)
        ok = import.from.rmd(rmd.file)
        if (ok) {
          update.all.chunks()
          createAlert(session,inputId = "exportAlert", 
            title = paste0("Imported solution from ", rmd.file), 
            message= "",
            type = "info", append=FALSE
          )
        } else {
          ps = get.ps()
          createAlert(session,inputId = "exportAlert", 
            title = paste0("Import failed"), 
            message= ps$failure.message,
            type = "warning", append=FALSE
          )          
        }
      }
    })  

  })
  eval(expr,server.env)
}


save.sav = function(file=ps$sav.file, user.name=get.user()$name,ps=get.ps(), copy.into.global=TRUE) {
  restore.point("save.sav")
  sav = list(
    ps.name = ps$name,
    user.name = user.name,
    stud.code = ps$cdt$stud.code,
    mode = ps$cdt$mode,
    is.solved = ps$cdt$is.solved
  )
  save(sav, file=file)
  
  copy.into.env(source=ps$stud.env, dest=globalenv())
}

# load a sav file and set the current problem set to it
load.and.set.sav = function(file=ps$sav.file, ps=get.ps()) {
  restore.point("load.and.set.sav")
  sav = load.sav(file, ps)
  # need to check whether the problem set is identical
  
  res = compare.sav.with.ps(sav=sav, ps=ps)
  if (!res$ok) {
    ps$failure.message = res$msg
    return(FALSE)
  }
  
  ps$sav.file = file
  ps$cdt$mode = sav$mode
  ps$cdt$stud.code = sav$stud.code
  ps$cdt$is.solved = sav$is.solved  
  rerun.solved.chunks(ps)
  
  return(TRUE)
}

load.sav = function(file =ps$sav.file, ps=get.ps()) {
  restore.point("load.sav")

  if (is.null(file))
    return(NULL)
  if (!file.exists(file))
    return(NULL)
  load(file)
  return(sav)
}

compare.sav.with.ps = function(sav, ps) {
  
  if (ps$name != sav$ps.name) {
    msg = paste0("Your stored solution is from problem set ", sav$ps.name, " but you are currently working on problem set ", ps$name,".")
    return(list(ok=FALSE,msg=msg))
  }
  return(list(ok=TRUE, msg=NULL))
}


update.chunk = function(chunk.ind, ps = get.ps()) {
  restore.point("update.chunk")
  id = paste0("r.chunk_",chunk.ind,".ui.mode")  
  ps[[id]]$counter=isolate(ps[[id]]$counter+1)
  ps$cdt$has.ui.renderer[chunk.ind] = TRUE
}

update.all.chunks = function(ps=get.ps()) {
  restore.point("update.all.chunks")
  for (chunk.ind in ps$cdt$chunk.ps.ind) {
    update.chunk(chunk.ind, ps)  
  }
} 


export.solution = function(rmd.file =paste0(ps$name,"_",user.name,"_export.rmd"),user.name=get.user()$name, ps=get.ps(), copy.into.global=TRUE,...) {
  restore.point("export.solution")

  export.to.rmd(rmd.file)
  if (copy.into.global)
    copy.into.env(source=ps$stud.env, dest=globalenv())
  return(rmd.file)
}



export.to.rmd = function(rmd.file =paste0(ps$name,"_",user.name,"_export.rmd"),dir = getwd(), ps=get.ps(), user.name=get.user()$name) {
  restore.point("export.to.rmd")
  cdt = ps$cdt
  rps = ps$rps
  
  txt = rps$empty.rmd.txt

  cl = rps$empty.rmd.chunk.lines
  rownames(cl) = cl$chunk.name
  chunks = intersect(cdt$chunk.name, cl$chunk.name)
  
  cl.ind = match(chunks, cl$chunk.name)
  cdt.ind = match(chunks, cdt$chunk.name)
  stud.code = cdt$stud.code[cdt.ind]
  start.lines = cl$start.line[cl.ind]
  clear.lines = do.call("c",lapply(cl.ind, function(i) int.seq(cl$start.line[i]+1,cl$end.line[i]-1)))
  
  txt[rps$empty.rmd.user.name.line] = paste0("user.name = '", user.name,"'")
  txt[rps$empty.rmd.ps.dir.line] = paste0("ps.dir = '", dir,"'")
  txt[rps$empty.rmd.ps.file.line] = paste0("ps.file = '", rmd.file,"'")
  
  txt[start.lines] = paste0(txt[start.lines],"\n",stud.code)
  if (length(clear.lines)>0)
    txt = txt[-clear.lines]
  
  writeLines(txt, rmd.file)

}

import.from.rmd = function(rmd.file, ps = get.ps()) {

  stud.code <- import.stud.code.from.rmd(rmd.file, ps = ps)

  if (length(stud.code) != length(ps$cdt$stud.code)) {
    ps$failure.message=paste0("The file ", rmd.file, " has a different number of chunks than the current problem set.")
    return(FALSE)
  }

  ps$cdt$stud.code = stud.code
  ps$cdt$mode = "output"
  ps$cdt$is.solved = FALSE
  
  return(TRUE)
}

import.stud.code.from.rmd = function(rmd.file, ps = get.ps()) {
  #rmd.file = "Example_Seb_export.rmd"
  restore.point("import.from.rmd")
  txt = readLines(rmd.file)
  
  cl = get.chunk.lines(txt)
  rps = ps$rps
  
  chunks = intersect(ps$cdt$chunk.name, cl$chunk.name)
  cl.ind = match(chunks, cl$chunk.name)
  cdt.ind = match(chunks, ps$cdt$chunk.name)

  import.code = sapply(cl.ind, function(i) {
    paste0(txt[int.seq(cl$start.line[i]+1,cl$end.line[i]-1)], collapse="\n")
  })
  stud.code = ps$cdt$stud.code
  stud.code[cdt.ind] = import.code
  stud.code
}
