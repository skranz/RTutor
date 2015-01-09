make.view.ui = function(view.ind, ps=get.ps()) {
  restore.point("make.view.ui")  
  shiny.dt = ps$shiny.dt
  cdt = ps$cdt

  if (view.ind==1) {
    rows = which(shiny.dt$view.ind == view.ind | shiny.dt$view.ind == 0)
  } else {
    rows = which(shiny.dt$view.ind == view.ind)      
  }
  ui.li = lapply(rows, function(i) {
    if (shiny.dt$type[i]=="chunk") {
      chunk.ind = which(cdt$chunk.name==shiny.dt$chunk.name[i])
      ui=make.initial.chunk.ui(chunk.ind)
      return(ps$cdt$ui[[chunk.ind]])
    } else {
      #return(shiny.dt$html[[i]])

      return(mathJaxRTutor(shiny.dt$html[[i]]))
    }
  })
  #do.call("fluidRow", ui.li)
  w = 12-ps$left.margin-ps$right.margin
  my.ui = do.call("column", c(list(width=w, offset=ps$left.margin),ui.li))
  fluidRow(my.ui)
}

# Make the default ui for each view and add it view.ui.li to ps
make.view.ui.li = function(view.inds = NULL,ps=get.ps()) {
  
  restore.point("make.view.ui.li")  
  shiny.dt = ps$shiny.dt
  if (is.null(view.inds))
    view.inds = setdiff(unique(ps$shiny.dt$view.ind),0)
  
  #make.view.ui(1)
  view.ui.li = lapply(view.inds, make.view.ui) 
  ps$view.ui.li = view.ui.li
  invisible(view.ui.li)  
}

make.ex.ui = function(ex.ind, ps = get.ps(), session=ps$session) {
  restore.point("make.ex.ui")
  shiny.dt = ps$shiny.dt
  cdt = ps$cdt  

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
  
  # Button for next exercise
  if (ex.ind < max(ps$cdt$ex.ind)) {
    btnId = paste0("nextExBtn", ex.ind)
    nextExBtn = actionButton(btnId,"Go to next exercise...")
    li = c(li, list(nextExBtn))    
    buttonHandler(btnId, ex.ind=ex.ind, function(session,ex.ind,...) {
      cat("\nnextExBtn pressed...")
      updateTabsetPanel(session, inputId="exTabsetPanel", selected = paste0("exPanel",ex.ind+1))
    })
  }
  
  do.call("tabPanel", 
    c(list(title=ex.name, value=paste0("exPanel",ex.ind)), li)
  )
}

make.ex.ui.li = function(ex.inds = NULL, ps = get.ps()) {
  restore.point("make.ex.ui.li")
  shiny.dt = ps$shiny.dt
  cdt = ps$cdt 
  edt = ps$edt
  
  if (is.null(ex.inds)) {
    ex.inds = setdiff(unique(edt$ex.ind),0)
    if (!is.null(ps$shiny.ex.inds))
      ex.inds = intersect(ex.inds, ps$shiny.ex.inds)    
  }
 
  ps$ex.ui.li = lapply(ex.inds, make.ex.ui)
  invisible(ps$ex.ui.li)
}


make.rtutor.ui = function(shiny.dt = ps$shiny.dt,cdt=ps$cdt, ps=get.ps()) {
  restore.point("make.rtutor.ui")
  
  view.ui.li = make.view.ui.li(ps=ps)
  ex.ui.li = make.ex.ui.li(ps=ps)
  
  dataExplorerPanel = tabPanel("Data Explorer",value="dataExplorerTabPanel", data.explorer.ui())
  loadSavePanel = tabPanel("File",value="loadSaveTabPanel", load.save.ui())

  
  #doc = fluidRow(column(8, offset=2,
  doc=  do.call("tabsetPanel", c(
      list(id="exTabsetPanel"),ex.ui.li,list(dataExplorerPanel,loadSavePanel)
    ))
  #))
  
#   ret = navbarPage("RTutor", header=
#     tags$head(
#       tags$script(src = 'http://yandex.st/highlightjs/7.3/highlight.min.js', type = 'text/javascript'),
#       tags$script(src = 'http://yandex.st/highlightjs/7.3/languages/r.min.js', type = 'text/javascript'),
#       tags$link(rel = 'stylesheet', type = 'text/css',
#       href = 'http://yandex.st/highlightjs/7.3/styles/github.min.css')
#     ),                   
#     tabPanel(ps$name, mathJaxRTutor(doc))
#   )

  
#   tabset.ui = do.call("tabsetPanel", c(
#     list(id="exTabsetPanel"),ex.ui.li,list(dataExplorerPanel,loadSavePanel)
#   ))
#   
#  

  # WARNING: If highlightjs cannot be loaded, whole problem set
  # fails to work (very hard to detect bug)

  # Link to local highlightjs version
  dir = paste0(system.file('www', package='RTutor'),"/highlightjs")
  addResourcePath('highlightjs', paste0(system.file('www', package='RTutor'),"/highlightjs"))

  ret = navbarPage("RTutor", header=
    tags$head(
      tags$script(src = 'highlightjs/highlight.min.js',
                  type = 'text/javascript'),
      tags$script(src = 'highlightjs/languages/r.min.js',
                  type = 'text/javascript'),
      tags$link(rel = 'stylesheet', type = 'text/css',
                href = 'highlightjs/styles/github.min.css')
    ),
    tabPanel(ps$name, mathJaxRTutor(doc))
  )

  return(ret)
}

# Show a view ui
show.view.ui = function(view.ind, ps = get.ps(), session=ps$session) {
  restore.point("show.view.ui")
  id = paste0("viewUI",view.ind)
  ui = ps$view.ui.li[[view.ind]]
  #browser()
  updateUI(session,id, ui)
}

show.view.ui.of.ex = function(ex.ind, ps = get.ps()) {
  restore.point("show.view.ui.of.ex")
  
  if (ex.ind==1) {
    rows = which(ps$shiny.dt$ex.ind == ex.ind | ps$shiny.dt$ex.ind == 0)
  } else {
    rows = which(ps$shiny.dt$ex.ind == ex.ind)      
  }
  view.inds = setdiff(unique(ps$shiny.dt$view.ind[rows]),0)
  for (view.ind in view.inds)
    show.view.ui(view.ind, ps)
}

show.ex.ui = function(ex.ind, ps=get.ps()) {
  restore.point("show.ex.ui")
  show.view.ui.of.ex(ex.ind)
  chunk.inds = which(ps$cdt$ex.ind == ex.ind)
  for (chunk.ind in chunk.inds) {
    update.chunk.ui(chunk.ind)
  }
}