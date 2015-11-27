examples.dyn.collapse = function() {
  app = eventsApp()
  ui = fluidPage(
    dynCollapse(id="collapse", labels=c("Adfndskgdsjgkjdkvndcnvkdfhve djjdbidzgfu sjfjnsdfj","B","C"), panel.fun=function(...) {
      args = list(...)
      restore.point("inner")
      cat("I am pressed!!!!")
      HTML(paste0("I am pressed...", sample.int(10e9,1)))
    })
  )
  app$ui = ui
  runEventsApp(app, launch.browser=rstudioapi::viewer)
  
}


dynCollapse = function(id, labels, values=labels, panel.fun, block=TRUE,...) {
  restore.point("dynCollapse")
  li = lapply(seq_along(values), function(i) {
    btnid = paste0(id,"__btn",i)
    ui.id = paste0(id,"__ui",i)
    btn = bsButton(btnid, label=labels[[i]], block=block, type="toggle")
    #btn$attribs$class = "btn btn-default sbs-toggle-button btn-block"
    btn$attribs$class = "btn-default sbs-toggle-button btn-block panel-heading panel-title text-left"
    
    #btn = shinyBS:::removeClass(btn,"btn")
    #btn = shinyBS:::removeClass(btn,"btn-default")
    #btn = shinyBS:::addClass(btn,"panel-heading")
    #btn = shinyBS:::addClass(btn,"panel-title")
    #btn = shinyBS:::addClass(btn,"btn-default")
    #btn = shinyBS:::addClass(btn,"text-left")

    #btn = as.character(btn)
    changeHandler(btnid, dynCollapse.click,  collapseId=id, collapseValue=values[[i]], substitute.fun=FALSE, panel.fun=panel.fun, ui.id=ui.id,...)
    list(btn, uiOutput(ui.id))
  })
  do.call("c",li)
  #unlist(li)
  
}

dynCollapse.click = function(panel.fun,ui.id,value,...) {
  args = list(...)
  restore.point("dynCollapse.click")
  cat("\nToogle value: ", value)
  if (value) {
    ui = panel.fun(ui.id=ui.id,...)
  } else {
    ui = HTML("")
  }
  setUI(ui.id,ui)
}
