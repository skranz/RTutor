chunk.special.output = function(txt, chunk.ind, output=ps$cdt$chunk.opt[[chunk.ind]]$output, ps = get.ps(), nali=NULL,...) {
  restore.point("chunk.special.output")

  opts = ps$cdt$chunk.opt[[chunk.ind]]
  if (output=="htmlwidget") {
    res = chunk.output.htmlwidget(txt=txt, widget.name=opts$widget, chunk.ind=chunk.ind, nali=nali,...)
    return(res)
  } else {
    stop(paste0("Unknown chunk output ", output, "."))
  }
      
  
}

chunk.output.htmlwidget = function(txt, widget.name,chunk.ind=ps$chunk.ind, widget.id=paste0("chunkHtmlWidget_",ps$cdt$nali[[chunk.ind]]$name), outputFun = NULL, ps = get.ps(), nali=NULL, app=getApp(), width="100%", height="400px",...) {
  restore.point("chunk.output.htmlwidget")

  txt = paste0("{\n", paste0(txt, collapse="\n"),"\n}")
  expr = parse(text=txt)[[1]]
  
  if (is.null(outputFun)) {
    outputFun <- function(outputId, width = "100%", height = "400px",...) {
      htmlwidgets::shinyWidgetOutput(outputId, widget.name, width, height)
    }    
  }
  
  ui = outputFun(widget.id,width=width,height=height)
  stud.env = ps$cdt[["stud.env"]][[chunk.ind]]

  app$output[[widget.id]] = app$session$output[[widget.id]] =  htmlwidgets::shinyRenderWidget(expr=expr, outputFunction=outputFun, env=stud.env, quoted=TRUE)


  #app$session$output[[widget.id]] = shinyRenderWidget(expr=expr, outputFunction=outputFun, env=stud.env, quoted=TRUE)
  
  ui
}
