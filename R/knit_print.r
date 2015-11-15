add.htmlwidget.as.shiny <- function(x,
   outputId=paste0("htmlwidget_output",sample.int(100000,1)),
   session = getDefaultReactiveDomain(), app=getApp()) 
{
  restore.point("add.htmlwidget.as.shiny")

  widget.name = class(x)[1]
  
  # output function for widget
  outputFun <- function(outputId, width = "100%", height = "400px",...) {
    htmlwidgets::shinyWidgetOutput(outputId, widget.name, width, height)
  }

  # add renderer that simply evaluates the htmlwidget x
  env = new.env(parent=globalenv())
  env$x = x
  
  renderer = htmlwidgets::shinyRenderWidget(quote(x), outputFunction=outputFun, env=env, quoted=TRUE)
  app$output[[outputId]] = renderer 
  if (!is.null(session))
    session$output[[outputId]] = renderer
  
  # create ui
  ui = outputFun(outputId)

  return(ui)
}

# overwrite knit_print for htmlwidget
rtutor.knit_print.htmlwidget = function(x,...) {
  restore.point("rtutor.knit_print.htmlwidget")
    
  ps = get.ps()
  chunk.ind = ps$chunk.ind
  outputId=paste0("chunk_htmlwidget_",ps$cdt$nali[[chunk.ind]]$name)

  ui = add.htmlwidget.as.shiny(x, outputId = outputId)
  ui = add.htmlwidget.as.shiny(x)
  restore.point("ndjndhvbrubr")
  
  # knitr shall output ui
  knit_print.shiny.tag.list(ui)
}

rtutor.knit_print.shiny.tag.list = function (x, ...) 
{
  restore.point("rtutor.knit_print.shiny.tag.list")
  
  x <- htmltools:::tagify(x)
  output <- surroundSingletons(x)
  deps <- resolveDependencies(htmltool:::findDependencies(x))
    content <- htmltool:::takeHeads(output)
    head_content <- htmltool:::doRenderTags(tagList(content$head))
    meta <- if (length(head_content) > 1 || head_content != "") {
        list(structure(head_content, class = "shiny_head"))
    }
    meta <- c(meta, deps)
    knitr::asis_output(HTML(format(content$ui, indent = FALSE)), 
        meta = meta)
}

rtutor.knit_print.data.frame = function(x, inline = FALSE, MAX.ROW=22, ...) {
  if (NROW(x)>MAX.ROW) {
    rows = 1:MAX.ROW
    h1 = RTutor:::html.table(x[rows,],...)
    html = c(h1, as.character(p(paste0("... ", NROW(x)-MAX.ROW, " rows not shown  ..."))))

  } else {
    html = RTutor:::html.table(x,...)
  }
  asis_output(html)
}
