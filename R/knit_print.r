
# We need now to explicitly call registerS3method
# see https://github.com/yihui/knitr/issues/1580
register.knit.print.functions = function(knit.print.opts) {
  for (opt in knit.print.opts) {
    if (!is.null(opt$fun)) {
      for (class in opt$classes) {
        registerS3method("knit_print", class, opt$fun)
      }
    }
  }
}


set.knit.print.opts = function(html.data.frame=TRUE,table.max.rows=25, table.max.cols=NULL, round.digits=8, signif.digits=8, show.col.tooltips = TRUE, print.data.frame.fun = NULL, print.matrix.fun=NULL, env=.GlobalEnv, opts=NULL, data.frame.theme = c("code","html","kable")[1+html.data.frame],...) {
  restore.point("set.knit.print.opts")
  #cat(output)
  if (is.null(opts)) {
    #if (is(output,"try-error")) output="html"
    #if (is.null(output)) output = "html"
    opts = make.knit.print.opts(data.frame.theme=data.frame.theme, ,table.max.rows=table.max.rows, table.max.cols=table.max.cols, round.digits=round.digits, signif.digits=signif.digits, show.col.tooltips = FALSE, print.data.frame.fun = print.data.frame.fun, print.matrix.fun=print.matrix.fun)
  }
  register.knit.print.functions(opts)  
#  for (opt in opts) {
#    fun.names = paste0("knit_print.",opt$classes)
#    if (!is.null(opt$fun)) {
#      for (fun.name in fun.names)
#        env[[fun.name]] = opt$fun
#    }
#  }

}

make.knit.print.opts = function(html.data.frame=TRUE,table.max.rows=25, table.max.cols=NULL, round.digits=8, signif.digits=8, show.col.tooltips = TRUE, print.data.frame.fun = NULL, print.matrix.fun=NULL, data.frame.theme = c("code","html","kable")[1+html.data.frame]) {
  opts = list()
  restore.point("make.knit.print.opts")
  
  #attr(opts,"knit.params") = nlist(html.data.frame,table.max.rows, round.digits, signif.digits, show.col.tooltips)
  
  if (!is.null(print.data.frame.fun)) {
   opts[["data.frame"]] = list(
      fun= print.data.frame.fun,
      classes=c("data.frame")
    )
  } else {
    opts[["data.frame"]] = list(
      fun= function(x, options=NULL, ...) {
        restore.point("ndnfhdubfdbfbfbh")
        
        rtutor.knit_print.data.frame(x,table.max.rows=table.max.rows,table.max.cols=table.max.cols, round.digits=round.digits, signif.digits=signif.digits, show.col.tooltips=show.col.tooltips, options=options, data.frame.theme=data.frame.theme,...)  
      },
      classes=c("data.frame","tbl","tbl_df","grouped_df")
    )
  } 
  if (!is.null(print.matrix.fun)) {
    opts[["matrix"]] = list(
      fun= print.data.frame.fun,
      classes=c("matrix")
    )
  }
  opts
}

make.knit.print.funs = function(knit.print.opts, parent.env = globalenv()) {
  env = new.env(parent=parent.env)
  for (opt in knit.print.opts) {
    fun.names = paste0("knit_print.",opt$classes)
    if (!is.null(opt$fun)) {
      for (fun.name in fun.names)
        env[[fun.name]] = opt$fun
    }
  }
  as.list(env)
}

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
  #restore.point("ndjndhvbrubr")
  
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

rtutor.knit_print.data.frame = function(x, table.max.rows=25, table.max.cols=NULL, round.digits=8, signif.digits=8, data.frame.theme=c("code","html","kable")[1], show.col.tooltips=TRUE, col.tooltips=NULL, options=NULL, ...) {
  restore.point("rtutor.knit_print.data.frame")
  
  if (is.matrix(x))
    x = as.data.frame(x)
  
  # chunk options have precedent over passed arguments
  copy.non.null.fields(dest=environment(), source=options, fields=c("table.max.rows","table.max.cols", "round.digits","signif.digits","data.frame.theme","show.col.tooltips"))
  
  #col.tooltips = NULL
  if (show.col.tooltips & is.null(col.tooltips) & data.frame.theme=="html") {
    var.dt = get.ps()$rps$var.dt
    if (!is.null(var.dt)) {
      vars = colnames(x)
      col.tooltips = get.var.descr.dt(vars=vars, var.dt=var.dt)$descr
      col.tooltips = paste0(vars, ": ", col.tooltips)
      col.tooltips = sapply(col.tooltips,USE.NAMES = FALSE, function(str) {
        paste0(strwrap(str, width=30), collapse="\n")
      })
    }
  }
  
  adapt.data = FALSE
  missing.txt = NULL
  if (!is.null(table.max.rows)) {
    if (NROW(x)>table.max.rows) {
      adapt.data = TRUE
      missing.txt = paste0("... only ", table.max.rows, " of ", NROW(x), " rows")
    }
  }
  if (!is.null(table.max.cols)) {
    if (NCOL(x)>table.max.cols) {
      adapt.data = TRUE
      if (is.null(missing.txt)) {
        missing.txt = paste0("... only ", table.max.cols, " of ", NROW(x), " columns")
      }
      missing.txt = paste0(missing.txt, " and ", table.max.cols, " of ", NCOL(x), " columns")
    }
  }
  if (adapt.data) {
    missing.txt = paste0(missing.txt, " shown ...")
    x = max.size.df(x,table.max.rows, table.max.cols)
    if (data.frame.theme=="html") {
      h1 = RTutor:::html.table(x,round.digits=round.digits, signif.digits=signif.digits, col.tooltips=col.tooltips,...)
      html = c(h1, as.character(p(missing.txt)))
      return(asis_output(html))
    } else if (data.frame.theme=="kable") {
      dat = pretty.df(x,signif.digits = signif.digits, round.digits = round.digits)
      txt = c(knit_print(kable(dat)),"",missing.txt,"")
      return(asis_output(txt))
    } else {
      dat = pretty.df(x,signif.digits = signif.digits, round.digits = round.digits) 
      txt = capture.output(print(dat))
      txt = c(paste0(txt,collapse="\n"),missing.txt)
      return(txt)
    }
  } else {
    if (data.frame.theme=="html") {
      html = RTutor:::html.table(x,round.digits=round.digits, signif.digits=signif.digits, col.tooltips=col.tooltips, ...)
      return(asis_output(html))
    } else if (data.frame.theme == "kable") {
      dat = pretty.df(x,signif.digits = signif.digits, round.digits = round.digits)
      txt = c(knit_print(kable(dat)),"","")
      return(asis_output(txt))
    } else {
      dat = pretty.df(x,signif.digits = signif.digits, round.digits = round.digits) 
      txt = paste0(capture.output(print(dat)), collapse="\n")
      return(txt)
    }
  }

}

format.vals = function(vals, signif.digits=NULL, round.digits=NULL) {
  if (is.numeric(vals)) {
    if (is.null(signif.digits) & is.null(round.digits)) {
      return(vals)
    } else if (!is.null(signif.digits) & is.null(round.digits)) {
      return(signif(vals, signif.digits))
    } else if (is.null(signif.digits) & !is.null(round.digits)) {
      return(round(vals, signif.digits))
    } else {
      return(signif(round(vals, round.digits), signif.digits))
    }
  }
  vals
}

max.size.df = function(x, max.rows=NULL, max.cols=NULL) {
  if (!is.null(max.rows)) {
    if (NROW(x)>max.rows) {
      x = x[1:max.rows,]
    }
  }
  if (!is.null(max.cols)) {
    if (NCOL(x)>max.cols) {
      x = x[,1:max.cols]
    }
  }
  x  
}

pretty.df = function(x, signif.digits=NULL, round.digits=NULL, max.rows=NULL, max.cols=NULL) {
  if (!is.null(max.rows) | ! is.null(max.cols))
    x = max.size.df(x, max.rows, max.cols)
  as.data.frame(lapply(x, format.vals, signif.digits=signif.digits, round.digits=round.digits))
}
