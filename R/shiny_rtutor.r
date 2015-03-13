


examples.show.shiny.ps = function() {
  library(restorepoint)
  set.restore.point.options(display.restore.point=TRUE)
  #options(shiny.error=browser)
  library(RTutor)
  setwd("D:/libraries/RTutor/examples")
  ps.name = "Example"
  show.ps(ps.name, launch.browser=TRUE, import.rmd=TRUE)
  
  show.shiny.ps(ps.name, user.name="Seb", load.sav=FALSE, sample.solution=FALSE, import.rmd=TRUE, catch.errors=FALSE)
  
  show.shiny.ps(ps.name, user.name="Seb", load.sav=TRUE)
  show.shiny.ps(ps.name, launch.browser=TRUE)

  windows(title="Plots")

  setwd("D:/libraries/RTutor/examples")
  ps.name = "The Impact of Shrouded Fees 3"
  show.shiny.ps(ps.name, load.sav=FALSE, launch.browser=TRUE, sample.solution=TRUE, run.solved=TRUE)

  show.shiny.ps(ps.name, launch.browser=TRUE)
  
  options(shiny.error=traceback)
  show.shiny.ps(ps.name)
}



#' Run a problem set in the webbroser (or in the viewer pane).
#' 
#' ... contains parameters specified in init.shiny.ps. They are explained here.
#' 
#' @param load.sav shall the last saved be loaded?
#' @param sample.solution shall the sample solution be shown
#' @param run.solved if sample.solution or load.sav shall the correct chunks be automatically run when the problem set is loaded? (Starting the problem set then may take quite a while)
#' @param import.rmd shall the solution be imported from the rmd file specificed in the argument rmd.file
#' @param lauch.browser if TRUE (default) show the problem set in the browser. Otherwise it is shown in the RStudio viewer pane
#' @param catch.errors by default TRUE only set FALSE for debugging purposes in order to get a more informative traceback()
#' @param offline (FALSE or TRUE) Do you have no internet connection. By default it is checked whether RTutor can connect to the MathJax server. If you have no internet connection, you cannot render mathematic formulas. If RTutor wrongly thinks you have an internet connection, while you don't, your chunks may not show at all. If you encounter this problem, set manually offline=TRUE.
#' @param is.solved DEPRECEATED
show.ps = function(ps.name, user.name="Seb", sav.file=NULL, load.sav = !is.null(sav.file), sample.solution=FALSE, run.solved=load.sav, import.rmd=FALSE, rmd.file = paste0(ps.name,"_",user.name,"_export.rmd"), launch.browser=TRUE, catch.errors = TRUE, dir=getwd(), rps.dir=dir, offline=!can.connect.to.MathJax(), left.margin=2, right.margin=2, is.solved, make.web.app=FALSE, make.session.ps=make.web.app, save.nothing=FALSE, show.solution.btn = TRUE, disable.graphics.dev=TRUE, ...) {

  app = eventsApp()
  #browser()
  ps = init.shiny.ps(ps.name=ps.name, user.name=user.name,sav.file=sav.file, 
                     load.sav=load.sav, sample.solution=sample.solution,
                     run.solved = run.solved,import.rmd=import.rmd, rmd.file=rmd.file,
                     dir=dir, rps.dir=rps.dir, save.nothing=save.nothing,
                     show.solution.btn = show.solution.btn,...)
  ps$catch.errors = catch.errors
  ps$offline=offline
  ps$left.margin = left.margin
  ps$right.margin = right.margin
  
  restore.point("show.shiny.ps")

  n = NROW(ps$cdt)
  
  
  ui = make.rtutor.ui()
  ex.inds = 1:NROW(ps$edt)
  for (ex.ind in ex.inds)
    show.ex.ui(ex.ind)
    
  for (chunk.ind in 1:n) {
    make.chunk.handlers(chunk.ind=chunk.ind)
  }
  make.load.save.handlers()
  
  txt = as.character(ui)
  setAppUI(ui, app)
  
  if (make.session.ps) {
    app$initHandler = function(session, input, output,app,...) {
      # make local copy of ps
      ops = get.ps(TRUE)
      ops$running.web.app = TRUE
      ps = copy.ps.for.session(ops)
    
      app$ps = ps
      ps$session = session
      ps$input = input
      ps$output = output
    } 
  } else {
    app$initHandler = function(session, input, output,...) {
      ps = get.ps(TRUE)
      ps$running.web.app = TRUE
      ps$session = session
      ps$input = input
      ps$output = output
    }     
  }
  
  if (make.web.app) {
    return(app)
  }
  
  if (!isTRUE(launch.browser))
    launch.browser = rstudio::viewer
  
  
  if (disable.graphics.dev)
   try(png("NUL"),silent=TRUE)
  
  runEventsApp(app=app,ui=ui,launch.browser=launch.browser, quiet=FALSE)
  
  if (disable.graphics.dev)
    try(dev.off(),silent=TRUE)
}

show.shiny.ps = show.ps


init.shiny.ps = function(ps.name,dir=getwd(), user.name="Seb",  sav.file=NULL, load.sav = !is.null(sav.file), ex.inds =NULL, sample.solution=FALSE, run.solved=load.sav, import.rmd=FALSE, rmd.file = paste0(ps.name,"_",user.name,"_export.rmd"), rps.dir=dir, save.nothing=FALSE, show.solution.btn=TRUE) {
  restore.point("init.shiny.ps")
  setwd(dir)
  ps = init.ps(ps.name,dir=dir, rps.dir=rps.dir, save.nothing=save.nothing)
  ps$is.shiny = TRUE
  ps$show.solution.btn = show.solution.btn

  
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
    if (sample.solution) {
      cat(paste0("Show sample solution instead of saved solution..."))
      load.sav = FALSE      
    } else if (!file.exists(sav.file)) {
      cat(paste0("Cannot find saved solution '", sav.file, "'.\nShow empty problem set..."))
      load.sav = FALSE
    }
  }
  
  if (load.sav) {
    sav = load.sav(ps$sav.file)
    ps$cdt$mode = sav$mode
    ps$cdt$stud.code = sav$stud.code
    if (run.solved) {
      ps$cdt$is.solved = sav$is.solved  
      rerun.solved.chunks(ps)
    }
  } else {
    ps$cdt$mode = "output"
    ps$cdt$mode[1] = "input"
    if (sample.solution) {
      ps$cdt$stud.code = ps$cdt$sol.txt
      if (run.solved) {
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



observe.nextExBtns = function(session, ps=get.ps()) {
  restore.point("observe.nextExBtns")
  cdt = ps$cdt
  ex.inds = setdiff(unique(cdt$ex.ind),0)
  if (!is.null(ps$shiny.ex.inds))
    ex.inds = intersect(ex.inds, ps$shiny.ex.inds)


  ex.ind = 1
  for (ex.ind in setdiff(ex.inds,max(ex.inds))) {
    btn = paste0(paste0("nextExBtn", ex.ind))
    observe({
      cat("observe ", btn)
      if (has.counter.increased(btn, session$input[[btn]])) {
        cat("Go to exercise ",paste0("exPanel",ex.ind+1),"...")
        updateTabsetPanel(session, inputId="exTabsetPanel", selected = paste0("exPanel",ex.ind+1))
      }
    })
  }
}


eval.to.string = function(code, envir=parent.frame(), convert=TRUE) {
  restore.point("eval.to.string")
  txt = sep.lines(code)
  ok = FALSE
  
  all.str = tryCatch({
      li <- parse.text.with.source(txt)
      ok <- TRUE
    },
    error = function(e) {
      as.character(e)
    }
  )

  if (ok) {
    all.str = NULL
    add = function(...) {
      str = paste0(..., collapse="\n")
      all.str <<- paste0(all.str,str, sep="\n")
    }
    i = 1
    for (i in seq_along(li$expr)) {
      source = "Source was not parsed..."
      
      add("> ",paste0(li$source[[i]], collapse="\n+ "))
      out = tryCatch(capture.output(eval(li$expr[[i]], envir=envir)),
                     error = function(e) {
                       adapt.console.err.message(as.character(e))
                     })                
      if (length(out)>0) {
        add(out)
      }
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
  html ="Evaluation error!"
  
  cat("knit2html")
  restore.point("chunk.to.html.knit2html")
  html = try(
    knitr::knit2html(text=txt, envir=stud.env,fragment.only = TRUE),
  )
  
  # correct weird error if a summary is shown
  # the siginifcance levels '***' cause a latex error
  #html = gsub("&#39;","",html)
  #html = gsub("&gt;","",html)
  #html = gsub("|","",html,fixed=TRUE)
  #html = gsub("---","",html,fixed=TRUE)

  #html = gsub(":","",html) 
  
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

# Use local version of MathJax so that problem sets really run offline
mathJaxRTutor <- function(html, ps=get.ps()) {
  restore.point("mathJaxRTutor")
  
  if (isTRUE(ps$offline))
    return(html)
  
  #path =  paste0(system.file('www', package='RTutor'),"/MathJax")
  #if (!file.exists(path))
  path <- '//cdn.mathjax.org/mathjax/latest'
  
  command = paste0(path, '/MathJax.js?config=TeX-AMS-MML_HTMLorMML')
  #path <- 'https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML'
  
  tagList(
  tags$head(
  singleton(tags$script(src = command, type = 'text/javascript'))
  ),
  html,
  tags$script(HTML('MathJax.Hub.Queue(["Typeset", MathJax.Hub]);'))
  )
}

can.connect.to.MathJax = function() {
  library(RCurl)
  url.exists("http://cdn.mathjax.org/mathjax/latest/MathJax.js")
 # url.exists("http://www.mathjax.org/")
}
