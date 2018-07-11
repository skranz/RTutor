CREATE.PS.ENV = new.env()

examples.create.ps = function() {
  library(restorepoint)
  library(stringtools)
  setwd("D:/libraries/RTutor/RTutor/vignettes/problemsets")
  sol.file = "Example2_sol.Rmd"
  create.ps(sol.file = sol.file)

  setwd("D:/libraries/RTutor/examples")
  ps.name = "Example"; sol.file = paste0(ps.name,"_sol.Rmd")
  libs = NULL # character vector of all packages you load in the problem set
  #name.rmd.chunks(sol.file) # set auto chunk names in this file
  create.ps(sol.file=sol.file, ps.name=ps.name, user.name=NULL,libs=libs, whitelist.report = TRUE)
  show.ps(ps.name, load.sav=FALSE,  sample.solution=TRUE, run.solved=FALSE, catch.errors=TRUE, launch.browser=TRUE)
}


#' Generate a problem set from a solution file
#'
#' Generates  .rps file, and .rmd files for empty ps , sample solution
#' and output solution
#'
#' @param sol.file file name of the _sol.rmd file that specifies the problem set
#' @param ps.name the name of the problem set
#' @param user.name can pick a default user.name (will typically not be set)
#' @param sol.user.name the user.name set in the sample solution
#' @param dir the directory in which all files are found and wil be saved to
#' @param libs character vector with names of libraries that will be used by the problem set
#' @param extra.code.file the name of an r file that contains own functions that will be accessible in the problme set
#' @param var.txt.file name of the file that contains variable descriptions (see thee vignette for an explanation of the file format)
#' @param rps.has.sol shall the sample solution be stored in the .rps file. Set this option to FALSE if you use problem sets in courses and don't want to assess students the sample solution easily
#' @param use.memoise shall functions like read.csv be memoised? Data sets then only have to be loaded once. This can make problem sets run faster. Debugging may be more complicated, however.
#' @param memoise.funs character vector of function names that will be memoised when use.memoise = TRUE. By default a list of functions that load data from a file.
#' @param preknit shall sample solution of chunks be knitted when problem set is generated. Default = FALSE
#' @param precomp shall chunk environments be computed from sample solution when problem set is generated? Default = FALSE
#' @param force.noeval shall problem set only be shown in noeval mode? (Used as a security against accidentially forgetting to set noeval=TRUE in show.ps, when showing the problem set in a web app.)
#' @param e.points how many points does the user get per required expression in a chunk (expressions in a task do not count). Default=1
#' @param chunk.points you may also specify fixed points given for solving a chunk that will be added to the points per expression. Default=0
#' @param min.chunk.points minimal points for checking a chunk even if no none-task expression has to be entered. By default=0.5. I feel there may be a higher motivation to continue a problem set if there are may be some free point chunks farther below. Also it feels nice to get points, even if it is just for pressing the check button.
#' 
#'
#' @export
create.ps = function(sol.file, ps.name=NULL, user.name= "ENTER A USER NAME HERE", sol.user.name="Jane Doe", dir = getwd(), header="", footer="", libs=NULL, stop.when.finished=FALSE, extra.code.file = NULL, var.txt.file = NULL, rps.has.sol=TRUE, fragment.only=TRUE, add.enter.code.here=FALSE, add.shiny=TRUE, addons=NULL, whitelist.report=FALSE, wl=rtutor.default.whitelist(),use.memoise=FALSE, memoise.funs = rtutor.default.memoise.funs(), precomp=FALSE, preknit=FALSE, force.noeval=FALSE,  html.data.frame=TRUE,table.max.rows=25, round.digits=8, signif.digits=8, knit.print.opts=make.knit.print.opts(html.data.frame=html.data.frame,table.max.rows=table.max.rows, round.digits=round.digits, signif.digits=signif.digits), e.points = 1, min.chunk.points=0, chunk.points=0) {
  restore.point("create.ps")

  CREATE.PS.ENV$fragment.only = fragment.only
  CREATE.PS.ENV$add.enter.code.here = add.enter.code.here
  Addons = make.addons.list(addons)

  setwd(dir)
  txt = readLines(sol.file, warn=FALSE)
  txt =  name.rmd.chunks(txt=txt)
  txt = mark_utf8(txt)
  
  te = get.empty.te(Addons=Addons)
  te = parse.sol.rmd(txt=txt, te=te)

  te$knit.print.params = nlist(html.data.frame,table.max.rows, round.digits, signif.digits) 
  te$e.points = e.points
  te$chunk.points = chunk.points
  te$min.chunk.points = min.chunk.points
  
  te$items.df = rbindlist(te$items[1:te$num.items])

  if (!is.null(ps.name))
    te$ps.name = ps.name

  write.sample.solution(te=te, header=header,footer=footer,
                        user.name=sol.user.name, ps.dir=dir)


  task.txt = write.empty.ps(te=te,  header=header,footer=footer,
                            user.name=user.name, ps.dir=dir)
  rps = te.to.rps(te=te)

  rps$force.noeval = force.noeval
  
  # Store information about empty problem set in order to easily export
  # an html problem set into it
  task.txt = sep.lines(task.txt)
  
  rmd.header = output.solution.header(rps=rps, te=te)
  rmd.txt = c(rmd.header,sep.lines(te$task.txt))
  rps$empty.rmd.txt = rmd.txt
  rps$empty.rmd.chunk.lines = get.chunk.lines(rmd.txt)
  #rps$empty.rmd.user.name.line = which(str.starts.with(task.txt,"user.name = '"))[1]
  #rps$empty.rmd.ps.dir.line = which(str.starts.with(task.txt,"ps.dir =  '"))[1]
  #rps$empty.rmd.ps.file.line = which(str.starts.with(task.txt,"ps.file = '"))[1]

  if (add.shiny) {
    rps$shiny.dt = make.shiny.dt(rps=rps, txt=task.txt)
    rps$cdt$task.html = create.cdt.task.html(rps$cdt)
  }

  source.rps.extra.code(extra.code.file, rps)
  if (!is.null(var.txt.file)) {
    rps$var.dt = read.var.txt(var.txt.file)
  } else {
    rps$var.dt = NULL
  }
  rps$libs = libs
  rps$has.sol = rps.has.sol
  if (!rps.has.sol) {
    rps$cdt$sol.txt = rep("",NROW(rps$cdt))
  }

  if (whitelist.report) {
    rtutor.whitelist.report(rps=rps, te=te, wl=wl)
  }

  rps$use.memoise = use.memoise
  if (use.memoise)
    rps$memoise.fun.li = memoise.fun.li(memoise.funs)

  if (preknit | precomp) {
    rps = preknit.rps(rps=rps,precomp=precomp, knit.print.opts=knit.print.opts)
  }

  write.output.solution(te=te,rps=rps)

  #rps$knit.print.opts = knit.print.opts
  
  save.rps(rps)
  remove.ups(ps.name=rps$ps.name)
  if (stop.when.finished) {
    stop.without.error("The problem set files have been succefully created.")
  }
}

source.rps.extra.code = function(extra.code.file, rps) {
  restore.point("source.rps.extra.code")
  # Source extra.code
  rps$extra.code.file = extra.code.file
  if (!is.null(extra.code.file)) {
    rps$extra.code.env = new.env()
    source(extra.code.file, local = rps$extra.code.env)
  } else {
    rps$extra.code.env = NULL
  }
}

examples.parse.sol.rmd = function() {
  library(restorepoint)
  library(stringtools)
  setwd("D:/libraries/RTutor/RTutor/vignettes/problemsets")
  sol.file = "Example2_sol.Rmd"
}

save.rps = function(rps,file = paste0(rps$ps.name,".rps")) {
  save(rps,file=file)
}


load.rps = function(ps.name=NULL,file = paste0(ps.name,".rps")) {
  load(file=file)
  rps
}


parse.sol.rmd = function(sol.file=NULL, txt=NULL, te = get.empty.te()) {
  if (is.null(txt))
    txt = readLines(sol.file, warn=FALSE)

  row = 0
  while (row<length(txt)) {
    row = row+1
    te <- parse.sol.line(row=row,txt=txt,te=te)
  }
  te
}


write.sample.solution = function(file = paste0(ps.name,"_sample_solution.Rmd"), sol.txt=te$sol.txt,ps.name=te$ps.name, te,...) {
  restore.point("write.sample.solution")
  sol.txt = include.ps.extra.lines(sol.txt, ps.file=file, ps.name=ps.name,te=te,...)
  writeLines(sol.txt, file, useBytes=TRUE)
}

output.solution.header = function(rps, te, ps.name=te$ps.name) {
  restore.point("output.solution.header")
  libs = paste0("library(", c(rps$libs,"RTutor"),")", collapse="\n")  
  source.txt = if (!is.null(rps$extra.code.file)) paste0('source("',rps$extra.code.file,'")') else ""
  
  knit.print.params =  te$knit.print.params
  knit.print.params$html.data.frame = FALSE
  knit.opts =  paste0(names(knit.print.params), " = ", knit.print.params, collapse=", ")
  header = paste0(
'
---
title: Problem Set ', rps$ps.name,'
output: 
  html_document: 
    keep_md: yes
    toc: yes
---

```{r setup, include=FALSE, echo=FALSE}
# Load libraries and source extra code
',libs,'
',source.txt,'

# options for rendering data frames
RTutor::set.knit.print.opts(html.data.frame=FALSE, table.max.rows=25, round.digits=8, signif.digits=8)

# continue knitting even if there is an error
knitr::opts_chunk$set(error = TRUE) 
```
'
)
  header  
}

write.output.solution = function(file = paste0(ps.name,"_output_solution.Rmd"), out.txt=te$out.txt,ps.name=te$ps.name, te, rps,...) {
  restore.point("write.output.solution")

  header = output.solution.header(rps=rps, te=te, ps.name=ps.name)
  
  out.txt = c(header, out.txt)
  out.txt = name.rmd.chunks(txt = out.txt,only.empty.chunks = FALSE,keep.options = TRUE,valid.file.name = TRUE)
  out.txt = mark_utf8(out.txt)
  writeLines(out.txt, file, useBytes=TRUE)
}


write.empty.ps = function(file = paste0(te$ps.name,".Rmd"), task.txt=te$task.txt,ps.name=te$ps.name, te,...) {


  task.txt = include.ps.extra.lines(task.txt, ps.file=file, ps.name=ps.name,te=te,...)
  writeLines(task.txt, file, useBytes=TRUE)
  invisible(task.txt)
}

te.to.rps = function(te) {
  restore.point("te.to.rps")
  rps = new.env()

  copy.into.envir(source=te, dest=rps,
    names=c("ps.name","infos","awards")
  )
  ex.ind = 1

  ex.ind = 3

  # Create a data frame with chunk metadata
  li = lapply(seq_along(te$ex), function(ex.ind) {
    restore.point("te.to.rps.inner")
    ex = te$ex[[ex.ind]]
    if (length(ex$chunks)==0)
      return(NULL)

    add.chunk = sapply(ex$chunks, function(ck) isTRUE(ck$add))
    names(add.chunk) = NULL # important to fix bug
    num.e = sapply(ex$chunks, function(ck) length(ck$e.li))
    num.e.task = sapply(ex$chunks, function(ck) ck$num.e.task)

    str = sapply(ex$chunks, function(ck) str.trim(paste0(ck$test.txt,collapse="")))
    has.test = nchar(str)>0

    chunk.opt = lapply(ex$chunks, function(ck) ck$chunk.opt)

    optional = sapply(ex$chunks, function(ck) isTRUE(ck$chunk.opt$optional))
#     replace.sol = sapply(ex$chunks, function(ck) {
#       if (!is.null(ck$chunk.opt[["replace.sol"]])) {
#         return(ck$chunk.opt$replace.sol)    
#       } else {
#         return(NA)
#       }
#     })
    

    test.expr = lapply(ex$chunks, function(ck) {
      lapply(ck$test.txt, parse.text)
    })
    hint.expr = lapply(ex$chunks, function(ck) {
      lapply(ck$hint.txt, parse.text)
    })
    chunk.hint = lapply(ex$chunks, function(ck) {
      if (is.null(ck$chunk.hint.txt)) return(NULL)
      parse.text(ck$chunk.hint.txt)
    })

    sol.txt =  sapply(ex$chunks, function(chunk) paste0(chunk$sol.txt, collapse="\n"))

    task.txt =  sapply(ex$chunks, function(chunk) paste0(chunk$task.txt, collapse="\n"))
    part =  lapply(ex$chunks, function(chunk) chunk$part)
    e.li = lapply(ex$chunks, function(ck) {
      ck$e.li
    })
    e.source.li = lapply(ex$chunks, function(ck) {
      ck$e.source.li
    })


    dt = data.table(ex.ind = ex.ind, ex.name = names(te$ex)[ex.ind], chunk.ps.ind=0, chunk.ex.ind = seq_along(ex$chunks), chunk.name = names(ex$chunks), chunk.opt=chunk.opt, part=part, num.e = num.e, num.e.task=num.e.task, has.test = has.test, e.li = e.li, e.source.li = e.source.li, test.expr=test.expr, hint.expr=hint.expr, task.txt = task.txt, sol.txt=sol.txt, optional=optional, chunk.hint=chunk.hint)
    # Remove chunks without expressions
    dt = dt[add.chunk,]
    if (NROW(dt)>0)
      dt$chunk.ex.ind = 1:NROW(dt)
    dt
  })
  cdt = do.call(rbind,li)
  cdt$chunk.ps.ind = 1:NROW(cdt)
  # Add has.passed for each test
  cdt$e.tests.passed = lapply(cdt$test.expr, function(test.expr.li) {
    lapply(test.expr.li, function(et) {
      rep(FALSE, length(et))
    })
  })
  
  cdt$points = pmax(
    te$chunk.points + te$e.points * (cdt$num.e - cdt$num.e.task),
    # we may give points even for just 'click check' chunks
    # this may bring a bit of happiness
    te$min.chunk.points
  )

  items.df = te$items.df

  # Addons data.table: ao.dt


  li = lapply(te$addons, function(ao) {
    rta = ao$rta
    list(id=rta$id,type=rta$type,optional=rta$optional,changes.env=rta$changes.env, max.points=rta$max.points, solved=rta$solved, points=rta$points)
  })

  ao.dt = dplyr::as_data_frame(rbindlist(li))

  rows =  items.df$type == "addon"
  ao.dt$award.name = items.df$award.name[rows]
  ao.dt$item.pos = items.df$item.pos[rows]
  ao.dt$ex.name = items.df$ex.name[rows]


  rps$ao.dt = ao.dt
  rps$Addons = te$Addons
  rps$addons = te$addons


  # Match awards to cdt
  rows = match(cdt$chunk.name, items.df$id)
  cdt$award.name = items.df$award.name[rows]
  cdt$item.pos = items.df$item.pos[rows]

  rps$cdt = cdt
  rps$awards = te$awards


  # Add has.passed for each test

  li = lapply(cdt$chunk.ps.ind, function(ci) {
    restore.point("dhfjdgjghbh")
    ck = cdt[ci,]
    exi = ck$ex.ind
    li = lapply(seq_along(ck$test.expr[[1]]), function(ei) {
      restore.point("dhfjdgjghbh nfhdbfhb")
      et = ck$test.expr[[1]][[ei]]
      data.table(ex.ind=exi, chunk.ps.ind=ci, e.ind=ei, test.e.ind = seq_along(et), test.ps.ind=0, test.passed=FALSE)
    })
    rbindlist(li)
  })
  tdt = rbindlist(li)
  if (NROW(tdt)>0) {
    tdt$test.ps.ind = 1:NROW(tdt)
  } else {
    tdt$test.ps.ind = c()
  }
  rps$tdt=tdt

  # Just store exercise names
  num.chunks = sapply(seq_along(te$ex), function(ex.ind) sum(cdt$ex.ind==ex.ind))
  import.var = lapply(te$ex, function(act.ex) act.ex$import.var)

  rps$edt = data.table(ex.ind = seq_along(te$ex),ex.name = names(te$ex), num.chunks=num.chunks, import.var = import.var)



  rps
}


parse.sol.line = function(row,txt, te) {
  restore.point("parse.sol.line")
  str = txt[row]

  te$row = row
  chunk.starts = str.starts.with(str,"```{r")
  chunk.ends = str.starts.with(str,"```") & ! chunk.starts
  block.starts = str.starts.with(str,"#<")
  block.ends = str.starts.with(str,"#>")

  command.line  = str.starts.with(str,"#!")
  #if (command.line) stop()

  change = chunk.starts | chunk.ends | block.starts | block.ends | command.line

  if (!change) {
    parse.no.change.line(row,str,txt,te)
  } else if (chunk.starts) {
    parse.chunk.starts(row,str,txt,te)
  } else if (chunk.ends) {
    if (!is.true(te$in.chunk | te$block.type %in% te$markdown.blocks)) {
      display("in row ", row, " there was a line ``` but no code chunk was opened. Interpret it as a verbatim chunk.")
      parse.no.change.line(row,str,txt,te)
    } else {
      parse.chunk.ends(row,str,txt,te)
    }
  } else if (block.starts) {
    parse.block.starts(row,str,txt,te)
  } else if (block.ends) {
    parse.block.ends(row,str,txt,te)
  } else if (command.line) {
    parse.command.line(row,str,txt,te)
  }
  te
}

parse.no.change.line = function(row,str,txt, te) {
  restore.point("parse.no.change.line")
  # Normal Markdown text without being in a block
  if (!te$in.chunk & !te$in.block) {
    te$task.txt = c(te$task.txt, str)
    te$sol.txt = c(te$sol.txt, str)
    te$out.txt = c(te$out.txt, str)


    if (str.starts.with(str,"# Problemset ")) {
      te$ps.name = str.trim(str.right.of(str, "# Problemset "))
    } else if (str.starts.with(str,"## Exercise ")) {
      parse.exercise.starts(row, str, txt, te)
    } else {
      part.rows = which(grepl("#'[ ]?([a-z]|[ivx]*)\\)",str))
      if (length(part.rows)>0)
        te$part = str.right.of(str.left.of(str,")"),"#' ",not.found = NA)
    }

  # Normal line of code without beeing in a block
  # Treat as a "chunk" block
  } else if (te$in.chunk & !te$in.block) {
    te$block.txt = c(te$block.txt, str)

  # Within a block
  } else if (te$in.block) {
    te$block.txt = c(te$block.txt, str)
  }
}

parse.exercise.starts = function(row,str,txt, te) {
  restore.point("te.exercise.starts")

  te$prev.ex.name=te$ex.name
  te$ex.name = str.trim(str.between(str, "# Exercise ","-- "))
  te$part = ""
  ex = get.empty.ex()
  ex$ex.name = te$ex.name
  te$act.ex = ex
  te$ex[[ex$ex.name]] = ex
}

parse.chunk.starts = function(row,str,txt, te) {
  restore.point("parse.chunk.starts")
  if (te$block.type %in% te$markdown.blocks) {
    te$block.txt = c(te$block.txt, str)
  } else if (te$in.block | te$in.chunk) {
    stop(paste0("in row ", row, " you start a chunk without having closed the chunk before."), call.=FALSE)
  } else {
    opt = chunk.opt.string.to.list(str, keep.name=TRUE)
    chunk.name = opt[[1]]
    te$chunk.head = str
    te$chunk.opt = opt[-1]
    te$chunk.name = gsub('"','', chunk.name, fixed=TRUE)
    te$in.chunk = TRUE
    te$chunk.str = paste0(" in chunk ", te$chunk.name)
    te$block.type = "chunk"
    te$block.start = row+1

    ck = get.empty.chunk()
    ck$chunk.name = te$chunk.name
    ck$ex.name = te$act.ex$ex.name
    ck$chunk.opt = te$chunk.opt

    ck$part = te$part
    te$act.ex$chunks[[te$chunk.name]] = ck
    te$act.chunk = ck
  }
}

parse.chunk.ends = function(row,str,txt, te) {
  restore.point("parse.chunk.ends")

  if (te$block.type %in% te$markdown.blocks) {
    te$block.txt = c(te$block.txt, str)
  } else if (te$in.block) {
    stop(paste0(te$chunk.str, " ending in row ", row, " you forgot to close your ", te$block.type," block with #>"), call.=FALSE)
  } else {
    te$block.end = row-1
    add.te.block(te)
    add.te.chunk(te,te$act.chunk)

    te$prev.chunk.name = te$chunk.name
    te$prev.chunk.end = row
    te$in.chunk = FALSE
    te$chunk.name = ""
    te$chunk.str = ""
    te$chunk.code = NULL
    te$block.txt = NULL
  }
}

parse.block.starts = function(row,str,txt, te) {
  restore.point("parse.block.starts")
  #if (row==25) stop()
  if (te$in.block) {
    stop(paste0(te$chunk.str," in row ", row, " you start a new block without having closed the previous ", te$block.type, " block."), call.=FALSE)
  }

  # Add the virtual code block
  if (te$in.chunk & nchar(paste0(str.trim(te$block.txt),collapse=""))>0) {
    te$block.end = row-1
    add.te.block(te)
  }
  te$block.txt = NULL
  te$block.start.row = row
  te$block.head = str
  te$block.type = str.trim(str.between(str,"#< "," "))
  te$in.block = TRUE

  if (!te$block.type %in% te$blocks) {
    stop(paste0(te$chunk.str," in row ", row, " you open a block of unknown type:\n  '", te$block.type,"'\nI only know the block types:\n  ", paste0(te$blocks, collapse=", "),"."), call.=FALSE)
  }
  if (te$in.chunk & !te$block.type %in% te$code.blocks) {
    stop(paste0(te$chunk.str, " in row ", row, " you open a '", te$block.type,"' block. But '", te$block.type, "' blocks can only be opened in your markdown text outside of code chunks."), call.=FALSE)
  }
  if (!te$in.chunk & !te$block.type %in% te$markdown.blocks) {
    stop(paste0(" in row ", row, " you open a '", te$block.type,"' block outside a code chunk. But '", te$block.type, "' blocks can only be opened inside code chunks."), call.=FALSE)
  }
}

parse.block.ends = function(row,str,txt, te) {
  restore.point("parse.block.ends")
  if (!te$in.block) {
    stop(paste0(te$chunk.str, " in row ", row, " you close a block with #>, but have no open block."), call.=FALSE)
  } else {
    add.te.block(te)
    te$in.block = FALSE
    te$block.txt = NULL

    if (te$in.chunk) {
      te$block.type = "chunk"
      te$block.start = row+1
    } else {
      te$block.type = ""
    }
  }
}

parse.command.line = function(row,str,txt, te) {
  restore.point("parse.command.line")
  #if (row==25) stop()

  if (te$in.block) {
    stop(paste0("In row ", row, " you have written the command \n",str,"\n inside a block. Command lines must be outside blocks"), call.=FALSE)
  }
  if (te$in.chunk) {
    stop(paste0("In row ", row, " you have written the command \n",str,"\n inside a chunk. Command lines must be outside chuncks"), call.=FALSE)
  }

  str = str.trim(str.right.of(str,"#!"))
  com = str.left.of(str," ")
  if (com == "start_note" | com == "end_note") {
    te$task.txt = c(te$task.txt, paste0("#! ", str))
  } else {
    stop(paste0("In row ", row, " you have written an unknown command:\n",str), call.=FALSE)
  }

}


add.te.chunk = function(te,ck) {
  restore.point("add.te.chunk")
  if (length(ck$e.li)>0 | isTRUE(ck$has.task)) {
    te$task.txt = c(te$task.txt, te$chunk.head, ck$task.txt,"```")
    te$sol.txt = c(te$sol.txt, te$chunk.head, ck$sol.txt,"```")
    te$out.txt = c(te$out.txt, te$chunk.head, ck$out.txt,"```")
    ck$add = TRUE
    add.te.item(te=te, type="chunk", id = ck$chunk.name)
  }

}

add.te.item = function(te, type="", id="") {
  num.items = te$num.items+1
  te$num.items = num.items
  te$items[[num.items]] = list(item.pos = num.items, ex.name=te$ex.name, type=type,id=id, award.name=NA_character_)
}

add.te.block = function(te) {
  restore.point("add.struc.block")
  type = te$block.type
  args = str.trim(str.right.of(te$block.head, te$block.type))
  ck = te$act.chunk
  btxt = te$block.txt

  # Check if code in block can be parsed
  if (type %in% te$code.blocks) {
    expr = tryCatch(parse.text(btxt),
    error = function(e) {
        str = paste0(" when parsing your code",te$chunk.str," between rows ", te$block.start, " and ", te$block.end, ":\n ", str.right.of(paste0(as.character(e), collapse="\n"),":") )
      stop(str, call.=FALSE)
    })
  }
  if (type %in% c("task","task_notest")) {
    ck$has.task = TRUE
  }

  # Add test code
  if (type %in% c("chunk","task","task_notest","notest")) {
    add.te.code(te,ck)
  } else if (type == "extra_test") {
    ind = length(ck$test.txt)
    ck$test.txt[[ind]] = paste0(ck$test.txt[[ind]],"\n",btxt)
  } else if (type == "test") {
    test.txt = paste0(btxt, collapse="\n")
    ck$test.txt[length(ck$test.txt)] <- test.txt
    # Remove default hint for manual tests
    ck$hint.txt[length(ck$hint.txt)] <- ""
  } else if (type == "test_arg") {
    test.txt = test.code.for.e(te$last.e, extra.arg = paste0(btxt,collapse=", "))
    ck$test.txt[length(ck$test.txt)] <- test.txt
  } else if (type == "test_hint_arg") {
    #browser()
    extra.arg = paste0(btxt,collapse=",")
    test.txt = test.code.for.e(te$last.e, extra.arg = extra.arg)
    ck$test.txt[length(ck$test.txt)] <- test.txt

    hint.txt = hint.code.for.e(te$last.e, extra.arg = extra.arg)
    ck$hint.txt[length(ck$hint.txt)] <- hint.txt

  } else if (type == "test_calls") {
     test.txt = test.code.for.e(te$last.e, extra.arg = paste0(btxt,collapse=", "))
     ck$test.txt[length(ck$test.txt)] <- test.txt
  } else if (type == "compute") {
    var = args
    add.te.compute(te,ck,var)
  } else if (type == "hint") {
    restore.point("shfkjdkfhdkhfurhui")

    if (length(ck$hint.txt) == 0) {
      ck$chunk.hint.txt =  paste0(btxt,collapse="\n")
    } else {
      ck$hint.txt[length(ck$hint.txt)] <- paste0(btxt,collapse="\n")
    }
  } else if (type == "add_to_hint") {
    hint.txt = hint.code.for.e(te$last.e,extra.code = btxt)
    ck$hint.txt[length(ck$hint.txt)] <- hint.txt
  } else if (type == "settings") {
    add.te.settings(te)
  } else if (type == "info") {
    add.te.info(te)
  } else if (type == "award") {
    add.te.award(te)
  } else if (type == "ignore") {
  } else if (type %in% names(te$Addons)) {
    args.li = eval(parse(text=paste0("list(",args,")")))
    add.te.addon(te,type=type, args=args.li)
  } else {
    str = paste0(chunk.str, " there is an unknown block head: ", te$block.head)
    stop(str, call.=FALSE)
  }
  te$code.txt = NULL
  te$block.head = NULL
}

add.te.code = function(te,ck) {
  restore.point("add.te.code")

  #if (te$block.type=="chunk")
    #stop("")
  task = te$block.type == "task" | te$block.type == "task_notest"
  notest = te$block.type == "notest" | te$block.type == "task_notest"

  ck$sol.txt = c(ck$sol.txt, te$block.txt)
  ck$out.txt = c(ck$out.txt, te$block.txt)
  if (task) {
    ck$task.txt = c(ck$task.txt, te$block.txt)
  }

  if (!notest) {
    code.txt = str.trim(te$block.txt)
    code.txt = code.txt[nchar(code.txt)>0]

    ret = tryCatch(parse.text.with.source(te$block.txt),
      error = function(e) {
        e.str = paste0(as.character(e), collapse="\n")
        str = paste0(" when parsing your code",te$chunk.str," between rows ", te$block.start, "-", te$block.end, ":\n ", str.right.of(e.str,":"))
        stop(str, call.=FALSE)
    })
    e.li = ret$expr
    e.source.li = ret$source

    if (length(e.li)>0) {
      test.txt = sapply(seq_along(e.li), function(i) test.code.for.e( e.li[[i]]))
      hint.txt = sapply(seq_along(e.li), function(i) hint.code.for.e( e.li[[i]]))

      te$counter = te$counter+length(e.li)
      ck$test.txt = c(ck$test.txt,test.txt)
      ck$hint.txt = c(ck$hint.txt,hint.txt)
      ck$e.li = c(ck$e.li, e.li)
      ck$num.e = ck$num.e + length(e.li)
      if (task) {
        restore.point("jdsnndhfnruenfenrfkerfu84")
        #stop()
        ck$num.e.task = ck$num.e.task + length(e.li)
      }
      
      ck$e.source.li  = c(ck$e.source.li, e.source.li)
      te$last.e = e.li[[length(e.li)]]
      if (CREATE.PS.ENV$add.enter.code.here) {
        enter.code.str =  "\n# enter your code here ...\n"
      } else {
        enter.code.str =  ""
      }
      if (!task &
        !identical(te$task.txt[length(te$task.txt)], enter.code.str)) {
        ck$task.txt = c(ck$task.txt, enter.code.str)
      }
    # Empty code.txt
    } else {
      te$last.e = NULL
    }
  }
}

# Add a compute block to te
add.te.compute = function(te,ck,var) {
  restore.point("add.te.compute")
  hint.txt = hint.code.for.compute(te$block.txt,var=var)
  test.txt = test.code.for.compute(te$block.txt,var=var)

  ck$test.txt = c(ck$test.txt,test.txt)
  ck$hint.txt = c(ck$hint.txt,hint.txt)
  ck$sol.txt = c(ck$sol.txt, te$block.txt)
  ck$out.txt = c(ck$out.txt, te$block.txt)

  ret = tryCatch(parse.text.with.source(te$block.txt),
    error = function(e) {
      e.str = paste0(as.character(e), collapse="\n")
      str = paste0(" when parsing your code",te$chunk.str," between rows ", te$block.start, "-", te$block.end, ":\n ", str.right.of(e.str,":"))
      stop(str, call.=FALSE)
  })
  e.li = list(ret$expr)
  e.source.li = list(ret$source)

  te$counter = te$counter+length(e.li)
  ck$e.li = c(ck$e.li, e.li)
  ck$num.e = ck$num.e + length(e.li)

  ck$e.source.li  = c(ck$e.source.li, e.source.li)
  te$last.e = e.li[[length(e.li)]]

  enter.code.str =  "\n# enter your code here ...\n"
  enter.code.str =  ""
  if (!identical(te$task.txt[length(te$task.txt)], enter.code.str)) {
    ck$task.txt = c(ck$task.txt, enter.code.str)
  }
}


add.te.settings = function(te) {
  restore.point("add.te.settings")
  txt = te$block.txt
  env = new.env()
  eval(base::parse(text=txt,srcfile=NULL), envir=env)
  import.var = as.list(env$import.var)
  if (length(import.var)>0) {
    if (is.null(names(import.var)))
      names(import.var) = rep("", length(import.var))
    names(import.var)[names(import.var) == ""] = te$prev.ex.name
  }
  te$act.ex$import.var = import.var
}


add.te.info = function(te) {
  restore.point("add.te.info")
  #stop()
  require(knitr)
  require(markdown)
  str = te$block.head
  info.name = str.between(str, '"','"')

  txt = te$block.txt

  if (is.null(txt)) {
    txt = "-- EMPTY INFO BLOCK --"
    warning("You have an empty info block \n:", str)
  }
  #txt = c(paste0("**",info.name,":** "), txt)
  ktxt = knit(text=txt)
  html= markdownToHTML(text=ktxt, fragment.only=CREATE.PS.ENV$fragment.only)

  if (FALSE) {
    htmlFile <- tempfile(fileext=".html")
    writeLines(html,htmlFile)
    rstudioapi::viewer(htmlFile)
  }
  info = list(info.name=info.name,type="html", html=html, rmd=txt)
  str = paste0('info("', info.name,'") # Run this line (Strg-Enter) to show info')
  te$task.txt = c(te$task.txt,str)
  te$sol.txt = c(te$sol.txt, str)
  te$out.txt = c(te$out.txt,"\n***\n", paste0("### Info: ", info.name),te$block.txt,"\n***\n")

  te$infos[[info.name]] = info
}


add.te.addon = function(te,type,args=NULL) {
  restore.point("add.te.addon")
  #stop()
  name = args[[1]]

  txt = te$block.txt
  Addon = te$Addons[[type]]
  ao = Addon$parse.fun(txt,type=type,name=name,args=args[-1])


  rta = ao$rta
  rta$id = paste0("addon__",type,"__",name)

  if (rta$id %in% names(te$addons)) {
    stop(paste0("You have defined more than once a ", type, " with name ",'"', name,'"', ". Please pick a unique name for every ", type, "."))
  }
  
  placeholder = paste0("#! ", rta$id)


  te$task.txt = c(te$task.txt,placeholder)
  te$sol.txt = c(te$sol.txt, Addon$sol.txt.fun(ao))
  te$out.txt = c(te$out.txt, Addon$out.txt.fun(ao))

  te$addons[[rta$id]] = ao

  add.te.item(te=te, type="addon", id = rta$id)
}


add.te.award = function(te) {
  restore.point("add.te.award")
  #stop()
  require(knitr)
  require(markdown)

  str = te$block.head
  name = str.between(str, '"','"')

  # Duplicated award names lead to 
  # hard to debug errors
  if (name %in% names(te$awards)) {
    stop(paste0("Your problem set has twice an award with the name '", name, "'. Each award needs a unique name!"))
  }
  
  txt = te$block.txt
  txt = c(paste0("### Award: ",name,"\n"), txt)
  ktxt = knit(text=txt)
  html= markdownToHTML(text=ktxt, fragment.only=CREATE.PS.ENV$fragment.only)
  if (FALSE) {
    htmlFile <- tempfile(fileext=".html")
    writeLines(html,htmlFile)
    rstudioapi::viewer(htmlFile)
  }

  # item (chunk or addon) to which the award belongs
  te$items[[te$num.items]]$award.name = name

  award = list(award.name=name, html=paste0(html,collapse="\n"), txt=paste0(te$block.txt, collapse="\n"))
  te$out.txt = c(te$out.txt,"\n***\n", paste0("### Award: ", name),te$block.txt,"\n***\n")

  
  te$awards[[name]] = award

  
  
}


examples.test.code.for.e = function() {
  f = function(e) {
    e = substitute(e)
    test.code.for.e(e)
  }

  f(fun <- function(x) {x*x})
}

get.expr.test.args = function(e) {
  restore.point("get.expr.test.args")

  funs = find.funs(e)

  no.value.funs = c("plot","hist","qplot","geom_point","geom_line","geom_smooth","geom_density","lines","points","facet_wrap")
  if (any(funs %in% no.value.funs)) {
    args = "check.arg.by.value=FALSE, allow.extra.arg=TRUE,ok.if.same.val = FALSE"
  } else {
    args = ""
  }
  args

}

test.code.for.e = function(e, extra.arg=get.expr.test.args(e)) {
  restore.point("test.code.for.e")
  if (is.null(e))
    return("")

  extra.arg = ifelse(extra.arg=="","",paste0(",",extra.arg))
  if (is.assignment(e)) {
    var = deparse1(e[[2]],collapse="\n")
    rhs = deparse1(e[[3]],collapse="\n")
    call.name = name.of.call(e[[3]])
    if (call.name == "function") {
      code=paste0("check.function(", var, "<-",rhs,extra.arg,")")
    } else {
      code = paste0("check.assign(", var, "<- ",rhs,extra.arg,")")
    }
  } else {
    estr = deparse1(e)
    code = paste0("check.call(", estr,extra.arg,")")
  }
  code
}

hint.code.for.e = function(e, extra.code = NULL, extra.arg = NULL) {
  restore.point("hint.code.for.e")
  if (is.null(e))
    return("")
  if (!is.null(extra.arg))
    extra.arg =  paste0(",", extra.arg)

  if (!is.null(extra.code)) {
    extra.code = paste0("\n  ",paste0(extra.code,collapse="\n  "))
  }
  estr = deparse1(e)
  if (is.assignment(e)) {
    var = deparse1(e[[2]])
    rhs = deparse1(e[[3]])
    call.name = name.of.call(e[[3]])

    if (call.name == "function") {
      rhs = deparse1(e[[3]], collapse="\n")
      code = paste0("hint.for.function(",var ,"<-",rhs, extra.arg,")",
                    extra.code)
    } else {
      code = paste0("hint.for.assign(",var ,"<-",rhs,extra.arg,")",
                    extra.code)
    }
  } else {
    code = paste0("hint.for.call(",estr,extra.arg,")", extra.code)
  }
  code
}

test.code.for.compute = function(code, var, extra.arg="") {
  restore.point("test.code.for.compute")
  code.txt = paste0("{\n", paste0(code, collapse="\n"),"\n",var,"\n}")
  test.txt = paste0("check.variable('", var,"',",code.txt,extra.arg,")")
  test.txt
}

hint.code.for.compute = function(code, var, extra.code = NULL) {
  restore.point("hint.code.for.compute")
  ec = parse.expr.and.comments(code, comment.start="## ")
  comments = lapply(ec$comments, function(str) {
    ret=gsub('"',"'",str, fixed=TRUE)
    if (length(ret)==0)
      ret=""
    ret
  })
  comment.code = paste0("list(",paste0('"',comments,'"', collapse=", "),")")

  code = paste0(code, collapse="\n")
  com = paste0("hint.for.compute({\n",code,"\n},",comment.code,", var= '",var,"'",
               extra.code,"\n)")
  com
}

get.empty.ex = function() {
  ex = new.env()
  ex$chunks = list()
  ex
}
get.empty.chunk = function() {
  ck = new.env()
  ck$test.txt = NULL
  ck$hint.txt = NULL
  ck$chunk.hint.txt = NULL
  ck$task.txt = NULL
  ck$sol.txt = NULL
  ck$out.txt = NULL
  ck$expr = NULL
  ck$num.e = 0
  ck$num.e.task = 0
  ck
}

get.empty.te = function(Addons=NULL) {
  te = new.env()
  te$Addons = Addons
  te$block.type = ""
  te$in.block = FALSE
  te$in.chunk = FALSE
  te$block.head = NULL

  te$task.txt = NULL
  te$sol.txt = NULL
  te$out.txt = NULL
  te$code.txt = NULL

  te$part = NULL
  te$last.e = NULL
  te$counter = 0

  te$markdown.blocks = c("info","award","ignore",names(te$Addons))
  te$code.blocks = c("test","test_arg","test_hint_arg","extra_test","test_calls",
                  "hint","add_to_hint",
                  "task","task_notest","notest",
                  "compute","settings")
  te$blocks = c(te$markdown.blocks, te$code.blocks, names(te$Addons))
  te$act.chunk = NULL
  te$act.ex = NULL
  te$ps.name = NULL
  te$ex = te$infos = te$awards = te$addons = list()

  te$items = vector("list",1000)
  te$num.items = 0

  te
}

include.ps.extra.lines = function(txt, ps.file, ps.name=te$ps.name,te=NULL,...) {
  chunk.row = which(str.starts.with(txt,"# Problemset"))[1]
  if (is.na(chunk.row))
    chunk.row = 1
  str = ps.rtutor.chunk(ps.name=ps.name, ps.file=ps.file,...)
  txt[chunk.row] = paste0(txt[chunk.row],"\n\n",paste0(str,collapse="\n"))
  txt

}

ps.rtutor.chunk = function(ps.name,ps.dir = "C:/problemsets/", ps.file = paste0(ps.name,".Rmd"), header="", user.name="ENTER A USER NAME HERE",...) {

  str = paste0("
```{r 'check_ps', include=FALSE}
",header,"
user.name = '",user.name,"' # set to your user name

# To check your problem set, save your file (Ctrl-S) and then run the RStudio Addin 'Check Problemset'

# Alternatively run the following lines 
library(RTutor)
ps.dir = getwd() # directory of this file
ps.file = '", ps.name,".Rmd' # name of this file
check.problem.set('",ps.name,"', ps.dir, ps.file, user.name=user.name, reset=FALSE)
```
")
  str
}


#' Generate default header text for a Rmd file
#' @export
install.header.txt = function() {
"
# Remove comments below if you need to install packages
# install.packages('devtools');install.packages('whisker');install.packages('stringr')
# install.packages('RJSONIO');
# library(devtools)
# install_github(repo = 'restorepoint', username = 'skranz')
# install_github(repo = 'RTutor', username = 'skranz')
"
}

#' Generate default footer text for a Rmd file
zip.submit.footer.txt = function(ps.name) {
paste0("
#'
#'## Sumbitting your solution
#'
#' Submit your solution as a zip file with name
#'`solution_",ps.name,"_by_username.zip`
#' that contains the files
#' `",ps.name,".rmd, ", ps.name,".log, username_",ps.name,".ups`
#' (replace `username` by your user name)
#'
#' If you have installed RTools (http://cran.r-project.org/bin/windows/Rtools/) and updated your Windows PATH variable you can also try calling
#' `zip.solution()`
#' to generate the zip file automatically.
")
}


#' Set default names for the chunks of problem set rmd files
#' @param rmd.file file name
#' @param txt alternative the code as txt file
#' @param only.empy.chunks if FALSE (default) name all chunks.
#'        Otherwise only empty chunks are overwritten
#' @param keep.option if TRUE (default) don't change chunk options;
#'        otherwise clear all chunk options (dangerous)
#'
name.rmd.chunks = function(rmd.file=NULL, txt=readLines(rmd.file, warn=FALSE), only.empty.chunks=FALSE, keep.options=TRUE, valid.file.name = FALSE) {
  restore.point("name.rmd.chunks")
  ex.name = ""
  part.name = ""
  in.code = FALSE
  i = 2
  counter = 1

  used.chunk.names = NULL
    
  str = "```{r 'out_chunk_2_1_b', fig.width=5, fig.height=5, eval=FALSE, echo=TRUE}"
  for (i in 1:length(txt)) {
    str = txt[i]


    if (str.starts.with(str, "```{r")) {
      if ((!only.empty.chunks) | str.trim(str) == "```{r }" | str.trim(str) == "```{r}") {
        counter.str = ifelse(counter==1,"", paste0(" ",counter))
        
        # preserve chunk options
        if (has.substr(str,"=")) {
          rhs.str = paste0(",",chunk.opt.list.to.string(chunk.opt.string.to.list(str)))
        } else {
          rhs.str = ""
        }
        chunk.name = paste0(ex.name,' ',part.name, counter.str)

        chunk.name = str.to.valid.chunk.name(str.trim(chunk.name))
          
        if (chunk.name %in% used.chunk.names) {
          str = paste0("I generated the chunk name ", chunk.name, " twice. Make sure that you have unique exercise names and don't duplicate exerice parts like a) b) a).")
          warning(str)
          chunk.name = paste0(chunk.name, "___", sample.int(10000000,1))
        }
        used.chunk.names = c(used.chunk.names, chunk.name)
        
        txt[i] = paste0('```{r "',chunk.name,'"', rhs.str,"}")
      }
      counter = counter+1
    } else if (str.starts.with(str,"## Exercise ")) {
      ex.name = str.right.of(str,"## Exercise ")
      ex.name = gsub("#","", ex.name, fixed=TRUE)
      #ex.name = str.left.of(ex.name," --", not.found="all")
      ex.name = str.left.of(ex.name," --", not.found=ex.name)
      ex.name = substring(str.to.valid.chunk.name(ex.name),1,20)
      if (!valid.file.name)
        counter = 1
      part.name = ""
    } else if (!is.na(temp.part <- str_extract(str,"^([a-z]|[ivx]*)\\)")[1]  )) {
      part.name = gsub(")","",temp.part, fixed=TRUE)
      if (!valid.file.name)
        counter = 1
    }
  }
  if (!is.null(rmd.file))
    writeLines(txt, rmd.file)
  invisible(txt)
}

examples.str.to.valid.file.name = function() {
 str.to.valid.file.name("chunk 1 a)")
}
str.to.valid.chunk.name = function(str, replace.char = "_") {
  str = gsub("[^a-zA-Z0-9_]",replace.char,str)
  str
}

str.to.valid.file.name = function(str, replace.char = "_") {
  str = gsub("[ \\(\\)\\.\\:]",replace.char,str)
  str
}
get.chunk.lines = function(txt) {
  restore.point("get.chunk.lines")
  chunk.start = str.starts.with(txt,"```{")
  chunk.end = which(str.starts.with(txt,"```") & !chunk.start)
  chunk.start = which(chunk.start)
  chunk.end = remove.verbatim.end.chunks(chunk.start,chunk.end)

  header = txt[chunk.start]
  chunk.name = sapply(header,USE.NAMES=FALSE, function(str) chunk.opt.string.to.list(str, keep.name=TRUE)[[1]])

  quick.df(chunk.name=chunk.name, start.line=chunk.start, end.line=chunk.end)
}

make.shiny.dt = function(rps, rmd.file, txt = readLines(rmd.file, warn=FALSE)) {
  restore.point("make.shiny.dt")
  library(stringtools)
  library(markdown)
  txt = sep.lines(merge.lines(txt))

  chunk.start = str.starts.with(txt,"```{")
  chunk.end = which(str.starts.with(txt,"```") & !chunk.start)
  chunk.start = which(chunk.start)

  chunk.end = remove.verbatim.end.chunks(chunk.start,chunk.end)

  chunk.end.plus1 = chunk.end+1
  ex.start = which(str.starts.with(txt,"## Exercise "))
  info.start = which((str.starts.with(txt,"info(")))
  addon.start = which((str.starts.with(txt,"#! addon__")))
  cont.start = which((str.starts.with(txt,"#. continue")))

  note.start = which((str.starts.with(txt,"#! start_note")))
  note.end = which((str.starts.with(txt,"#! end_note")))
  if (length(note.start) != length(note.end)) {
    stop(paste0("You have ",length(note.start)," '#! start_node' commands but ",length(note.end), " end_node commands!"))
  }
  note.name = str.right.of(txt[note.start],"#! start_note ")
  note.name = str.between(note.name,'"','"')


  df.chunk = data.frame(start=chunk.start, type="chunk", type.ind=seq_along(chunk.start))
  df.info = data.frame(start=info.start, type=rep("info", length(info.start)), type.ind=seq_along(info.start))
  df.addon = data.frame(start=addon.start, type=rep("addon", length(addon.start)), type.ind=seq_along(addon.start))
  df.cont = data.frame(start=cont.start, type=rep("continue", length(cont.start)), type.ind=seq_along(cont.start))

  if (length(note.start)>0) {
    df.note.start = data.frame(start=note.start, type="note.start", type.ind=seq_along(note.start))
    df.note.end = data.frame(start=note.end, type="note.end", type.ind=seq_along(note.end))
  } else {
    df.note.start = df.note.end = NULL
  }


  df.task = data.frame(start=sort(c(1,ex.start,note.start+1, note.end+1,chunk.end+1,addon.start+1, info.start+1, cont.start+1)), type="task")



  df.task$type.ind = 1:NROW(df.task)


  df = rbind(df.chunk,df.info,df.addon,df.cont, df.task, df.note.start, df.note.end)
  df = df[!duplicated(df$start),]
  df = arrange(df, start)
  df$end = c(df$start[-1]-1, length(txt))
  df

  in.note = cumsum(df$type=="note.start") - cumsum(df$type=="note.end")
  df$note.ind = cumsum(df$type=="note.start")*in.note
  df$note.label = ""
  df$note.label[in.note==1] = note.name[df$note.ind[in.note==1]]

  df = df[! df$type %in% c("note.start","note.end"),]

  n = NROW(df)

  df.ex = data.frame(start=c(1,ex.start), ex.ind = c(0,seq_along(ex.start)))
  if (df.ex$start[2]==1)
    df.ex = df.ex[-1,]
  #df.ex$end = c(df.ex$start[-1]-1, length(txt))
  df.ex

  df$ex.ind = df.ex$ex.ind[findInterval(df$start, df.ex$start)]

  # views
  views = sort(c(cont.start, ex.start))
  df$view.ind = findInterval(df$start, views)
  df


  dt = data.table(fragment.ind = 1:n,ex.ind=df$ex.ind, view.ind=df$view.ind, type=df$type, type.ind=df$type.ind, chunk.name="",chunk.ind=0,info.name="", html=vector("list", n), code="", note.ind = df$note.ind, note.label=df$note.label, addon.id="")
  keep.row = rep(TRUE, NROW(dt))

  i = 5
  for (i in 1:n) {
    if (dt$type[i]=="chunk") {
      header = txt[df$start[i]]
      opt = chunk.opt.string.to.list(header, keep.name=TRUE)
      chunk.name = opt[[1]]
      chunk.ind = which(rps$cdt$chunk.name == chunk.name)[1]
      if (is.na(chunk.ind)){
        keep.row[i] = FALSE
        next
      }
      dt$chunk.name[i] = chunk.name
      dt$chunk.ind[i] = chunk.ind
      code = txt[(df$start[i]+1):(df$end[i]-1)]
      dt$code[[i]] = paste0(code, collapse="\n")
      #dt$code[[i]] = mark_utf8(paste0(code, collapse="\n"))
      #shiny.dt$html[[i]] = editChunkUI(chunk.name=chunk.name,code=code)
    } else if (dt$type[i]=="task") {
      code = txt[df$start[i]:df$end[i]]
      #dt$code[[i]] = code
      #if (any(str.starts.with(code, "a)"))) {
      #restore.point("jkhskjfhdkjfkjdn")
      #  stop()
      #}
      if (nchar(paste0(code, collapse="\n"))==0)  {
        keep.row[i] = FALSE
      } else {
        #dt$html[[i]] = withMathJax(HTML(markdownToHTML(text=code, fragment.only=!TRUE)))
        dt$html[[i]] = HTML(markdownToHTML(text=code, fragment.only=CREATE.PS.ENV$fragment.only))
      }
    } else if (dt$type[i]=="info") {
      header = txt[df$start[i]]
      info.name = str.between(header,'"','"')
      #html = withMathJax(HTML(rps$infos[[info.name]]$html))
      html = HTML(rps$infos[[info.name]]$html)

      collapseId = paste0("collapse_info_",i)
      collapsePanelId = paste0("collapse_panel_info_",i)
      dt$html[[i]] = bsCollapse(open = NULL, id = collapseId,
        bsCollapsePanel(paste0("Info: ",info.name),value=collapsePanelId, html )
      )
    } else if (dt$type[i]=="addon") {
      dt$addon.id[[i]] = str.right.of(txt[df$start[i]],"#! ")
    } else if (dt$type[i]=="continue") {
    }
  }

  
  
  dt = dt[keep.row,]
  
  # Mark as UTF8 to deal with special characters like
  # German Umlaute
  # dt$html = lapply(dt$html, function(html) {
  #   txt = mark_utf8(as.character(html))
  #   HTML(txt)
  # })
  # 
  dt
}

remove.verbatim.end.chunks = function(chunk.start, chunk.end) {
  restore.point("remove.verbatim.end.chunks")
  df = data.frame(ind =c(0, seq_along(chunk.start), seq_along(chunk.end)),
                  row=c(0, chunk.start,chunk.end),
                  type=c("f",
                         rep("s",length(chunk.start)),
                         rep("e",length(chunk.end))
                       )
                  )
  df = arrange(df,row)
  df$del =  df$type == "e" & !is.true(lag(df$type) == "s")

  keep.ind = df$ind[df$type=="e" & !df$del]
  chunk.end[keep.ind]
}


chunk.opt.string.to.list = function(str, keep.name=FALSE) {
  restore.point("chunk.opt.string.to.list")
  #str = "```{r 'out_chunk_2_1_b', fig.width=5, fig.height=5, eval=FALSE, echo=TRUE}"

  tokens = str.split(str,",")
  str = str.between(str,"{r","}")
  code = paste0("list(",str,")")
  li = eval(base::parse(text=code,srcfile=NULL))

  if (keep.name) return(li)
  if (length(li)==0) return(li)

  #if ("replace.sol" %in% names(li))
  #  stop("nbfbfurb")
  # remove chunk.name
  if (is.null(names(li))) {
    return(li[-1])
  } else if (nchar(names(li)[1]) == 0) {
    return(li[-1])
  }
  li
}


