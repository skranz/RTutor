examples.frame.ps = function() {
  library(rmdtools)
  library(EconCurves)
  setwd("D:/libraries/rmdtools")
  txt = readLines("test.rmd")
  te = rtutor.make.frame.ps.te(txt)
  te$lang = "de"
  bdf = te$bdf; br = bdf[bi,]; str = te$txt[br$start:br$end]

  frame.inds = which(bdf$type == "frame")
  ind = frame.inds[2]
  fr = bdf[ind,]
  ui = fr$ui[[1]]
  
  view.html(ui=ui, browser=TRUE)
  
}

rtutor.make.frame.ps.te = function(txt,addons="quiz",...) {
  restore.point("rtutor.make.frame.ps.te")

  te = new.env()
  te$addons = addons
  te$Addons = make.addons.list(addons)

  
  if (length(txt)==1)  
    txt = sep.lines(txt)
  
  
  
  Encoding(txt) = "UTF8"
  txt = mark_utf8(txt)
  
  dot.levels = rtutor.dot.levels()
  df = find.rmd.nested(txt, dot.levels)
  parent.types = c("frame","column","chunk","preknit","precompute","knit","compute","info")
  pt = get.levels.parents.by.types(df$level, df$type, parent.types)

  bdf = cbind(data.frame(index = 1:NROW(df)),df,pt) %>% as_data_frame
  bdf$obj = bdf$ui = vector("list", NROW(bdf))
  bdf$prefixed = bdf$ui.in.parent = bdf$has.ui = bdf$is.addon =  FALSE
  
  
  
  te$bdf = bdf
  te$txt = txt
  
  # Create env
  ps.baseenv = new.env(parent=parent.env(globalenv()))
  te$pre.env = ps.baseenv
  te$env = new.env(parent=parent.env(globalenv()))
  
  # Go through blocks and chunks, ordered by end
  binds = order(bdf$end, -bdf$start)
  bi = binds[1]
  for (bi in binds) {
    res = try(rtutor.parse.block(bi,te), silent=TRUE)
    if (is(res,"try-error")) {
      br = te$bdf[bi,]
      msg = paste0("Error when parsing ",br$type," block lines ", br$start, " to ", br$end,"\n\n", as.character(res))
      stop(msg)
    }
  }
  te
}

rtutor.dot.levels = function() {
  dot.levels = c(
    exercise = 0,
    frame = 1,
    row = 2,
    references = 2,
    column = 3,
    success = 3,
    when = 3
  )
  dot.levels  
}

rtutor.parse.block = function(bi,te) {
  restore.point("rtutor.parse.block")
  
  # Don't parse blocks inside chunks here
  if (te$bdf$parent_chunk[[bi]] >0) return()

  type = te$bdf$type[[bi]]
  if (type %in% te$addons) {
    bdf = te$bdf; br = bdf[bi,];
    str = te$txt[br$start:br$end]
    
    ao = te$Addons[[type]]$parse.fun(str, id = paste0(type,"__",bi))
    ui = te$Addons[[type]]$shiny.ui.fun(ao)
    set.bdf.ui(ui,bi,te)
    te$bdf$obj[[bi]] = list(ao=ao)
    te$bdf$is.addon[[bi]] = TRUE
    return()
  }
  
  
  fun.call = parse(text=paste0("rtutor.parse.",type))
  
  fun = try(eval(fun.call), silent=TRUE)
  if (is(fun,"try-error")) {
    cat(paste0("\nWe don't have a function ",paste0("rtutor.parse.",type)," to deal with block type ", type))
    bdf = te$bdf; br = bdf[bi,];
    str = te$txt[br$start:br$end]
    set.bdf.ui(HTML(str),bi,te)
    return()
  }
  fun(bi,te)
}

rtutor.parse.chunk = function(bi,te) {
  restore.point("rtutor.parse.chunk")
  bdf = te$bdf; br = bdf[bi,]; str = te$txt[br$start:br$end]
  if (br$parent_info | br$parent_preknit) {
    te$bdf$prefixed[[bi]] = TRUE
    return()
  }
  args = parse.chunk.args(arg.str = br$arg.str)
  
  chunk.precompute = br$parent_precompute >0 | isTRUE(args$precompute)
  chunk.preknit = isTRUE(args$preknit)
  
  code = str[-c(1,length(str))]
  if (chunk.precompute) {
    expr = parse(text=code)
    res = eval(expr,te$pre.env)
    te$bdf$obj[[bi]]$pre.env = copy.env(te$pre.env)
    te$bdf$prefixed[[bi]] = TRUE
  } else if (chunk.preknit) {
    set.bdf.ui(uiOutput(paste0("chunkUI__",bi)),bi,te)
  }
}

rtutor.parse.preknit = function(bi,te) {
  restore.point("rtutor.parse.preknit")
  bdf = te$bdf; br = bdf[bi,];
  str = te$txt[(br$start+1):(br$end-1)]
  
  ui = knit.rmd(str,envir = te$pre.env,out.type="shiny")
  set.bdf.ui(ui,bi,te)
}

rtutor.parse.precompute = function(bi,te) {
  # all work is done in the chunks inside
}


rtutor.parse.column = function(bi,te) {
  restore.point("rtutor.parse.column")
  bdf = te$bdf; br = bdf[bi,];
  
  children = bdf$parent == bi 
  res = get.children.and.fragments.ui.list(bi,te, keep.null=FALSE, children=children)
  ui.li = res$ui.li
  args = parse.block.args(arg.str = br$arg.str)
  te$bdf$ui.in.parent[children] = TRUE 
  
  if (is.null(args$width)) args$width = 12 
  if (is.null(args$offset)) args$offset = 0 
  ui = column(width = args$width, offset=args$offset,
    ui.li
  )
  
  set.bdf.ui(ui,bi,te)
}

rtutor.parse.image = function(bi,te) {
  restore.point("rtutor.parse.image")
  bdf = te$bdf; br = bdf[bi,];
  
  args = parse.block.args(arg.str = br$arg.str)
  yaml = get.bi.te.str(bi,te)
  if (!is.null(yaml)) {
    yaml.arg = yaml.load(paste0(yaml,collapse="\n"))
    args[names(yaml.arg)] = yaml.arg
  }
  
  if (is.null(args$height)) args$height = "auto"
  if (is.null(args$height)) args$width = "100%"
  
  if (!is.null(args$url)) {
    html = paste0('<img src=',args$url,'">')
  } else {
    html = "<p>image here</p>"
  }
  set.bdf.ui(HTML(html),bi,te)
}

 
rtutor.parse.solved = function(bi,te) {
  restore.point("rtutor.parse.success")
  res = get.children.and.fragments.ui.list(bi,te, keep.null=FALSE)
  ui.li = res$ui.li

  id = paste0("solved_block__",bi)
  te$bdf$obj[[bi]] = list(ui = ui.li, id=id)
  set.bdf.ui(uiOutput(id),bi,te)
}


rtutor.parse.row = function(bi,te) {
  restore.point("rtutor.parse.row")
  bdf = te$bdf; br = bdf[bi,];
  
  children = bdf$parent == bi 
  res = get.children.and.fragments.ui.list(bi,te, keep.null=FALSE, children=children)
  ui.li = res$ui.li
  te$bdf$ui.in.parent[children] = TRUE 
  ui = fluidRow(
    ui.li
  )
  set.bdf.ui(ui,bi,te)
}


rtutor.parse.frame = function(bi,te) {
  restore.point("rtutor.parse.frame")
  #stop()
  bdf = te$bdf; br = bdf[bi,];
  args = parse.block.args(arg.str = br$arg.str)
  
  children = bdf$parent == bi 
  res = get.children.and.fragments.ui.list(bi,te, children=children, keep.null=TRUE)
  ui.li = res$ui.li
  is.child = !res$is.frag
  te$bdf$ui.in.parent[children] = TRUE 
  
  if (is.null(args$title.offset)) args$title.offset=0
  if (!is.null(args$name)) {
    title = fluidRow(column(offset=args$title.offset,width=12-args$title.offset,h4(args$name)))
  } else {
    title = NULL
  }
  ui = tagList(
    title,
    ui.li
  )
  
  set.bdf.ui(ui,bi,te)
}

rtutor.parse.info = rtutor.parse.note =  function(bi,te) {
  restore.point("rtutor.parse.info")
  #stop()
  bdf = te$bdf; br = bdf[bi,];
  args = parse.block.args(arg.str = br$arg.str)
  
  children = bdf$parent == bi 
  res = get.children.and.fragments.ui.list(bi,te, children=children, keep.null=TRUE)
  ui.li = res$ui.li
  is.child = !res$is.frag
  te$bdf$ui.in.parent[children] = TRUE 
  
  if (!is.null(args$name)) {
    title = args$name
  } else {
    title = "Info"
  }
  ui = make.rtutor.collapse.note(id=paste0("info_collapse_",bi),content=ui.li, title=title)  
  set.bdf.ui(ui,bi,te)
}


rtutor.parse.references = function(bi,te) {
  restore.point("references.block.render")
  title = "References"
  if (isTRUE(te$lang=="de")) {
    title = "Referenzen"
  }
  str = get.bi.te.str(bi,te)
  html = md2html(str)
  ui = make.rtutor.collapse.note(id=paste0("ref_collapse_",bi),html=html, title)
  set.bdf.ui(ui,bi,te)  
}

get.bi.te.str = function(bi,te, remove.header.footer=TRUE) {
  restore.point("get.bi.te.str")
  
  start = (te$bdf$start[bi]+1)
  end = te$bdf$end[bi]-1*(te$bdf$form[[bi]]=="block")
  if (end<start) return(NULL)
  str = te$txt[start:end]
  str
}

make.rtutor.collapse.note = function(id, html, title="Note", content=NULL) {
  if (is.null(content))
    content = HTML(paste0(html, collapse="\n"))
  
  shinyBS::bsCollapse(id =id, shinyBS::bsCollapsePanel(title=title,content))
  
}

set.bdf.ui = function(ui,bi,te) {
  te$bdf$ui[[bi]] = ui
  te$bdf$has.ui[[bi]] = TRUE
}

get.children.and.fragments.ui.list = function(bi,te,bdf=te$bdf, keep.null=TRUE, empty.as.null=FALSE, children=te$bdf$parent == bi ) {
  restore.point("get.children.and.fragments.ui.list")
  
  res = get.non.children.fragments(bi,te, child.ind = which(children))
  is.frag = res$is.frag
  is.child = !is.frag
  ui = res$frag

  ui[is.frag] = lapply(ui[is.frag], function(txt) {
    HTML(md2html(txt, fragment.only = TRUE))
  })
  
  ui[is.child] = lapply(which(children), function(ind) {
    if (bdf$ui.in.parent[[ind]]) return(NULL)
    bdf$ui[[ind]]
  })
  
  if (!keep.null) {
    null.ui = sapply(ui, is.null)
    ui = ui[!is.null(ui)]
    is.frag = is.frag[null.ui]
  }
  names(ui) = NULL
  list(ui.li=ui, is.frag=is.frag)
}

get.non.children.fragments = function(bi,te,bdf=te$bdf, child.ind = which(bdf$parent == bi), keep.header.footer=FALSE) {
  restore.point("get.non.children.fragments")
  
  cpos = cbind(bdf$start[child.ind],bdf$end[child.ind])
  has.footer = bdf$form[[bi]] != "dotblock"
  start = bdf$start[bi]+ (1-keep.header.footer)
  end = bdf$end[bi] - (1- (keep.header.footer | !has.footer))
  
  pos = pos.complement(cpos, is.sorted=TRUE, keep.pos=TRUE, start=start, end = end)
  is.frag = attr(pos,"complement")
  
  # we may have one end line too much
  valid = pos[,1]<=pos[,2]
  pos = pos[valid,,drop=FALSE]
  is.frag = is.frag[valid]
  
  if (NROW(pos)==0) return(NULL)
  
  frag.li = lapply(1:NROW(pos), function(row) {
    if (!is.frag[row]) return(NULL)
    te$txt[pos[row,1]:pos[row,2]]
  })
  list(frags = frag.li, is.frag=is.frag)
}


copy.env = function(env) {
  new = as.environment(as.list(env))
  parent.env(new) = parent.env(env)
  new
}

#' Parse the name of a knitr chunk and its arguments
#' @export
parse.chunk.args = function(header, arg.str=NULL) {
  restore.point("parse.chunk.opt.and.name")
  if (!is.null(arg.str)) {
    if (is.na(arg.str)) return(list())
    return(knitr:::parse_params(arg.str))
  }

  str = str.right.of(header,"r ",not.found = NA)
  if (is.na(str)) return(list())
  knitr:::parse_params(str.left.of(str,"}"))
}

