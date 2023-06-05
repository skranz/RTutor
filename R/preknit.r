examples.rps.to.wps = function() {
  ps.name = "WebExample"
  setwd("D:/libraries/RTutorWeb/examples")
  rps = knit.all.rps.sol(ps.name=ps.name)
  save.rps(rps)
}

preknit.rps = function(rps=load.rps(file=file),precomp=TRUE,file=paste0(ps.name,".rps"), ps.name=NULL, quiet=FALSE, knit.print.opts=make.knit.print.opts()) {
  restore.point("knit.all.rps.sol")

  if (!isTRUE(rps$has.sol)) {
    stop("You must call create.ps with the option  `rps.has.sol=TRUE` to generate a .rps file from the .rps file.")
  }

  # Load all required libraries
  load.ps.libs(rps$libs)

  

  # Replace knit.print.funs in globalenv
  #knit.print.funs = make.knit.print.funs(knit.print.opts)
  #old.knit.print.funs = replace.fields(dest=globalenv(), source=knit.print.funs)
  # restore old functions on exit
  #on.exit(replace.fields(dest=globalenv(), source=old.knit.print.funs), add=TRUE)
  
  # We need now to explicitly call registerS3method
  register.knit.print.functions(knit.print.opts)

  

  # Modifiy the cdt file
  cdt = rps$cdt

  chunk.envs = vector("list", NROW(cdt))
  chunk.sol.html = vector("character", NROW(cdt))



  # Run and knit solution for all chunks
  ps.baseenv = new.env(parent=parent.env(globalenv()))
  chunk.env = new.env(parent=ps.baseenv)

  # load functions from extra.code.file 
  if (!is.null(rps$extra.code.env)) {
    copy.into.env(source=rps$extra.code.env, dest = ps.baseenv)
  }

  
  chunk.ind = 1
  for (chunk.ind in 1:NROW(cdt)) {
    # store the start.env into
    if (precomp) {
      # make a shallow copy of chunk.env and store it in chunk.envs
      start.env = as.environment(as.list(chunk.env))
      parent.env(start.env) <- ps.baseenv
      chunk.envs[[chunk.ind]] = start.env
    }

    html = create.rps.chunk.html(cdt=cdt, chunk.ind=chunk.ind, chunk.env=chunk.env, quiet=quiet)
    chunk.sol.html[[chunk.ind]] = html
  }

  cdt$sol.html = chunk.sol.html

  # Determine which chunks can be tested
  # if we show the problem set in noeval mode
  cdt$can.noeval.test = sapply(1:NROW(cdt), can.test.chunk.with.noeval,cdt=cdt)

  if (precomp) {
    cdt[["stud.env"]] = chunk.envs
  }

  rps$cdt = cdt
  rps$preknit = TRUE
  rps$precomp = precomp

  rps
}

can.test.chunk.with.noeval = function(chunk.ind, cdt) {
  restore.point("can.test.chunk.with.noeval")

  ck = cdt[chunk.ind,]
  noeval.tests = c("check.call","check.assign")
  test.exprs = ck$test.expr[[1]]
  for (expr in test.exprs) {
    for (call in expr) {
      name = as.character(call[[1]])
      if (!name %in% noeval.tests) return(FALSE)
    }
  }
  return(TRUE)
}

create.rps.chunk.html = function(cdt, chunk.ind, chunk.env, success.message=isTRUE(cdt$is.solved[[chunk.ind]]),eval=TRUE, echo=TRUE, quiet=FALSE) {



  txt = cdt$sol.txt[[chunk.ind]]
  if (cdt$num.e[[chunk.ind]] > cdt$num.e.task[[chunk.ind]]) {
    add = c("# Great, solved correctly!")
  } else {
    add = "# You successfully checked the chunk! "
  }
  if (cdt$points[[chunk.ind]]==1) {
    add = paste0(add, " (1 point)")
  } else if (cdt$points[[chunk.ind]]>0) {
    add = paste0(add, " (",cdt$points[[chunk.ind]], " points)")
  }
  txt = paste0(add,"\n",txt)

  
  name = cdt$chunk.name[[chunk.ind]]

  opt = default.out.chunk.options()
  copt = cdt$chunk.opt[[chunk.ind]]
  if (length(copt)>0) {
    opt[names(copt)] = copt
  }
  opt$eval = eval
  opt$echo = echo

  header = paste0("```{r '",name,"'",chunk.opt.list.to.string(opt,TRUE),"}")


  txt = c(header,sep.lines(txt),"```")

  html ="Evaluation error!"
  html = try(knit.to.html(text=txt, envir=chunk.env,fragment.only = TRUE,quiet = quiet))

  nali = make.chunk.nali(chunk.name = name, chunk.ind = chunk.ind, ps=NULL)
  chunkUI = nali$chunkUI
  # Add syntax highlightning
  if (!is.null(chunkUI)) {
    html = paste0(paste0(html,collapse="\n"),"\n",
     "<script>$('#",chunkUI," pre code').each(function(i, e) {hljs.highlightBlock(e)});</script>")
  }

  html
}


create.cdt.task.html = function(cdt) {
  restore.point("create.cdt.task.html")
  if(length(cdt$chunk.name)>0){
    task.html = sapply(1:length(cdt$chunk.name), create.task.chunk.html, cdt=cdt)
  } else {
    task.html = character()
  }    
  task.html
}

create.task.chunk.html = function(chunk.ind,cdt, eval=FALSE, echo=TRUE, quiet=TRUE) {
  restore.point("create.task.chunk.html")
  
  txt = paste0("# Not yet solved...\n# Press 'edit' to enter your code.\n\n",cdt$task.txt[[chunk.ind]])
  name = cdt$chunk.name[[chunk.ind]]
  opt = default.out.chunk.options()
  opt$eval = eval
  opt$echo = echo
  header = paste0("```{r '",name,"'",chunk.opt.list.to.string(opt,TRUE),"}")
  
  txt = c(header,sep.lines(txt),"```")
  html ="Evaluation error!"
  html = try(
    knit.to.html(text=txt,fragment.only = TRUE,quiet = quiet)
  )
  
  nali = make.chunk.nali(chunk.name = name, chunk.ind = chunk.ind, ps=NULL)
  chunkUI = nali$chunkUI
  html = paste0(html,collapse="\n")
  # Add syntax highlightning
  if (!is.null(chunkUI)) {
    html = paste0(html,"\n",
                  "<script>$('#",chunkUI," pre code').each(function(i, e) {hljs.highlightBlock(e)});</script>")
  }
  
  html
}


