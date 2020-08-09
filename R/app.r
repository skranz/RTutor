# Make a problem set app suited for hosting RTutor in the web
# 
# The user first opens the login app, which creates a session file
# and then calls this app.
#
#
# @param load.sav shall the last saved be loaded?
# @param sample.solution shall the sample solution be shown
# @param run.solved if sample.solution or load.sav shall the correct chunks be automatically run when the problem set is loaded? (Starting the problem set then may take quite a while)
# @param import.rmd shall the solution be imported from the rmd file specificed in the argument rmd.file
# @param lauch.browser if TRUE (default) show the problem set in the browser. Otherwise it is shown in the RStudio viewer pane
# @param catch.errors by default TRUE only set FALSE for debugging purposes in order to get a more informative traceback()
# @param offline (FALSE or TRUE) Do you have no internet connection. By default it is checked whether RTutor can connect to the MathJax server. If you have no internet connection, you cannot render mathematic formulas. If RTutor wrongly thinks you have an internet connection, while you don't, your chunks may not show at all. If you encounter this problem, set manually offline=TRUE.
# @param is.solved DEPRECEATED
# @param html.data.frame shall data.frames and matrices be printed as html table if a chunk is checked? (Default=TRUE)
# @param table.max.rows the maximum number of rows that is shown if a data.frame is printed as html.table
# @param round.digits the number of digits that printed data.frames shall be rounded to
RTutorPSApp = function(ps.name, user.name="default_user", sample.solution=FALSE, run.solved=load.sav, import.rmd=FALSE, rmd.file = paste0(ps.name,"_",user.name,"_export.rmd"), catch.errors = TRUE, dir=getwd(), rps.dir=dir, ups.dir=paste0(dir,"/ups"), offline=!can.connect.to.MathJax(), left.margin=2, right.margin=2, save.nothing=FALSE, show.solution.btn = TRUE, show.data.exp = FALSE, disable.graphics.dev=TRUE, clear.user=FALSE, check.whitelist=!is.null(wl), wl=NULL, verbose=FALSE, html.data.frame=TRUE,table.max.rows=25, round.digits=8, signif.digits=8, knit.print.opts=make.knit.print.opts(html.data.frame=html.data.frame,table.max.rows=table.max.rows, round.digits=round.digits, signif.digits=signif.digits), precomp=FALSE, noeval=FALSE, need.login=TRUE, sessions.dir = paste0(dir,"/sessions"), session.key = NULL, use.secure.eval=TRUE, secure.eval.timeout = 10, secure.eval.profile=NULL, hint.noeval=noeval, show.points=TRUE, replace.sol=precomp, ups.save = default.ups.save(chunk.ind=TRUE, code=!(replace.sol | noeval)), log.file = paste0(dir,"/log/",ps.name,".log"), session.timeout.sec=300,  ...) {

  cat("\nInitialize problem set, this may take a while...")
  app = eventsApp(verbose = verbose)

   
  #browser()
  ps = init.shiny.ps(
    ps.name=ps.name, user.name=user.name, sample.solution=sample.solution,
    import.rmd=import.rmd, rmd.file=rmd.file,
    dir=dir, rps.dir=rps.dir, ups.dir=ups.dir, save.nothing=save.nothing,
    show.solution.btn = show.solution.btn, show.data.exp=show.data.exp,
    clear.user=clear.user,
    check.whitelist=check.whitelist, wl=wl,
    precomp=precomp, noeval=noeval,
    replace.sol=replace.sol,
    ups.save = ups.save, log.file=log.file,
    ...
  )
  
  ps$show.points = show.points
  ps$hint.noeval = hint.noeval
  ps$use.secure.eval = use.secure.eval
  ps$secure.eval.timeout = secure.eval.timeout
  ps$secure.eval.profile = secure.eval.profile
  
  if (isTRUE(ps$use.secure.eval)) {
    if (is.null(secure.eval.profile)) {
      stop("You need to specify the name of your apparmor profile in the argument 'secure.eval.profile'")
    }
  }
  
  ps$need.login = need.login
  ps$sessions.dir = sessions.dir
  
  ps$catch.errors = catch.errors
  ps$offline=offline
  ps$left.margin = left.margin
  ps$right.margin = right.margin

  # Replace knit.print.funs in globalenv
  #knit.print.funs = make.knit.print.funs(knit.print.opts)
  #old.knit.print.funs = replace.fields(dest=globalenv(), source=knit.print.funs)
  
  # We need now to explicitly call registerS3method
  register.knit.print.functions(knit.print.opts)

  
  restore.point("RTutorPSApp")

  n = NROW(ps$cdt)


  
  ps$ps.ui = make.rtutor.ui(just.inner=TRUE)
  
  
  ex.inds = 1:NROW(ps$edt)
  for (ex.ind in ex.inds)
    show.ex.ui(ex.ind)

  for (chunk.ind in 1:n) {
    make.chunk.handlers(chunk.ind=chunk.ind)
  }

  app$ui = make.rtutor.page.ui(inner=uiOutput("psMainUI"),ps=ps)

  
  #setAppUI(ui, app)

  ps$session.key = session.key
  app$initHandler = function(session, input, output,app,...) {
    # make local copy of ps
    ops = get.ps(TRUE)
    ops$running.web.app = TRUE
    ps = copy.ps.for.session(ops)

    app$ps = ps
    ps$session = session
    ps$input = input
    ps$output = output
    
    
    # autocomplete in first open chunk
    if(nrow(ps$cdt)>0){
      set.chunk.autocomp.observer(inputId = ps$cdt$nali[[1]]$editor, chunk.ind = 1)
    }
    
    rtutor.observe.html.query(app=app, ps=ps, session.timeout.sec=session.timeout.sec)
  }

  if (disable.graphics.dev) {
    try(png("NUL"),silent=TRUE)
    on.exit(try(dev.off(),silent=TRUE), add=TRUE)
  }
  
  return(app)


  #runEventsApp(app=app,ui=ui,launch.browser=launch.browser, quiet=FALSE)

}




# This function must be called in the initHandler of the app
rtutor.observe.html.query = function(app=getApp(), ps = get.ps(), session.timeout.sec=300) {
  restore.point("rtutor.login.dispatch")
  session = app$session
  observe(priority = -100,x = {
    query <- parseQueryString(session$clientData$url_search)
    if (is.null(query$key)) {
      query$key = ps$session.key
    }
    rtutor.dispatch.html.query(query, session.timeout.sec=session.timeout.sec)
  })
}

rtutor.dispatch.html.query = function(query, app=getApp(), ps = get.ps(), session.timeout.sec=300) {
  restore.point("rtutor.dispatch.html.query")
  
  if (!isTRUE(ps$need.login)) {
    setUI("psMainUI", ps$ps.ui)
    return()
  }
  failed.ui = rtutor.failed.login.ui()
  
  key = query$key
  if (is.null(key)) {
    setUI("psMainUI", failed.ui)
    return()
  }

  # check if session file exists
  file = paste0(ps$sessions.dir,"/",key,".ses")
  if (!file.exists(file)) {
    setUI("psMainUI", failed.ui)
    return()
  }

  # load ses
  ses = readRDS(file=file)
  now = as.numeric(Sys.time())
  if (isTRUE(now > as.numeric(ses$time)+session.timeout.sec)) {
    html="<h2>Timout. Your session is not active anymore. Please login again.</h2>"
    setUI("psMainUI", HTML(html))
    return()
  }
  
  rtutor.show.user.session(user.name=ses$userid, ps=ps)

}

rtutor.show.user.session = function(user.name, ps=get.ps()) {
  restore.point("rtutor.show.user.session")
  
  cat(user.name)
  
  ups = load.ups(user.name=user.name,ps = ps)
  ps$ups = ups
  ps$user.name = user.name
  cdt = ps$cdt
  if (is.null(ups$chunk.ind)) ups$chunk.ind = 1

  if (ps$noeval | isTRUE(ps$precomp)) {
    changed = ups$cu$solved != ps$cdt$is.solved
    changed[unique(c(ps$chunk.ind,ups$chunk.ind))] = TRUE
    
    cdt$is.solved = ups$cu$solved
    
    rows = cdt$is.solved & changed
    cdt$stud.code[rows] = cdt$sol.txt[rows]

    rows = !cdt$is.solved & changed
    cdt$stud.code[rows] = cdt$task.txt[rows]
    
  } else {
    cdt$is.solved = FALSE
    cdt$stud.code = cdt$task.txt
    ups$chunk.ind = 1
    #stop("Apps without precomp or noeval are not yet implemented!")
  }
  
  ps$cdt = cdt
  shiny.set.ex.chunk(chunk.ind=ups$chunk.ind)

  
  chunk.inds = 1:NROW(ps$cdt)
  for (chunk.ind in chunk.inds) {
    update.chunk.ui(chunk.ind)
  }
  
  setUI("psMainUI", ps$ps.ui)
}

rtutor.failed.login.ui = function(app=getApp()) {
  html="<h2>Login failed</h2>"
  HTML(html)
}

shiny.set.ex.chunk = function(ex.ind=NULL, chunk.ind=NULL,to.top = is.null(chunk.ind), ps = get.ps(), app=getApp()) {
  
  restore.point("rtutor.set.ex.chunk")
  
  ps$cdt$mode = "output"
  
  if (is.null(chunk.ind) & is.null(ex.ind)) chunk.ind = 1
  
  if (is.null(chunk.ind)) {
    chunk.ind = which(ps$cdt$ex.ind==ex.ind)[1]
  } else if (is.null(ex.ind)) {
    ex.ind = ps$cdt$ex.ind[chunk.ind]
  }
  
  ps$cdt$mode[chunk.ind] = "input"
  
  try(updateTabsetPanel(session=app$session, inputId="exTabsetPanel", selected = paste0("exPanel",ex.ind)))
}




RTutorLoginApp = function(psapps, db.dir = paste0(getwd(),"/db"), init.userid="", init.password="",loginapp.url, psapp.url, app.title="RTutor Login", email.domain = NULL, check.email.fun = NULL, email.text.fun=default.email.text.fun, use.db=TRUE, main.header=rtutor.login.main.default.header(), smtp = NULL) {
  restore.point("RTutorLoginApp")
  
  
  library(shinyjs)
  library(loginPart)
  library(RSQLite)
  
  app = eventsApp()

  psapps = lapply(psapps, rtutor.login.init.psa)
  
  app$glob$psapps = psapps
  app$glob$cur.inst = rep(NA_integer_, length(psapps))
  
  login.fun = function(app=getApp(),userid,...) {
    show.rtutor.login.main(userid=userid, header=main.header)
  }

  if (is.null(check.email.fun)) {
    if (!is.null(email.domain)) {
      check.email.fun = function(email,...) {
        check.email.domain(email, email.domain)
      }
    } else {
      check.email.fun = function(email,...) {
        list(ok=TRUE,msg="")
      }
    }
  }
 
  db.arg = list(dbname=paste0(db.dir,"/userDB.sqlite"),drv=SQLite())

  lop = loginPart(db.arg = db.arg, login.fun=login.fun, check.email.fun=check.email.fun, email.text.fun = email.text.fun, app.url=loginapp.url, app.title=app.title,init.userid=init.userid, init.password=init.password,container.id = "mainUI")
  set.lop(lop)
  lop.connect.db(lop=lop)
  lop$login$ui = lop.login.ui(lop)
  
  if (is.null(smtp)) smtp = lop.get.smtp()
  lop$smtp = smtp

  appInitHandler(function(session,...) {
    initLoginDispatch(lop)
  })

  jsCode <- "shinyjs.openLink = function(url){window.open(url,'_blank');}"
  app$ui = tagList(
    useShinyjs(),
    extendShinyjs(text = jsCode),
    fluidPage(
      uiOutput("mainUI")
    )
  )
  app$lop = lop
  app
}

rtutor.login.init.psa = function(psa) {
  restore.point("rtutor.login.init.psa")
  
  psa = copy.into.missing.fields(psa, source=list(
    sessions.dir = paste0(psa$appdir,"/sessions"),
    ups.dir = paste0(psa$appdir,"/ups")
  ))
  psa
}

rtutor.login.main.default.header = function() {
"
<h3>Welcome {{userid}}</h3>
<br>
<p>Choose your problem set...</p>
<br>
"
}


show.rtutor.login.main = function(userid="guest", psapps = app$glob$psapps, app = getApp(), header = "") {
  restore.point("show.rtutor.login.main")
  
  psapps = lapply(psapps, function(psa) {
    psa$session.key = paste(sample(c(0:9, letters, LETTERS),40, replace=TRUE),collapse="")
    psa
  })

  
  psh = lapply(seq_along(psapps), function(i) {
    psa = psapps[[i]]
    url = psa$url
    html = paste0('<a href="', url,'" class="button" target="_blank">',psa$label,'</a>')
    link = HTML(html)
    
    btnId = paste0("openPSAppBtn__",i)
    linkUIId = paste0("openPSAppLinkUI__",i)
    buttonHandler(id=btnId,rtutor.open.psapp.click, i=i, psa=psa, url=url, userid=userid)
    
    list(
      bsButton(btnId,psa$label),
      uiOutput(linkUIId),
      hr()
    )
  })

  header = replace.whisker(header, list(userid=userid))
  ui = fluidRow(column(offset = 2, width=8,
    h3("Welcome ", userid),
    br(),
    p("Choose your problem set..."),
    br(),
    psh
  ))  
  setUI("mainUI", ui)
} 


rtutor.open.psapp.click = function(i,psa,url,userid,app=getApp(), ...) {
  restore.point("rtutor.open.psapp.click")
  glob = app$glob
  
  if (isTRUE(psa$instances>0)) {
    if (is.na(glob$cur.inst[[i]])) {
      glob$cur.inst[[i]] = sample.int(psa$instances,1)
    } else {
      glob$cur.inst[[i]] = ((glob$cur.inst[[i]]+1) %% psa$instances)+1
    }
    url = paste0(url,"_inst/i",glob$cur.inst[[i]],"/")
  }
  url = paste0(url,'?key=',psa$session.key)
  rtutor.write.session.file(userid=userid, session.key = psa$session.key, sessions.dir=psa$sessions.dir)
  
  js$openLink(url)

  linkUIId = paste0("openPSAppLinkUI__",i)
  
  
  html = paste0('<a href="', url,'" class="button" target="_blank">Click here if problem set does not open automatically.</a>')
  setUI(linkUIId,HTML(html))
}

rtutor.write.session.file = function(userid, session.key, sessions.dir) {
  restore.point("rtutor.write.session.file")
  
  ses = nlist(userid, time = Sys.time())
  file = paste0(sessions.dir, "/", session.key, ".ses")
  saveRDS(ses, file=file)
}