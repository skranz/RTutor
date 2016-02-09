
RTutorTeacherApp = function(psapps, teachers,db.dir = paste0(getwd(),"/db"), data.dir = paste0(getwd(),"/data"), init.userid="", init.password="", app.url="", app.title="RTutor Teacher Center", smtp = NULL) {
  restore.point("RTutorTeacherApp")
  
  library(shinyjs)
  library(loginPart)
  library(RSQLite)
  
  app = eventsApp()
  

  psapps = lapply(psapps, rtutor.login.init.psa)
  app$glob$app.title = app.title
  app$glob$psapps = psapps
  app$glob$teachers = teachers
  
  login.fun = function(app=getApp(),userid,...) {
    restore.point("teacher.login.fun")
    
    if (!userid %in% app$glob$teachers) {
      setUI("teacherMainUI", fluidRow(column(width=10, offset=1,
        p("Your userid has no permission to use the teacher control center.")  
      )))
      return()
    }
    rtutor.teacher.handlers()
    show.rtutor.teacher.main()
  }

  db.arg = list(dbname=paste0(db.dir,"/userDB.sqlite"),drv=SQLite())

  lop = loginPart(db.arg = db.arg, login.fun=login.fun, app.url=app.url, app.title=app.title,init.userid=init.userid, init.password=init.password,container.id = "teacherMainUI")
  set.lop(lop)
  lop.connect.db(lop=lop)
  lop$login$ui = lop.login.ui(lop)
  
  if (is.null(smtp)) smtp = lop.get.smtp()
  lop$smtp = smtp  

  appInitHandler(function(session,...) {
    initLoginDispatch(lop)
  })

  app$ui = tagList(
    fluidPage(
      uiOutput("teacherMainUI")
    )
  )
  app$lop = lop
  app
}

import.psapps.stats = function(psapps) {
  li = lapply(psapps, import.psa.stats)
  bind_rows(li)
}

import.psa.stats = function(psa) {
  restore.point("import.psa.stats")

  ups.dir = psa$ups.dir
  rps.file = paste0(psa$appdir,"/",psa$psname,".rps")
  load(rps.file)
  
  files = list.files(ups.dir, pattern=glob2rx("*.ups"),full.names = TRUE)  
  li = lapply(files, function(file) {
    restore.point("import.psa.stats.inner")
    
    load(file)
    df = stats(ups = ups, rps=rps,do.display = FALSE)
    #colnames(df) = c("Excercise","Solved (%)","Points", "Max. Points", "Hints")
    colnames(df) = c("ex","share","points", "max.points", "hints")

    cbind(data_frame(ps.name=rep(psa$psname,NROW(df)),user.name=rep(ups$user.name,NROW(df))),df)
  })
  df = bind_rows(li)
  df = filter(df, ex != "Total")
  df
}

update.teacher.stat = function(app=getApp()) {
  restore.point("update.teacher.stat")
  
  psapps = app$glob$psapps

  app$stats = stats = import.psapps.stats(psapps)
  app$pustats = pustats = app$stats %>% 
    group_by(ps.name, user.name) %>%
    summarise(points=sum(points), max.points=sum(max.points)) %>%
    mutate(share=points / max.points) %>%
    group_by(ps.name) %>%
    arrange(-points)
  library(tidyr)
  
  app$ps.max.points = (group_by(pustats, ps.name) %>% summarise(max.points=mean(max.points)))$max.points
  app$max.points = sum(app$ps.max.points)
  
  upoints = spread(select(pustats, user.name, ps.name, points),key=ps.name, value=points)
  
  
  mat = as.matrix(upoints[,-1])
  total = rowSums(mat,na.rm = TRUE)
  
  app$ustats = ustats = data.frame(user.name=upoints$user.name,total.perc = round(100*(total/app$max.points),1), total = total, max.points=app$max.points, upoints[,-1]) %>% arrange(-total)

  
  of.df = as.data.frame(lapply(1:(NCOL(upoints)-1),function(ps.ind) {
    paste0(unlist(upoints[,ps.ind+1])," of ",app$ps.max.points[[ps.ind]])  
  }))
    

    
  ushow =  cbind(data_frame(
    upoints$user.name,
    round(100*(total/app$max.points),1),
    total,
    app$max.points
  ), of.df)
  
  colnames(ushow) = c("Student","Total %","Total Points","Max. Points", colnames(ustats)[-(1:4)])
  app$ushow = ushow[order(-ushow[,2]),]
}

show.rtutor.teacher.main = function(app=getApp(),...) {
  restore.point("show.rtutor.login.main")
  
  update.teacher.stat()

  html = html.table(app$ushow)
  ui = fluidRow(column(offset = 1, width=10,
    h4(app$glob$app.title),
    actionButton("refreshTeacherStatsBtn","Refresh"),
    downloadButton('downloadOverview', 'Download Overview'),
    downloadButton('downloadDetails', 'Download Details'),
    h4("Results of problem sets..."),
    HTML(html)
  ))  
  setUI("teacherMainUI", ui)
} 

rtutor.teacher.handlers = function(app=getApp(), session=app$session) {
  buttonHandler("refreshTeacherStatsBtn",show.rtutor.teacher.main)

  setDownloadHandler("downloadOverview",
    filename = 'overview.csv',
    content = function(file) {
      write.csv(getApp()$ushow, file,row.names = FALSE)
    }
  ) 
  setDownloadHandler("downloadDetails",
    filename = 'details.csv',
    content = function(file) {
      write.csv(getApp()$stats, file,row.names = FALSE)
    }
  )  

}
