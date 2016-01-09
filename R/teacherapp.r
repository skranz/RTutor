
RTutorTeacherApp = function(psapps, teachers,db.dir = paste0(getwd(),"/db"), init.userid="", init.password="", app.url="", app.title="RTutor Teacher Center") {
  restore.point("RTutorTeacherApp")
  
  library(shinyjs)
  library(loginPart)
  library(RSQLite)
  
  app = eventsApp()

  psapps = lapply(psapps, rtutor.login.init.psa)
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
    show.rtutor.teacher.main()
  }

  db.arg = list(dbname=paste0(db.dir,"/userDB.sqlite"),drv=SQLite())

  lop = loginPart(db.arg = db.arg, login.fun=login.fun, app.url=app.url, app.title=app.title,init.userid=init.userid, init.password=init.password,container.id = "teacherMainUI")
  set.lop(lop)
  lop.connect.db(lop=lop)
  lop$login$ui = lop.login.ui(lop)
  lop$smtp = lop.get.smtp()

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

show.rtutor.teacher.main = function(app=getApp(), import.stats = TRUE) {
  restore.point("show.rtutor.login.main")
  
  psapps = app$glob$psapps
  if (import.stats) {
    app$stats = stats = import.psapps.stats(psapps)
    app$pustats = pustats = app$stats %>% 
      group_by(ps.name, user.name) %>%
      summarise(points=sum(points), max.points=sum(max.points)) %>%
      mutate(share=points / max.points) %>%
      group_by(ps.name) %>%
      arrange(-points)
    library(tidyr)
    
    app$ps.max.points = (group_by(pustats, ps.name) %>% summarise(max.points=mean(max.points)))$max.points
    app$max.points = sum(max.points)
    
    upoints = spread(select(pustats, user.name, ps.name, points),key=ps.name, value=points)
    
    
    mat = as.matrix(upoints[,-1])
    total = rowSums(mat)
    app$ustats = ustats = data.frame(user.name=upoints$user.name,total.perc = round(100*(total/app$max.points),1), total = total, max.points=app$max.points, upoints[,-1]) %>% arrange(-total)
    
    ushow =  data.frame(
      "Student"= upoints$user.name,
      "Total %" = round(100*(total/app$max.points),1),
      "Total Points" = total,
      "Max. Points"= app$max.points,
      paste0(upoints[,-1]," / ",app$ps.max.points)
    )
    colnames(ushow) = c("Student","Total %","Total Points","Max. Points", colnames(ustats)[-(1:4)])
    app$ushow = ushow[order(-ushow[,2]),]
    
  }
  html = html.table(app$ushow)
  ui = fluidRow(column(offset = 1, width=10,
    h3("Results of problem sets..."),
    br(),
    HTML(html)
  ))  
  setUI("teacherMainUI", ui)
} 
