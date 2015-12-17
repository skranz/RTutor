rtutor.update.stats.panel = function(app = getApp(),ps=get.ps(),...) {
  restore.point("rtutor.update.stats.panel")
  
  df = stats(do.display = FALSE,use.old.stats = FALSE)
  perc = df[NROW(df),2]
  html = html.table(df)
  html = paste0("<h4>You have solved ", perc, "% of the problem set...</h3><br>\n", html)
  setUI("uiProblemSetStats", fluidRow(column(width = 8, offset=2,HTML(html))))
} 

