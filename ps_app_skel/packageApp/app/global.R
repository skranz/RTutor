library(PACKAGE_NAME)
setwd("./work") 
app = run.ps(user.name = "Guest",deploy.local = FALSE,
	         make.web.app = TRUE, save.nothing=TRUE,
	         offline=FALSE,sample.solution = FALSE)

app$verbose = FALSE
appReadyToRun(app)
#shinyApp(ui = app$ui, server = app$server)

#runEventsApp(app)
