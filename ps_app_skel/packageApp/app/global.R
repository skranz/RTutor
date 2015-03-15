library(PACKAGE_NAME)
setwd("./work") 
app = run.ps(user.name = "Guest",deploy.local = FALSE,
	         make.web.app = TRUE, save.nothing=FALSE,
	         offline=FALSE,sample.solution = FALSE)

app$verbose = FALSE
app$is.running = TRUE
#shinyApp(ui = app$ui, server = app$server)

#runEventsApp(app)
