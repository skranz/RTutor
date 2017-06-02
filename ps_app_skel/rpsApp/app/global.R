library(RTutor)
DEPENDS_LIBRARIES

setwd("./work")
app =  show.ps(user.name = "Guest",
			   ps.name = "PS_NAME",
	           make.web.app = TRUE, save.nothing=TRUE,
	           offline=FALSE,sample.solution = FALSE)

app$verbose = FALSE
appReadyToRun(app)

#shinyApp(ui = app$ui, server = app$server)
#runEventsApp(app)
