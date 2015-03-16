library(RTutor)
DEPENDS_LIBRARIES

setwd("./work")
app =  show.ps(user.name = "Guest",
			   ps.name = "PS_NAME",
	           make.web.app = TRUE, save.nothing=FALSE,
	           offline=FALSE,sample.solution = FALSE)

app$verbose = FALSE
app$is.running = TRUE

#shinyApp(ui = app$ui, server = app$server)
#runEventsApp(app)
