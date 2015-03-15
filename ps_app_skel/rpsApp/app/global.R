library(RTutor)
app =  show.ps(user.name = "Guest",
			   ps.name = "PS_NAME",
			   dir = "./work",
			   rps.dir = "./rps",
	           make.web.app = TRUE, save.nothing=FALSE,
	           offline=FALSE,sample.solution = FALSE)

setwd("./work")
app$verbose = FALSE
app$is.running = TRUE

#shinyApp(ui = app$ui, server = app$server)
#runEventsApp(app)
