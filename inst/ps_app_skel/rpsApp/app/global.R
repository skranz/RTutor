library(RTutor)
DEPENDS_LIBRARIES

setwd("./work")
app =  show.ps(PS_OPTIONS)

app$verbose = FALSE
appReadyToRun(app)

#shinyApp(ui = app$ui, server = app$server)
#runEventsApp(app)
