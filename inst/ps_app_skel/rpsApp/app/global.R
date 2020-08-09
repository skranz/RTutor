library(RTutor)
DEPENDS_LIBRARIES

# does not seem to work anymore on shinyapps.io
#setwd("./work") 
app =  show.ps(PS_OPTIONS)

app$verbose = FALSE
appReadyToRun(app)

#shinyApp(ui = app$ui, server = app$server)
#runEventsApp(app)
