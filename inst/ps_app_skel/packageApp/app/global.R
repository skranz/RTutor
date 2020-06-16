library(PACKAGE_NAME)
setwd("./work") 
app = run.ps(PS_OPTIONS)

app$verbose = FALSE
appReadyToRun(app)
#shinyApp(ui = app$ui, server = app$server)

#runEventsApp(app)
