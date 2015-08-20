examples.code_complete = function() {
  
  x = list(a=5,ba=10)
  linebuffer = "plot("
  linebuffer = "plo"
  linebuffer = "x$"
  cursorPosition=3
  ret = utils:::.win32consoleCompletion(linebuffer, cursorPosition,
                            check.repeat = TRUE,
                            minlength = -1)
  ret
  
  app = eventsApp()
  library(shinyjs)
  jsCode <- "
  shinyjs.pageCol = function(params){$('body').css('background', params);}
  
  shinyjs.codecompl = function() {
      editor__myace.execCommand('startAutocomplete');
  }
  "
 
  ui = fluidPage(
    useShinyjs(),
    extendShinyjs(text = jsCode),
    actionButton("btn","JS"),
    aceEditor(outputId = "myace",autoComplete = "enabled",mode = "r",hotkeys = list(tabKey="Tab|F1"))
  )
  
  aceHotkeyHandler("tabKey",function(editorId, text, cursor, selection, ...) {
    cat("Tab was pressed handler:\n")
    args = list(...)
    text = getInputValue(editorId)
    restore.point("tab_completion")
    txt = sep.lines(text)
    line = cursor$row+1
    cursorPosition = cursor$column+1
    
    #js$codecompl()
    js$pageCol(sample(c("red","white","blue","yellow"),1))
    
    ret = utils:::.win32consoleCompletion(linebuffer=txt[line], cursorPosition=cursor$column+1, check.repeat = TRUE,minlength = 1)

    print(list(...))
    print(text)
  })

  buttonHandler("btn", function(...) js$pageCol(sample(c("red","white","blue","yellow"),1)))
  
  app$ui = ui
  app$initHandler = function(session,...) {
    ac.myace <- myaceAutocomplete("myace",session)
    ac.myace$resume()
  }
  runEventsApp(app, launch.browser=rstudio::viewer)
  
}