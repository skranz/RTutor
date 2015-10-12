examples.shiny.test.highlight = function() {
  library(knitr)
  library(shiny)
  stxt = 
    '```{r "static_chunk", eval=FALSE}
    # I am a static chunk
    1:10
    "Hi"
    ```'
  dtxt = 
    '```{r "dynamic_chunk", eval=FALSE}
    # I am a dynamic chunk
    1:5
    "Hello"
    ```'
  ui = fluidPage(
    tags$head(
      tags$script(src = 'http://yandex.st/highlightjs/7.3/highlight.min.js', type = 'text/javascript'),
      tags$script(src = 'http://yandex.st/highlightjs/7.3/languages/r.min.js', type = 'text/javascript'),
      tags$link(rel = 'stylesheet', type = 'text/css',
      href = 'http://yandex.st/highlightjs/7.3/styles/github.min.css')
    ),
    title = 'Knitr Examples',
    HTML(knitr::knit2html(text=stxt)),
    uiOutput('ex1')
  )
  server = function(input, output, session) {
    output$ex1 <- renderUI({
      dhtml = knitr::knit2html(text=dtxt)
      fluidRow(
        textInput("myInput in ex1", "My Input", value = "Text"),        
        HTML(paste0(dhtml,
             "
             <script>$('#ex1 pre code').each(function(i, e) {hljs.highlightBlock(e)});</script>
             "
        ))
      )
      
    })
  }

  runApp(list(
    ui=ui,
    server=server))

}



examples.shiny.test.mathjax = function() {

  mathJaxObj = withMathJax()
  obj = withMathJax(HTML('Dynamic output 1:  $$\\alpha^2$$'))
  
  ui = fluidPage(
    title = 'MathJax Examples',
    withMathJax(),
    HTML("Now134567"),
    uiOutput('ex1'),
    checkboxInput('ex5_visible', 'Show Example 5', FALSE),
    uiOutput('ex5')
  )
  server = function(input, output, session) {
    output$ex1 <- renderUI({
      fluidRow(
        obj,
        HTML('Dynamic output 2:  $$\\beta^2$$')
      )
    })
    output$ex5 <- renderUI({
      if (!input$ex5_visible) return()
      withMathJax(
        helpText('You do not see me initially: $$e^{i \\pi} + 1 = 0$$')
      )
    })
  }

  runApp(list(
    ui=ui,
    server=server)
  , launch.browser = rstudioapi::viewer)
}


examples.shiny.tabset = function() {

  last.counter = 0
  ui = fluidPage(
    tabsetPanel(id="tabset",
      tabPanel("Plot", value="Plot",
        actionButton("nextTab1","Next Tab"),
        plotOutput("plot")
      ),
      tabPanel("Summary",value="Summary",
        verbatimTextOutput("summary")
      ),
      tabPanel("Table", value="Table",tableOutput("table"))
    )
  
  )
  server = function(input, output, session) {
    observe({
      counter = input$nextTab1
      if (counter > last.counter) {
        cat("Button pressed")
        last.counter = counter
        updateTabsetPanel(session, "tabset", selected = "Summary")
      }
    })
  }

  runApp(list(ui=ui,server=server),launch.browser = rstudioapi::viewer)
}


examples.shiny.navlistPanel = function() {

  last.counter = 0
  ui = fluidPage(
    navlistPanel(id="tabset",widths=c(2,10),
      tabPanel("Plot", value="Plot",
        actionButton("nextTab1","Next Tab"),
        plotOutput("plot")
      ),
      tabPanel("Summary",value="Summary",
        verbatimTextOutput("summary")
      ),
      tabPanel("Table", value="Table",tableOutput("table"))
    )
  
  )
  server = function(input, output, session) {
    observe({
      counter = input$nextTab1
      if (counter > last.counter) {
        cat("Button pressed")
        last.counter = counter
        updateTabsetPanel(session, "tabset", selected = "Summary")
      }
    })
  }

  runApp(list(ui=ui,server=server),launch.browser = rstudioapi::viewer)
}
