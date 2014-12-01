
update.data.explorer.ui = function(ps=get.ps()) {
  
  restore.point("update.data.explorer.ui")
  chunk.ind = ps$chunk.ind
  cdt = ps$cdt
  chunk.name = cdt$chunk.name[[chunk.ind]]
  stud.env = ps$stud.env
  data = ps$de.dat
  
  updateUI("radioDataExplorerUI",{
      cat("\noutput$radioDataExplorerUI")
      variable.selector.ui(stud.env, chunk.name)    
  })

  make.data.explorer.handlers()
  vars = get.environment.data.var()
  
  if (length(vars)>0) {
    var = vars[1]
    cat("\nupdate for var ", var)
    update.data.explorer.data(var)
  } else {
    updateDataTable("tableDataExplorer",NULL) 
    updateUI("variablesDescrUI",NULL) 
  }
    
}


set.data.explorer.data  = function(var=NULL, env = ps$stud.env, ps=get.ps()) { 
  if (is.null(var)) {
    var = ps$session$input$radioDataExplorer
  }
  restore.point("set.data.explorer.data")
  if (length(var)>0 & is.character(var)) {
    if (exists(var, env)) {
      ret = get(var,env)
      if (is.matrix(ret)) {
        ret = as.data.frame(ret)
      }
      ps$de.dat = ret
    }
  } else {
    ps$de.dat = NULL
  }
  return(ps$de.dat)
}

update.data.explorer.data  = function(var) {
  data = set.data.explorer.data(var)
  updateDataTable("tableDataExplorer",signif.cols(data,4),
      options = list(orderClasses = TRUE,
                     lengthMenu = c(5, 10, 25,50,100),
                     pageLength = 5)) 
  updateUI("variablesDescrUI",{make.var.descr.ui(data)}) 
  
}

#examples.rtutor.shiny()
make.data.explorer.handlers = function() {
  restore.point("data.explorer.server")  
  changeHandler("radioDataExplorer", function(value,...,ps=get.ps()) {
    cat("changeRadioDataExplorer...")
    update.data.explorer.data(var=value)
  })
}



#examples.rtutor.shiny()
data.explorer.server = function() {
  restore.point("data.explorer.server")
  ca = quote({
    output$radioDataExplorerUI = renderUI({
      cat("\noutput$radioDataExplorerUI")
      #browser()
      chunk.ind = ps$chunk.ind
      r.data.counter$counter
      ps = get.ps()
      cdt = ps$cdt
      stud.env = ps$stud.env
      chunk.name = cdt$chunk.name[[chunk.ind]]
      variable.selector.ui(stud.env, chunk.name)
    })
    r.dataExplorerData = reactive({
      cat("r.dataExplorerData")
      r.data.counter$counter
      
      var = input$radioDataExplorer
      cat("Selected variable: ", var)
      ps = get.ps()
      env = ps$stud.env
      if (length(var)>0 & is.character(var)) {
        if (exists(var, env)) {
          ret = get(var,env)
          if (is.matrix(ret)) {
            ret = as.data.frame(ret)
          }
          return(ret)
        }
      }
      return(NULL)      
    })
    
    output$tableDataExplorer = renderDataTable({
      signif.cols(r.dataExplorerData(),4)
     }, options = list(bSortClasses = TRUE, aLengthMenu = c(5, 10, 25,50,100),iDisplayLength = 5))
    
    output$dataSummariseUI = renderUI({
      dat = r.dataExplorerData()
      data.summarise.ui(dat)
    })
    output$dataPlotUI = renderUI({
      dat = r.dataExplorerData()
      data.plot.ui(dat)
    })
    output$variablesDescrUI = renderUI({
      dat = r.dataExplorerData()
      #HTML(get.var.descr.html(dat))
      make.var.descr.ui(dat)
    })

    output$plotData = renderPlot({
      counter =  input$showPlotBtn
      if (counter==0)
        return(NULL)
      dat = isolate(r.dataExplorerData())
      code = isolate(make.ggplot.code(data.name="dat",type=input$plotTypeInput, xvar=input$xvarInput, yvar=input$yvarInput, colorVar = input$colorvarInput))
      eval(code)
    })
    
    
    output$tableDataSummarise = renderDataTable({
      dat = r.dataExplorerData()
      groups = input$groupByInput
      if (length(groups)>0)
        dat = s_group_by(dat, groups)
      cols = input$colSelectInput
      if (length(cols)==0)
        cols = colnames(dat)
      
      out = xsummarise_each_(dat,funs(xmean, xmin,xmax), cols)
      out
     }, options = list(aLengthMenu = c(5, 10, 25,50,100),iDisplayLength = 25))

    
  })
  as.list(ca[-1])
}

my.env = as.environment(list(x=data.frame(a=1, b= 1:5),y=3, m=matrix(runif(9),3,3)))


data.explorer.ui = function() {
  fluidRow(
    column(2,uiOutput("radioDataExplorerUI")),
    column(10,
        tabsetPanel(
          tabPanel("Data",dataTableOutput("tableDataExplorer")),
          tabPanel("Description",uiOutput("variablesDescrUI"))
          #tabPanel("Summary", uiOutput("dataSummariseUI")),
          #tabPanel("Plot", uiOutput("dataPlotUI"))
        )
    )
  )
}

get.environment.data.var=function(env=ps$stud.env, ps=get.ps()) {
  vars = ls(env)
  dvars = unlist(lapply(vars, function(var) {
    x = env[[var]]
    if(is.matrix(x) | is.data.frame(x))
      return(var)
    return(NULL)
  }))
  dvars
}

variable.selector.ui = function(env=ps$stud.env, chunk.name,ps=get.ps()) {
  restore.point("variable.selector.ui")
  
  dvars = get.environment.data.var(env)
  if (length(dvars)>0) {
    ret=radioButtons("radioDataExplorer", label = paste0("Chunk ", chunk.name), choices = as.list(dvars), selected = dvars[1] )
  } else {
   ret=HTML("The current chunk environment has no data frames or matrixes.")
  }
  ret
}


data.summarise.ui = function(data) {
  if (is.null(data))
    return(NULL)
  
  vars = colnames(data)
  group_by_ui = selectizeInput("groupByInput",label="group by", choices=vars, multiple=TRUE)
  col_select_ui = selectizeInput("colSelectInput",label="variables", choices=vars, multiple=TRUE) 
  #funs = c("mean","sd","min","max")
  #fun_select_ui = selectizeInput("funSelectInput",label="functions", choices=funs, selected=funs, multiple=TRUE)
  
  changeHandler("groupByInput",update.data.explorer.summarise)
  changeHandler("colSelectInput",update.data.explorer.summarise)
  #changeHandler("funSelectInput",update.data.explorer.summarise)

  update.data.explorer.summarise()
  fluidRow(
    fluidRow(
      column(4,group_by_ui),
      column(4,col_select_ui)
    ),
    dataTableOutput("tableDataSummarise")
  )
  
}

update.data.explorer.summarise = function(dat=ps$de.dat,input=ps$session$input,ps = get.ps(), session=ps$session,...) {
  restore.point("update.data.explorer.summarise")
  
  updateDataTable("tableDataSummarise",{
    #browser()
    cat("\ntrigger new summarise...")
    groups = isolate(input$groupByInput)
    if (length(groups)>0)
      dat = s_group_by(dat, groups)
    cols = isolate(input$colSelectInput)
    if (length(cols)==0)
      cols = colnames(dat)
        
    out = xsummarise_each_(dat,funs(xmean,xsd,xmax,xmin), cols)
    out
  }, options = list(lengthMenu = c(5, 10, 25,50,100),pageLength = 25)
  )
}


data.plot.ui = function(data) {
  if (is.null(data))
    return(NULL)
  
  vars = c("",colnames(data))
  plot_type_ui = selectizeInput("plotTypeInput",label="plot type", choices=c("point","line"), multiple=FALSE)
  xvar_ui = selectizeInput("xvarInput",label="x", choices=vars, multiple=FALSE)
  yvar_ui = selectizeInput("yvarInput",label="y", choices=vars, multiple=FALSE)
  xfacet_ui = selectizeInput("xfacetInput",label="x facet", choices=vars, multiple=FALSE)
  yfacet_ui = selectizeInput("yfacetInput",label="y facet", choices=vars, multiple=FALSE)

  colorvar_ui = selectizeInput("colorvarInput",label="color group", choices=vars, multiple=FALSE)
  
  buttonHandler("showPlotBtn",update.data.explorer.plot)
  
  fluidRow(
    fluidRow(
      column(4,plot_type_ui,colorvar_ui),
      column(4,xvar_ui,xfacet_ui),
      column(4,yvar_ui,yfacet_ui)
    ),
    actionButton("showPlotBtn","Show Plot"),
    plotOutput("plotData")
  )
  
}


update.data.explorer.plot = function(dat=ps$de.dat,input=ps$session$input,ps = get.ps(),...) {
  updatePlot("plotData", {
    code = isolate(make.ggplot.code(data.name="dat",type=input$plotTypeInput, xvar=input$xvarInput, yvar=input$yvarInput, colorVar = input$colorvarInput))
    eval(code)
  })
}


make.ggplot.code = function(data.name="dat",type="line",xvar="",yvar="",colorvar="",xfacet="",yfacet="",...) {
  restore.point("make.ggplot.code")
  li = c(x=xvar,y=yvar,color=colorvar)
  str = NULL
  for (i in seq_along(li)) {
    if (nchar(li[[i]])>0) {
      str = paste0(str,",", names(li)[i],"=",li[i])
    }        
  }
  geom = type
  
  com = paste0("qplot(data=",data.name,str,",geom='",geom,"')")
  parse(text=com,srcfile=NULL)  
}

make.var.descr.ui = function(dat) {
 
  restore.point("make.var.descr.html")
  dt = get.var.descr.dt(dat=dat)
  if (!is.null(dt)) {
    title =  paste0(dt$var,": ", dt$descr)
  } else {
    title = colnames(dat)
  }
  body = lapply(dat, function(v) {
    HTML(var.summary.html(v))
  })
  panels = lapply(seq_along(title), function(i) {
    bsCollapsePanel(title[i], body[[i]])    
  })
  do.call(bsCollapse, panels)
}


var.summary.html = function(v,...) {
  restore.point("var.summary.html ")

  library(hwriter)
  dli = describe.var(v)  
  lab1 = paste0(names(dli)[1:3],":")
  val1 = as.character(dli[1:3])

  top = get.top.x.obs(v,3)
  top.var = c(as.character(top$var), rep("",3))
  top.share = c(paste0(round(top$share*100,1),"%"),rep("",3))
  
  if (length(dli)>3) {
    dli = c(dli,list("","",""))
    lab2 = paste0("  ",names(dli)[4:6],":")
    val2 = as.character(xsignif(dli[4:6],4))
    lab3 = paste0("  ",1:3, ": ",top.var[1:3])
    val3 = top.share[1:3]
    df = data.frame(lab1,val1,lab2,val2,lab3,val3)
    colnames(df)=c("",""," Summary","         ","Most common values","Share")
  } else {
    lab3 = paste0("  ",top.var[1:3])
    val3 = top.share[1:3] 
    df = data.frame(lab1,val1,lab3,val3)
    colnames(df)=c("","","value","share")
  }
  out = hwrite(df, NULL, border=2, row.names=FALSE, cellpadding=5)
  out
}

xround = function(x,digits=0) {
  if (is.list(x))
    return(lapply(x, xround, digits=digits))
  if (is.numeric(x))
    x = round(x, digits)
  x
}


xsignif = function(x,digits=6) {
  if (is.list(x))
    return(lapply(x, xsignif, digits=digits))
  if (is.numeric(x))
    x = signif(x, digits)
  x
}

xmean = function(..., na.rm=TRUE) {
  tryCatch(suppressWarnings(mean(..., na.rm=na.rm)),
           error = function(e){return(NA)})
}
xmax = function(..., na.rm=TRUE) {
  tryCatch(suppressWarnings(max(..., na.rm=na.rm)),
           error = function(e){return(NA)})
}
xmin = function(..., na.rm=TRUE) {
  tryCatch(suppressWarnings(min(..., na.rm=na.rm)),
           error = function(e){return(NA)})
}
xsd = function(..., na.rm=TRUE) {
  tryCatch(suppressWarnings(sd(..., na.rm=na.rm)),
           error = function(e){return(NA)})
}


