suspend.chunk.autocomp.observer = function(chunk.ind, app=getApp(), ps=get.ps()) {
  restore.point("suspend.chunk.autocomp.observer")

  if (!is.null(ps$chunk.autcomp.observers[[chunk.ind]])) {
    ps$chunk.autcomp.observers[[chunk.ind]]$suspend()
  }
}

set.chunk.autocomp.observer = function(inputId, chunk.ind, app=getApp(), ps=get.ps()) {
  restore.point("set.chunk.autocomp.observer")
  
  if (is.null(app$session)) return()
  restore.point("set.chunk.autocomp.observer.inner")

  if (is.null(ps$chunk.autcomp.observers)) {
    ps$chunk.autcomp.observers = vector("list", NROW(ps$cdt))
  }
  if (!is.null(ps$chunk.autcomp.observers[[chunk.ind]])) {
    ps$chunk.autcomp.observers[[chunk.ind]]$resume()
    return()
  }
  session = app$session
  #cat("\nset autcomp.observer for chunk: ", chunk.ind)
  ps$chunk.autcomp.observers[[chunk.ind]] = shiny::observe({
    ps = get.ps()
    stud.env = ps$stud.env

    value <- session$input[[paste0("shinyAce_", inputId, "_hint")]]
    #cat("\n*******************************\nchunk autcomp observer called...", round(runif(1)*1000))
    #restore.point("chunk.autocomp.observer")
    if(is.null(value)) return(NULL)
    str = substring(value$linebuffer,1,value$cursorPosition)
    
    arg.words =  autcomp.function.args(str)
    var.words = autocomp.vars(str,var.env=stud.env)
    col.words = autocomp.cols(str,var.env=stud.env)
    meta = c(rep("arg",length(arg.words)),rep("var",length(var.words)),rep("col",length(col.words)))
    score = c(rep(103,length(arg.words)),rep(102,length(var.words)),rep(101,length(col.words)))      
    words = c(arg.words,var.words,col.words)
    df = data.frame(name=words,value=words,meta=meta, score=score)
    set.autocomplete.list(inputId,df=df)
  })
}

autocomp.js = function() {
  js = '
Shiny.addCustomMessageHandler("setAceAutoCompleteWords", function(msg) {
    var callback = $("#" + msg.id).data("autoCompleteCallback");
    if(callback !== undefined) callback(null, msg.words);
});
'
  js
}

set.autocomplete.list = function(inputId,  words="", score=100, meta="R", df=data.frame(name=words,value=words,score=score,meta=meta),session = app$session, app=getApp()) {
  restore.point("set.autocomplete.list")
  json = jsonlite::toJSON(df)
  session$sendCustomMessage(type = 'setAceAutoCompleteWords',list(id=inputId,words=json))
}


fun.arg.names = function(fun, envir=globalenv()) {
  if (is.character(fun)) {
    fun = try(get(fun, envir), silent=TRUE)
    if (!is.function(fun)) return(NULL)
  }
  names(formals(fun))
}

autocomp.find.current.function = function(str,vec = strsplit(str,"",fixed = TRUE)[[1]]
) {
  library(stringtools)
  open = which(vec=="(")
  if (length(open)==0) return(NULL)
  closed = which(vec==")")
  if (length(closed) >= length(open)) return(NULL)
  end.pos = open[length(open)-length(closed)]-1
  
  vec = vec[1:end.pos]
  noname = which(!vec %in% c(letters,LETTERS,"_",".",0:9)) 
  if (length(noname)==0) {
    start.pos = 1
  } else {
    start.pos = max(noname)+1
  }
  if (start.pos >= end.pos) return(NULL)
  
  substring(str, start.pos, end.pos)
}



find.varname.at.end.of.string = function(str, vec=strsplit(str,"",fixed = TRUE)[[1]]) {
  noname = which(!vec %in% c(letters,LETTERS,"_",".",0:9)) 
  if (length(noname)==0) {
    start.pos = 1
  } else {
    start.pos = max(noname)+1
  }
  substring(str,start.pos)
} 

autcomp.function.args = function(str, fun.env=globalenv()) {
  restore.point("autcomp.function.args")
  
  fun = autocomp.find.current.function(str)
  if (is.null(fun)) return(NULL)
  
  fun.args = fun.arg.names(fun, fun.env)
  if (length(fun.args)>0) {
    if ("..." %in% fun.args) {
      fun.args = c(paste0(setdiff(fun.args,"..."), " = "),"...")
    } else {
      fun.args = paste0(fun.args, " = ")      
    }
  }
  fun.args
}


autocomp.vars = function(str, var.env = NULL) {
  restore.point("autocomp.vars.and.cols")
  if (is.null(var.env)) return(NULL)
  vars = ls(var.env)
  return(vars)
}

autocomp.cols = function(str, var.env = NULL, vars=ls(var.env)) {
  restore.point("autocomp.vars.and.cols")
  words = NULL
  if (is.null(var.env)) return(NULL)
  cols = unique(unlist(lapply(vars, function(var) {
      val = var.env[[var]]
      if (is.data.frame(val)) return(names(val))
      NULL
  })))
  cols
}