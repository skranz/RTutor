debug.diff.rps = function() {
  setwd("D:/libraries/RTutor/examples/creditbooms")
  rps1 = load.rps(file = "old.rps")
  rps2 = load.rps(file = "new.rps")
  find.diffs(rps1,rps2)
  
  shiny.dt1 = rps1$shiny.dt
  shiny.dt2 = rps2$shiny.dt
  shiny.dt1$html[[10]]
  shiny.dt2$html[[10]]
  
}

difference.df = function(name, descr) {
  if (length(descr)==0) return(NULL)
  quick.df(name=rep(name,length.out=descr),descr=descr)
}


find.diffs = function(x,y,name="",...) {
  UseMethod("find.diffs")
} 

find.diffs.default = function(x,y,name="",...) {
  if (!(identical(x,y))) {
    return(quick.df(name=name,descr="not identical"))
  }
  return(NULL)
}

find.diffs.environment = function(x,y,name="",...) {
  #restore.point("find.diffs.environment")
  xvars = ls(x)
  yvars = ls(y)
  descr = NULL
  missing.y = setdiff(xvars,yvars)
  missing.x = setdiff(yvars,xvars)
  descr = c(descr,sc(missing.y, " missing in y."))
  descr = c(descr,sc(missing.x, " missing in x."))
  
  df = difference.df(name,descr)
  vars = intersect(xvars,yvars)
  li = lapply(vars, function(var) {
    find.diffs(x[[var]],y[[var]],name=paste0(name,"$",var))
  })
  df.li = as.data.frame(rbindlist(li))
  rbind(df,df.li)
}


find.diffs.data.frame = function(x,y,name="",...) {
  if (identical(x,y))
    return(NULL)
  restore.point("find.diffs.data.frame")

  descr = NULL
  if (NROW(x) != NROW(y))
    descr = c(descr,sc("nrow(x)=",nrow(x)," nrow(y)=",nrow(y)))
    
  xvars = names(x)
  yvars = names(y)
  descr = NULL
  missing.y = setdiff(xvars,yvars)
  missing.x = setdiff(yvars,xvars)
  descr = c(descr,sc("col ",missing.y, " missing in y."))
  descr = c(descr,sc("col ",missing.x, " missing in x."))
  
  df.li = NULL
  if (NROW(x) == NROW(y)) {
    cols = intersect(xvars,yvars)
    li = lapply(cols, function(col) {
      find.diffs(x[[col]],y[[col]],name=paste0(name,"$",col))
    })
    df.li = as.data.frame(rbindlist(li))
  }
  
  if (length(descr)==0 & length(df.li)==0) {
    descr="not identical"
  }
  df = difference.df(name,descr)
  rbind(df,df.li)
}


find.diffs.list = function(x,y,name="",...) {
  if (identical(x,y))
    return(NULL)
  restore.point("find.diffs.list")

  descr = NULL
  
  if (length(x) != length(y)) {
    descr = c(descr,sc("nrow(x)=",nrow(x)," nrow(y)=",nrow(y)))
  }
  
  named.x = !is.null(names(x))
  named.y = !is.null(names(y))
  
  if (named.x != named.y) {
    if (named.x)
      descr = c(descr,sc("x is named, y is unnamed"))
    if (named.y)
      descr = c(descr,sc("x is unnamed, y is named"))
    
    df = difference.df(name,descr)
    return(df)
  }
  
  if (!named.x & length(x) != length(y)) {
    df = difference.df(name,descr)
    return(df)
  }
    
  if (!named.x) {
    li = lapply(seq.int(1,length(x)), function(i) {
      find.diffs(x[[i]],y[[i]],name=paste0(name,"[[",i,"]]"))
    })
    df.li = as.data.frame(rbindlist(li))
  } else {
    xvars = names(x)
    yvars = names(y)
    descr = NULL
    missing.y = setdiff(xvars,yvars)
    missing.x = setdiff(yvars,xvars)
    descr = c(descr,sc("col ",missing.y, " missing in y."))
    descr = c(descr,sc("col ",missing.x, " missing in x."))
    
    df.li = NULL
    if (NROW(x) == NROW(y)) {
      cols = intersect(xvars,yvars)
      li = lapply(cols, function(col) {
        find.diffs(x[[col]],y[[col]],name=paste0(name,"$",col))
      })
      df.li = as.data.frame(rbindlist(li))
    }
  }

  if (length(descr)==0 & length(df.li)==0) {
    descr="not identical"
  }
  df = difference.df(name,descr)
  rbind(df,df.li)
}


debug.show.shiny.dt = function() {
  ps = get.ps()
  shiny.dt = ps$rps$shiny.dt
  rows = 1:NROW(shiny.dt)
  
  for (row in rows) {
    cat("\n\nHTML fragment", row, " exercise " , shiny.dt$ex.ind[row],"\n\n")
    html = as.character(shiny.dt$html[[row]])
    cat(html)
    show.html(html)
    readline(prompt="Press Enter")
  }
}

show.html = function(html, browser=FALSE) {
  #html = shiny.dt$html[[1]]
  
  
  htmlFile <- tempfile(fileext=".html")
  htmlPage <- paste(html)
  cat(htmlPage, file=htmlFile)

  if (browser) {
    utils::browseURL(htmlFile)
    return()
  }
  
  viewer <- getOption("viewer")
  if (!is.null(viewer) &&
        is.function(viewer)){
    # (code to write some content to the file)
    viewer(htmlFile)
  }else{
    utils::browseURL(htmlFile)
  }
  
}