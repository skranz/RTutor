examples.is.ggplot.call = function() {
  call = quote(p <- qplot(x=time, y=value))
  call = quote(p <- p + geom_line(x=time, y=value))

  is.ggplot.call(call)
}

# checks whether a call creates a ggplot object without evaluating the call
is.ggplot.call = function(call.obj) {
  restore.point("is.ggplot.call")
  call = call.obj
  if (is.assignment(call)) {
    call = call[[3]]
  }
  na = name.of.call(call)
  if (na == "+") {
    subs = describe.chain.call(call, chain.operator="+")$args
  } else {
    subs = list(call)
  }
  call.names = sapply(subs,name.of.call)
  
  ggplot.start = c("ggplot","qplot", "geom_","scale_","theme_","coord_","facet_","guide_","label_","position_")
  
  #ggplot.names = c("ggplot","qplot")
  
  for (na in call.names) {
    if (any(str.starts.with(na, ggplot.start)))
      return(TRUE)
  }
  return(FALSE)
}