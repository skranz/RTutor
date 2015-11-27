find.funs = function(call, max.level=Inf, level=1) {
  if (level > max.level) return(NULL)
  if (!is.call(call)) return(NULL)
  fun.name = as.character(call[1])
  sub.names = lapply(call[-1], function(e1) {
    find.funs(e1, max.level=max.level, level=level+1)
  })
  names = unique(c(fun.name,unlist(sub.names, use.names=FALSE)))
  names
}