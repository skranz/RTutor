# Some tool functions

str.left.of = function(str,pattern,...) {
  pos = str.locate.first(str, pattern,...)
  substring(str,1,pos[,1]-1)
}


str.right.of = function(str,pattern,...) {
  pos = str.locate.first(str, pattern,...)
  substring(str,pos[,2]+1,)
}


print.example = function(code) {
  cat(paste0("\n",code,"\n"))
  print(eval(parse(text=code,srcfile=NULL)))
}

examples.print.example = function() {
  print.example('rep(c("A","B","C"),length.out = 10)')
}

signif.or.round = function(val, digits=3) {
  if (val>10^digits)
    return(round(val))
  return(signif(val,digits))
}


replace.whisker = function(txt,...,signif.digits=3) {
  require(whisker)
  args = list(...)
  restore.point("replace.whisker")
  for (i in seq_along(args)) {
    if (is.numeric(args[[i]]))
      args[[i]] = signif.or.round(args[[i]],signif.digits)
  }
  whisker.render(txt,args)
}


extract.command = function(txt,command) {
  #restore.point("extract.command")
  lines = which(substring(txt,1,nchar(command))==command)
  if (length(lines)==0)
    return(NULL)
  val = str_trim(substring(txt[lines],nchar(command)+1))
  data.frame(line=lines, val=val, stringsAsFactors=FALSE)
} 

match.call.object = function(call) {
  if (length(call)==1)
    return(call)
  ret = call
  com <- paste0("match.call(", as.character(call[[1]]), ", call=call)")
  tryCatch({
    ret <- eval(parse(text=com,srcfile=NULL))
  }, error = function(e) {}
  )
  ret
}

name.of.call = function(call) {
  if (is.symbol(call))
    return(as.character(call))
  as.character(call[[1]])  
}


examples.code.has.call = function() {
  code.str = "
  plot(5,y=3)
  x*2
  x$a
  x[['a']]
  "
  call.str = "plot(x=5,y=3,main='Hi')"
  
  call.str = 'x[["a"]]'
  
  find.matching.calls(code.str, call.str)
}


find.matching.calls = function(code.str, call.str, call = parse(text=call.str, srcfile=NULL)[[1]]) {
  
  code.li = as.list(parse(text=code.str, srcfile=NULL))
  call = 

  code.names = sapply(code.li, name.of.call)
  call.name = name.of.call(call)
  
  ind = which(code.names %in% call.name)
  if (length(ind)==0) {
    return(NULL)
  }
  return(code.li[ind])
  
  as.list(code)
  
  co = code[[3]]
  co
  co = match.call.object(co)
  
  names(co)
  as.character(co[[1]])
  co[[2]]
  co[[3]]
  class(co[[1]])
  call. = co
  f()
  
  
  names(co)
  
  args(co)
  call_tree(co)
  standardise_call(co)
  str(co)
  class(co)
  co[[2]]
}