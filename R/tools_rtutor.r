# Some tool functions


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
  lines = which(substring(txt,1,nchar(command))==command)
  if (length(lines)==0)
    return(NULL)
  val = str_trim(substring(txt[lines],nchar(command)+1))
  data.frame(line=lines, val=val)
} 
