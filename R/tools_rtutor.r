deparse1 = function(call) {
  paste0(deparse(call, width=500),collapse="")
}

nlist = function (...) 
{
    li = list(...)
    li.names = names(li)
    names = unlist(as.list(match.call())[-1])
    if (!is.null(li.names)) {
        no.names = li.names == ""
        names(li)[no.names] = names[no.names]
    }
    else {
        names(li) = names
    }
    li
}

display = function (..., collapse = "\n", sep = "") 
{
    str = paste("\n", paste(..., collapse = collapse, sep = sep), 
        "\n", sep = "")
    invisible(cat(str))
}

is.assignment = function(call) {
  if (length(call)==1)
    return(FALSE)
  
  char.op = as.character(call[[1]])
  char.op == "=" | char.op == "<-"
}


# Some tool functions
examples.qlist = function() {
  qlist({x=5;3*x})
}

qlist = function (..., .env = parent.frame()) 
{
  as.list(match.call()[-1])
  #structure(as.list(match.call()[-1]), env = .env, class = "quoted")
}

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
  if (any(val>10^digits))
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


get.txt.blocks = function(txt, start=NULL, end=NULL, start.with=NULL, end.with=NULL, complements=FALSE, inner = TRUE) {
  restore.point("get.txt.blocks")
  if (!is.null(start))
    start.rows = which(str.trim(txt) == start)
  if (!is.null(start.with))
    start.rows = which(str.starts.with(txt, start.with))
  if (!is.null(end))
    end.rows = which(str.trim(txt) == end)
  if (!is.null(end.with))
    end.rows = which(str.starts.with(txt, end.with))
  

  if (length(start.rows)==0) {
    if (complements)
      return(list(txt))
    return(list())
  }
  if (!complements) {  
    str = lapply(1:length(start.rows), function(i) txt[(start.rows[i]+inner):(end.rows[i]-inner)])
  }
  if (complements) {
    n = length(start.rows)
    new.start.rows = c(1,end.rows+inner)
    end.rows = c(start.rows-inner, length(txt))
    start.rows = new.start.rows
    zero.len = start.rows > end.rows
    start.rows = start.rows[!zero.len]
    end.rows = end.rows[!zero.len]
    if (length(start.rows)==0)
      return(list())
    str = lapply(1:length(start.rows), function(i) txt[(start.rows[i]):(end.rows[i])])

  }
  return(str)

  
}
