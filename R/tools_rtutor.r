#' Overwrite the base function data, copy data by default into the calling environment instead of the global environment
data = function(..., envir = parent.frame()) {
  utils:::data(..., envir=envir)
}

#' Calls a function with a specified random.seed
#' @export
with.random.seed <- function (expr, seed = 1234567890) 
{
    old.seed = get(".Random.seed", .GlobalEnv)
    set.seed(seed)
    ret = eval(expr)
    assign(".Random.seed", old.seed, .GlobalEnv)
    runif(1)
    return(ret)
}


#' Like paste0 but returns an empty vector if some string is empty
sc = function(..., sep="", collapse=NULL) {
  str = list(...)
  restore.point("str.combine")
  len = sapply(str,length)
  if (any(len==0))
    return(vector("character",0))
  paste0(...,sep=sep,collapse=collapse)
}

copy.into.env <- function (source = sys.frame(sys.parent(1)), dest = sys.frame(sys.parent(1)), 
    names = NULL, exclude = NULL, overwrite = TRUE, all.names = TRUE, set.fun.env.to.dest = FALSE) 
{
    if (is.null(names)) {
        if (is.environment(source)) {
            names = ls(envir = source, all.names = all.names)
        }
        else {
            names = names(source)
        }
    }
    if (!overwrite) {
        exclude = c(exclude, ls(envir = dest))
    }
    names = setdiff(names, exclude)
    if (is.environment(source)) {
      for (na in names) {
        if (set.fun.env.to.dest) {
          tryCatch({
              val <- get(na,envir=source)
              # Set enclosing environment to dest
              if (is.function(val))
                environment(val) <- dest
              assign(na,val,envir=dest)
            }, error = function(e) {
                message(paste("Variable ", na, " was missing."))
          })                      
        } else {
          tryCatch({
              val <- get(na,envir=source)
              assign(na,val,envir=dest)
            }, error = function(e) {
                message(paste("Variable ", na, " was missing."))
          })          
        }
      }
    } else if (is.list(source)) {
        for (na in names) {
            assign(na, source[[na]], envir = dest)
        }
    }
}

deparse1 = function(call, collapse="") {
  paste0(deparse(call, width=500),collapse=collapse)
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

#' Displays the given text
#' @export
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

examples.str.left.of = function() {
  str.left.of("Hi","i")
  str.left.of("Ha","i")

}

str.left.of = function(str,pattern,..., not.found="NA") {
  pos = str.locate.first(str, pattern,...)
  res = substring(str,1,pos[,1]-1)
  if (not.found=="all") {
    res[is.na(res)] = str[is.na(res)]
  }
  res
}


str.right.of = function(str,pattern,..., not.found="NA") {
  pos = str.locate.first(str, pattern,...)
  res = substring(str,pos[,2]+1,)
  if (not.found=="all") {
    res[is.na(res)] = str[is.na(res)]
  }
  res

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

get.expr.src.lines = function(expr) {
  sapply(attr(expr,"srcref"), function(e) e[1])
}

examples.parse.expr.and.comments = function() {
  code = '
  # compute y
  y = 2+1
  # comment for z
  # another comment
  z = "Hi"
  # last comment
  '
  e = parse(text=code)
  parse.expr.and.comments(code)
}

parse.expr.and.comments = function(code, comment.start = "#") {
 code = sep.lines(code,"\n")
 e = parse(text=code)
 er = get.expr.src.lines(e)
 cr = which(str.starts.with(str.trim(code),comment.start))
 c2e = findInterval(cr,er)+1
 i = 2
 comments = lapply(seq_along(er), function(i) {
   rows = cr[which(c2e==i)]
   if (length(rows)==0)
     return(NULL)
   paste0(str.right.of(code[rows], comment.start), collapse="\n")
 })
 list(expr=e, comments=comments)
}

