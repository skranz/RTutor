quick.df = function (...) 
{
  df = list(...)
  attr(df, "row.names") <- 1:length(df[[1]])
  attr(df, "class") <- "data.frame"
  df
}


is.true = function(val) {
  if (length(val)==0)
    return(FALSE)
  val[is.na(val)] = FALSE
  return(val)
}
is.false = function(val) {
  if (length(val)==0)
    return(FALSE)
  val[is.na(val)] = TRUE  
  return(!val)
}
new.stud.env = function(chunk.ind, ps = get.ps()) {
  restore.point("new.stud.env")
  
  stud.env = new.env(parent=ps$ps.baseenv)
  #stud.env = 
  stud.env$..chunk.ind <- chunk.ind
  class(stud.env) = c("StudEnv",class(stud.env))
  #cat("\nnew.")
  #print(stud.env)
  stud.env
}

print.StudEnv = function(stud.env,...) {
  cat("stud.env chunk", stud.env$..chunk.ind,":")
  env = stud.env
  class(env) = "environment"
  print(env)
  obj = ls(stud.env)
  if (length(obj)>0) {
    cat("  objects: ", paste0(obj, collapse=", "))
  }
}

copy.stud.env = function(env, new.chunk.ind=env$..chunk.ind, ps = get.ps()) {
  restore.point("copy.stud.env")
  stud.env = as.environment(as.list(env, all.names=TRUE))
  parent.env(stud.env) <- ps$ps.baseenv

  #all.parent.env(ps$ps.baseenv)
  #all.parent.env(globalenv())
  #parent.env(stud.env) <- parent.env(globalenv())
  stud.env$..chunk.ind = new.chunk.ind
  class(stud.env) = c("StudEnv",class(stud.env))
  #cat(" copy.stud.env: ")
  #print(stud.env)
  stud.env
}

as.named.env = function(env, name) {
  env$..name <- name
  class(env) = c("named.env", class(env))
  env
}

print.named.env = function(env,...) {
  cat("\n<named environment:", env$..name, ">")
  print(ls(env))
}

copy.named.env = function(env, name = env$..name) {
  as.named.env(as.environment(as.list(ps$stud.env, all.names=TRUE)), name)
}


all.parent.env = function(env=globalenv()) {
  if (identical(env,emptyenv()))
    return(NULL)
  penv = parent.env(env)
  c(list(penv), all.parent.env(penv))
}

stop.without.error <- function(...){ 
  opt <- options(show.error.messages=FALSE) 
  on.exit(options(opt))
  display(...)
  stop() 
} 

view.in.pane = function(html=NULL, markdown=NULL) {
  library(knitr)
  library(markdown)
  #f <- system.file("examples", "knitr-minimal.Rmd", package = "knitr")
  #knit(f)
  htmlFile <- tempfile(fileext=".html")
  if (!is.null(markdown)) {
    markdownToHTML(text=txt,output=htmlFile)
  } else if (!is.null(html)) {
    writeLines(html,htmlFile)
  }
  rstudio::viewer(htmlFile)
}



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

copy.into.envir <- function (source = sys.frame(sys.parent(1)), dest = sys.frame(sys.parent(1)), 
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
display = function (..., collapse = "\n", sep = "", start.char="\n",end.char="\n") 
{
    str = paste(start.char, paste(..., collapse = collapse, sep = sep), end.char, sep = "")
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
  library(whisker)
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

parse.text.with.source = function(text) {
  restore.point("parse.text.with.source")
  if (is.null(text))
    return(NULL)
  e = parse(text=text,srcfile)
  if (length(e)==0)
    return(NULL)
  str = sapply(attr(e,"srcref"), function(e) paste0(as.character(e), collapse="\n"))
  
  list(expr = e, source = str) 
}


parse.text = function(text) {
  if (is.null(text))
    return(NULL)
  parse(text=text,srcfile=NULL)
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
 if (is.null(code))
   return(list(expr=NULL, comments=NULL))
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

# Find words in the sense of valid function names at the current cursor position
word.at.pos = function(txt, pos) {
  mat.li = str_locate_all(txt,"[A-Za-z0-9._]*")
  i = 1
  li = lapply(seq_along(mat.li), function(i) {
    mat = mat.li[[i]]
    row = which(mat[,1]<=pos[i] & mat[,2]>=pos[i])
    if (length(row)==0)
      return(c(nchar(txt)+1,0))
    mat[row,]
  })
  m = do.call(rbind,li)  

  substring(txt, m[,1],m[,2])
}

examples.my.help = function() {
  my.help(topic="mean")
}

my.help = function (topic, package = NULL, lib.loc = NULL, verbose = getOption("verbose"), help_type = "html",...) 
{
  restore.point("my.help")
  paths <- utils:::index.search(topic, find.package(loadedNamespaces()))  
  paths <- unique(paths)
  rd = utils:::.getHelpFile(paths)
#   library(staticdocs)
#   srd = structure(staticdocs:::set_classes(rd), class = cd("Rd_doc", "Rd"))
#   html = to_html(srd)
  file = tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".html")
  html = tools::Rd2HTML(rd, out=file)
  #writeLines(html, file)
  browseURL(file)
}

my.help.online = function (topic, package = NULL, lib.loc = NULL, verbose = getOption("verbose"), help_type = "html",...) 
{
  url = paste0("http://rdocs-staging.herokuapp.com/#", topic)
  browseURL(url)
}

describe.data = function(dt) {
  li = lapply(dt,describe.var)
  li
  rbindlist(li)
  stat = as.data.frame(do.call("rbind",li))
}

describe.var = function(...) {
  UseMethod("describe.var")
}


describe.var.default= function(v, name=NULL, funs = c("valid.obs","unique.obs")) {
  describe.var.internal(v,name,funs)
}
describe.var.integer = function(v, name=NULL, funs = c("valid.obs","unique.obs", "mean","min","max","sd")) {
  describe.var.internal(v,name,funs)
}
describe.var.numeric = function(v, name=NULL, funs = c("valid.obs","unique.obs", "mean","min","max","sd")) {
  describe.var.internal(v,name,funs)
}
describe.var.date= function(v, name=NULL, funs = c("valid.obs","unique.obs", "min","max")) {
  describe.var.internal(v,name,funs)
}
describe.var.logical= function(v, name=NULL, funs = c("valid.obs","unique.obs","mean")) {
  describe.var.internal(v,name,funs)
}


describe.var.internal = function(v, name=NULL, funs = c("valid.obs","unique.obs", "mean","min","max","sd")) {
  restore.point("describe.var")
  vec = lapply(seq_along(funs), function(i) {
    res = tryCatch(
      suppressWarnings(do.call(funs[[i]],list(v, na.rm=TRUE))),
      error = function(e) NA
    )
    res
  })
  names(vec) = funs
  if (!is.null(name)) {
    c(list(name=name,class=class(vec)[1]),vec)
  } else {
    c(list(class=class(v)[1]),vec)
  }
}




get.top.x.obs = function(v, top.x=5, digits=4) {
  restore.point("get.top.x.obs")
  uv = unique(v)
  shares = (table(v, useNA="ifany") / length(v))
  shares = sort(shares, decreasing = TRUE)
  names = as.character(names(shares))
  if (is.numeric(v)) {
    names = as.character(signif(as.numeric(names),digits))
  }
  names[is.na(names)] = "<NA>"
  top.x = min(top.x, length(shares))
  
  dt = data.frame(var=names[1:top.x], share=as.numeric(shares[1:top.x]))
  dt
}

int.seq = function(from, to) {
  if (from > to)
    return(NULL)
  from:to
}

valid.obs = function (x, na.rm = TRUE) 
{
    return(ifelse(na.rm, sum(!is.na(x)), length(x)))
}

unique.obs = function (x, na.rm = TRUE) 
{
  length(unique(x))
}

move.library = function(name, pos=2) {
  ns = paste0("package:",name)
  suppressWarnings(detach(ns,character.only=TRUE, force=TRUE))
  attachNamespace(name,pos=pos)
}