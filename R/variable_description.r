make.var.txt.from.df = function(df, cat=TRUE) {
  restore.point("make.var.txt.from.df")

  var = colnames(df) 
  content = rep("", length(var))
  if (!is.null(attributes(df)$var.labels)) {
    content = attributes(df)$var.labels      
  }
  names(content) = var
  empty = which(nchar(content)==0)
  content[empty] = var[empty]
  
  orgvar = var
  
  paste0(c("orgvar",var), " | ", c("var",var), " | ", c("descr", content))
}

make.var.txt.from.files = function(files=NULL, dir=getwd(), ext = c("dta","csv"), cat=TRUE, outfile=NULL) {  
  restore.point("make.var.txt.from.files")
  if (is.null(files)) {
    ext.regexp = paste0("(\\.",ext,"$)", collapse="|")
    files = list.files(path=dir, pattern=ext.regexp, full.names=TRUE)
  } 
  
  txt = lapply(files,make.var.txt.from.file, cat=FALSE)
  txt = paste0(txt, collapse="\n\n")
  if (cat)
    cat(txt)
  
  if (!is.null(outfile)) {
    writeLines(txt, outfile)
    
  }
  txt
}

make.var.txt.from.file = function(file,...,cat=TRUE) {
  restore.point("make.var.txt.from.file")
  table.name = str.left.of(basename(file),".")
  if (str.ends.with(file,".dta")) {
    df = read.dta(file)
  } else if (str.ends.with(file,".csv")) {
    df = read.csv(file,...,nrows=1)
  } else {
    df = read.table(file,...,nrows=1)
  }
  return(make.var.txt.from.df(df,table.name,cat=cat))
}

examples.make.var.txt.from.files = function() {
  txt.file = "D:/libraries/RTutor/examples/temp_shroudedvar.txt"

  make.var.txt.from.files(dir="D:/libraries/RTutor/examples/shroudedfees", outfile = txt.file)
 
  txt.file = "D:/libraries/RTutor/examples/shroudedvar.txt"
  dt = read.var.txt(txt.file)
  
  txt.file = "D:/libraries/RTutor/examples/bank runs variables.txt"
  dt = read.var.txt(txt.file)
  
}

read.var.txt = function(txt.file) {
  restore.point("read.var.txt")
  txt = str.trim(readLines(txt.file, warn=FALSE)) 
  ignore = str.starts.with(txt,"#") | nchar(txt)==0
  txt = txt[!ignore]
  txt
  dt = read.table(textConnection(txt), sep="|", quote="", header=TRUE)
  dt = as.data.table(lapply(dt, str.trim))
  dupl = duplicated(dplyr::select(dt,orgvar,var))
  dt = dt[!dupl,]
  dt
}

get.var.descr.dt = function(vars=colnames(dat),dat=NULL, var.dt = get.ps()$rps$var.dt, no.descr = "- no description -") {
  restore.point("get.var.descr.dt")
  if (is.null(var.dt))
    return(NULL)
  tab = data.frame(var=vars, stringsAsFactors =FALSE)
  res = left_join(tab, as.data.frame(var.dt), by="var")
  res$descr[is.na(res$descr)] = no.descr
  res
}

get.var.descr.markdown = function(...) {
  dt = get.var.descr.dt(...)
  restore.point("get.var.descr.markdown")
  txt = paste0("  - `", dt$var,"`: ", dt$descr, collapse="\n\n")
  txt
}
get.var.descr.html = function(...) {
  txt = get.var.descr.markdown(...)
  restore.point("get.var.descr.html")
  html = markdownToHTML(text=txt)
  html
}



#' Translates variable names of a data frame
#' 
#' uses the variable description provided with the problem set
#' to change variable names from orgvar to var
#' @export
translate.var.names = function(df, orgvar=var.dt$orgvar, var=var.dt$var, var.dt = get.ps()$rps$var.dt) {  
  
  restore.point("translate.var.names")
  
  if (is.null(orgvar) | is.null(var))
    return(df)

  ind = which(orgvar %in% names(df))
  col.ind = match(orgvar[ind],names(df))
  
  names(df)[col.ind] = var[ind]
  df
}

examples.translate.var.names.in.code = function() {
  file = "Bank Runs_sol.Rmd"
  var.file = "D:/libraries/RTutor/examples/bank runs variables.txt"
  translate.var.names.in.code(file=file, var.file=var.file)
}

#' Tool to translate automatically variable names in a .r or .rmd file
#' useful to update 
translate.var.names.in.code = function(
    txt=readLines(file), file=NULL, out.file=file, var.file,
    backup.file = paste0("backup__", sample.int(1e8,1),"__",file)
) {  
  if (!is.null(backup.file)) {
    writeLines(txt, backup.file)
    display("created backup file: ", backup.file)
  }
  var.dt = read.var.txt(txt.file)
  if (is.null(var.dt$orgvar) | is.null(var.dt$var))
    return(invisible(txt))

  for (i in seq_along(var.dt$orgvar)) {
    txt = gsub(var.dt$orgvar[i], var.dt$var[i],txt, fixed=TRUE)
  }
  txt
  if (!is.null(out.file)) {
    writeLines(txt, out.file)
    display("wrote to file: ", out.file)
  }
  invisible(txt)
  
}